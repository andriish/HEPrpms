#-------------------------------------------------------------------------------
# Author(s):
#    Frank Tackmann
#
# Copyright:
#    Copyright (C) 2018 Geneva Collaboration
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#-------------------------------------------------------------------------------

"""Provides the execute function to execute a list of commands.

In MPI mode, parallelized commands are run on all processes. Others are run
on a single process. Those that can run in parallel (i.e. same serial
executable with different arguments) are executed in parallel on different
processes. All commands require an executable with a Python main function.

In non-MPI mode, parallelized commands are run by themselves. Others are run
as subprocess. Those that can run in parallel are executed in parallel as
multiple subprocesses.

In all cases, the output is captured and written to log files. The next command
is only executed if the previous command (with one ore more parallel processes)
completed successfully.
"""

import os
import sys
import subprocess
import io

import process
import config
import utils


def execute(commands, quiet):
   if process.master:
      # Check that we have enough processes or cores for the number of commands
      # to run in parallel.
      num_runs = max(len(cmd[2]) for cmd in commands)

      if process.mpi:
         if not quiet:
            process.log("Performing up to {0} parallel runs via MPI.".format(num_runs))
         if num_runs > process.size:
            process.signal_error(
               ("Only {0} MPI processes available, but need at least {1}.\n"
               + "Please relaunch with 'mpirun -n {1}' (or equivalent).").format(process.size, num_runs),
               False)
      else:
         if not quiet:
            process.log("Performing up to {0} parallel runs via multiprocessing.".format(num_runs))
         if num_runs > process.num_cores:
            process.log_warning(
               ("You appear to have only {0} cores available. You should "
               + "probably choose --num-runs <= {0}").format(process.num_cores),
               False)

   process.exit_if_error()

   if process.master and not quiet:
      process.log("Sit back and relax, this might take some time.\n"
                  "The .log files of individual runs are in the log/ "
                  "subdirectory of the respective output directory.")

   if process.parallel:
      cmds = process.mpi.bcast(commands)
   else:
      cmds = commands

   for cmd in cmds:
      name = cmd[0]
      executable = cmd[1]
      if executable.startswith("geneva-"):
         executable = os.path.join(sys.path[0], executable)
      args = cmd[2]
      logs = cmd[3]
      assert len(args) == len(logs)

      if process.master and not quiet:
         process.log("Executing " + name + " ...")
         sys.stdout.flush()

      # Figure out what and how to call.
      try:
         module = utils.load_executable(executable)
         main_func = getattr(module, 'main')
         try:
            is_parallelized = module.is_parallelized
         except AttributeError:
            is_parallelized = False
      except (IOError, OSError):
         process.signal_error("Could not load executable: " + executable)
      except Exception:
         is_parallelized = False
         if process.mpi:
            process.signal_error("Could not load Python main function of: "
                                 + executable)

      process.exit_if_error()

      if process.mpi:
         if is_parallelized:
            assert len(args) == 1
            _execute_main_func_mpi(executable, main_func, args[0], logs[0])
         else:
            if process.rank < len(args):
               _execute_main_func(executable, main_func, args[process.rank], logs[process.rank])
      else:
         if is_parallelized:
            assert len(args) == 1
            _execute_main_func(executable, main_func, args[0], logs[0])
         else:
            _execute_subprocess(executable, args, logs)

      process.exit_if_signal()

   if process.master and not quiet:
      process.log("done.")
      sys.stdout.flush()

#-------------------------------------------------------------------------------
# private functions
#-------------------------------------------------------------------------------

def _execute_main_func(executable, main_func, args, log):
   """Execute main function on a single process."""
   with utils.open_outfile(log) as out:
      out.write("Executing command:\n" + executable + " " + args + "\n--- log output ---\n")

   with open(log, "a") as out:
      errmsg = _call_function(main_func, args, out)
      _finish(errmsg, log, out)

def _execute_main_func_mpi(executable, main_func, args, log):
   """Execute parallel (MPI-aware) main function on multiple processes."""
   if process.master:
      with utils.open_outfile(log) as out:
         out.write("Executing command:\n" + executable + " " + args + "\n--- log output ---\n")

   output, errmsg = _call_function(main_func, args)

   # Combine the output and error messages.
   output_all = process.mpi.gather(output)

   if process.has_error():
      errmsg_all = process.mpi.gather(process.format_message(errmsg))
      if process.master:
         errmsg = "Some processes failed:\n" + "".join(list(filter(bool, errmsg_all)))

   if process.master:
      with open(log, "a") as out:
         out.write("".join(output_all))
         _finish(errmsg, log, out)

def _execute_subprocess(executable, arguments, logs):
   """Execute executable as one or more subprocesses."""
   jobs = []
   outs = []
   for job_id in range(0, len(arguments)):
      log = logs[job_id]
      args = arguments[job_id]
      with utils.open_outfile(log) as out:
         out.write("Executing command:\n" + executable + " " + args + "\n--- log output ---\n")

      out = open(log, "a")
      command = [executable] + args.split()
      jobs.append(subprocess.Popen(command, stdout = out, stderr = subprocess.STDOUT,
                                   env = config.getEnvironment()))
      outs.append(out)

   for job_id in range(0, len(jobs)):
      job = jobs[job_id]
      log = logs[job_id]
      out = outs[job_id]
      errmsg = ""
      try:
         returnval = job.wait()
         if returnval:
            errmsg = "Subprocess failed with return value: {0}".format(returnval)
            process.signal_exit(1)
      except Exception:
         errmsg = process.format_error_message("Subprocess failed with ")
         process.signal_exit(1)
      except SystemExit as exit:
         if exit.code:
            errmsg = "Subprocess failed with exit code: {0}".format(exit.code)
            process.signal_exit(exit.code)
      finally:
         out.close()

      with open(log, "a") as out:
         _finish(errmsg, log, out)

def _call_function(function, args, logfile = None):
   """Call function, redirecting stdout and stderr either to the given logfile
   or to a stream.
   """
   try:
      errmsg = ""

      if logfile:
         # Redirect at file descriptor level directly to logfile. This should
         # also catch stdout from C and Fortran libraries and subprocesses.
         stdout_fd = sys.stdout.fileno()
         stdout_fd_saved = os.dup(stdout_fd)
         sys.stdout.flush()
         os.dup2(logfile.fileno(), stdout_fd)

         stderr_fd = sys.stderr.fileno()
         stderr_fd_saved = os.dup(stderr_fd)
         sys.stderr.flush()
         os.dup2(logfile.fileno(), stderr_fd)
      else:
         # Just redirect at the level of Python streams.
         tmp = io.BytesIO()
         stdout_saved = sys.stdout
         stderr_saved = sys.stderr
         sys.stdout = tmp
         sys.stderr = tmp

      returnval = function(args.split())
      if returnval:
         errmsg = "Failed with return value: {0}".format(returnval)
         process.signal_exit(returnval)
   except Exception:
      errmsg = process.format_error_message("Failed with ")
      process.signal_exit(1)
   except SystemExit as exit:
      if exit.code:
         errmsg = "Failed with exit code: {0}".format(exit.code)
      process.signal_exit(exit.code)
   finally:
      # Restore stdout and stderr.
      if logfile:
         sys.stdout.flush()
         os.dup2(stdout_fd_saved, stdout_fd)
         os.close(stdout_fd_saved)

         sys.stderr.flush()
         os.dup2(stderr_fd_saved, stderr_fd)
         os.close(stderr_fd_saved)
      else:
         sys.stdout = stdout_saved
         sys.stderr = stderr_saved

   if logfile:
      return errmsg
   else:
      return tmp.getvalue(), errmsg

def _finish(errmsg, log, out):
   """Finish up."""
   if errmsg:
      out.write("--- failed ---\n" + errmsg)
      if not errmsg.endswith("\n"):
         out.write("\n")

      # Notify of errors even in quiet mode.
      process.log(errmsg)
      process.log("For possible details see the log file: " + log)
   else:
      out.write("--- done ---\n")
