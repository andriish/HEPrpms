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

"""Provides utilities to handle the process execution environment.

Read-only attributes (do not modify these):
   mpi:             The MPI_WORLD communicator (if available, otherwise None)
   pool:            The multiprocessing pool (if available, otherwise None)
   size (int):      The number of parallel MPI processes
   parallel (bool): Equivalent to size >= 2.
   rank (int):      The process number (the rank in MPI terms)
   master (bool):   True for the master process (equivalent to rank == 0)
   num_cores (int): The number of available cores. In MPI mode this is the
                    number of allocated slots (given by the universe size).
                    Otherwise this is the number of available physical cores.

Customizable attributes:
   name (str):          The name of the process. By default it is initialized to
                        "master" and "process rank" for ranks >= 1.
   name_tag (function): A function that can be called with name as its single
                        argument and returns a string. It is used to construct
                        the "(name) " tag in log messages.

   debug (bool): If set to True produces [debug] messages to make it easier to
                 follow the execution flow (defaults to False).

If use_mpi is True or an OpenMPI environment is detected (i.e. the program
was called via mpirun), it tries to load the mpi4py module which initializes
the MPI environment. If the mpi4py module is unavailable, or the
initialization fails, it exits the program with a corresponding error
message. If successful, the mpi attribute provides the MPI_WORLD
communicator.

Otherwise, mpi is None, and the program runs in single-process or serial
mode. Note that serial mode only means that the main program code is run by
a single process. Parallel execution can still be performed via child
processes spawned by Subprocess.Popen or multiprocessing.Process.

In client code, this should typically be used as follows

import process

... # figure out whether to use mpi or not
process.init(True or False)

if process.master:
   ... # common code for serial mode or mpi root process

if process.parallel:
   runParallel(...) # run parallel (MPI-aware) code
else:
   runSingle(...)   # run serial or multiprocessing alternative
"""

import os
import sys
import atexit
import atexit
import atexit
import atexit
import traceback

mpi = None
pool = None
size = 1
parallel = False
rank = 0
master = True
num_cores = 1

name = "master"
name_tag = None

debug = False

__this = sys.modules[__name__]
__initialized = False
__return_value = None

#-------------------------------------------------------------------------------
# Initialization
#-------------------------------------------------------------------------------

def init(use_mpi = False, quiet = False):
   """Initialize the process environment.

   Parameters:
      use_mpi (bool): Whether to initialize the MPI environment.
      quiet (bool): Whether to suppress the log message on the running mode.

   To use MPI, this must be called on all processes with True. It loads the
   mpi4py module. If mpi4py is not available or the MPI environment failed to
   initialize, the program exits with an error message.

   If use_mpi is False, mpi4py is not initialized and the process is treated as
   an independent master process, which is essentially the same state as before
   any call to init(). The main effect of calling init() is to explicitly
   prevent a later call with use_mpi. In addition, it tries to load the
   multiprocessing module and set num_cores. (Failure to do so is not treated
   as an error and num_cores is set to None.)

   Note: This only has an effect on its very first call. Further calls are
   allowed (to allow for unchecked calls) but are ignored (because it is
   impossible to switch back and forth between MPI).

   Note: If the OpenMPI environment variables are detected, which means the
   program was started with OpenMPI's mpirun (or similar), for convenience
   init(True) is automatically called when the module is loaded. For other MPI
   implementations init(True) has to be called explicitly.
   """
   if __initialized:
      if debug:
         log_debug("Ignoring a call to process.init({0}), ".format(use_mpi)
                   + "process was already initialized with use_mpi = {0}.".format(bool(mpi)))
      return

   if use_mpi:
      try:
         from mpi4py import MPI
      except Exception:
         log_error("Could not load mpi4py module. Please ensure mpi4py is "
                   "available for running with MPI.")
         sys.exit(1)

      if not MPI.Is_initialized():
         log_error("The mpi4py module was loaded but failed to initialize the "
                   "MPI environment.\nPlease ensure that mpi4py and MPI are "
                   "correctly installed.", False)
         sys.exit(1)

      __this.mpi = MPI.COMM_WORLD

      __this.size = mpi.Get_size()
      __this.parallel = size >= 2

      __this.rank = mpi.Get_rank()
      __this.master = (rank == 0)

      __this.num_cores = mpi.Get_attr(MPI.UNIVERSE_SIZE)

      if master:
         __this.name = "master"
      else:
         __this.name = "process {0}".format(rank)
      __this.name_tag = __parallel_name_tag

      __this._or = MPI.LOR
      __this._and = MPI.LAND

      sys.excepthook = __mpi_excepthook

      if master and not quiet:
         log("Running with {0} parallel MPI processes on {1} slots.".format(size, num_cores))

   else:
      try:
         import multiprocessing
         __this.num_cores = multiprocessing.cpu_count()
      except Exception:
         log_warning("Could not determine the number of available cores.")
      if not num_cores:
         __this.num_cores = 1
      if not quiet:
         if num_cores >= 2:
            log("Running in serial/multiprocessing mode on up to {0} cores.".format(num_cores))
         else:
            log("Running in serial/multiprocessing mode on single core.")

   __this.__initialized = True

   # Make sure all processes are initialized before continuing.
   barrier()

#-------------------------------------------------------------------------------
# Pool handling
#-------------------------------------------------------------------------------

def open_pool(pool_size = 0):
   """Open the process.pool with pool_size worker processes.

   If pool_size is not given or 0, num_cores is used. If pool_size is larger
   than the number of cores a warning is logged.

   After this call, the process.pool is defined and can be used. To close the
   pool, use close_pool(), do not call pool.close() directly.

   In MPI mode, this does nothing and silently returns.
   """
   if mpi:
      return
   try:
      import multiprocessing
      if not pool_size:
         pool_size = num_cores
      elif pool_size > num_cores:
         log_warning("Using a pool of {0} processes on only {1} available core(s).".format(pool_size, num_cores), False)
      __this.size = pool_size
      __this.parallel = (size >= 2)
      __this.pool = multiprocessing.Pool(pool_size, __init_worker)
      __this.name_tag = __parallel_name_tag

   except Exception:
      log_warning("Could not open multiprocessing pool.")
      __this.pool = None
      __this.size = 1
      __this.parallel = False

def close_pool():
   """Close the process.pool.

   Silently returns if the pool was not open.
   """
   if pool:
      pool.close()
      pool.join()
      __this.pool = None
      __this.size = 1
      __this.parallel = False
      __this.name_tag = __empty_name_tag

def __init_worker():
   import multiprocessing
   __this.rank = int(multiprocessing.current_process().name[-1])
   __this.name = "worker {0}".format(rank)
   __this.name_tag = __parallel_name_tag

#-------------------------------------------------------------------------------
# Process flow control, error and exit handling
#-------------------------------------------------------------------------------

def return_value():
   return __return_value

def signal_exit(return_value = 0):
   """Signal to exit the program with return_value (defaults to 0).

   The next call to exit_if_signal() will exit the program on all processes. An
   exit can be signaled by one or more processes.

   Note: Another call to signal_exit replaces the previously scheduled
   return_value. A process can undo its own desire to exit by explicitly
   calling signal_exit(None).
   """
   __this.__return_value = return_value
   if debug:
      log_debug("Asking to exit with value: {0}.".format(return_value))

def exit_if_signal():
   """Perform an orderly synchronized exit.

   In parallel mode, immediately exits the program if one or more processes
   have previously called signal_exit(). Each process exits with its current
   return value. Processes that did not signal the exit return with None.

   Since this function provides an orderly exit with clean up it should be
   preferred if at all possible to an unsynchronized dirty mpi.Abort().

   In serial mode, simply calls sys.exit() with the signaled return value.

   The program continues if there were no previous calls to signal_exit
   (with a return value other than None).

   Note: This acts as a barrier and must be reached by all processes.
   """
   if parallel:
      do_exit = mpi.allreduce(__return_value is not None, op = _or)
      barrier()
   else:
      do_exit = __return_value is not None

   if do_exit:
      atexit.register(__scheduled_exit_function)
      sys.exit(__return_value)

def exit(return_value = 0):
   """Perform an immediate synchronized exit.

   Exits the master process with return_value, while all other processes exit
   with their current return value (set via signal_exit or signal_error).

   This is equivalent to calling signal_exit(return_value) on the master
   process immediately followed by exit_if_signal().

   Note: This acts as a barrier and must be reached by all processes.
   """
   if master:
      signal_exit(return_value)
   exit_if_signal()

def signal_error(message = "", include_exc_info = True):
   """Logs the error and signals for exit with return_value 1."""
   log_error(message, include_exc_info)
   signal_exit(1)

def exit_if_error(message = ""):
   """Perform an orderly synchronized exit in case of a previous error.

   In parallel mode, immediately exits the program if one or more processes
   have previously called signal_error() or signal_exit with a nonempty return
   value (one that converts to True). Each process exits with its current return
   value. If message is nonempty, it will be logged as an additional error
   message by master.

   Since this function provides an orderly exit with clean up it should be
   preferred if at all possible to an unsynchronized dirty mpi.Abort().

   In serial mode, simply calls sys.exit() with the current return value.

   The program continues if there were no previous calls to signal_error or
   signal_exit with nonempty return value.

   Note: This acts as a barrier and must be reached by all processes.
   """
   if has_error():
      if master:
         log_error(message, False)
      atexit.register(__scheduled_exit_function)
      sys.exit(__return_value)

def has_error():
   """Return True if any process had an error.

   Note: This acts as a barrier and must be reached by all processes.
   """
   if parallel:
      error = mpi.allreduce(bool(__return_value), op = _or)
      barrier()
   else:
      error = bool(__return_value)

   return error

def barrier():
   """Plain barrier that must be reached by all processes before execution can
   continue.
   """
   if mpi:
      if debug:
         log_debug("Waiting for the rest of my family ...")
      mpi.Barrier()

def check_size(min_size):
   """Check that at least min_size processes are available.

   Exits with an error message if not enough processes are available. Without
   MPI this will always fail for min_size >= 2.

   Note: This acts as a barrier and must be reached by all processes.
   """
   if size < min_size:
      if master:
         signal_error(
            ("Only {0} MPI processes available, but need at least {1}.\n"
            + "Please relaunch with 'mpirun -n {1}' (or equivalent).").format(size, min_size),
            False)
   exit_if_error()

#-------------------------------------------------------------------------------
# Logging functions
#-------------------------------------------------------------------------------

def log_debug(message):
   msg = format_message(message, "[debug] ")
   if msg:
      sys.stdout.write(msg)
      sys.stdout.flush()

def log(message):
   msg = format_message(message)
   if msg:
      sys.stdout.write(msg)

def format_message(message, tag = ""):
   if not message:
      return message
   msg = tag + name_tag(name) + message
   if not msg.endswith("\n"):
      msg += "\n"
   return msg

def log_warning(message = "", include_exc_info = True):
   msg = format_error_message("[WARNING] " + name_tag(name),
                              message, include_exc_info)
   if msg:
      sys.stderr.write(msg)

def log_error(message = "", include_exc_info = True):
   msg = format_error_message("[ERROR] " + name_tag(name),
                              message, include_exc_info)
   if msg:
      sys.stderr.write(msg)

def format_error_message(tag, message = "", include_exc_info = True):
   msg = ""
   if include_exc_info:
      exc_type, value, tb = sys.exc_info()
      if exc_type:
         msg = tag
         if not debug:
            msg += ''.join(traceback.format_exception_only(exc_type, value))
         else:
            msg += ''.join(traceback.format_exception(exc_type, value, tb))

   if message:
      msg += tag + message
   if msg and not msg.endswith("\n"):
      msg += "\n"
   return msg

#-------------------------------------------------------------------------------
# Private helper functions
#-------------------------------------------------------------------------------

def __empty_name_tag(name):
   return ""

def __parallel_name_tag(name):
   return "(" + str(name) + ") "

def __empty_exit_function():
   if debug:
      log_debug("Normal exit ...")

def __mpi_unscheduled_exit_function():
   if debug:
      log_debug("Unscheduled exit, aborting ...")
   mpi.Abort()

def __scheduled_exit_function():
   if debug:
      log_debug("Happy family exit ...")

def __mpi_excepthook(exception_type, value, traceback):
   sys.__excepthook__(exception_type, value, traceback)
   if debug:
      log_debug("Uncaught exception ...")
   atexit.register(__mpi_unscheduled_exit_function)

name_tag = __empty_name_tag
atexit.register(__empty_exit_function)

#-------------------------------------------------------------------------------
# auto-initialize if we were started with OpenMPI mpirun
if os.getenv("OMPI_COMM_WORLD_SIZE") is not None:
   init(True)
