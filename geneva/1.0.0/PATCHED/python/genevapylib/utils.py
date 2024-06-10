#-------------------------------------------------------------------------------
# Author(s):
#    Simone Alioli, Frank Tackmann
#
# Copyright:
#    Copyright (C) 2017, 2018 Geneva Collaboration
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#
# Description:
#    Various utility functions
#-------------------------------------------------------------------------------

import os
import sys
from importlib.machinery import SourceFileLoader
import shutil

from . import process

#-------------------------------------------------------------------------------
# Executable handling
#-------------------------------------------------------------------------------

# utility command that allows to see output of subprocess in real time
def run_command(command):
   import subprocess
   try:
      p = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
      while True:
         output = p.stdout.readline()
         if output == '' and p.poll() is not None:
            break
         if output:
            print(output.strip())
      rc = p.poll()
      return rc
   except Exception as error:
      process.log("Failed to execute: " + command)
      sys.exit(1)
   return

def load_executable(executable):
   """Load executable as module without producing a compiled version of it.

   Parameters:
      executable (str): Name of the Python script to load, can include a
                        relative or absolute path as necessary.

   Exceptions in case executable cannot be loaded are propagated to the caller.
   """
   try:
      tmp = sys.dont_write_bytecode
      sys.dont_write_bytecode = True
      return SourceFileLoader(os.path.basename(executable), executable).load_module()
   finally:
      sys.dont_write_bytecode = tmp

def parse_args(executable, command_line):
   """Parse command_line for executable.

   Parameters:
      executable (str):   Name of the Python script.
      command_line (str): The command line string to parse.

   Uses the executable's parse_args function to parse the command_line. If the
   executable cannot be loaded or does not have a parse_args function, this
   causes the program to exit. Otherwise the parsed arguments are returned.
   Any exceptions during parsing are forwarded to the caller.

   """
   try:
      module = load_executable(executable)
      parse_args = getattr(module, "parse_args")
   except Exception:
      process.log_error("Could not parse command line: "
                        + executable + " " + command_line)
      sys.exit(1)
   return parse_args(command_line.split())

def check_options(executable, command_line):
   """Check that command_line is valid for executable.

   Parameters:
      executable (str):   Name of the Python script.
      command_line (str): The command line string to parse.

   Calls parse_args(executable, command_line) to try parsing the command_line
   for executable. If the parsing fails for any reason, the program exits.
   If the parsing itself raises an exception, it tries to call parse_args with
   the --help option to generate a help message.
   """
   try:
      parse_args(executable, command_line)
      return True
   except Exception:
      if not any(x in command_line.split() for x in ["-h", "--help"]):
         parse_args(executable, "--help")
   sys.exit(1)

#-------------------------------------------------------------------------------
# Formatting
#-------------------------------------------------------------------------------

# Formats the given path to ensure that it has a trailing path separator
# (unless it is empty).
def format_path(path):
   if path and not path.endswith(os.sep):
      return path + os.sep
   else:
      return path

# Joins the given path segments with path separators. The result will have a
# trailing path separator (unless it is empty).
def join_paths(*paths):
   return format_path(os.path.join(*paths))

# Returns a list of files that is constructed as
# stem + _i + ext with the label i running over the range [first, last].
def get_filenames(stem, ext, first, last):
   return [stem + format_tag(i) + ext for i in range(first, last + 1)]

# Formats the given name tag to ensure it has a leading '_' separator. If the
# tag is empty or already has one or more leading '_' it is left unchanged.
def format_tag(tag):
   tag = str(tag)
   if tag and not tag.startswith('_'):
      return '_' + tag
   else:
      return tag

#-------------------------------------------------------------------------------
# File handling
#-------------------------------------------------------------------------------

def ensure_dir_exists(directory, log_error = True):
   """Ensure that the directory dir exists.

   Checks if dir exists, and if not attempts to create it.

   In case of failure, logs an error message if log_error is True and raises
   an exception (OSError).
   """
   if directory and not os.path.isdir(directory):
      try:
         os.makedirs(directory)
      except OSError:
         if not os.path.isdir(directory):
            if log_error:
               process.log_error("Could not create directory: " + directory)
            raise

def check_file_exists(filename, log_warning = True):
   """Check if filename is an existing file.

   Returns True if filename exists and otherwise False. If log_warning is True
   logs a warning message that the file does not exist.

   A typical use case is to filter out nonexisting files from a list of input
   files as follows:

   infiles = list(filter(check_file_exists, infiles))
   """
   if os.path.isfile(filename):
      return True

   if log_warning:
      process.log_warning("Skipping nonexisting file: " + filename, False)
   return False;

def open_outfile(filename, append = False):
   """Open a (new) file for output, creating any intermediate directories.

   The filename can include an absolute or relative path. The file and any
   intermediate directories are created if necessary. The file is opened for
   writing only.

   An existing file at the given location will be overwritten if append is
   False (the default) or be appended to if append is True.

   Possible exceptions (IOError or OSError) are propagated to the caller.
   """
   ensure_dir_exists(os.path.dirname(filename))
   if append:
      return open(filename, 'a')
   else:
      return open(filename, 'w')

def remove_files(filenames, file_type = "file"):
   """Tries to remove the files with the given filenames.

   Logs a message if successful and a warning if not for each file, where the
   file_type is included in the message.
   """
   for name in filenames:
      try:
         os.remove(name)
         process.log("Removed " + file_type + ": " + name)
      except OSError:
         if os.path.exists(name):
            process.log_warning("Could not remove " + file_type + ": " + name)

def backup_files(filenames, backup_path, keep, file_type = "file"):
   """Tries to backup the files with the given filenames to backup_dir.

   If keep is True, the files are copied, otherwise they are moved. The
   backup_dir is created if necessary.

   Logs a message if successful for each file. If anything fails, an error is
   logged and the exception is propagated to the caller.
   """
   ensure_dir_exists(backup_path)

   if keep:
      for name in filenames:
         try:
            shutil.copy2(name, backup_path)
            process.log("Copied " + file_type + ": " + name)
         except Exception:
            process.log_error("Could not copy " + file_type + ": " + name)
            raise
   else:
      for name in filenames:
         try:
            shutil.move(name, os.path.join(backup_path, os.path.basename(name)))
            process.log("Moved " + file_type + ": " + name)
         except Exception:
            process.log_error("Could not move " + file_type + ": " + name)
            raise
