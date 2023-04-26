from distutils.core import setup
from distutils.extension import Extension
from glob import glob

import os
srcdir = os.environ["PWD"] #< assume makefile is called from base dir  TODO: use cwd()?
libdir = os.path.abspath("lib") #< assume makefile is called from base dir  TODO: use srcdir var?
incdir = os.path.abspath("include") #< assume makefile is called from base dir  TODO: use srcdir var?
os.environ.setdefault("CC", "g++")
os.environ.setdefault("CXX", "g++")

ext = Extension("professor2.core",
                ["pyext/professor2/core.cpp"],
                language="C++",
                depends=glob("../include/*.h"),
                include_dirs=[incdir, os.path.join(srcdir, "pyext", "professor2")],
                extra_compile_args="-std=c++11 -O3 -Wno-unused-but-set-variable -Wno-sign-compare".split(),
                library_dirs=[libdir],
                runtime_library_dirs=[],
                libraries=["Professor2"])

v = os.environ.get("PROF_VERSION", "2.3.3")
setup(name = "professor2",
      version=v,
      ext_modules = [ext],
      packages = ["professor2", "professor2/ml", "professor2/misc"],
      package_dir = {"": "pyext"},
      description="Professor version 2",
      author="Professor collaboration",
      author_email="hschulz@fnal.gov",
      url="https://bitbucket.org/iamholger/professor")
