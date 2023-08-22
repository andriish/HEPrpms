from __future__ import print_function
from setuptools import setup, Extension
from glob import glob

import os
srcdir = os.path.relpath("./")
libdir = os.path.relpath("../lib")
incdir = os.path.relpath("../include")
os.environ.setdefault("CC", "g++")
os.environ.setdefault("CXX", "g++")

# print("****", srcdir)
# print("****", libdir)
# print("****", incdir)

ext = Extension("professor2.core",
                ["professor2/core.cpp"],
                language="C++",
                depends=glob("../include/*.h"),
                include_dirs=[incdir, os.path.join(srcdir, "pyext", "professor2")],
                extra_compile_args="-std=c++11 -O3 -Wno-unused-but-set-variable -Wno-sign-compare".split(),
                library_dirs=[libdir],
                runtime_library_dirs=[],
                libraries=["Professor2"])

v = os.environ.get("PROF_VERSION", "X.Y.Z")
setup(name = "professor2",
      version=v,
      ext_modules = [ext],
      packages = ["professor2", "professor2/ml", "professor2/misc"],
      description="Professor version 2",
      author="Professor collaboration",
      author_email="andy.buckley@cern.ch",
      url="https://gitlab.com/hepcedar/professor")
