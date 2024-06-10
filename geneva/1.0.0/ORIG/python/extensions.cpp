//------------------------------------------------------------------------------
/// \file extensions.cpp
//
// Author(s):
//    Simone Alioli
//
// Copyright:
//    Copyright (C) 2017, 2018 Geneva Collaboration
//
//    This file is part of the Geneva MC framework. Geneva is distributed under
//    the terms of the GNU General Public License version 3 (GPLv3), see the
//    COPYING file that comes with this distribution for details.
//    Please respect the academic usage guidelines in the GUIDELINES file.
//
// Description:
//    Geneva Python C++ extensions library
//------------------------------------------------------------------------------

#include "Geneva/Wrapper/GenevaWrapper.hpp"

#include "Geneva/Driver/Geneva.hpp"

#include "Geneva/Core/UI/OptionMap.hpp"
#include "Geneva/Core/UI/Exception.hpp"
#include "Geneva/Core/UI/Log.hpp"

#include <Python.h>

#include <string>

namespace Geneva
{

//------------------------------------------------------------------------------
static PyObject* get_banner(PyObject* self, PyObject* args)
{
   return PyString_FromString(Geneva::header().c_str());
}

//------------------------------------------------------------------------------
static PyObject* get_options(PyObject* self, PyObject* args)
{
   char* command;
   if (!PyArg_ParseTuple(args, "s", &command))
      return NULL;
   PyObject* d = PyDict_New();
   try {
      Log::setVerbosity(Log::kWarning);
      OptionMap opt(Geneva(std::string(command)).optionMap());
      for (OptionMap::const_iterator it = opt.begin(); it != opt.end(); ++it) {
         PyDict_SetItemString(d, it->first.c_str(), PyString_FromString(it->second->getValueStr().c_str()));
      }
   } catch (...) {
      mainExceptionHandler();
   }
   return d;
}

//------------------------------------------------------------------------------
static PyObject* setup_xsec(PyObject* self, PyObject* args)
{
   char* command;
   if (!PyArg_ParseTuple(args, "s", &command))
      return NULL;
   return Py_BuildValue("i", GenevaWrapper::instance().setupCrossSection(std::string(command)));
}

//------------------------------------------------------------------------------
static PyObject* generate_events(PyObject* self, PyObject* args)
{
   char* command;
   char* name;

   if (!PyArg_ParseTuple(args, "ss", &name, &command))
      return NULL;

   return Py_BuildValue("i", GenevaWrapper::instance().generateEvents(std::string(name), std::string(command)));
}

#ifdef geneva_USE_PYTHIA8
//------------------------------------------------------------------------------
static PyObject* lhef_analyze(PyObject* self, PyObject* args)
{
   char* command;
   char* name;

   if (!PyArg_ParseTuple(args, "ss", &name, &command))
      return NULL;

   return Py_BuildValue("i", GenevaWrapper::instance().analyzeLhef(std::string(name), std::string(command)));
}

//------------------------------------------------------------------------------
static PyObject* shower_pythia8(PyObject* self, PyObject* args)
{
   const char* command;
   char* name;

   if (!PyArg_ParseTuple(args, "ss", &name, &command))
      return NULL;

   return Py_BuildValue("i", GenevaWrapper::instance().showerPythia8(std::string(name), std::string(command)));
}
#endif //geneva_USE_PYTHIA8


#ifdef geneva_USE_RIVET
//------------------------------------------------------------------------------
static PyObject* rivet_analyze(PyObject* self, PyObject* args)
{
   char* command;
   char* name;

   if (!PyArg_ParseTuple(args, "ss", &name, &command))
      return NULL;

   return Py_BuildValue("i", GenevaWrapper::instance().analyzeRivet(std::string(name), std::string(command)));
}
#endif // geneva_USE_RIVET


//------------------------------------------------------------------------------
static PyMethodDef cpp_extensions_methods[] = {
   {"get_banner", get_banner, METH_VARARGS, "returns geneva banner"},
   {"get_options", get_options, METH_VARARGS, "returns geneva options"},
   {"setup_xsec", setup_xsec, METH_VARARGS, "setup geneva cross section"},
   {"generate_events", generate_events, METH_VARARGS, "run main event generation"},
#ifdef geneva_USE_PYTHIA8
   {"lhef_analyze", lhef_analyze, METH_VARARGS, "analyze LHEF events"},
   {"shower_pythia8", shower_pythia8, METH_VARARGS, "shower LHEF events with Pythia8"},
#endif
#ifdef geneva_USE_RIVET
   {"rivet_analyze", rivet_analyze, METH_VARARGS, "analyze HepMC events with Rivet"},
#endif
   {NULL, NULL, 0, NULL}
};

//------------------------------------------------------------------------------
PyMODINIT_FUNC initcpp_extensions(void)
{
   Py_InitModule("cpp_extensions", cpp_extensions_methods);
}

} // namespace Geneva
