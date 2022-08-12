// -*- C++ -*-
//
// DynamicLoader.cc is a part of ThePEG - Toolkit for HEP Event Generation
// Copyright (C) 1999-2019 Leif Lonnblad
//
// ThePEG is licenced under version 3 of the GPL, see COPYING for details.
// Please respect the MCnet academic guidelines, see GUIDELINES for details.
//
//
// This is the implementation of the non-inlined, non-templated member
// functions of the DynamicLoader class.
//

// macro is passed in from -D compile flag
#ifndef THEPEG_PKGLIBDIR
#error Makefile.am needs to define THEPEG_PKGLIBDIR
#endif

#include "DynamicLoader.h"
#include "ThePEG/Utilities/StringUtils.h"
#include "ThePEG/Config/algorithm.h"
#include "config.h"

#ifdef ThePEG_HAS_DLOPEN
#include <dlfcn.h>
#endif

#include <cstdlib>

#ifdef ThePEG_HAS_FENV
#include <fenv.h>
#endif

using namespace ThePEG;

void DynamicLoader::dlname(string sofile) {
  if ( StringUtils::suffix(sofile) == "so" ) {
    string lafile = StringUtils::remsuf(sofile) + ".la";
    ifstream is(lafile.c_str());
    string line;
    while ( getline(is, line) ) {
      if ( line.find("dlname='") != string::npos ) {
	int pos = line.find('\'') + 1;
	int l = line.rfind('\'') - pos;
	sofile = StringUtils::basename(sofile);
	versionMap[sofile] = line.substr(pos, l);
      }
    }
  } else if ( StringUtils::suffix(StringUtils::remsuf(sofile)) == "so" ) {
    versionMap[StringUtils::basename(StringUtils::remsuf(sofile))] =
      StringUtils::basename(sofile);
  }

}

string DynamicLoader::dlnameversion(string libs) {
  string ret;
  do {
    string soname = StringUtils::car(libs);
    string dir = StringUtils::dirname(soname);
    if ( dir.length() ) dir += '/';
    libs = StringUtils::cdr(libs);
    if ( versionMap.find(StringUtils::basename(soname)) != versionMap.end() )
      ret += dir + versionMap[StringUtils::basename(soname)] + " ";
    else
      ret += soname + " ";
  } while ( libs.length() );
  return StringUtils::stripws(ret);
}

bool DynamicLoader::loadcmd(string file) {
#ifdef ThePEG_HAS_DLOPEN
  dlname(file);
#ifdef ThePEG_HAS_FENV
  int oldfpe = fegetexcept();
#endif
  bool ret = dlopen(file.c_str(), RTLD_LAZY|RTLD_GLOBAL) != 0;
#ifdef ThePEG_HAS_FENV
  feenableexcept(oldfpe);
#endif
  if ( !ret ) lastErrorMessage += string(dlerror()) + string("\n");
  return ret;
#else
#error ThePEG can only be run on platforms which support
#error dynamic loading.
  return false;
#endif
}

void DynamicLoader::appendPath(string path) {
  if ( path[path.size()-1] != '/' ) path += '/';
  paths.push_back(path);
  apppaths.push_back(path);
}

void DynamicLoader::prependPath(string path) {
  if ( path[path.size()-1] != '/' ) path += '/';
  paths.insert(paths.begin(), path);
  prepaths.push_back(path);
}

bool DynamicLoader::load(string name) {
  lastErrorMessage = "";
  static set<string> loaded;
  if ( loaded.find(name) != loaded.end() ) return true;
  loaded.insert(name);
  bool success = false;
  const string name_dylib = StringUtils::remsuf(name) + ".dylib";
  if ( name[0] == '/' ) {
    success = loadcmd(name) || loadcmd(name_dylib);
  }
  else {
    for ( unsigned i = 0; i < paths.size(); ++i ) {
      string path = paths[i];
      if ( path[path.size() - 1] != '/' ) path += '/';
      if ( loadcmd(path + name) || loadcmd(path + name_dylib) ) {
	success = true;
	break;
      }
    }
  }
  if ( success || loadcmd(name) || loadcmd(name_dylib) ) {
    lastErrorMessage = "";
    return true;
  }
  loaded.erase(name);
  return false;
}

const vector<string> & DynamicLoader::appendedPaths() {
  return apppaths;
}

const vector<string> & DynamicLoader::prependedPaths() {
  return prepaths;
}

const vector<string> & DynamicLoader::allPaths() {
  return paths;
}

vector<string> DynamicLoader::paths = DynamicLoader::defaultPaths();

vector<string> DynamicLoader::prepaths = vector<string>();

vector<string> DynamicLoader::apppaths = vector<string>();

string DynamicLoader::lastErrorMessage;

map<string,string> DynamicLoader::versionMap;

vector<string> DynamicLoader::defaultPaths() {
  vector<string> vpaths;
  // macro is passed in from -D compile flag
  string instpath = THEPEG_PKGLIBDIR;
  vpaths.push_back(instpath);
  vpaths.push_back(".");
  return vpaths;
}
