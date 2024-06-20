// alphas for TMDlib
#include "tmdlib/TMDlib.h"
#include <iostream>
#include <fstream>
#include "yaml-cpp/yaml.h"
#include <stdio.h>      /* printf, NULL */
#include <stdlib.h>     /* strtod */
#include <string>
#include <vector>
#include <cassert>
#include <stdexcept>
#include <vector>

using namespace TMDlib;

//using namespace std;

// from LHAPDF6.3
/// Split a string by a given separator
inline std::vector<std::string> split(const std::string& s, const std::string& sep) {
    std::vector<std::string> rtn;
    std::string tmp = s; // non-const working copy, to be incrementally truncated
    while (true) {
        const size_t delim_pos = tmp.find(sep);
        if (delim_pos == std::string::npos) break;
        const std::string s = tmp.substr(0, delim_pos);
        if (!s.empty()) rtn.push_back(s); // Don't insert "empties"
        tmp.replace(0, delim_pos+1, ""); // Remove already-processed part
    }
    if (!tmp.empty()) rtn.push_back(tmp); // Don't forget the trailing component!
    return rtn;
}



extern std::string pdfpath;

int noiselevel = 0;

double TMD::TMDgetKtmin() { return 0;}
double TMD::TMDgetKtmax() { return 0;}
void TMD::TMDinit(const std::string name, int irep, int imode  ) {}
double TMDlib::ipow(double, int) {return 0;}
int TMDlib::iipow(int, int) {return 0;}

double TMD::get_key_val_as_double(std::string s)
/** Helper routine to return value from info file as double */
{
    double number;
    if ( s != "" ) {
        std::stringstream ss;
        ss << s;
        ss >> number;
        // cout << " get_key_val " << s << " number " << number << endl;
        return number;
    }
    else { return -9999.;}
}

int TMD::get_key_val_as_int(std::string s)
/** Helper routine to return value from info file as int */
{
    int number;
    // cout << " get_key_val " << s  << endl;
    if ( s != "" ) {
        std::stringstream ss;
        ss << s;
        ss >> number;
        // cout << " get_key_val " << s << " number " << number << endl;
        return number;
    }
    else { return -9999.;}
}

bool TMD::DoesFileExist (std::string name) {
    /** Helper routine to check existence of file */
    // cout << " DoesFIle exit " << name << endl;
    if (FILE *file = fopen(name.c_str(), "r")) {
        fclose(file);
        return true;
    } else {
        return false;
    }

}


void TMD::TMDinfo(const std::string name ) {
    /** Helper to access informtion from info file using YAML.
       Old format (TMDlib1) is hard coded.
    */
    std::string pdfdir="" ;

//  here treat the TMDlib1 data sets
    NewFormat = 0;
    std::size_t found ;
    std::string setname = name ;
    found = name.find("GBWlight");
    if (found!=std::string::npos) {pdfdir="GBWlight/" ;}
    found = name.find("GBWcharm");
    if (found!=std::string::npos) {pdfdir="GBWcharm/" ;}
    found = name.find("Bluem");
    if (found!=std::string::npos) {pdfdir="Blueml/" ;}
    found = name.find("ccfm-JH-2013-set1");
    if (found!=std::string::npos) {pdfdir="ccfm-JH-2013-set1/" ;}
    found = name.find("ccfm-JH-2013-set2");
    if (found!=std::string::npos) {pdfdir="ccfm-JH-2013-set2/" ;}
    found = name.find("ccfm-JH-set1");
    if (found!=std::string::npos) {pdfdir="ccfm-JH-set1/" ;}
    found = name.find("ccfm-JH-set2");
    if (found!=std::string::npos) {pdfdir="ccfm-JH-set2/" ;}
    found = name.find("ccfm-JH-set3");
    if (found!=std::string::npos) {pdfdir="ccfm-JH-set3/" ;}
    found = name.find("ccfm-JS-2001");
    if (found!=std::string::npos) {pdfdir="ccfm-JS-2001/" ;}
    found = name.find("ccfm-setA1");
    if (found!=std::string::npos) {pdfdir="ccfm-setA1/" ;}
    found = name.find("ccfm-setB1");
    if (found!=std::string::npos) {pdfdir="ccfm-setB1/" ;}
    found = name.find("ccfm-setA0");
    if (found!=std::string::npos) {pdfdir="ccfm-setA0/" ;}
    found = name.find("ccfm-setB0");
    if (found!=std::string::npos) {pdfdir="ccfm-setB0/" ;}
    //found= name.find("PB-NLO-HERAI+II");
    //if (found!=std::string::npos) {pdfdir="PB-NLO-2018/";}
    found= name.find("PB-NLO-HERAI+II-2018-set1");
    if (found!=std::string::npos) {pdfdir="PB-NLO-HERAI+II-2018-set1/";}
    found= name.find("PB-NLO-HERAI+II-2018-set1-q0");
    if (found!=std::string::npos) {pdfdir="PB-NLO-HERAI+II-2018-set1-q0/";}
    found= name.find("PB-NLO-HERAI+II-2018-set2");
    if (found!=std::string::npos) {pdfdir="PB-NLO-HERAI+II-2018-set2/";}
    found= name.find("PB-NLO-HERAI+II-2018-set2-q0");
    if (found!=std::string::npos) {pdfdir="PB-NLO-HERAI+II-2018-set2-q0/";}
    //found= name.find("PB-LO-HERAI+II");
    //if (found!=std::string::npos) {pdfdir="PB-LO-2018/";}
    found= name.find("PB-NLO_ptoPb208-set1");
    if (found!=std::string::npos) {pdfdir="PB-NLO_ptoPb208-set1/";}
    found= name.find("PB-NLO_ptoPb208-set2");
    if (found!=std::string::npos) {pdfdir="PB-NLO_ptoPb208-set2/";}
    found= name.find("KS-2013-linear");
    if (found!=std::string::npos) {pdfdir="KS-2013-linear/";}
    found= name.find("KS-2013-non-linear");
    if (found!=std::string::npos) {pdfdir="KS-2013-non-linear/";}
    found= name.find("KS-hardscale-linear");
    if (found!=std::string::npos) {pdfdir="KS-hardscale-linear/";}
    found= name.find("KS-hardscale-non-linear");
    if (found!=std::string::npos) {pdfdir="KS-hardscale-non-linear/";}
    //found= name.find("Kutak");
    //if (found!=std::string::npos) {pdfdir="KS/";}
    found= name.find("BHKS");
    if (found!=std::string::npos) {pdfdir="BHKS/";}
    found= name.find("EKMP");
    if (found!=std::string::npos) {pdfdir="EKMP/";}
    found= name.find("PB-EPPS16nlo_CT14nlo_Pb208-set1");
    if (found!=std::string::npos) {pdfdir="PB-EPPS16nlo_CT14nlo_Pb208-set1/";}
    found= name.find("PB-EPPS16nlo_CT14nlo_Pb208-set2");
    if (found!=std::string::npos) {pdfdir="PB-EPPS16nlo_CT14nlo_Pb208-set2/";}
    found= name.find("PB-nCTEQ15FullNuc_208_82-set1");
    if (found!=std::string::npos) {pdfdir="PB-nCTEQ15FullNuc_208_82-set1/";}
    found= name.find("PB-nCTEQ15FullNuc_208_82-set2");
    if (found!=std::string::npos) {pdfdir="PB-nCTEQ15FullNuc_208_82-set2/";}
    found= name.find("SBRS-2013-TMDPDFs");
    if (found!=std::string::npos) {pdfdir="SBRS-2013-TMDPDFs/";}
    found= name.find("SBRS-2013-TMDPDFs-par");
    if (found!=std::string::npos) {pdfdir="SBRS-2013-TMDPDFs-par/";}
    found = name.find("PV19_grid_pdf");
    if (found!=std::string::npos) {pdfdir = "PV19_grid_pdf/"; NewFormat = 1;}
    found = name.find("PV17_grid_pdf");
    if (found!=std::string::npos) {pdfdir = "PV17_grid_pdf/"; NewFormat = 1;}
    found = name.find("PV17_grid_ff_Pip");
    if (found!=std::string::npos) {pdfdir = "PV17_grid_ff_Pip/"; NewFormat = 1;}
    found = name.find("PV17_grid_ff_Pim");
    if (found!=std::string::npos) {pdfdir = "PV17_grid_ff_Pim/"; NewFormat = 1;}


    // checking for members
    int nmem = 0;
    const size_t slashpos = name.find("/");
    std::string smem;
    TMD_Mem="0000";
    setname = trim(name.substr(0, slashpos));
    if (slashpos != std::string::npos) {
        smem = name.substr(slashpos+1);
        // cout << " smem " << smem << endl ;
        nmem = std::stoi( smem ) ;
        if( nmem < 10 )  { TMD_Mem = "000"+smem;}
        else if ( nmem < 100 )  { TMD_Mem = "00"+smem;}
        else if ( nmem < 1000 )  { TMD_Mem = "0"+smem;}
    }
    //else { cout << "No submembers  " << name<< " " << setname << endl;}


    if ( pdfdir == "" ) {
        // cout << " TMDinfo: pdfdir was not set: do we have a new set ? " << endl;

        // cout << " TMDinfo:  name = " << setname << " with member " << nmem << endl;
        pdfdir=setname+"/";
        NewFormat = 1 ;
    }
    TMD_Name = setname ;
    TMD_Dir = pdfdir ;

    char* pathsvar = std::getenv("TMDLIBPATH");
    std::string filepath;
    // But fall back to looking in LHAPATH if the preferred var is not defined
    if (pathsvar == nullptr) pathsvar = getenv("TMDLIBOATH");
    const std::string spathsvar = (pathsvar != 0) ? pathsvar : "";
    // Split the paths variable as usual
    std::vector<std::string> rtn = split(spathsvar, ":");
    // Look in the install prefix after other paths are exhausted, if not blocked by a trailing ::

    for (auto &s: rtn) {
        // std::cout << s << std::endl;
        filepath = s+"/"+pdfdir+setname+".info";
        std::cout<< " does file exist " <<DoesFileExist(filepath) << " " << filepath << std::endl;
        if (DoesFileExist(filepath)) {
            pdfpath = s+"/" ;
            std::cout<< " TMDutils: file found from environment varialble " << filepath << std::endl;
            break;
        }
    }

    filepath =pdfpath+pdfdir+setname+".info";

    // cout << " nmem " << nmem << endl;

    if (!DoesFileExist(filepath)) {
        std::cout << " TMDinfo: file does not exist " << filepath << std::endl;
        std::cout << " return on EXIT_FAILURE " << std::endl;
        exit (EXIT_FAILURE);
    }

    if (filepath.empty()) std::cout << "Empty PDF file name given to Info::load  " << filepath << std::endl;
    // cout << " TMDinfo: " << filepath << " name = " << name << endl;
    std::ifstream file(filepath);
    std::string docstr, line;
    while (getline(file, line)) {
        if (line == "---") break;
        docstr += line + "\n";
    }
    TMDLIB_YAML::Node doc = TMDLIB_YAML::Load(docstr);
    TMDLIB_YAML::const_iterator it = doc.begin();
    std::size_t nProperty = doc.size();
    for (std::size_t iProperty = 0; iProperty < nProperty; ++iProperty) {
    std::string key = it->first.as<std::string>();
    TMDLIB_YAML::Node val = it->second;
        if (val.IsScalar()) {
            //  Scalar value
            // cout << key<< ": " <<val.as<string>() << endl;
            TMDdict[key] = val.as<std::string>();
        } else {
            // Process the sequence entries into a comma-separated string
            std::string seqstr = "";
            for (size_t i = 0; i < val.size(); ++i)
                seqstr += val[i].as<std::string>() + ((i < val.size()-1) ? "," : "");
            TMDdict[key] = seqstr;
            // cout << key << ":  " <<  seqstr << endl;
        }
    ++it;
    }
    /*
        const std::string SearchKey = "SetDesc";
        std::cout << "SetDesc: " << TMDdict.find("SetDesc")->second << '\n';
        std::cout << "XMin: " << TMDdict.find("XMin")->second << '\n';
        std::cout << "XMax: " << TMDdict.find("XMax")->second << '\n';
        std::cout << "Flavors: " << TMDdict.find("Flavors")->second << '\n';
    */

}


std::string TMD::TMDgetDesc( ) {
    /** TMDlib helper to return Descripton from info file */

    return TMDdict.find("SetDesc")->second;


}
std::string TMD::TMDgetIndex( ) {
    /** TMDlib helper to return SetIndexfrom info file */

    return TMDdict.find("SetIndex")->second;


}
std::string TMD::TMDgetScheme( ) {
    /** TMDlib helper to return Scheme for TMD evolution from info file */

    return TMDdict.find("TMDScheme")->second;


}

double TMD::TMDgetLam4( ) {
    /** TMDlib helper to return QCDlam from info file */

    double QCDlam =  get_key_val_as_double(TMDdict.find("AlphaS_QCDlam")->second);
    if(QCDlam == 0 ) {
        std::cout << " TMDgetLam4: QCDlam is not defined for iset = " << iset << std::endl;
        return -9999.;
    }
    else {
        return QCDlam;
    }
}

int TMD::TMDgetNf( ) {
    /** TMDlib helper to return Nr of flavors from info file */

    int index =  get_key_val_as_int(TMDdict.find("NumFlavors")->second);
    // cout << " TMDgetNF: " << TMDdict.find("NumFlavors")->second << " index = " << index <<  endl;
    return index;

}
int TMD::TMDgetNumMembers( ) {
    /** TMDlib helper to return Nr of Member from info file */

    int index =  get_key_val_as_int(TMDdict.find("NumMembers")->second);
    // cout << " TMDgetNumMembers: " << TMDdict.find("NumMembers")->second << " index = " << index <<  endl;
    return index;

}
int TMD::TMDgetOrderAlphaS( ) {
    /** TMDlib helper to return order of alphas (nr of loops) from info file */

    int index =  get_key_val_as_int(TMDdict.find("AlphaS_OrderQCD")->second);
    // cout << " TMDgetOrderAlphas: " << TMDdict.find("AlphaS_OrderQCD")->second << " index = " << index <<  endl;
    return index;
}

int TMD::TMDgetOrderPDF( ) {
    /** TMDlib helper to return order of evolution (nr of loops) from info file */

    int index =  get_key_val_as_int(TMDdict.find("OrderQCD")->second);
    // cout << " TMDgetOrderPDF: " << TMDdict.find("OrderQCD")->second << " index = " << index <<  endl;
    return index;

}

double TMD::TMDgetXmin() {
    /** TMDlib helper to return XMin of TMD parametrization from info file */
    double XMin =  get_key_val_as_double(TMDdict.find("XMin")->second);
    //cout << " TMDgetXMin as double : " << XMin << endl;
    if(XMin < 0 ) {
        std::cout << " TMDgetXMin: XMin is not defined for iset = " << iset <<
                  " name = " <<TMDdict.find("XMin")->second << std::endl;
        return -9999.;
    }
    else {
        return XMin;
    }

}

double TMD::TMDgetXmax() {
    /** TMDlib helper to return XMax of TMD parametrization from info file */
    double XMax =  get_key_val_as_double(TMDdict.find("XMax")->second);
    //cout << " TMDgetXMax as double : " << XMax << endl;
    if(XMax < 0 ) {
        std::cout << " TMDgetXMax: XMax is not defined for iset = " << iset <<
                  " name = " <<TMDdict.find("XMax")->second << std::endl;
        return -9999.;
    }
    else {
        return XMax;
    }
}

double TMD::TMDgetQmin() {
    /** TMDlib helper to return QMin of TMD parametrization from info file */
    double QMin =  get_key_val_as_double(TMDdict.find("QMin")->second);
    // cout << " TMDgetQmin as double : " << Qmin << endl;
    if(QMin < 0 ) {
        std::cout << " TMDgetQmin: QMin is not defined for iset = " << iset <<
                  " value = " <<TMDdict.find("QMin")->second << std::endl;
        return -9999.;
    }
    else {
        return QMin;
    }
}

double TMD::TMDgetQ2min() {
    /** TMDlib helper to return Q2Min of TMD parametrization from info file */
    double QMin =  get_key_val_as_double(TMDdict.find("QMin")->second);
    // cout << " TMDgetQ2min as double : " << QMin << endl;
    if(QMin < 0 ) {
        std::cout << " TMDgetQ2min: Q2Min is not defined for iset = " << iset << " value = " <<TMDdict.find("QMin")->second << std::endl;
        return -9999.;
    }
    else {
        return QMin*QMin ;
    }

}
double TMD::TMDgetQ2max() {
    /** TMDlib helper to return Q2Max of TMD parametrization from info file */
    double QMax =  get_key_val_as_double(TMDdict.find("QMax")->second);
    // cout << " TMDgetQ2max as double : " << QMax << endl;
    if(QMax < 0 ) {
        std::cout << " TMDgetQ2max: QMax is not defined for iset = " << iset << " value = " <<TMDdict.find("QMax")->second <<std::endl;
        return -9999.;
    }
    else {
        return QMax*QMax;
    }

}

double TMD::TMDgetQmax() {
    /** TMDlib helper to return QMax of TMD parametrization from info file */
    double QMax =  get_key_val_as_double(TMDdict.find("QMax")->second);
    // cout << " TMDgetQmax as double : " << QMax << endl;
    if(QMax < 0 ) {
        std::cout << " TMDgetQmax: QMax is not defined for iset = " << iset << " value = " <<TMDdict.find("QMax")->second <<std::endl;
        return -9999.;
    }
    else {
        return QMax;
    }

}
std::string TMD::TMDgetExtrapolation_Q2() {
    /** TMDlib helper to return Extrapolation of TMD parametrization from info file */
    return TMDdict.find("Extrapolation_Q2")->second;

}
std::string TMD::TMDgetExtrapolation_kt() {
    /** TMDlib helper to return Extrapolation of TMD parametrization from info file */
    return TMDdict.find("Extrapolation_x")->second;

}
std::string TMD::TMDgetExtrapolation_x() {
    /** TMDlib helper to return Extrapolation of TMD parametrization from info file */
    return TMDdict.find("Extrapolation_kt")->second;

}

int TMD::TMDnumberPDF(std::string name) {
    /** TMDlib helper to return index number for TMDset name from info file */

//   cout << " TMDnumberPDF: " << name <<  endl;

    if(name == "" ) { std::cout << " TMDnumberPDF: name is not defined " << name << std::endl; return -99999; }

    else {
        int index =  get_key_val_as_int(TMDdict.find("SetIndex")->second);
        // cout << " TMDnumberPDF: " << name << " index = " << index <<  endl;
        return index;
    }
}

std::string TMD::TMDstringPDF(int index) {
    /** TMDlib helper to return name for index  for TMDset */
    std::string error="error";
    //  cout << " TMDstringPDF: " << index <<  endl;

    if(index == 0 ) { std::cout << " TMDstringPDF: iset is not defined " << index << std::endl; return error; }

    else {
        //    static map<int, string> TMDindex;
        if (TMDindex.empty()) { // The map needs to be populated first
            char* pathsvar = std::getenv("TMDLIBPATH");
            std::string filepath;
            // But fall back to looking in LHAPATH if the preferred var is not defined
            if (pathsvar == nullptr) pathsvar = getenv("TMDLIBOATH");
            const std::string spathsvar = (pathsvar != 0) ? pathsvar : "";
            // Split the paths variable as usual
            std::vector<std::string> rtn = split(spathsvar, ":");
            // Look in the install prefix after other paths are exhausted, if not blocked by a trailing ::
            for (auto &s: rtn) {
                filepath = s+"/tmdpdfsets.index";
                std::cout<< " does file exist " <<DoesFileExist(filepath) << " " << filepath << std::endl;
                if (DoesFileExist(filepath)) {
                    pdfpath = s+"/" ;
                    std::cout<< " TMDutils:  tmdpdfsets.index file found from environment varialble " << filepath << std::endl;
                    break;
                }
            }
            std::string indexpath =pdfpath+"tmdpdfsets.index";
            //  cout << " TMDstringPDF indexpath = " << indexpath << endl;
            if (indexpath.empty()) {std::cout << " TMDstringPDF: Could not find a tmdpdfsets.index file" <<std::endl; };
            try {
                std::ifstream file(indexpath.c_str());
                std::string line;
                while (getline(file, line)) {
                    if (line.empty() || line.find("#") == 0) continue;
                    std::istringstream tokens(line);
                    int id;
                    std::string setname;
                    tokens >> id;
                    tokens >> setname;
                    TMDindex[id] = setname;
                    // cout << id << " -> " << TMDindex[id] << endl;
                }
            } catch (const std::exception& ex) {
                std::cout << "Trouble when reading " + indexpath + ": " + ex.what() << std::endl;
            }
        }

        std::string name = TMDindex.find(index)->second;
        // cout << " TMDstringPDF: " << TMDindex.find(index)->second << " index = " << index <<  endl;
        return name;
    }
}

void TMD::setVerbosity(int Inputnoiselevel) {
    noiselevel = Inputnoiselevel ;
    std::cout << " TMD setverbosity: " << Inputnoiselevel << " " << noiselevel << std::endl;
}
int TMD::TMDverbosity( ) {
    /** TMDlib helper to return verbosity */
    // cout << " TMDverbosity: " << noiselevel <<  endl;
    return noiselevel;
}



