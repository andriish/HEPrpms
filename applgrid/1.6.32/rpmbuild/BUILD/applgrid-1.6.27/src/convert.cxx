

#include "appl_grid/appl_grid.h"
#include "amconfig.h"

int Usage( std::ostream& s=std::cout, int i=0 ) { 
  s << "Usage: applgrid-convert [options] <grid0> [grid1] [grid2] ...\n";
  s << "\noption:\n";
  s << "   -p | --pdf   value    \t record specifed pdf to the grid\n"; 
  s << "   -i | --ipdf  value    \t record specifed ipdf from set to the grid\n\n"; 
  s << "   -h | --help           \t this help\n"; 
  s << "\nSee " << PACKAGE_URL << " for more details\n"; 
  s << "\nReport bugs to <" << PACKAGE_BUGREPORT << ">";
  s << std::endl;
  return i;
}


int main( int argc, char** argv ) { 

  std::vector<std::string> grids;

  std::string  pdf = "";
  int         ipdf = 0;

  for ( int i=1 ; i<argc ; i++ ) {
    std::string arg = argv[i];
    
    if      ( arg.find("-")!=0 ) grids.push_back( arg );
    else if ( arg=="-h" || arg=="--help" ) return Usage();
    else if ( arg=="-p" || arg=="--pdf" ) { 
      if ( ++i<argc ) pdf = argv[i];
      else return Usage( std::cerr, -1);
    }
    else if ( arg=="-i" || arg=="--ipdf" ) { 
      if ( ++i<argc ) ipdf = std::atoi(argv[i]);
      else return Usage( std::cerr, -1);
    }
   
  }
  
  for ( size_t i=0 ; i<grids.size() ; i++ ) {
	
    std::string applname = grids[i];
    std::string newname  = grids[i];
    
    size_t pos = applname.find(".root");
    
    if ( pos==(applname.size()-5) ) newname.replace( pos, 5, ".appl" );
    else { 
      size_t pos = applname.find(".appl");
      if ( pos==(applname.size()-5) ) newname.replace( pos, 5, ".root" );
    }

    appl::grid g( applname );
    if ( pdf!="" ) { 
      g.setGeneratedPDF( pdf );
      g.setGeneratediPDF( ipdf );
    }

    std::cout << "converting: " << argv[i] << " -> " << newname << std::endl; 
    g.Write( newname );
  }    
  
  return 0;
} 
