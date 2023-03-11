/**
 **     @file    combine.cxx
 **
 **     @brief   increasingly complex code to add together many 
 **              grids 
 **
 **     @author  mark sutton
 **     @date    Sat 13 Jul 2013 09:54:51 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: combine.cxx, v0.0   Sat 13 Jul 2013 09:54:51 CEST sutt $
 **
 **/


#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <string>
#include <cstdlib>
#include <sys/stat.h>

#include <algorithm>

#include <regex>
#include <map>

#include "appl_grid/simpletimer.h"

#include "appl_grid/appl_root.h"

#include "appl_grid/appl_grid.h"
#include "appl_grid/appl_file.h"
#include "amconfig.h"

#include "appl_grid/appl_timer.h"


#ifdef USEROOT
#include "TFile.h"
#include "TPad.h"
#endif


template<typename T>
bool bdelete( std::vector<T>& v, const T& t ) {
  for ( typename std::vector<T>::iterator itr=v.begin() ; itr!=v.end() ; ) { 
    if ( *itr == t ) v.erase( itr );
    else itr++;
  }
  return true;
} 


double integral( appl::TH1D* h ) { 
  double d = 0;
  for ( int i=0 ; i<h->GetNbinsX() ; i++ ) d += h->GetBinContent(i+1);
  return d;
}


void print( appl::TH1D* h ) { 
  for ( int i=1 ; i<=h->GetNbinsX() ; i++ ) std::cout << h->GetBinContent(i) << " ";
  std::cout << std::endl;
}


int usage(std::ostream& s, int argc, char** argv) { 
  if ( argc<1 ) return -1; /// should never be the case 
  s << "Usage: " << argv[0] << " [OPTIONS] -o output_grid.root  input_grid.root [input_grid1.root ... input_gridN.root]\n\n";
  s << "  APPLgrid \'" << argv[0] << "\' adds " << PACKAGE_STRING << " grid files together into a single grid\n\n"; 
  s << "Configuration: \n";
  s << "    -o filename   \t name of output grid (filename required)\n\n";
  s << "Options: \n";
  s << "    -g, --gscale   value\t rescale output grid by value, \n";
  s << "    -r, --rscale   value\t rescale reference histogram by value, \n";
  s << "    -s, --scale    value\t rescale both output grid and reference histogram by value\n";
  s << "    -w, --wscale   value\t rescale the weight normalisation for the output grid\n";
  s << "        --weight   value\t set the value of the weight normalisation for the output grid directly\n";
  s << "    -n, --normfile value\t file containing bin-by-bin normalisations for adding grids\n";
  s << "    -p, --pdf      value\t set the default pdf to value\n";
  s << "    -i, --iset     value\t set the default set from the pdf group  to value\n";
  s << "    -a, --all           \t add all grids (default)\n";
  s << "        --optimise      \t optimise the output grid\n";
  s << "        --compress value\t try to reduce the number of parton luminosity\n"
    << "                        \t combinations\n";
  s << "    -c, --chi2     value\t if set, exclude grids with a chi2 with respect\n"
    << "                        \t to the median larger than value\n";
  s << "        --verbose       \t display grid documentation during add\n";
  s << "    -v, --version       \t displays the APPLgrid version\n";
  s << "    -h, --help          \t display this help\n";
  s << "\nSee " << PACKAGE_URL << " for more details\n"; 
  s << "\nReport bugs to <" << PACKAGE_BUGREPORT << ">";
  s << std::endl;
  return 0;
}





/// bins 


struct bin {

  bin( double y=0, double ye=0, int n=1 ) : _x(0), _y(y), _ye(ye), _y2(_y*_y), _n(n) { 
    _ys.push_back(_y);
  } 

  bin( appl::TH1D* h, int i ) : _x(h->GetBinCenter(i)), _y(h->GetBinContent(i)), _ye(h->GetBinError(i)), _y2(_y*_y), _n(1) { 
    _ys.push_back(_y);
  } 

  bin( const bin& b) : _x(b._x), _y(b._y), _ye(b._ye), _y2(b._y2), _n(b._n), _ys(b._ys) { } 

  unsigned size() const { return _ys.size(); }

  bin& operator+=( const bin& b) { 
    _y  += b._y;
    _y2 += b._y2;
    _ye  = std::sqrt( _ye*_ye + b._ye*b._ye );
    _n  += b._n;
    _ys.push_back(b._y);
    return *this;
  }

  bin& operator-=( const bin& b) { 
    _y  -= b._y;
    _y2 -= b._y2;
    _ye  = std::sqrt( _ye*_ye - b._ye*b._ye );
    _n  -= b._n;
    bdelete( _ys, b._y );
    return *this;
  }



  bin& operator*=( double d ) { 
    _y  *= d;
    _ye *= d;
    _y2 *= d*d;
    //    _n  *= d;
    for ( unsigned i=0 ; i<_ys.size() ; i++ ) _ys[i] *= d;
    return *this;
  }

  bin& operator/=( double d ) { return operator*=(1/d); } 


  bool operator<( const bin& b)  const { return _y<b._y; }
  bool operator>( const bin& b)  const { return _y>b._y; }
  bool operator<=( const bin& b) const { return _y<=b._y; }
  bool operator>=( const bin& b) const { return _y>=b._y; }
  bool operator==( const bin& b) const { return _y==b._y; }
  bool operator!=( const bin& b) const { return _y!=b._y; }
  

  double _x;
  double _y;
  double _ye;

  double _y2;

  int    _n;

  std::vector<double> _ys;


  double mean()   const { return _y/_n; }  
  double rms()    const { return std::sqrt( ( _y2 - _y*_y/_n )/_n ); }  
  double mean_error()  const { return std::sqrt( ( _y2 - _y*_y/_n )/(_n*_n) ); }  


  double median() const {
    std::vector<double> v = _ys;
    std::sort( v.begin(), v.end() );
    if ( (v.size()&1) ) return v[v.size()/2];
    return 0.5*(v[v.size()/2] + v[(v.size()-1)/2]);
  }


};




bin operator+( const bin& b0, const bin& b1 ) { return bin( b0._y+b1._y, std::sqrt( b0._ye*b0._ye* + b1._ye*b1._ye), b0._n+b1._n ); }
bin operator*( double d, const bin& b )       { return bin( d*b._y, d*b._ye, d*b._n ); }



/// cross section 

struct Xsection { // : public std::vector<bin> {
  
  Xsection() { }

  Xsection(appl::TH1D* h) { // : _bins(*this) { 
    add( h ); //for ( int i=0 ; i<h->GetNbinsX() ; i++ ) _bins.push_back( bin( h, i+1 ) );
  }
  
  void add( bin& b ) {  _bins.push_back( b ); }

  
  void add( appl::TH1D* h ) { 
    if ( _bins.empty() ) { 
      for ( int i=0 ; i<h->GetNbinsX() ; i++ ) _bins.push_back( bin( h, i+1 ) );
    }
    else { 
      if ( _bins.size()!=unsigned(h->GetNbinsX()) ) { 
	std::cerr << "Xsection::add() bin mismatch " << _bins.size() << " " << h->GetNbinsX() << std::endl;
	return;
      }
      for ( int i=0 ; i<h->GetNbinsX() ; i++ ) _bins[i] += bin( h, i+1 );
    }
  }


  bin  operator[](int i) const { return _bins[i]; }
  bin& operator[](int i)       { return _bins[i]; }

  unsigned size()  const { return _bins.size(); };
  bool     empty() const { return _bins.empty(); };

  Xsection& operator+=( const Xsection& x) { 
    for ( unsigned i=0 ; i<_bins.size() ; i++ ) _bins[i] += x._bins[i];
    return *this;
  }


  Xsection& operator-=( const Xsection& x) { 
    for ( unsigned i=0 ; i<_bins.size() ; i++ ) _bins[i] -= x._bins[i];
    return *this;
  }


  Xsection& operator+=( appl::TH1D* h ) { 
    add( h );
    return *this;
  }

  Xsection& operator*=( double d ) { 
    for ( unsigned i=0 ; i<_bins.size() ; i++ ) _bins[i] *= d;
    return *this;
  }


  std::vector<bin>  mean() const {
    std::vector<bin> _mean;
    for ( unsigned i=0 ; i<_bins.size() ; i++ ) {
      _mean.push_back( bin( _bins[i].mean(), _bins[i].mean_error(), _bins[i]._n ) ); 
    }
    return _mean;
  }


  std::vector<bin>  median() const {
    std::vector<bin> _median;
    for ( unsigned i=0 ; i<_bins.size() ; i++ ) {
      _median.push_back( bin( _bins[i].median(), _bins[i].mean_error(), _bins[i]._n ) ); 
    }
    return _median;
  }

  /// data members 

  std::vector<bin> _bins;

}; 



Xsection operator*( const Xsection& x, double d ) { 
  Xsection xs(x);
  return (xs *= d);
}


Xsection operator*( double d, const Xsection& x )  { return x*d; }


Xsection operator-( const Xsection& x1, const Xsection& x2 ) { 
  Xsection xs = x1;
  xs += (-1*x2);
  return xs;
}






double chi2( const Xsection& xs1, const Xsection& xs2 ) { 

  if ( xs1.empty() ) return 0;

  if ( xs1.size()!=xs2.size() ) return 0;
  
  double c2 = 0;

  for ( unsigned i=0 ; i<xs1.size() ; i++ ) {
 
    double d = xs1[i].mean() - xs2[i]._y;
    
    /// take fractional sigma and turn it to actual sigma
    double s2 = ( xs1[i].mean_error()*xs1[i].mean_error() + xs2[i]._ye*xs2[i]._ye );

    /// 5*"sigma on the mean" will be treated as our
    /// ad hoc figure of merrit
    if ( s2>0 ) c2 += d*d/s2;
    
  }

  /// return chi2 per dof
  return c2/xs1.size();

}




double chi2_diff( Xsection& v1,  Xsection& v2, double chi2_limit=5 ) { 

  if ( v1.size()!=v2.size() ) return 0;
  
  double chi2 = 0;
  
  bool neg = false;
  bool pos = false;

  double max_chi2 = 0;

  for ( size_t i=0 ; i<v1.size() ; i++ ) { 
    
    //    double d  = v1[i]._y - v2[i]._y;
    //    double s2 = v1[i]._ye*v1[i]._ye + v2[i]._ye*v2[i]._ye;

    double d = v1[i].mean() - v2[i]._y;
    /// take fractional sigma and turn it to actual sigma
    double s2 = ( v1[i].mean_error()*v1[i].mean_error() + v2[i]._ye*v2[i]._ye );

       
    double c2 = 0;

    if ( s2!=0 ) c2 = d*d/s2;
   
    chi2 +=  c2;
    
    //    std::cout << "diff " << i << " " << d << " " << c2 << std::endl;

    /// ensure inly large excursions in adjacent bins 
    /// are counted
    if ( c2 > chi2_limit ) { 
      if ( d < 0 ) { 
	neg = true;
	if ( pos && c2>max_chi2 ) max_chi2 = c2;  
      }
      else if ( d > 0 ) { 
	pos = true; 
	if ( neg && c2>max_chi2 ) max_chi2 = c2;  
      }
    }
    else { 
      neg = false;
      pos = false;
    }

  }
  
  return max_chi2;
}









/// streamers 

std::ostream& operator<<( std::ostream& s, const bin& b ) { 
  return s << "( " << b._x << " :\t" << b._y << " +- " << b._ye << " " << ( b._y!=0 ? 100*b._ye/b._y : 0 ) << "%   n: " << b._n << " )";
}

#if 0
template<class T>
std::ostream& operator<<( std::ostream& s, const std::vector<T>& v ) { 
  for ( unsigned i=0 ; i<v.size() ; i++ ) s << " " << v[i] << "\n";
  return s;
}
#endif

std::ostream& operator<<( std::ostream& s, const Xsection& x ) {  return s << x._bins; }





double exclude( Xsection xsec, appl::TH1D* ref, double chi2_limit=5 ) { 

    Xsection txsec( ref );
    Xsection diff = xsec;
      
    diff -= txsec;

    /// chi2 of this xsec with respect to the mean *excluding* this one
    double c2 = chi2_diff( diff, txsec, chi2_limit ); 

    if ( c2 < chi2_limit ) return c2;

    return 0;
}


std::vector<std::string> split( const std::string& input) { 
    std::istringstream buffer(input);
    std::vector<std::string> ret( (std::istream_iterator<std::string>(buffer)), 
                                 std::istream_iterator<std::string>());
    return ret;
}


double purestod( const std::string& s ) { return std::stod(s); }

class NormVal { 

public:

  typedef std::map< std::string, std::vector<double> > map_type;

public:

  NormVal( const std::string& f ) : mfile(f), mif(0) { 
    if ( mfile=="" ) return; 
    
    /// first test if file actually exists ...
    if ( !appl::file_exists( mfile)  ) {
      std::cerr << "whoops, specified grid weight file does not exist" << std::endl;
      return;
    }
    
    /// then open it ...
    mif = new std::ifstream(mfile.c_str());

  } 
  
  

 
 
  void read() { 

    if ( mif==0 || mgridnames.size() ) return;

    std::cout << "NormVal::read() " << mfile << std::endl;

    mgridnames.clear();

    std::string line = "";

    int Ngrids = 0;

    while ( std::getline( *mif, line ) ) { 

      if ( line.find("#")==0 ) continue;

      std::vector<std::string> vs = split( line );

      if ( vs.size() ) { 
	Ngrids++;
	mgridnames.push_back(vs[0]);
	std::vector<double> v; v.reserve( vs.size()-1 );
	for ( size_t i=1 ; i<vs.size() ; i++ ) v.push_back( std::stod(vs[i]) );
	mmap.insert( map_type::value_type( mgridnames.back(), v ) );
      }
    }

  }

    
  std::ifstream* stream() { return mif; }

  const std::vector<double>& weights( const std::string& s ) {
    map_type::const_iterator it = mmap.find( s );
    if ( it==mmap.end() ) throw appl::grid::exception( std::string( "grid::grid() find weights for grid : ")+s ); 
    return it->second;
  } 

  const std::vector<std::string>& grids() const { return mgridnames; }

  map_type::const_iterator begin() const { return mmap.begin(); }
  map_type::const_iterator end()   const { return mmap.end(); }

  map_type gridmap() const { return mmap; }

private:
  
  std::string                mfile;
  std::ifstream*             mif;
  std::vector<std::string>   mgridnames;
  
  //  std::vector< std::vector<double> > mnormval;
  map_type  mmap;

};


std::ostream& operator<<( std::ostream& s, const NormVal& n ) { 
  NormVal::map_type::const_iterator it = n.begin();
  while ( it!=n.end() ) { 
    s << "\t" << it->first << " : " << it->second.size() << "\n";
    it++;
  }
  return s;
}

///  if requested, calculate the mean, median etc of all the cross sections
///  from the grids, and exclude any (complete) grids where the xsection in 
///  any bin is larger than some  chi2 limit for that bin with respect to 
///  the mean ( or median ? ) without including that grid
///  - returns the updated ref vector, and fgrids vector with only the 
///    accepted grids
///
///  NB: developing changes to exclude grids due to large, oposite sign 
///      chi2 variations in *individual* adjacent bins only.

bool thin_grids( double chi2_limit, std::vector<appl::TH1D*>& ref, std::vector<std::string>& fgrids ) { 

  /// need at least 2 grids for an rms

  if ( ref.size()<2 ) return false; 
 
  /// calculate mean and median ...

  Xsection xsec;
  
  for ( size_t i=0 ; i<ref.size() ; i++ ) xsec += ref[i];

  //  std::cout << "median " << xsec.median() << std::endl;
  //  std::cout << "mean   " << xsec.mean()   << std::endl;
  
  /// now find grids outside the relevant ranges ...

  std::vector<std::string> fgrids_;

  for ( size_t i=0 ; i<ref.size() ; i++ ) { 

    double c2 = exclude( xsec, ref[i], chi2_limit );

    std::cout << "grid: " << i << " " << c2 << std::endl;

    if ( c2<chi2_limit ) fgrids_.push_back( fgrids[i] );

  }
  
  if ( fgrids.size()>fgrids_.size() ) fgrids = fgrids_;

  return true;
}






int main(int argc, char** argv) { 

  /// check correct number ofg parameters
  if ( argc<2 ) return usage( std::cerr, argc, argv );


  std::string output_grid = "";

  /// handle the "perform and exit" parameters 
  for ( int i=1 ; i<argc ; i++ ) { 
    if ( std::string(argv[i])=="-h" || std::string(argv[i])=="--help" )    return usage( std::cout, argc, argv ); 
    if ( std::string(argv[i])=="-v" || std::string(argv[i])=="--version" ) {
      std::cout << argv[0] << " APPLgrid version " << PACKAGE_VERSION << std::endl; 
      return 0;
    }
  }

  /// scaling factors
  double dscale = 1; /// overall
  double hscale = 1; /// histogram only
  double rscale = 1; /// grid only

  /// flags to see whether hscale of rscale have been set
  bool hset = false;
  bool rset = false;


  bool verbose = false; 

  /// by default add all histograms
  bool addall = true;

  /// if set, the following will disable adding all histigrams 
  /// exclude those with a chi2 larger than this value
  double chi2_limit = 0;

  /// the list of grids to process
  std::vector<std::string> fgrids;

  double weight = 0;
  double wscale   = 1;

  bool optimise = false;
  bool shrink   = false;

  std::string newpdfname = "";
  std::string normfile   = "";

  std::string pdfname = ""; 
  int         ipdf    = 0;

  /// handle configuration parameters
  for ( int i=1 ; i<argc ; i++ ) { 
    if      ( std::string(argv[i])=="--verbose" ) verbose = true;
    else if ( std::string(argv[i])=="-o" ) { 
      ++i;
      if ( i<argc ) output_grid = argv[i];
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-s" || std::string(argv[i])=="--scale" ) { 
      ++i;
      if ( i<argc ) dscale = std::atof(argv[i]);
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-g" || std::string(argv[i])=="--gscale" ) { 
      ++i;
      if ( i<argc ) { 
	rscale = std::atof(argv[i]);
	rset = true;
      }
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="--optimise" ) optimise = true;
    else if ( std::string(argv[i])=="--compress" ) { 
      ++i;
      if ( i<argc ) { 
	newpdfname = argv[i];
	shrink = true;
      }
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="--weight" ) {  
      ++i;
      if ( i<argc ) { 
	weight = std::atof(argv[i]);
      }
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-w" || std::string(argv[i])=="--wscale" ) {  
      ++i;
      if ( i<argc ) { 
	wscale = std::atof(argv[i]);
      }
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-r" || std::string(argv[i])=="--rscale" ) { 
      ++i;
      if ( i<argc ) { 
	hscale = std::atof(argv[i]);
	hset = true;
      }
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-c" || std::string(argv[i])=="--chi2" ) { 
      ++i;
      if ( i<argc ) { 
	chi2_limit = std::atof(argv[i]);
	addall = false;
      }
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-a" || std::string(argv[i])=="--all" ) addall = true;
    else if ( std::string(argv[i])=="-n" || std::string(argv[i])=="--normfile" ) { 
      ++i;
      if ( i<argc ) normfile = argv[i];
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-p" || std::string(argv[i])=="--pdf" ) { 
      ++i;
      if ( i<argc ) pdfname = argv[i];
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])=="-i" || std::string(argv[i])=="--ipdf" ) { 
      ++i;
      if ( i<argc ) ipdf = std::atoi(argv[i]);
      else  return usage( std::cerr, argc, argv );
    }
    else if ( std::string(argv[i])[0]=='-' ) return usage( std::cerr, argc, argv ); 
    else { 
      if ( appl::file_exists(argv[i]) ) fgrids.push_back(argv[i]);
    }
  }

  if ( dscale!=1 ) { 
    if ( !hset ) hscale = dscale;
    if ( !rset ) rscale = dscale; 
  }

  if ( output_grid=="" ) return usage(std::cerr, argc, argv);

  /// if reading in normalisation file ...

  NormVal* nv = 0;

  std::cout << "normfile: " << normfile << std::endl;

  if ( normfile!="" ) {
    
    /// read in all the coefficients and the grid file names ...
    
    if ( nv==0 ) nv = new NormVal( normfile );

    nv->read();  //    std::cout << "NormVal:\n" << *nv << std::endl;

    /// get all the grid files names to open ...

    fgrids = nv->grids(); 

  }


  //  if ( fgrids.size()<1 && normfile!="" ) return usage(std::cerr, argc, argv);
  if ( fgrids.size()<1 ) return usage(std::cerr, argc, argv);


  /// before adding the grids together, need tpo go through them to reject
  /// the outliers

  /// start by reading all the reference histograms and calculating the means etc

  std::cout << "reading grids:\n" << fgrids << std::endl;
  std::cout << "output to: "      << output_grid << std::endl;

  std::vector<appl::TH1D*> ref;
  ref.reserve(fgrids.size());

  std::vector<std::string>::iterator gitr=fgrids.begin();

  while ( gitr!=fgrids.end()  ) { 

    appl::TH1D* h = 0;

    if ( gitr->find(".root")==(gitr->size()-5) ) { 
#ifdef USEROOT
	TFile f(gitr->c_str());
	TH1D* h_ = (TH1D*)f.Get("grid/reference");
	h = convert( h_ );
	delete h_;
	f.Close();
#else
	std::cerr << "compiled without root - cannot read root files" << std::endl;
	std::exit(-1);
#endif
    }
    else { 
      appl::file f( *gitr );
      h = new appl::TH1D(f.Read<appl::TH1D>("reference"));
      f.Close();
    }

    /// remove obvious non-grid files
    if ( h ) { 
      ref.push_back(h);
      gitr++;
    }
    else {
      std::cout << "skipping file: " << *gitr << std::endl;
      fgrids.erase( gitr );
    }
  }

  std::cout << "main() read reference histograms" << std::endl;


  if ( ref.empty() ) { 
    std::cerr << "grid list empty " << std::endl;
    return 0;
  }


  if ( fgrids.empty() ) return 0;

  if ( fgrids.size()==1 ) addall = true;

  if ( !addall ) thin_grids( chi2_limit, ref, fgrids );

  struct timeval tstart = appl_timer_start();


  /// now add the grids together
  
  appl::grid*  gp = 0;

  /// will use the rms of the different reference histograms to estimate the proper 
  /// uncertainties

#if 0  

  int ig = 1;

  g.getReference()->SetLineColor( ig++ );
  g.getReference()->DrawCopy();

  gPad->SetLogy(true);

#endif

  for ( unsigned i=0 ; i<fgrids.size() ; i++ ) { 

    double t = appl_timer_stop( tstart )*0.001; 

    double remaining = fgrids.size()*t/i - t;

    std::cout << "applgrid-combine: adding grid " << i+1 << " of " << fgrids.size() 
	      << "\ttime so far " << t << "s"  
	      << "\testimated time remaining " << remaining << "s"  
	      << std::endl;  

    appl::grid*  _gp = new appl::grid( fgrids[i] );
    
    appl::grid& _g = *_gp;

    _g.untrim();
    if ( verbose ) std::cout << _g.getDocumentation() << std::endl;

    if ( nv!=0 ) { 

      std::vector<double> vc = nv->weights( fgrids[i] );

      if ( vc.size() != size_t(_g.Nobs()) ) { 
	std::cerr << "bin size mismatch: " << vc.size() << " " << _g.Nobs() << std::endl;
	std::exit(-1);
      }

      if ( _g.run() ) { 
	_g *= 1/_g.run();
	_g.run() = 0;
      }

      //      std::cout << "vc.size: " << vc.size() << std::endl;
      //      for ( size_t iv=0 ; iv<vc.size() ; iv++ ) vc[iv] *= fgrids.size(); 

      std::cout << "scaling grid: " << fgrids[i] << std::endl;

      _g *= vc;

    }

    if ( i==0 )  gp  = _gp;  
    else         { 
      (*gp) += _g;
      delete _gp;
    }

  }

  if ( gp==0 ) { std::cerr << "couldn't open grid " << std::endl; std::exit(-1); }

  appl::grid& g = *gp;
  
  ///  for ( int i=0 ; i<h->GetNbinsX() ; i++ ) { std::cout << "h " << i << " " << h->GetBinContent(i+1) << std::endl; }   
  ///  std::cout << "raw " << xsec << "\n" << xsec.mean() << "\n" << (xsec*fgrids.size()).mean() << std::endl;
  
  //  std::cout << "rintegral " << integral( g.getReference() ) << std::endl;


  if ( rscale!=1 ) { 
    g *= rscale;                         /// scales the grid *and* the reference histogram
    g.getReference()->Scale( 1/rscale ); /// scale back down the reference histogram
    //    g.Write(output_grid);
  }


  //  if ( hscale!=rscale ) g.getReference()->Scale( hscale/rscale );
  if ( hscale!=1 ) { 
    g.getReference()->Scale( hscale );
  }

  if      ( wscale!=1 ) g.run() *= wscale;
  else if ( weight!=0 ) g.run()  = weight;


  if ( shrink ) { 
    struct timeval toptstart = appl_timer_start(); 
    g.shrink( newpdfname ); 
    double topt = appl_timer_stop( toptstart ); 
    std::cout << argv[0] << ": compressed grid in " << topt << " ms" << std::endl;   
  }


  if ( optimise ) { 
    struct timeval toptstart = appl_timer_start(); 
    g.optimise();
    double topt = appl_timer_stop( toptstart ); 
    std::cout << argv[0] << ": optimised grid in " << topt << " ms" << std::endl;   
  }


  if ( pdfname!="" ) {
    g.setGeneratedPDF( pdfname );
    g.setGeneratediPDF( ipdf );
  }

  //  std::cout << "writing " << output_grid << std::endl;
  g.Write(output_grid);

  double t = appl_timer_stop( tstart )*0.001; 
  
  std::cout << argv[0] << ": added " << fgrids.size() << " grids in " << t << " s" << std::endl; 
  std::cout << argv[0] << ": output to  " << output_grid << std::endl; 
  

  /// stupid threads don't always die properly
  std::exit(0);
  return 0;
}
