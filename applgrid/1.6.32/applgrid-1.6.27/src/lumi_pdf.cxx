/**
 **     @file    lumi_pdf.cxx
 **
 **     @author  mark sutton
 **     @date    Tue  9 Jul 2013 08:14:47 CEST 
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **     $Id: lumi_pdf.cxx, v0.0   Tue  9 Jul 2013 08:14:47 CEST sutt $
 **
 **/


#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

#include "appl_grid/lumi_pdf.h"


std::string str_replace( std::string s ) { 
  std::string a = s;
  for ( int i=a.size() ; i-- ; ) if ( a[i]=='_' ) a[i]='-';
  return a;
}


void latex( const lumi_pdf& p, const std::string& d);

bool lumi_pdf::m_runlatex = false;

lumi_pdf::lumi_pdf(const std::string& s, const std::vector<int>& combinations ) : // , int Wcharge ) :  //, bool amcflag ) : 
  appl_pdf(s, true), m_filename(s), 
  m_lookup( std::vector<std::vector<std::vector<int> > >(0) ) 
  //,  m_amcflag(amcflag)
{

  /// need to decode the input std::vector

  if ( combinations.size() ) { 

    /// std::vector initialised from serialised std::vector
    unsigned iv = 0;

    unsigned nproc = combinations[iv++];

    for ( unsigned i=0 ; i<nproc && iv<combinations.size() ; i++ ) {
      int index  = combinations[iv++];
      int npairs = combinations[iv++];

      std::vector<int> v(npairs*2+2);
      v[0] = index;
      v[1] = npairs;
      
      for ( int j=0 ; j<npairs ; j++ ) { 
	v[j*2+2] = combinations[iv++];
	v[j*2+3] = combinations[iv++];
      }

      combination c(v);
      if ( c.size() ) add(c); 
    } 

    /// extra value on the end for the W+- charge (if required) - flags that 
    /// ckm matrix is to be used    
    if ( iv<combinations.size() ) m_ckmcharge = combinations[iv];

  }
  else if ( m_filename!="" ) {  
    /// else read from file ...
  
    std::ifstream& infile = openpdf( m_filename  );

    /// will never fail, appl_pdf::open() would have thrown already
    ///  if ( infile.fail() ) throw exception( std::cerr << "lumi_pdf::lumi_pdf() cannot open file " << m_filename << std::endl ); 
 
    std::string   line;

    infile >> m_ckmcharge;
    
    ///    std::cout << "ckmcharge " << m_ckmcharge << std::endl;  

    while (std::getline(infile, line)) {
      //    std::cout << "line: " << line << std::endl;
      combination c( line );
      if ( c.size() ) add(c); 
    }
    
    infile.close();
  }

  if ( m_ckmcharge>0 ) { 
    std::cout << "lumi_pdf::lumi_pdf() setting W+ cmk matrix" << std::endl;
    make_ckm(true);
  }
  else if ( m_ckmcharge<0  ) { 
    std::cout << "lumi_pdf::lumi_pdf() setting W- cmk matrix" << std::endl;
    make_ckm(false);
  }

  
  // some checking
  
  //  for ( int i=0 ; i<m_combinations.size() ; i++ ) { 
  //    if ( m_combinations[i].
  //  }

  m_Nproc = m_combinations.size();


  // create the reverse lookup 

  create_lookup();

  //  std::cout << "decideSuprocess " << decideSubProcess( 0, 0 ) << std::endl;
  //  std::cout << "lumi_pdf::lumi_pdf() " << s << "\tv size " << m_combinations.size() << " lookup size " << m_lookup.size() << std::endl; 
  //  std::cout << *this << std::endl;

  //  lumi_pdf* _pdf = dynamic_cast<lumi_pdf*>(appl::appl_pdf::getpdf(name()));
  //  std::cout << "done " << _pdf << _pdf->decideSubProcess( 0, 0 ) << std::endl;

  //  std::cout << *this << std::endl;

  if ( m_name!="" ) addtopdfmap(m_name, this);

}





lumi_pdf::lumi_pdf(const std::string& s, const std::vector<combination>& combinations, int ckmcharge ) : 
  appl_pdf(s), m_filename(s), m_combinations(combinations),
  m_lookup( std::vector<std::vector<std::vector<int> > >(0) ) 
{
  
  //  std::cout << "lumi_pdf::lumi_pdf() " << s << "\tv size " << combinations.size() << " lookup size " << m_lookup.size() << " " << this << std::endl; 

  /// no need need to decode the input std::vector

  /// the W+- charge (if required) - flags that 
  /// ckm matrix is to be used    
  m_ckmcharge = ckmcharge;

  if ( m_ckmcharge>0 ) { 
    std::cout << "lumi_pdf::lumi_pdf() setting W+ cmk matrix" << std::endl;
    make_ckm(true);
  }
  else if ( m_ckmcharge<0  ) { 
    std::cout << "lumi_pdf::lumi_pdf() setting W- cmk matrix" << std::endl;
    make_ckm(false);
  }

  m_Nproc = m_combinations.size();

  create_lookup();

  //  std::cout << *this << std::endl; 
  
}



void lumi_pdf::create_lookup() { 
  if ( m_lookup.size()==0 ) { 
    /// create a 14 x 14 lookup table (including a photon contribution! 
    m_lookup = std::vector<std::vector<std::vector<int> > >(14, std::vector<std::vector<int> >(14) ); 
    for ( unsigned i=size() ; i-- ; ) { 
      const combination& c = m_combinations[i];
      for ( unsigned j=c.size() ; j-- ; ) m_lookup[ c[j].first+6 ][ c[j].second+6 ].push_back(i);
    } 
  }

  m_proclookup.clear();

  for ( unsigned i=size() ; i-- ; ) { 
    const combination& c = m_combinations[i];
    for ( unsigned j=c.index().size() ; j-- ; ) { 
      std::map<int,int>::iterator itr = m_proclookup.find(c.index()[j]);
      if ( itr==m_proclookup.end() ) m_proclookup.insert( std::map<int,int>::value_type( c.index()[j], i ) );
    }
  } 
  
}



int lumi_pdf::decideSubProcess(const int iproc ) const {
  std::map<int,int>::const_iterator itr = m_proclookup.find(iproc);
  if ( itr==m_proclookup.end() ) return -1;
  else return itr->second;
}


void lumi_pdf::evaluate(const double* xfA, const double* xfB, double* H) const { 
  /// if need to include the ckm matrix ...
  if ( m_ckmcharge==0 )  {
    for ( unsigned i=size() ; i-- ; ) { 
      H[i] = m_combinations[i].evaluate( xfA, xfB ); 
    }
  }
  else { 
   for ( unsigned i=size() ; i-- ; ) { 
     H[i] = m_combinations[i].evaluate( xfA, xfB, m_ckmsum, m_ckm2 ); 
    }
  }
}


int  lumi_pdf::decideSubProcess(const int iflav1, const int iflav2) const { 
  //  std::cout << "lumi_pdf::decideSubProcess() " << name() << " " << m_lookup.size() << std::endl;
  if ( m_lookup[iflav1+6][iflav2+6].size()==1 ) return m_lookup[iflav1+6][iflav2+6][0];
  return -1;
}


size_t  lumi_pdf::nSubProcesses(const int iflav1, const int iflav2) const { 
  //  std::cout << "lumi_pdf::decideSubProcess() " << name() << " " << m_lookup.size() << std::endl;
  return m_lookup[iflav1+6][iflav2+6].size();
}


std::vector<int> lumi_pdf::decideSubProcesses(const int iflav1, const int iflav2) const { 
  //  std::cout << "lumi_pdf::decideSubProcess() " << name() << " " << m_lookup.size() << std::endl;
  return m_lookup[iflav1+6][iflav2+6];
}

#if 0
std::vector<int> lumi_pdf::decideSubProcesses(const int iflav ) const { 
  //  std::cout << "lumi_pdf::decideSubProcess() " << name() << " " << m_lookup.size() << std::endl;
  return m_lookup[iflav+6][iflav+6];
}

int  lumi_pdf::decideSubProcess(const int iflav ) const { 
  //  std::cout << "lumi_pdf::decideSubProcess() " << name() << " " << m_lookup.size() << std::endl;
  if ( m_lookup[iflav+6][iflav+6].size()==1 ) return m_lookup[iflav+6][iflav+6][0];
  return -1;
}
#endif


std::vector<int> lumi_pdf::serialise() const  { 

  std::vector<int> v;

  v.push_back( Nproc() );

  for ( int i=0 ; i<Nproc() ; i++ ) { 

    const combination& c = m_combinations[i];

    v.push_back( c.index()[0] );

    v.push_back( c.size() );
    for ( unsigned j=0 ; j<c.size() ; j++ ) { 
      v.push_back( c[j].first );
      v.push_back( c[j].second );
    }
  }  
  
  if      ( m_ckmcharge>0 ) v.push_back(1);
  else if ( m_ckmcharge<0 ) v.push_back(-1);
  else                      v.push_back(0);
			 
  return v;
}



std::vector<std::vector<int> > lumi_pdf::vectorise() const  { 

  std::vector<std::vector<int> > v;

  for ( int i=0 ; i<Nproc() ; i++ ) { 

    std::vector<int> v0;
    v0.push_back( i );

    const combination& c = m_combinations[i];
    for ( unsigned j=0 ; j<c.size() ; j++ ) { 
      v0.push_back( c[j].first );
      v0.push_back( c[j].second );
    }
    v.push_back( v0 );
  }  
  
  return v;
}





void lumi_pdf::write(std::ostream& s) const { 

  std::cout << "lumi_pdf::write() " << name() << std::endl;
  
  s << m_ckmcharge << "\n";

  for ( unsigned i=0 ; i<m_combinations.size() ; i++ ) { 

    //    std::cout << "swrite: " << m_combinations[i].index()[0] << std::endl;

    s << m_combinations[i].index()[0] << " ";
    s << m_combinations[i].size()  << " ";

    for ( unsigned j=0 ; j<m_combinations[i].size() ; j++ ) { 
      s << "  " << m_combinations[i][j].first << " " << m_combinations[i][j].second;
    }

    s << "\n";

  }

}


void lumi_pdf::write(const std::string& filename) const {  
  std::ofstream s(filename.c_str());
  write(s);
}


// std::string lumi_pdf::summary(std::ostream& s=std::cout) const { 
std::string lumi_pdf::summary() const { 
  std::stringstream s_;
  s_ << "lumi_pdf::lumi_pdf()\t" << name() << "\tcombinations " << m_combinations.size() << "\tlookup size " << m_lookup.size() << "\taddr: " << this; 
  return s_.str();
}




bool lumi_pdf::contains( int i ) const { 
  for ( int ic=0 ; ic<Nproc() ; ic++ ) { 
    const combination& c = m_combinations[ic];
    for ( size_t ip=0 ; ip<c.size() ; ip++ ) { 
      if ( c.pair(ip).first==i || c.pair(ip).second==i ) return true; 
    }
  }
  return false;
}
 


void lumi_pdf::removeDuplicates() { 

  size_t original_size = size();

  std::vector<combination> combinations;
  
  for ( unsigned i=0 ; i<size() ; i++ ) {
    bool unique = true;
    for ( unsigned j=0 ; j<combinations.size() ; j++ ) {
      if ( at(i) == combinations[j] ) {
	unique = false;
	// std::cout << "duplicate:\n" << at(i) << "\n" << combinations[j] << std::endl;
	combinations[j].add_index( at(i).index()[0] );
      }
    }
    if ( unique ) combinations.push_back( at(i) );
  }

  m_combinations = combinations;

  m_Nproc = m_combinations.size();

  create_lookup();

  std::cout << "lumi_pdf::removeDuplicates() " << name() << "\tsize " << original_size << " -> " << size() << std::endl; 

  if ( m_runlatex ) latex( *this, ".pdf" );

}
   




void lumi_pdf::restoreDuplicates() { 

  std::vector<combination> combinations;

  for ( unsigned i=0 ; i<size() ; i++ ) {

    std::vector<int> indices = at(i).index();

    for ( unsigned j=0 ; j<indices.size() ; j++ ) {

      combination c = at(i);

      c.index().clear();
      c.index().push_back(indices[j]);

      combinations.push_back( c );
    }
  }

  std::sort( combinations.begin(), combinations.end() );

  m_combinations = combinations;

  m_Nproc = m_combinations.size();

  create_lookup();
  
}


/// delete a combination
void lumi_pdf::remove( int i ) {   

    std::cout << "lumi_pdf::remove() remving combination: " << i << "\t" << m_combinations[i] << std::endl;

    std::vector<combination>::iterator itr = m_combinations.begin();
    for ( int ic=0 ; itr!=m_combinations.end() ; itr++, ic++ ) {
      std::cout << *itr << std::endl;
      if ( ic==i ) break;
    }
   
    if ( itr!=m_combinations.end() ) { 
      m_combinations.erase( itr );
      m_Nproc = m_combinations.size();
      create_lookup();
    }

}


   



#include <fstream>
#include <cstdlib>

/// decode as a latex table, and create pdf file

void latex( const lumi_pdf& p, const std::string& d) {

  std::cout << "latex() : " << d << std::endl; 

  std::ofstream zj( (d+p.name()+".tex").c_str() );
 
  std::string _f[14] = { 
    "\\bar{t}", 
    "\\bar{b}", 
    "\\bar{c}", 
    "\\bar{s}", 
    "\\bar{u}", 
    "\\bar{d}",
    "g",
    "d",
    "u", 
    "s", 
    "c", 
    "b", 
    "t",
    "\\gamma" };

  const std::string* f = _f+6; 


  //  std::cout << "\\bigskip\n";

  std::cout << "Contribution:   " << p.name() << "\t processes " << p.Nproc() << "\n";

  bool landscape = false;

  size_t maxind  = 20;
  size_t maxindc = 15;

  if ( landscape ) { 
    zj << "\\documentclass[7pt,a4paper,landscape]{article}\n\n";
        
    zj << "\\usepackage{makecell}\n\n";
    zj << "\\usepackage[a4paper,landscape]{geometry}\n\n";
        
    /// WTF ? 
    //  zj << "\\usepackage{atbegshi}% http://ctan.org/pkg/atbegshi\n";
    //  zj << "\\AtBeginDocument{\\AtBeginShipoutNext{\\AtBeginShipoutDiscard}}\n";
    
    zj << "\\topmargin=-3cm\n";
    zj << "\\textheight=18cm\n";
    zj << "\\textwidth=28cm\n";
    zj << "\\oddsidemargin=-1.5cm\n";

  }
  else { 
    //  zj << "\\documentclass[7pt,a4paper]{article}\n\n";
    
    //  zj << "\\usepackage{makecell}\n\n";
    //  zj << "\\usepackage[a4paper]{geometry}\n\n";
    
    //  zj << "\\topmargin=-3cm\n";
    //  zj << "\\textheight=28cm\n";
    //  zj << "\\textwidth=18cm\n";
    //  zj << "\\oddsidemargin=-1.5cm\n";

    maxind  = 3;
    maxindc = 4;

  }




  //  zj << "\\begin{document}\n";

  //  zj << "\\ \\\\ \\ \\\\ \\ \\\\\n";

  zj << "\\begin{table}\n" << std::endl;
  zj << "\\caption{Mapping between grid ID And NNLOJET ID for parton contributions at " << d << ".}\n" << std::endl;

  zj << "\\label{tab:" << d << "}" << std::endl;

  zj << "{\\footnotesize\n";


  size_t maxproc = 0;

  size_t totalproc = 0;

  //  std::cout << "Nproc: " << p.Nproc() << " " << p.size() << std::endl; 

  for ( int i=0 ; i<p.Nproc() ; i++ ) { 
    const combination& c = p[i];

    const std::vector<int>& ind = c.index();

    if ( ind.size()>maxproc ) maxproc = ind.size();

    totalproc += ind.size();
  }  

  //  size_t maxpairs = 20-maxproc;

  //  if ( maxpairs<1 ) maxpairs = maxproc;

  //  zj << "\\hspace{-12cm}";
  //  zj << "\\begin{minipage}[t]{18cm}\n";
  //  zj << "pdf : " << str_replace(p.name()) << "\tnprocesses: " << totalproc  << "\\\\" << std::endl; 
  zj << "\\begin{tabular}[t]{cll}\\\\\n";

  zj << "grid PID & NNLOJET PID & parton contributions\\\\\n\\hline\\\\[-2mm]" << std::endl;

  int irow = 0;

  for ( int i=0 ; i<p.Nproc() ; i++ ) { 

    const combination& c = p[i];

    const std::vector<int>& ind = c.index();

    int nrows = (ind.size()+27)/28;

    zj << "\\makecell[t]{ " << i;
    for ( int ig=1 ; ig<nrows ; ig++ ) zj << " \\\\ \\ ";
    zj << "}\t&\t";

    zj << "\\makecell[lt]{";
    // zj << "\\thead[l]{";

    for ( unsigned j=0 ; j<ind.size() ; j++ ) {
      //    if ( ind[j]<100 ) zj << "~";
      //    if ( ind[j]<10  ) zj << "~";
      zj << ind[j] << " ";
      if ( j>0 && (j+1)%maxind==0 ) zj << "\\\\";
    }

    zj << "}\t&\t";

    zj << "\\makecell[lt]{ ";

    for ( unsigned j=0 ; j<c.size() ; j++ ) {

      int p0 = c[j].first;
      int p1 = c[j].second;

      if ( j>0 ) zj << " + ";

      if ( j>0 && j%maxindc==0 ) zj << " \\\\ \\ ";
      // if ( j%maxindc==0 ) zj << " \\\\ \\ ";

      zj << "($" << f[p0] << "$, $" << f[p1] << "$)\t";  

    }

    for ( int ig=1 ; ig<nrows ; ig++ ) zj << " \\\\ \\ ";
    zj << "}";

    zj << "\\\\\n";
    zj << "\\\\[-2mm]\n";
    
    if ( irow==46 ) { 

      irow=0;

      zj << "\\\\\n";
      zj << "\\hline\n";
      zj << "\\end{tabular}\n";
      //      zj << "\\end{minipage}\n";

      zj << "\\pagebreak[4];\n\n\n";

      //      zj << "\\hspace{-12cm}";
      //      zj << "\\begin{minipage}[t]{18cm}\n";
      //    zj << "pdf : " << str_replace(p.name()) << "\tnprocesses: " << totalproc  << " (continued)\\\\" << std::endl; 
      zj << "\\begin{tabular}[t]{cll}\\hline\\\\\n";

    }
    else irow++;
  }

  zj << "\\\\\n";
  zj << "\\hline\n";
  zj << "\\end{tabular}\n";
  //  zj << "\\end{minipage}\n";

  zj << "}\n";

  zj << "\\end{table}\n" << std::endl;

  //  zj << "\\end{document}\n";

  zj.close();

  std::cout << "running latex to file : " << d+p.name() << std::endl;

  std::string cmd = std::string("pdflatex ") + d+p.name()+".tex > " + d+p.name() + ".log ";

  //  std::system( cmd.c_str() );

}




