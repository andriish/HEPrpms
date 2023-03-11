/**
 **   @file    appl_file.cxx        
 **                   
 **   @author  sutt
 **   @date    Wed 30 Dec 2020 15:25:42 GMT
 **
 **   $Id: appl_file.cxx, v0.0   Wed 30 Dec 2020 15:25:42 GMT sutt $
 **
 **   Copyright (C) 2020 sutt (sutt@cern.ch)    
 **
 **/


#include "appl_grid/appl_file.h"

#include <sys/stat.h>


appl::file::file( const std::string& name, const std::string& option ) : 
  mfilename(name), mfile(0), mopen(false), msize(0) { 

    mopt = option;

    /// make sure it is always binary mode
    if ( mopt.find("b")==std::string::npos ) mopt += "b";

    //    std::cout << "appl::file::opening file: " << filename() << "\toptions: " << mopt << std::endl; 

    /// file sizes - actual filesize and uncopmpressed size
    int filesize = 0;
    int usize    = 0;
    
    if ( mopt.find("r")!=std::string::npos ) { 

      /// first get file size ...

      struct stat stat_buf;
      int rc = stat(filename().c_str(), &stat_buf);
      if ( rc == 0 ) filesize = stat_buf.st_size;
      else { 
	std::cerr << "appl::file: can not determine file size for file: " << filename() << std::endl;
	return;
      }

      /// next check if it is in zipped format ...
      
      FILE* tmp_file = fopen( filename().c_str(), "rb" );
      int zip_signature = 0;
      fread( &zip_signature, sizeof(int), 1, tmp_file );
      if ( (zip_signature&0xffffff)==0x88b1f ) {   
	/// if it is read the file size, and the uncompressed filesize from the file ...
	/// uncompressed size
	int offset = filesize - 4;
	fseek( tmp_file, offset, SEEK_SET );
	fread( &usize, sizeof(int), 1, tmp_file );
      }
      else usize = filesize;
      
      //      std::cout << "\tfile size:         " << filesize << std::endl;
      //      std::cout << "\tuncompressed size: " << usize << std::endl;
      
      fclose( tmp_file );
      
    }
      
    /// now open the file ...

    mfile = gzopen( mfilename.c_str(), mopt.c_str() );

    mopen = true;

    /// write header
    if ( mopt.find("w")!=std::string::npos ) {

      double v = SB::WRITEGUARD;
      gzwrite( mfile, (void*)&v,  sizeof(SB::TYPE) );
      mindex.add( "header", sizeof(SB::TYPE) );

      msize += sizeof(SB::TYPE);

      /// the index is added at the end before the file is closed
    }
    if ( mopt.find("r")!=std::string::npos ) { 

      /// read the header ...

      double v = 0;
      gzread( mfile, (void*)&v,   sizeof(SB::TYPE) );
      
      /// if this one of our files ? 
      if ( v!=SB::WRITEGUARD ) { 
	std::cerr << "appl::file: incorrect file format file: " << filename() << std::endl;
	Close();
	mopen = false;
	return;
      }

      /// yes it is so ...

      /// first get the (uncompressed) file size from the last 4 bytes 
      /// of the compressed file, then use that get the offsets to the 
      /// index stored at the end of the file - *why* does this have to 
      /// be soooo difficult 

      //      std::cout << "opened file: " << filename() << "\tsize: " << filesize << std::endl;
      
      /// read the uncompressed file size from the comprfessed file
      /// so that we can navigate through the commpressed file, 
      /// to get the index in case we want unsequential access
      
      //      std::cout << "uncompressed size: " << usize << std::endl;
      
      /// get the total file size and offset of the index offset
      
      SB::TYPE vtrailer[3];
      
      gzseek( mfile, usize-sizeof(SB::TYPE)*3, SEEK_SET );
      gzread( mfile, (void*)vtrailer, sizeof(double)*3 );

      int index_size = (vtrailer[1]-vtrailer[0]-sizeof(SB::TYPE)*3); /// in bytes
      
      std::vector<SB::TYPE> vindex(index_size/sizeof(SB::TYPE)); 
           
      int indexptr = vtrailer[0];

      ///      std::cout << "vtrailer[0]: " << vtrailer[0] << std::endl;
      ///      std::cout << "vtrailer[1]: " << vtrailer[1] << std::endl;
      ///      std::cout << "vtrailer[2]: " << vtrailer[2] << std::endl;
      
      //      gzrewind( mfile );
      gzseek( mfile, indexptr, SEEK_SET ); 
      gzread( mfile, (void*)&vindex[0], index_size ); 
      
      mindex.deserialise( vindex );
      
      //      std::cout << mindex << std::endl;
      
      /// rewind to open the file for proper reading ...
      gzrewind( mfile );
      /// read the header again, so that we are aligned with the 
      /// first object in the file
      gzread( mfile, (void*)&v,   sizeof(SB::TYPE) );
      
      //      std::cout << "write guard " << v << std::endl;
      
    }
}
 

appl::file::~file() { Close(); }


void appl::file::Close() {
    
    if ( isopen() ) {

      /// if writing the file, update the global size 

      if ( mopt.find("w")!=std::string::npos ) { 
	double content_trailer = SB::WRITEGUARD;
	int bytes = gzwrite( mfile, (void*)&content_trailer,  sizeof(SB::TYPE) );
	mindex.add( "trailer", bytes ); 
	
	msize += bytes;
	
	double index_offset = msize;

	//	std::cout << "writing: index offset: " << index_offset << std::endl;  

	/// this is a thorny one - should we add the index to the index ?
	/// until the index has been written, it should not be in the index
	/// but once it has been written, then it *should* be in the index
	/// so we would need to add the index to itself, *before* writing 
	/// to the file
	/// basically, I think it is more sensible *not* to include the 
	/// index itself into the index, but of course, that could could 
	/// be changed if necessary
	/// what we *should* do, is to add some pointer to the index at 
	/// the start of the file, so that we can navigate to it directly 
	/// on openning the file     
	Write( mindex );
	
	/// this doesn;t work - need to work out why
	//	gzrewind( mfile );
	//	gzseek( mfile, sizeof(double), SEEK_SET );
	//    	double tsize = msize;

	SB::TYPE trailer[3] = { 0, 0, 0 };
	trailer[0] = index_offset;
	trailer[1] = msize+3*sizeof(SB::TYPE);
	trailer[2] = SB::WRITEGUARD; 

	bytes = gzwrite( mfile, (void*)&trailer, sizeof(SB::TYPE)*3 );
	//	std::cout << "writing trailer: " << std::endl;
	//	std::cout << "\tindex offset: " << trailer[0] << std::endl;  
	//	std::cout << "\ttotal size:   " << trailer[1] << std::endl;  
	//	std::cout << "\twrite guard:  " << trailer[2] << std::endl;  

	mindex.add("file trailer", bytes );
      }

      gzclose(mfile);
      mopen = false;

      mindex.clear();
      
    }

}




