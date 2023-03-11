/** emacs: this is -*- c++ -*- **/
/**
 **     @file    threadManager.h
 **
 **     @author  mark sutton
 **
 **     @copyright (C) 2002-2019 mark sutton (sutt @ cern.ch) 
 **
 **/


#ifndef __THREADMANAGER_H
#define __THREADMANAGER_H

#include <pthread.h>
#include <stdio.h>
#include <iostream>

#include <string>
using std::string;

    


class threadManager { 

public:

  threadManager(const string& s) : 
    mname(s), mrunning(false), mprocessing(false), mterminate(false) { init_mutex(); } 
  
  threadManager(const threadManager& t) : 
    mname(t.mname), mrunning(false), mprocessing(false), mterminate(false) { init_mutex(); }   

  void init_mutex() {  
    pthread_mutex_t tmpproc_mux = PTHREAD_MUTEX_INITIALIZER;
    proc_mux = tmpproc_mux;

    pthread_cond_t tmpproc_cv = PTHREAD_COND_INITIALIZER;
    proc_cv  = tmpproc_cv;


    pthread_mutex_t tmprun_mux = PTHREAD_MUTEX_INITIALIZER;
    run_mux = tmprun_mux;
    
    pthread_cond_t tmprun_cv = PTHREAD_COND_INITIALIZER;
    run_cv  = tmprun_cv;
    
      
    pthread_mutex_t tmpmu = PTHREAD_MUTEX_INITIALIZER;  
    mlock  = tmpmu;
  }

  virtual void start_thread() {
    mrunning = true;
    //    pthread_cond_t tmpcd = PTHREAD_COND_INITIALIZER;
    //   std::cout << "starting thread" ;
    mstatus = pthread_create( &mthread, 0, manage, this ); 
    //  std::cout << "thread started " << this << "  " << mname << std::endl;
  }
  
  virtual ~threadManager() { 
    if ( mrunning ) { 
      //      std::cout << "cancelling thread " << mname << " " << this << " ..." << std::endl;
      terminate();
      
      if ( this->ready() ) {  
	/// do we need to cancel the thread if it has been told to 
	/// finish and it has completed the processing?  
	pthread_cancel(mthread); 
	//	std::cout << "cancelled " << std::endl;
      }
    }
  }
  

  void    name(const string& name) { mname = name; }  
  string  name() const             { return mname; }  

  
  void lock()   { 
    //   return;
    pthread_mutex_lock(&mlock);
    //   printf("locked\n");
  }

  void unlock() {
    //  return;
    pthread_mutex_unlock(&mlock);
    //  printf("unlocked\n");
  }



  void suspend(bool _wait=true) { 
    lock_proc();

    lock_run(); 
    if ( mprocessing ) { 
      //  printf("sending signal\n");
      pthread_cond_signal(&run_cv);
      mprocessing = false;
    }
    unlock_run(); 

    if ( _wait ) wait_proc();

    unlock_proc();
  } 
  

  void process() {
    lock_proc(); 
    if ( mprocessing ) { 
      /// should never happen - any attempt to start the thread 
      /// processing should immediately be followed by waiting for 
      /// the thread to return itself to the suspended state   
      std::cerr << "error: thread already processing" << std::endl;
      unlock_proc(); 
      return;
    }
    mprocessing=true; 
    pthread_cond_signal(&proc_cv);
    unlock_proc();
  } 

  void terminate() {
    lock_proc(); 
    if ( mprocessing ) { 
      /// should never happen - any attempt to start the thread 
      /// processing should immediately be followed by waiting for 
      /// the thread to return itself to the suspended state   
      std::cerr << "error: thread already processing" << std::endl;
      unlock_proc(); 
      return;
    }
    mprocessing = true;
    mterminate  = true; 
    pthread_cond_signal(&proc_cv);
    unlock_proc();
  } 


 bool ready() {

    lock_proc();
    /// printf("waiting on thread 0x%lx (mprocessing %d)\n", (unsigned long)this, mprocessing );
    if ( !mprocessing ) { 
      unlock_proc();
      return true;
    }    
    unlock_proc();

    lock_run(); 
    /// printf("waiting ...\n");
    wait_run();
    ///    pthread_cond_signal(&run_cv);
    ///    printf("finished\n");
    unlock_run();
    return true;
  } 


  bool running() const { return mrunning; } 

  bool processing() const { return mprocessing; } 
    
  //  virtual void run() { std::cout << "don't call this" << std::endl; } ;
  virtual void run_thread() = 0;

protected:
  
  // this is clever, the thread function takes a parameter, 
  // but has to be a static function, so passing in the this
  // pointer, we can call non static member methods
  static void* manage(void* _this) {
    threadManager* me = (threadManager*)_this;
    me->run_thread();
    ///    std::cout << "thread completed " << me << std::endl; 
    return 0;
  }

protected:

  int             mstatus;
  string          mname;

  pthread_t       mthread;

  pthread_mutex_t mlock;

protected:

  bool            mrunning;

protected:

  bool            mprocessing;
  bool            mterminate;

  pthread_mutex_t proc_mux;
  pthread_cond_t  proc_cv;
  
  void lock_proc()   {  pthread_mutex_lock(&proc_mux); }
  void unlock_proc() {  pthread_mutex_unlock(&proc_mux); }
  
  void wait_proc()   { pthread_cond_wait(&proc_cv, &proc_mux); }
  

  pthread_mutex_t run_mux;
  pthread_cond_t  run_cv;

  void lock_run()   {  pthread_mutex_lock(&run_mux); }
  void unlock_run() {  pthread_mutex_unlock(&run_mux); }
  
  void wait_run()   { pthread_cond_wait(&run_cv, &run_mux); }


  pthread_mutex_t cache_mux;
  void lock_cache()   {  pthread_mutex_lock(&cache_mux); }
  void unlock_cache() {  pthread_mutex_unlock(&cache_mux); }
  

};

#endif //__THREADMANAGER_H
