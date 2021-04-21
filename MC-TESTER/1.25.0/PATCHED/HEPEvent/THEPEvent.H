/**
 * @class THEPEvent
 * @brief HEP Event information
 *
 * This class groups a set of THEPParticle objects into an event.
 * The functionality of this class together with class THEPParticle
 * is the same as this proided by COMMON/HEPEVT/ standard in FORTRAN77.
 * Particles makes a double-linked list: each particle have pointers
 * to its children, and children have pointers to a mother (or two
 * mothers). Event have a tree topology: particle-mother decays to
 * a few children. However sometimes this topology is violated: loops
 * occurs. To enable a posibility of multiple strange topologies, an
 * ordinary array is a storage for particles, and particles are refered
 * to by their identifiers (ordinal numbers), which simply maps to
 * array index. THEPParticle objects are stored/accessed from index 1,
 * that is not a rule of C/C++. This feature is based on FORTRAN77
 * default array indexing.
 * Maximum event size is defined in header file THEPEvent.H in
 * #define MAX_EVENT_SIZE directive. By default maximum number of
 * particles that can be stored inside THEPEvent is 10000. To change 
 * this defalut, modify MAX_EVENT_SIZE, recompile and relink
 * this library.
 *
 * The purpose of THEPEvent/THEPParticle classes is to provide universal
 * class capable of transporting an event record among various parts
 * of simulation/analysis programs.
 *
 * @author Piotr Golonka
 * @date 1999-04-21
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _PHOTOS_THEPEvent_H
#define _PHOTOS_THEPEvent_H

#ifdef _USE_ROOT_
#include <TObject.h>
#include <TBuffer.h>
#include <TClass.h>
#endif

#include "THEPParticle.H"
#include "HEPEvent.H"

#define MAX_EVENT_SIZE 10000 

class THEPEvent :public HEPEvent
#ifdef _USE_ROOT_
  ,public TObject 
#endif 
{
protected:
  HEPParticle** fParticles;      // storage array for particles...
  int           fSize;           // event record capacity

  int               fNumOfParticles; // number of particles in the event
  int               fEventNumber;    // event number

  THEPEvent();

 public:

           THEPEvent( int num );
           THEPEvent( const THEPEvent &e );
  virtual ~THEPEvent();

  virtual int         GetSize     ()        { return fSize;       }
  virtual const char* GetClassName()        { return "THEPEvent"; }
  
  virtual int   GetNumOfParticles ()        { return fNumOfParticles; }
  virtual int   GetEventNumber    ()        { return fEventNumber; }
  virtual void  SetEventNumber    (int num) { fEventNumber = num;  }
  virtual void  SetNumOfParticles (int num) { fNumOfParticles = num;  }
  
  const   THEPEvent&   operator=  ( const THEPEvent &e );
  const   bool         operator== ( const THEPEvent &e ) {return  isEqual(e);}
  const   bool         operator!= ( const THEPEvent &e ) {return !isEqual(e);}
  virtual bool         isEqual    ( const THEPEvent &e );
  virtual void         Diff       ( const HEPEvent &e );
  
  virtual int          GetIdOf    ( THEPParticle *particle);
  virtual void         AddParticle( HEPParticle *p );
  virtual void         SetParticle( int idx, HEPParticle *p );
  
  virtual void  InsertParticle(int at_idx,HEPParticle *p);
  virtual void  AddParticle( int id, int pdgid, int status,
			     int mother, int mother2,
			     int firstdaughter, int lastdaughter,
			     double E,double px, double py, double pz, double m,
			     double vx, double vy, double vz, double tau);
  



  virtual void         Clear      ( int fromIdx=1   );
  virtual void         ls         ( char *option=0  );
  
  virtual HEPParticle*  GetParticle   ( int idx );
  virtual THEPParticle*     GetHEPParticle( int idx ) 
                                   { return (THEPParticle*)GetParticle(idx); }
  #ifdef _USE_ROOT_
  ClassDef(THEPEvent,1) // HEP Event
  #endif
};


extern THEPEvent *gHEPEvent;



#ifdef _USE_ROOT_
TBuffer &operator<<(TBuffer &buf, THEPEvent *&obj);
TBuffer &operator>>(TBuffer &buf, THEPEvent *&obj);
#endif
      


#endif // _PHOTOS_THEPEvent_H
