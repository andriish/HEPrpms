/**
 * @class THEPParticle
 * @brief HEP Particle information
 *
 * This class stores the information about single particle from HEP event
 * record. Together with THEPEvent class it provides the functionality to
 * store the same information that was stored in COMMON/HEPEVT/ data
 * structure, which was a standard for HEP calculations.
 *
 * Global object THEPParticle *nullHEPParticle is automatically created.
 * it purpose is to serve as a convenience replacement for null pointer
 * of type THEPParticle*
 *
 * @author Piotr Golonka
 * @date 1999-04-21
 *
 * (C) Faculty of Nuclear Physics & Techniques, UMM Cracow.
 */

#ifndef _PHOTOS_THEPParticle_H
#define _PHOTOS_THEPParticle_H

#include "HEPParticle.H"
#include "HEPEvent.H"

#ifdef _USE_ROOT_
#include <TObject.h>
#endif

#include <stdio.h>

class THEPEvent;

class THEPParticle : public HEPParticle
#if defined _USE_ROOT_
  , public TObject
#endif 
{


 protected:
  THEPEvent*   fEvent;  // pointer to event that contains this particle
  
  int    fId;           // particle's identifier (ordinal number) in  event
  int    fPDG;          // identity code according to PDG standard
  int    fStatus;       // status code, aka HEPEVT

  int    fMother1;      // first  mother's   identifier
  int    fMother2;      // second mother's   identifier
  int    fDaughter1;    // first  daughter's identifier
  int    fDaughter2;    // last   daughter's identifier

  double fPx;           // momentum in x direction [GeV/c]
  double fPy;           // momentum in y direction [GeV/c]
  double fPz;           // momentum in z direction [GeV/c]
  double fE;            // energy [GeV]
  double fM;            // mass [GeV/c^2]
  
  double fVx;           // x coord of production vertex [mm]
  double fVy;           // y coord of production vertex [mm]
  double fVz;           // z coord of production vertex [mm]
  double fVtau;         // production time [mm/c]


public:
  THEPParticle(){}

  THEPParticle(int id, 
	       int pdgid,
	       int status, 
	       int mother1,
	       int mother2,
	       int daughter1,
	       int daughter2,
	       double px, 
	       double py, 
	       double pz, 
	       double E, 
	       double M,
	       double vx, 
	       double vy, 
	       double vz, 
	       double tau);

  const THEPParticle operator= (const THEPParticle &p);

  const bool         operator==( THEPParticle &p) { return  isEqual(p); }
  const bool         operator!=( THEPParticle &p) { return !isEqual(p); }


  virtual double const GetE ()                         { return fE;          }
  virtual double const GetPx()                         { return fPx;         }
  virtual double const GetPy()                         { return fPy;         }
  virtual double const GetPz()                         { return fPz;         }
 
  virtual double const GetM ()                         { return fM;          }
  virtual int    const GetPDGId()                      { return fPDG;        }
  virtual int    const GetStatus()                     { return fStatus;     }
  virtual int    const  IsStable()                     { return (GetStatus() == 1); }
  virtual int    const  Decays()                       { return (GetStatus() == 2); }
  virtual int    const  IsHistoryEntry()               { return (GetStatus() == 3); }
    
  virtual double const GetVx ()                        { return fVx;         }
  virtual double const GetVy ()                        { return fVy;         }
  virtual double const GetVz ()                        { return fVz;         }
  virtual double const GetTau()                        { return fVtau;       }

  virtual int    const GetId()                         { return fId;         }
  virtual int    const GetMother()                     { return fMother1;    }
  virtual int    const GetMother2()                    { return fMother2;    }
  virtual int    const GetFirstDaughter()              { return fDaughter1;  }
  virtual int    const GetLastDaughter()               { return fDaughter2;  }


  virtual void   SetE  ( double E  )        { fE   = E ; }
  virtual void   SetPx ( double px )        { fPx  = px; }
  virtual void   SetPy ( double py )        { fPy  = py; }
  virtual void   SetPz ( double pz )        { fPz  = pz; }
 
  virtual void   SetM ( double m  )         { fM   = m ;  }
  virtual void   SetPDGId( int id )         { fPDG = id;  }

  virtual void   SetStatus( int st )        { fStatus    = st; }

  virtual void   SetVx ( double vx )        { fVx        = vx; }
  virtual void   SetVy ( double vy )        { fVy        = vy; }
  virtual void   SetVz ( double vz )        { fVz        = vz; }
  virtual void   SetTau( double tau)        { fVtau      = tau;}

  virtual void   SetId     ( int id )       { fId        = id; }
  virtual void   SetMother ( int id )       { fMother1   = id; }
  virtual void   SetMother2( int id )       { fMother2   = id; }
  virtual void   SetFirstDaughter( int id ) { fDaughter1 = id; }
  virtual void   SetLastDaughter ( int id ) { fDaughter2 = id; }

  virtual void   SetMother( THEPParticle *mother) ;
  virtual void   SetMother2( THEPParticle *mother) ;
  virtual void   SetFirstDaughter( THEPParticle *daughter) ;
  virtual void   SetLastDaughter( THEPParticle *daughter) ;
  
  virtual THEPParticle* Mother(); 
  virtual THEPParticle* Mother2(); 
  virtual THEPParticle* FirstDaughter(); 
  virtual THEPParticle* LastDaughter(); 
  virtual THEPParticle* PrevDaughter(); 
  virtual THEPParticle* NextDaughter(); 
  virtual THEPEvent*   Event();
   
  virtual void   ls(char *option=0);

  virtual void   Diff(HEPParticle &p);
  virtual bool   isEqual(THEPParticle &p);
 


  // implementation of HEPEvent methods:
  virtual HEPEvent*    GetEvent        () { return (HEPEvent*) fEvent; }

  virtual void   SetEvent        ( HEPEvent       *event    );

  // functions to be hidden:
  virtual void   CastError(char* funcName);

  #ifdef _USE_ROOT_
  ClassDef(THEPParticle,1) // HEP Particle
  #endif

};

extern THEPParticle *nullHEPParticle; // particle used in place of null pointer

#endif // _PHOTOS_THEPParticle_H








