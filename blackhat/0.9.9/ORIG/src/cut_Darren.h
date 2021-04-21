#ifndef CUT_DARREN_H
#define CUT_DARREN_H

#include "partitions.h"
#include "cut_worker.h"
#include "momenta_evaluator.h"


#include "bubble_specification.h"
#include "triangle_specification.h"
#include "triangle_subtraction.h"

//
// Definitions for the number of points in the specifications
//

// The standard number of points, even numbers are more numerically stable.
#define BUBYPOINTS_STD 2
#define BUBPOINTS_STD 4 // We could use 3 here but 4 gives better numerical accuracy
#define TRIPOINTS_STD 8
#define CTRIPOINTS_STD 7	

// The higgs points
#define BUBYPOINTS_HIGGS 4
#define BUBPOINTS_HIGGS 4
#define TRIPOINTS_HIGGS 9
#define CTRIPOINTS_HIGGS 9	



namespace BH {
    class Rec_Tree;


}



#ifdef __cplusplus

#define JOIN( X, Y ) JOIN2(X,Y)
#define JOIN2( X, Y ) X##Y

namespace static_assert
{
    template <bool> struct STATIC_ASSERT_FAILURE;
    template <> struct STATIC_ASSERT_FAILURE<true> { enum { value = 1 }; };

    template<int x> struct static_assert_test{};
}

#define COMPILE_ASSERT(x) \
    typedef ::static_assert::static_assert_test<\
        sizeof(::static_assert::STATIC_ASSERT_FAILURE< (bool)( x ) >)>\
            JOIN(_static_assert_typedef, __LINE__)

#else // __cplusplus

#define COMPILE_ASSERT(x) extern int __dummy[(int)x]

#endif // __cplusplus

#define VERIFY_EXPLICIT_CAST(from, to) COMPILE_ASSERT(sizeof(from) == sizeof(to))





namespace BH {

namespace cut {



namespace Darren {


template <class BASE> class Darren_wrapper : public BASE {
    std::vector<TREE_TYPE*> d_trees;

public:
        Darren_wrapper(const BASE& cd);
        std::complex<R> eval_tree(int n,momentum_configuration<R>& mc,const std::vector<int>& ind);
        std::complex<RHP> eval_tree(int n,momentum_configuration<RHP>& mc,const std::vector<int>& ind);
        std::complex<RVHP> eval_tree(int n,momentum_configuration<RVHP>& mc,const std::vector<int>& ind);
        std::complex<R> eval_tree(int n,const eval_param<R>& ep);
        std::complex<RHP> eval_tree(int n,const eval_param<RHP>& ep);
        std::complex<RVHP> eval_tree(int n,const eval_param<RVHP>& ep);

#ifdef BH_USE_GMP
        std::complex<RGMP> eval_tree(int n,momentum_configuration<RGMP>& mc,const std::vector<int>& ind);
        std::complex<RGMP> eval_tree(int n,const eval_param<RGMP>& ep);
#endif

        int corner_size(int cor) const {return BASE::c(cor).size();}
        int corner_ind(int cor,int ind) const {return BASE::c(cor)[ind-1].ind();}
        virtual ~Darren_wrapper();
};
#ifdef SWIG
%template(DWbox) Darren_wrapper<BH::boxD>;
%template(DWtriangle) Darren_wrapper<BH::triangleD>;
%template(DWbubble) Darren_wrapper<BH::bubbleD>;
#endif

template <class cutDbase, int CPOINTS, int TPOINTSTRI> class box_Darren : public cutDbase, public box_Darren_eval_points<TPOINTSTRI> {
	long int mcID; // Used for tracking whether we have evaluated anything or not
	unsigned long int epID;
	std::vector<int> indID;

	C coeffs[2];
	CHP coeffs_HP[2];
	CVHP coeffs_VHP[2];
	C D0coeff;
    CHP D0coeff_HP;
    CVHP D0coeff_VHP;

    // Stores the information we need to relate the d_+ and d_- solutions to the correct pole
    Cmom<R> K1fsmom, K2fsmom;
    Cmom<RHP> K1fsmom_HP, K2fsmom_HP;
    Cmom<RVHP> K1fsmom_VHP, K2fsmom_VHP;
    momentum<complex<R> > K4fsmom;
    momentum<complex<RHP> > K4fsmom_HP;
    momentum<complex<RVHP> > K4fsmom_VHP;
    C Tnorm;
    CHP Tnorm_HP;
    CVHP Tnorm_VHP;

#ifdef BH_USE_GMP
	CGMP coeffs_GMP[2];
    CGMP D0coeff_GMP;
    Cmom<RGMP> K1fsmom_GMP, K2fsmom_GMP;
    momentum<complex<RGMP> > K4fsmom_GMP;
    CGMP Tnorm_GMP;
#endif


    int _k1leg; // Stores which legs are K1, K2 & K3
    int _k2leg;
    int _k3leg;
    int _k4leg;
    bool _massless_K1; //Records if the K1 leg is massless
    int _masslessleg_type; // Stores the type of massless leg we have

protected:
    //This vector is used for storing the indexes that get passed to the cut trees
        std::vector<int> indlst[4];

        eval_param<R>* _ep[4];
    	eval_param<RHP>* _ep_HP[4];
    	eval_param<RVHP>* _ep_VHP[4];
#ifdef BH_USE_GMP
    	eval_param<RGMP>* _ep_GMP[4];
#endif

public:
    template <class T> box_Darren(T&);
    template <class T> box_Darren(T*);
//        box_Darren(std::istream& is);
//        box_Darren(const boxD& box);
        virtual ~box_Darren();

        //! Evaluation of box coefficients
        /** \param mc momentum configuration to be used \param ind indices of the momenta to be used \returns box coefficient*/
        virtual C eval(momentum_configuration<double>& mc,const std::vector<int>& ind);
        virtual CHP eval(momentum_configuration<dd_real>& mc,const std::vector<int>& ind);
        virtual CVHP eval(momentum_configuration<qd_real>& mc,const std::vector<int>& ind);
#ifdef BH_USE_GMP
        virtual CGMP eval(momentum_configuration<RGMP>& mc,const std::vector<int>& ind);
#endif

    	virtual C eval(const eval_param<R>& ep);
    	virtual CHP eval(const eval_param<RHP>& ep);
    	virtual CVHP eval(const eval_param<RVHP>& ep);
#ifdef BH_USE_GMP
    	virtual CGMP eval(const eval_param<RGMP>& ep);
#endif

        // Computes the box subtraction terms for the triangle
        template <class T> void get_sub_terms(momentum_configuration<T>& mc,const std::vector<int>& ind, complex<T>* coeffsret, coeffparam<T,CPOINTS>& tp);
    	template <class T> void get_sub_terms(const eval_param<T>& ep, complex<T>* coeffsret, coeffparam<T,CPOINTS>& tp);

        inline void set_eval(long int mcset, const std::vector<int>& indset){mcID=mcset;indID=indset;}
    	inline void set_eval(unsigned long int mcset){epID=mcset;};
        inline bool is_eval(long int mcset, const std::vector<int>& indset){if((mcset==mcID)&&(indset==indID)){return true;}else{return false;}}
    	inline bool is_eval(unsigned long int mcset){if(mcset==epID){return true;} return false;};
    	int get_kleg(int i) const {switch (i) {case 1: return _k1leg; case 2: return _k2leg; case 3: return _k3leg; case 4: return _k4leg; }; }
    	int get_masslessleg_type() const {return _masslessleg_type; }
    	bool get_massless_K1() const {return _massless_K1; }
protected:
        // Computes the box coefficients
        template <class T> void get_coeffs_fn(momentum_configuration<T>& mc, const std::vector<int>& ind, coeffparam<T,CPOINTS>& tp);
        virtual void get_coeffs(momentum_configuration<R>& mc, const std::vector<int>& ind, coeffparam<R,CPOINTS>& tp){get_coeffs_fn(mc,ind,tp);}
        virtual void get_coeffs(momentum_configuration<RHP>& mc, const std::vector<int>& ind, coeffparam<RHP,CPOINTS>& tp){get_coeffs_fn(mc,ind,tp);}
        virtual void get_coeffs(momentum_configuration<RVHP>& mc, const std::vector<int>& ind, coeffparam<RVHP,CPOINTS>& tp){get_coeffs_fn(mc,ind,tp);}
#ifdef BH_USE_GMP
        virtual void get_coeffs(momentum_configuration<RGMP>& mc, const std::vector<int>& ind, coeffparam<RGMP,CPOINTS>& tp){get_coeffs_fn(mc,ind,tp);}
#endif

    	template <class T> void get_coeffs_fn(const eval_param<T>& ep, coeffparam<T,CPOINTS>& tp);
    	virtual void get_coeffs(const eval_param<R>& ep, coeffparam<R,CPOINTS>& tp){get_coeffs_fn(ep,tp);};
    	virtual void get_coeffs(const eval_param<RHP>& ep, coeffparam<RHP,CPOINTS>& tp){get_coeffs_fn(ep,tp);};
    	virtual void get_coeffs(const eval_param<RVHP>& ep, coeffparam<RVHP,CPOINTS>& tp){get_coeffs_fn(ep,tp);};
#ifdef BH_USE_GMP
    	virtual void get_coeffs(const eval_param<RGMP>& ep, coeffparam<RGMP,CPOINTS>& tp){get_coeffs_fn(ep,tp);};
#endif


        //Computes the indices that appear in the tree amplitudes in the corners
        void GenIndicesBox(const std::vector<int>& IND, std::vector<int*>& pmoml, std::vector<int*>& pmmoml, std::vector<int>& leg);

        void set_coeff(size_t i,C& value){coeffs[i]=value;}
        void set_coeff(size_t i,CHP& value){coeffs_HP[i]=value;}
        void set_coeff(size_t i,CVHP& value){coeffs_VHP[i]=value;}

        void get_coeff(C& value1,C& value2){value1=coeffs[0]; value2=coeffs[1];}
        void get_coeff(CHP& value1,CHP& value2){value1=coeffs_HP[0]; value2=coeffs_HP[1];}
        void get_coeff(CVHP& value1,CVHP& value2){value1=coeffs_VHP[0]; value2=coeffs_VHP[1];}

        void set_denfac(C value1){Tnorm=value1;}
        void set_denfac(CHP value1){Tnorm_HP=value1;}
        void set_denfac(CVHP value1){Tnorm_VHP=value1;}

        void get_denfac(C& value1){value1=Tnorm;}
        void get_denfac(CHP& value1){value1=Tnorm_HP;}
        void get_denfac(CVHP& value1){value1=Tnorm_VHP;}

        void set_Kif_coeffs(const Cmom<R>& K1fmom, const Cmom<R>& K2fmom, const momentum<complex<R> >& K4fmom){K1fsmom=K1fmom;K2fsmom=K2fmom;K4fsmom=K4fmom;}
        void set_Kif_coeffs(const Cmom<RHP>& K1fmom, const Cmom<RHP>& K2fmom, const momentum<complex<RHP> >& K4fmom){K1fsmom_HP=K1fmom;K2fsmom_HP=K2fmom;K4fsmom_HP=K4fmom;}
        void set_Kif_coeffs(const Cmom<RVHP>& K1fmom, const Cmom<RVHP>& K2fmom, const momentum<complex<RVHP> >& K4fmom){K1fsmom_VHP=K1fmom;K2fsmom_VHP=K2fmom;K4fsmom_VHP=K4fmom;}

        void get_Kif_coeffs(Cmom<R>& K1fmom, Cmom<R>& K2fmom, momentum<complex<R> >& K4fmom){K1fmom=K1fsmom;K2fmom=K2fsmom;K4fmom=K4fsmom;}
        void get_Kif_coeffs(Cmom<RHP>& K1fmom, Cmom<RHP>& K2fmom, momentum<complex<RHP> >& K4fmom){K1fmom=K1fsmom_HP;K2fmom=K2fsmom_HP;K4fmom=K4fsmom_HP;}
        void get_Kif_coeffs(Cmom<RVHP>& K1fmom, Cmom<RVHP>& K2fmom, momentum<complex<RVHP> >& K4fmom){K1fmom=K1fsmom_VHP;K2fmom=K2fsmom_VHP;K4fmom=K4fsmom_VHP;}

        void set_D0coeff(C value){D0coeff=value;}
        void set_D0coeff(CHP value){D0coeff_HP=value;}
        void set_D0coeff(CVHP value){D0coeff_VHP=value;}

        C get_DO_coeff(){return D0coeff;}
        CHP get_DO_HP_coeff(){return D0coeff_HP;}
        CVHP get_DO_VHP_coeff(){return D0coeff_VHP;}

        void get_ep(eval_param<R>**& ep){ep=_ep;}
    	void get_ep(eval_param<RHP>**& ep){ep= _ep_HP;}
    	void get_ep(eval_param<RVHP>**& ep){ep= _ep_VHP;}

#ifdef BH_USE_GMP
        void set_coeff(size_t i,CGMP& value){coeffs_GMP[i]=value;}
        void get_coeff(CGMP& value1,CGMP& value2){value1=coeffs_GMP[0]; value2=coeffs_GMP[1];}
        void set_denfac(CGMP value1){Tnorm_GMP=value1;}
        void get_denfac(CGMP& value1){value1=Tnorm_GMP;}
        void set_Kif_coeffs(const Cmom<RGMP>& K1fmom, const Cmom<RGMP>& K2fmom, const momentum<complex<RGMP> >& K4fmom){K1fsmom_GMP=K1fmom;K2fsmom_GMP=K2fmom;K4fsmom_GMP=K4fmom;}
        void get_Kif_coeffs(Cmom<RGMP>& K1fmom, Cmom<RGMP>& K2fmom, momentum<complex<RGMP> >& K4fmom){K1fmom=K1fsmom_GMP;K2fmom=K2fsmom_GMP;K4fmom=K4fsmom_GMP;}
        void set_D0coeff(CGMP value){D0coeff_GMP=value;}
        CGMP get_DO_GMP_coeff(){return D0coeff_GMP;}
    	void get_ep(eval_param<RGMP>**& ep){ep= _ep_GMP;}
#endif



private:
        //avoid copying
        box_Darren& operator=(const box_Darren& cd);
        box_Darren(const box_Darren& cd);

        void init();
        void init(const boxD& cd);
};


template <class cutDbase,class TriangleSpecs/* = Normal_Triangle_Specification<cutDbase>*/> class triangle_Darren : public cutDbase, public TriangleSpecs::CornerTreeStrategy, public TriangleSpecs::Subtraction {
	COMPILE_ASSERT(sizeof(Darren_wrapper<BH::triangleD> )!=sizeof(Darren_wrapper<BH::boxD> ));
	COMPILE_ASSERT(sizeof(cutDbase)!=sizeof(Darren_wrapper<BH::boxD> ));

	long int mcID;
	unsigned long int epID;
        std::vector<int> indID;

        C C0coeff;
        CHP C0coeff_HP;
        CVHP C0coeff_VHP;

         // Stores the information we need to relate the c coeffs with different opened bubble corners
        Cmom<R> K1fsmom, K2fsmom;
        Cmom<RHP> K1fsmom_HP, K2fsmom_HP;
        Cmom<RVHP> K1fsmom_VHP, K2fsmom_VHP;
        C coeffs[TriangleSpecs::CPOINTS];
        CHP coeffs_HP[TriangleSpecs::CPOINTS];
        CVHP coeffs_VHP[TriangleSpecs::CPOINTS];
        C gamma;
        CHP gamma_HP;
        CVHP gamma_VHP;
        C alp1,alp2;
        CHP alp1_HP,alp2_HP;
        CVHP alp1_VHP,alp2_VHP;

        std::vector<C> *coeffkeep, *polekeep, *denpole;
        std::vector<CHP> *coeffkeep_HP, *polekeep_HP, *denpole_HP;
        std::vector<CVHP> *coeffkeep_VHP, *polekeep_VHP, *denpole_VHP;

        //This vector is used for storing the indexes that get passed to the cut trees
        vector<int> indlst[3];

    	eval_param<R>* _ep[3];
    	eval_param<RHP>* _ep_HP[3];
    	eval_param<RVHP>* _ep_VHP[3];

#ifdef BH_USE_GMP
        CGMP C0coeff_GMP;
        Cmom<RGMP> K1fsmom_GMP, K2fsmom_GMP;
        CGMP coeffs_GMP[TriangleSpecs::CPOINTS];
        CGMP gamma_GMP;
        CGMP alp1_GMP,alp2_GMP;
        std::vector<CGMP> *coeffkeep_GMP, *polekeep_GMP, *denpole_GMP;
    	eval_param<RGMP>* _ep_GMP[3];
#endif


        typedef box_Darren<cutDbase,TriangleSpecs::CPOINTS,TriangleSpecs::TPOINTSTRI> daughter_type;
        typedef triangle_Darren_eval_points<TriangleSpecs::CPOINTS,TriangleSpecs::TPOINTSTRI> MomentaEvaluatorType;
        typedef Normal_Corner_Tree_Strategy<triangle_Darren_eval_points<TriangleSpecs::CPOINTS,TriangleSpecs::TPOINTSTRI>,cutDbase,TriangleSpecs::CPOINTS> CornerTreeStrategy;
public:
        int _k1leg; // Stores which legs are K1, K2 & K3
        int _k2leg;
        int _k3leg;
        int _masslessleg_type; // Stores the type of massless leg we have
        int _reverse; // Stores the direction the momentum goes around the loop
public:
    template <class T> triangle_Darren(T&);
    template <class T> triangle_Darren(T*);

        virtual ~triangle_Darren();

        virtual C eval(momentum_configuration<double>&,const std::vector<int>&);
        virtual CHP eval(momentum_configuration<dd_real>&,const std::vector<int>&);
        virtual CVHP eval(momentum_configuration<qd_real>&,const std::vector<int>&);

        virtual C eval(const eval_param<R>& ep);
    	virtual CHP eval(const eval_param<RHP>& ep);
    	virtual CVHP eval(const eval_param<RVHP>& ep);

        //These versions should never be called
        virtual C get_sub_terms(momentum_configuration<double>& mc, const std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return C(0,0);}
        virtual CHP get_sub_terms(momentum_configuration<dd_real>& mc, const std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return CHP(0,0);}
        virtual CVHP get_sub_terms(momentum_configuration<qd_real>& mc, const std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return CVHP(0,0);}

        virtual C get_sub_terms(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp) =0;
    	virtual CHP get_sub_terms(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp) =0;
    	virtual CVHP get_sub_terms(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp) =0;
#ifdef BH_USE_GMP
        virtual CGMP eval(momentum_configuration<RGMP>&,const std::vector<int>&);
    	virtual CGMP eval(const eval_param<RGMP>& ep);
        virtual CGMP get_sub_terms(momentum_configuration<RGMP>& mc, const std::vector<int>& ind, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){return CGMP(0,0);}
    	virtual CGMP get_sub_terms(const eval_param<RGMP>& ep, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp) =0;

#endif
        //Used for setting and checking whether this has already been computed
        inline void set_eval(long int mcset, const std::vector<int>& indset){mcID=mcset;indID=indset;}
    	inline void set_eval(unsigned long int mcset){epID=mcset;};
    	inline bool is_eval(long int mcset, const std::vector<int>& indset){if((mcset==mcID)&&(indset==indID)){return true;}else{return false;}};
    	inline bool is_eval(unsigned long int mcset){if(mcset==epID){return true;} return false;};

    	int get_kleg(int i) const {switch (i) {case 1: return _k1leg; case 2: return _k2leg; case 3: return _k3leg;  }; }
    	int get_masslessleg_type() const {return _masslessleg_type; }

    	void change_klegs(int k1,int k2,int k3);

protected:
        // Computes the triangle coefficients
        template <class T> void get_coeffs_fn(momentum_configuration<T>& mc, const  std::vector<int>& ind, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
        virtual void get_coeffs(momentum_configuration<R>& mc, const  std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(mc,ind,tp); }
        virtual void get_coeffs(momentum_configuration<RHP>& mc, const  std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(mc,ind,tp); }
        virtual void get_coeffs(momentum_configuration<RVHP>& mc, const  std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(mc,ind,tp); }
    	template <class T> void get_coeffs_fn(const eval_param<T>& ep, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
    	virtual void get_coeffs(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(ep,tp); };
    	virtual void get_coeffs(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(ep,tp); };
    	virtual void get_coeffs(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(ep,tp); };
#ifdef BH_USE_GMP
        virtual void get_coeffs(momentum_configuration<RGMP>& mc, const  std::vector<int>& ind, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(mc,ind,tp); }
    	virtual void get_coeffs(const eval_param<RGMP>& ep, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){ get_coeffs_fn(ep,tp); };
#endif

    	//Computes the indices needed for the processes
        void GenIndicesTri(const std::vector<int>& IND, std::vector<int*>& pmoml, std::vector<int*>& pmmoml, std::vector<int>& leg);

        // Gets the previously computed boxes for the subtraction of the boxes in the bubble
        inline void get_boxes(std::vector<C>*& coeffs, std::vector<C>*& poles, std::vector<C>*& denpoles){coeffs=coeffkeep;poles=polekeep;denpoles=denpole;}
        inline void get_boxes(std::vector<CHP>*& coeffs, std::vector<CHP>*& poles, std::vector<CHP>*& denpoles){coeffs=coeffkeep_HP;poles=polekeep_HP;denpoles=denpole_HP;}
        inline void get_boxes(std::vector<CVHP>*& coeffs, std::vector<CVHP>*& poles, std::vector<CVHP>*& denpoles){coeffs=coeffkeep_VHP;poles=polekeep_VHP;denpoles=denpole_VHP;}
#ifdef BH_USE_GMP
        inline void get_boxes(std::vector<CGMP>*& coeffs, std::vector<CGMP>*& poles, std::vector<CGMP>*& denpoles){coeffs=coeffkeep_GMP;poles=polekeep_GMP;denpoles=denpole_GMP;}
#endif
        void empty_box_store(const C& dummy){coeffkeep->clear(); polekeep->clear(); denpole->clear();}
        void empty_box_store(const CHP& dummy){coeffkeep_HP->clear(); polekeep_HP->clear();denpole_HP->clear();}
        void empty_box_store(const CVHP& dummy){coeffkeep_VHP->clear(); polekeep_VHP->clear(); denpole_VHP->clear();}
#ifdef BH_USE_GMP
        void empty_box_store(const CGMP& dummy){coeffkeep_GMP->clear(); polekeep_GMP->clear(); denpole_GMP->clear();}
#endif

        //Used for setting the full coefficient
        void set_C0coeff(C value){C0coeff=value;}
        void set_C0coeff(CHP value){C0coeff_HP=value;}
        void set_C0coeff(CVHP value){C0coeff_VHP=value;}
#ifdef BH_USE_GMP
        void set_C0coeff(CGMP value){C0coeff_GMP=value;}
#endif

        inline void boxcoeff_add(C& coeff1, C& coeff2, C& pole1, C& pole2, C& denval){
        	coeffkeep->push_back(coeff1);coeffkeep->push_back(coeff2);polekeep->push_back(pole1);polekeep->push_back(pole2);
        	denpole->push_back(denval);};
        inline void boxcoeff_add(CHP& coeff1, CHP& coeff2, CHP& pole1, CHP& pole2, CHP& denval){coeffkeep_HP->push_back(coeff1);
			coeffkeep_HP->push_back(coeff2);
			polekeep_HP->push_back(pole1);
			polekeep_HP->push_back(pole2);
			denpole_HP->push_back(denval);};
        inline void boxcoeff_add(CVHP& coeff1, CVHP& coeff2, CVHP& pole1, CVHP& pole2, CVHP& denval){coeffkeep_VHP->push_back(coeff1);
			coeffkeep_VHP->push_back(coeff2);
			polekeep_VHP->push_back(pole1);
			polekeep_VHP->push_back(pole2);
			denpole_VHP->push_back(denval);};
#ifdef BH_USE_GMP
        inline void boxcoeff_add(CGMP& coeff1, CGMP& coeff2, CGMP& pole1, CGMP& pole2, CGMP& denval){coeffkeep_GMP->push_back(coeff1);
			coeffkeep_GMP->push_back(coeff2);
			polekeep_GMP->push_back(pole1);
			polekeep_GMP->push_back(pole2);
			denpole_GMP->push_back(denval);};
#endif

        void set_tri_param_basis_vectors(const Cmom<R>& K1fmom, const Cmom<R>& K2fmom){K1fsmom=K1fmom;K2fsmom=K2fmom;}
        void set_tri_param_basis_vectors(const Cmom<RHP>& K1fmom, const Cmom<RHP>& K2fmom){K1fsmom_HP=K1fmom;K2fsmom_HP=K2fmom;}
        void set_tri_param_basis_vectors(const Cmom<RVHP>& K1fmom, const Cmom<RVHP>& K2fmom){K1fsmom_VHP=K1fmom;K2fsmom_VHP=K2fmom;}

        void set_tri_param_gamma(const C& gam){gamma=gam;}
        void set_tri_param_gamma(const CHP& gam){gamma_HP=gam;}
        void set_tri_param_gamma(const CVHP& gam){gamma_VHP=gam;}

        void set_tri_param_alp(const C& alp1_in,const C& alp2_in){alp1=alp1_in;alp2=alp2_in;}
        void set_tri_param_alp(const CHP& alp1_in,const CHP& alp2_in){alp1_HP=alp1_in;alp2_HP=alp2_in;}
        void set_tri_param_alp(const CVHP& alp1_in,const CVHP& alp2_in){alp1_VHP=alp1_in;alp2_VHP=alp2_in;}


        inline void coeffkeep_add(C (*coeff)){for(int icirc=0; icirc<TriangleSpecs::CPOINTS; icirc++){
                                                                                        coeffs[icirc]=coeff[icirc];
                                                                                        }};
        inline void coeffkeep_add(CHP (*coeff)){for(int icirc=0; icirc<TriangleSpecs::CPOINTS; icirc++){
                                                                                                coeffs_HP[icirc]=coeff[icirc];
                                                                                        }};
        inline void coeffkeep_add(CVHP (*coeff)){for(int icirc=0; icirc<TriangleSpecs::CPOINTS; icirc++){
                                                                                                coeffs_VHP[icirc]=coeff[icirc];
                                                                                        }};

        //Used for getting the vectors used to construct the trinagle param this was computed using
        void get_tri_param_basis_vectors(Cmom<R>& K1fmom, Cmom<R>& K2fmom){K1fmom=K1fsmom;K2fmom=K2fsmom;}
        void get_tri_param_basis_vectors(Cmom<RHP>& K1fmom, Cmom<RHP>& K2fmom){K1fmom=K1fsmom_HP;K2fmom=K2fsmom_HP;}
        void get_tri_param_basis_vectors(Cmom<RVHP>& K1fmom, Cmom<RVHP>& K2fmom){K1fmom=K1fsmom_VHP;K2fmom=K2fsmom_VHP;}

        void get_tri_param_gamma(C& gam){gam=gamma;}
        void get_tri_param_gamma(CHP& gam){gam=gamma_HP;}
        void get_tri_param_gamma(CVHP& gam){gam=gamma_VHP;}

        void get_tri_param_alp(C& alp1_in,C& alp2_in){alp1_in=alp1;alp2_in=alp2;}
        void get_tri_param_alp(CHP& alp1_in,CHP& alp2_in){alp1_in=alp1_HP;alp2_in=alp2_HP;}
        void get_tri_param_alp(CVHP& alp1_in,CVHP& alp2_in){alp1_in=alp1_VHP;alp2_in=alp2_VHP;}

        inline void coeffkeep_get(C*& coeff){coeff=coeffs;}
        inline void coeffkeep_get(CHP*& coeff){coeff=coeffs_HP;}
        inline void coeffkeep_get(CVHP*& coeff){coeff=coeffs_VHP;}

        void get_ep(eval_param<R>**& ep){ep=_ep;}
    	void get_ep(eval_param<RHP>**& ep){ep= _ep_HP;}
    	void get_ep(eval_param<RVHP>**& ep){ep= _ep_VHP;}

#ifdef BH_USE_GMP
        void set_tri_param_basis_vectors(const Cmom<RGMP>& K1fmom, const Cmom<RGMP>& K2fmom){K1fsmom_GMP=K1fmom;K2fsmom_GMP=K2fmom;}
        void set_tri_param_gamma(const CGMP& gam){gamma_GMP=gam;}
        inline void coeffkeep_add(CGMP (*coeff)){for(int icirc=0; icirc<TriangleSpecs::CPOINTS; icirc++){
                                                                                                coeffs_GMP[icirc]=coeff[icirc];
                                                                                        }};
        void set_tri_param_alp(const CGMP& alp1_in,const CGMP& alp2_in){alp1_GMP=alp1_in;alp2_GMP=alp2_in;}

        void get_tri_param_basis_vectors(Cmom<RGMP>& K1fmom, Cmom<RGMP>& K2fmom){K1fmom=K1fsmom_GMP;K2fmom=K2fsmom_GMP;}
        void get_tri_param_gamma(CGMP& gam){gam=gamma_GMP;}
        void get_tri_param_alp(CGMP& alp1_in,CGMP& alp2_in){alp1_in=alp1_GMP;alp2_in=alp2_GMP;}
        inline void coeffkeep_get(CGMP*& coeff){coeff=coeffs_GMP;}
    	void get_ep(eval_param<RGMP>**& ep){ep= _ep_GMP;}
#endif


private:
        //no copy
        triangle_Darren(const triangle_Darren& cd);
        triangle_Darren operator=(const triangle_Darren& cd);
        void init();
};


template <class cutDbase,class TriangleSpecs> class triangle_Darren_3mass : public triangle_Darren<cutDbase,TriangleSpecs> {
public:
//        triangle_Darren_3mass(std::istream& is) : triangle_Darren<cutDbase>(is){}
//        triangle_Darren_3mass(const triangleD& tri) : triangle_Darren<cutDbase>(tri){}
	template <class T> triangle_Darren_3mass(T&);
	template <class T> triangle_Darren_3mass(T*);

        virtual ~triangle_Darren_3mass(){}

        virtual C eval(momentum_configuration<double>& mc,const std::vector<int>& ind){return R(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}
        virtual CHP eval(momentum_configuration<dd_real>& mc,const std::vector<int>& ind){return RHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}
        virtual CVHP eval(momentum_configuration<qd_real>& mc,const std::vector<int>& ind){return RVHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}

        virtual C eval(const eval_param<R>& ep){return R(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};
    	virtual CHP eval(const eval_param<RHP>& ep){return RHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};
    	virtual CVHP eval(const eval_param<RVHP>& ep){return RVHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};

        virtual C get_sub_terms(momentum_configuration<double>& mc, const std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CHP get_sub_terms(momentum_configuration<dd_real>& mc, const std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CVHP get_sub_terms(momentum_configuration<qd_real>& mc, const std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}

        virtual C get_sub_terms(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CHP get_sub_terms(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CVHP get_sub_terms(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
#ifdef BH_USE_GMP
        virtual CGMP eval(momentum_configuration<RGMP>& mc,const std::vector<int>& ind){return RGMP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}
        virtual CGMP eval(const eval_param<RGMP>& ep){return RGMP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};
        virtual CGMP get_sub_terms(momentum_configuration<RGMP>& mc, const std::vector<int>& ind, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
    	virtual CGMP get_sub_terms(const eval_param<RGMP>& ep, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
#endif

private:
        template <class T> complex<T> get_sub_terms_work(momentum_configuration<T>& mc, const std::vector<int>& ind, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
        template <class T> complex<T> get_sub_terms_work(const eval_param<T>& ep, coeffparam<T,TriangleSpecs::CPOINTS>& tp);};


template <class cutDbase,class TriangleSpecs> class triangle_Darren_plusminus : public triangle_Darren<cutDbase,TriangleSpecs> {
public:
//        triangle_Darren_plusminus(std::istream& is) : triangle_Darren<cutDbase>(is) {}
//        triangle_Darren_plusminus(const triangleD& tri) : triangle_Darren<cutDbase>(tri) {}
    template <class T> triangle_Darren_plusminus(T& t);
    template <class T> triangle_Darren_plusminus(T* t);
        virtual ~triangle_Darren_plusminus(){}

        virtual C get_sub_terms(momentum_configuration<double>& mc, const std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CHP get_sub_terms(momentum_configuration<dd_real>& mc, const std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CVHP get_sub_terms(momentum_configuration<qd_real>& mc, const std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}

        virtual C get_sub_terms(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CHP get_sub_terms(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CVHP get_sub_terms(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
#ifdef BH_USE_GMP
        virtual CGMP get_sub_terms(momentum_configuration<RGMP>& mc, const std::vector<int>& ind, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
    	virtual CGMP get_sub_terms(const eval_param<RGMP>& ep, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
#endif
private:
        template <class T> complex<T> get_sub_terms_work(momentum_configuration<T>& mc, const std::vector<int>& ind, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
    	template <class T> complex<T> get_sub_terms_work(const eval_param<T>& ep, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
};

template <class cutDbase,class TriangleSpecs> class triangle_Darren_zero : public triangle_Darren<cutDbase,TriangleSpecs> {
public:
//        triangle_Darren_plusminus(std::istream& is) : triangle_Darren<cutDbase>(is) {}
//        triangle_Darren_plusminus(const triangleD& tri) : triangle_Darren<cutDbase>(tri) {}
    template <class T> triangle_Darren_zero(T& t);
    template <class T> triangle_Darren_zero(T* t);
        virtual ~triangle_Darren_zero(){}

        virtual C get_sub_terms(momentum_configuration<double>& mc, const std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CHP get_sub_terms(momentum_configuration<dd_real>& mc, const std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CVHP get_sub_terms(momentum_configuration<qd_real>& mc, const std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}

        virtual C get_sub_terms(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CHP get_sub_terms(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CVHP get_sub_terms(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
#ifdef BH_USE_GMP
        virtual CGMP get_sub_terms(momentum_configuration<RGMP>& mc, const std::vector<int>& ind, coeffparam<RGMP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
#endif

private:
        template <class T> complex<T> get_sub_terms_work(momentum_configuration<T>& mc, const std::vector<int>& ind, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
    	template <class T> complex<T> get_sub_terms_work(const eval_param<T>& ep, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
};


template <class cutDbase, class BubbleSpecs/* = Normal_Bubble_Specification<cutDbase> */> class bubble_Darren: public cutDbase,  public BubbleSpecs::CornerTreeStrategy ,  public BubbleSpecs::BubbleCombiner
{
        long int mcID;
    	unsigned long int epID;
        std::vector<int> indID;

        C B0coeff;
        CHP B0coeff_HP;
        CVHP B0coeff_VHP;
#ifdef BH_USE_GMP
        std::complex<RGMP> B0coeff_GMP;
#endif

protected:

        //This vector is used for storing the indexes that get passed to the cut trees
        vector<int> indlst[2];
    	eval_param<R>* _ep[2];
    	eval_param<RHP>* _ep_HP[2];
    	eval_param<RVHP>* _ep_VHP[2];
#ifdef BH_USE_GMP
    	eval_param<RGMP>* _ep_GMP[2];
#endif

public:
//  bubble_Darren(std::istream& is);
//  bubble_Darren(const bubbleD& bub);
   	template <class T> bubble_Darren(T&);
    template <class T> bubble_Darren(T*);
    virtual ~bubble_Darren();

        virtual C eval(momentum_configuration<double>& mc,const std::vector<int>& ind);
        virtual CHP eval(momentum_configuration<dd_real>& mc,const std::vector<int>& ind);
        virtual CVHP eval(momentum_configuration<qd_real>& mc,const std::vector<int>& ind);
#ifdef BH_USE_GMP
        virtual std::complex<RGMP> eval(momentum_configuration<RGMP>& mc,const std::vector<int>& ind);
#endif
        virtual C eval(const eval_param<R>& ep);
    	virtual CHP eval(const eval_param<RHP>& ep);
    	virtual CVHP eval(const eval_param<RVHP>& ep);
#ifdef BH_USE_GMP
    	virtual CGMP eval(const eval_param<RGMP>& ep);
#endif

        inline void set_eval(long int mcset, const std::vector<int>& indset){mcID=mcset;indID=indset;}
    	inline void set_eval(unsigned long int mcset){epID=mcset;};
        inline bool is_eval(long int mcset, const std::vector<int>& indset){if((mcset==mcID)&&(indset==indID)){return true;}else{return false;}}
    	inline bool is_eval(unsigned long int mcset){if(mcset==epID){return true;} return false;};

        // Sets up the ind for the different processes
         void GenIndicesBub(const std::vector<int>& IND, std::vector<int*>& pmoml, std::vector<int*>& pmmoml);

        typedef triangle_Darren<cutDbase,typename BubbleSpecs::TriangleSpecs> daughter_type;
       	typedef typename BubbleSpecs::MomentaEvaluator MomentaEvaluatorType;
protected:
        void set_B0coeff(C value){B0coeff=value;}
        void set_B0coeff(CHP value){B0coeff_HP=value;}
        void set_B0coeff(CVHP value){B0coeff_VHP=value;}

        void get_B0coeff(C& value){value=B0coeff;}
        void get_B0coeff(CHP& value){value=B0coeff_HP;}
        void get_B0coeff(CVHP& value){value=B0coeff_VHP;}

#ifdef BH_USE_GMP
        void set_B0coeff(const std::complex<RGMP>& value){B0coeff_GMP=value;}
        void get_B0coeff(std::complex<RGMP>& value){value=B0coeff_GMP;}
#endif

        void get_ep(eval_param<R>**& ep){ep=_ep;}
    	void get_ep(eval_param<RHP>**& ep){ep= _ep_HP;}
    	void get_ep(eval_param<RVHP>**& ep){ep= _ep_VHP;}

#ifdef BH_USE_GMP
    	void get_ep(eval_param<RGMP>**& ep){ep= _ep_GMP;}
#endif

        //Computes the actual bubble coefficients
    	template <class T> complex<T> get_coeffs(momentum_configuration<T>& mc,const std::vector<int>& ind);
        template <class T> complex<T> get_coeffs(const eval_param<T>& ep);
private:
        //no copy
        bubble_Darren(const bubble_Darren& cd);
        bubble_Darren& operator=(const bubble_Darren& cd);
        void init();
};

template <class cutBase, int CPOINTS, int TPOINTSTRI> struct triangle_daughter_type {
	typedef void type;
};

template <class cutBase,class BubbleSpecs> struct bubble_daughter_type {
	typedef void type;
};



//member not used are not instantiated
template <class cutDbase, class TriangleSpecs> class triangle_Darren_factory  {
public:
    triangle_Darren<cutDbase,TriangleSpecs>* new_triangle(std::istream& is);
    triangle_Darren<cutDbase,TriangleSpecs>* new_triangle(const triangleD& tri);
};

//Partially Specialize this class for the types of cut we use
template <class TriangleSpecs> class triangle_Darren_factory<BH::cut::worker::worker_cutD,TriangleSpecs>  {
public:
    triangle_Darren<BH::cut::worker::worker_cutD,TriangleSpecs>* new_triangle(std::istream& is);
    triangle_Darren<BH::cut::worker::worker_cutD,TriangleSpecs>* new_triangle(const triangleD& tri);
};

template <class TriangleSpecs> class triangle_Darren_factory<Darren_wrapper<triangleD>,TriangleSpecs>  {
public:
    triangle_Darren<Darren_wrapper<triangleD>,TriangleSpecs>* new_triangle(std::istream& is);
    triangle_Darren<Darren_wrapper<triangleD>,TriangleSpecs>* new_triangle(const triangleD& tri);
};

/*
 *
 *
 *
 * Explicit definitions for all used case
 *
 *
 *
 */

//
// The standard case
//


template <> struct triangle_daughter_type<Darren_wrapper<triangleD>,CTRIPOINTS_STD,TRIPOINTS_STD> {
	typedef box_Darren<Darren_wrapper<boxD>,CTRIPOINTS_STD,TRIPOINTS_STD> type;
};
template <> struct triangle_daughter_type<BH::cut::worker::worker_cutD,CTRIPOINTS_STD,TRIPOINTS_STD> {
	typedef box_Darren<BH::cut::worker::worker_cutD,CTRIPOINTS_STD,TRIPOINTS_STD> type;
};

template <class BubbleSpecs> struct bubble_daughter_type<Darren_wrapper<bubbleD>,BubbleSpecs> {
	typedef triangle_Darren<Darren_wrapper<triangleD>,typename BubbleSpecs::TriangleSpecs> type;
};
template <class BubbleSpecs> struct bubble_daughter_type<BH::cut::worker::worker_cutD,BubbleSpecs> {
	typedef triangle_Darren<BH::cut::worker::worker_cutD,typename BubbleSpecs::TriangleSpecs> type;
};


//
// The higgs case
//

template <> struct triangle_daughter_type<Darren_wrapper<triangleD>,9,9> {
	typedef box_Darren<Darren_wrapper<boxD>,9,9> type;
};
template <> struct triangle_daughter_type<BH::cut::worker::worker_cutD,9,9> {
	typedef box_Darren<BH::cut::worker::worker_cutD,9,9> type;
};

//
// A special test case
//
#define TRICOEFFS 7
#define TRICOEFFSEP 10
#define BUBCOEFFS 5
#define BUBCOEFFSEP 6

template <> struct triangle_daughter_type<Darren_wrapper<triangleD>,TRICOEFFS,TRICOEFFSEP> {
	typedef box_Darren<Darren_wrapper<boxD>,TRICOEFFS,TRICOEFFSEP> type;
};
template <> struct triangle_daughter_type<BH::cut::worker::worker_cutD,TRICOEFFS,TRICOEFFSEP> {
	typedef box_Darren<BH::cut::worker::worker_cutD,TRICOEFFS,TRICOEFFSEP> type;
};



/*
 *
 *
 *
 * Specializations for the usual case
 *
 *
 *
 */

//template <> template <> bubble_Darren<BH::cut::worker::worker_cutD,7,BUBPOINTS_CD,TRIPOINTS_CD,2>::bubble_Darren(std::istream& is);
//template <> template <> triangle_Darren_3mass<BH::cut::worker::worker_cutD,7,BUBPOINTS_CD,TRIPOINTS_CD,2>::triangle_Darren_3mass(std::istream& is);
//template <> template <> triangle_Darren_plusminus<BH::cut::worker::worker_cutD,7,BUBPOINTS_CD,TRIPOINTS_CD,2>::triangle_Darren_plusminus(std::istream& is);
//template <> template <> box_Darren<BH::cut::worker::worker_cutD,7,TRIPOINTS_CD>::box_Darren(std::istream& is);

//Specialize this template for the standard case


//template <class cutDbase> class bubble_Darren<cutDbase,7,BUBPOINTS_CD,TRIPOINTS_CD,2> : public cutDbase,  private Normal_Corner_Tree_Strategy<bubble_Darren_eval_points<BUBPOINTS_CD,2>, cutDbase, 7 > {
//template <class cutDbase> class bubble_Darren<cutDbase,7,BUBPOINTS_CD,TRIPOINTS_CD,2,Normal_Corner_Tree_Strategy<bubble_Darren_eval_points<BUBPOINTS_CD,2>, cutDbase, 7 > > : public cutDbase,  private Normal_Corner_Tree_Strategy<bubble_Darren_eval_points<BUBPOINTS_CD,2>, cutDbase, 7 > {

#if ugly_specialization
template <class cutDbase> class bubble_Darren<cutDbase,Normal_Bubble_Specification<cutDbase> > : public cutDbase,  public Normal_Bubble_Specification<cutDbase>::CornerTreeStrategy, public Normal_Bubble_Specification<cutDbase>::BubbleCombiner {

        long int mcID;
    	unsigned long int epID;
        std::vector<int> indID;

        C B0coeff;
        CHP B0coeff_HP;
        CVHP B0coeff_VHP;

protected:

    	eval_param<R>* _ep[2];
    	eval_param<RHP>* _ep_HP[2];
    	eval_param<RVHP>* _ep_VHP[2];

public:
        //This vector is used for storing the indexes that get passed to the cut trees
        vector<int> indlst[2];
//        bubble_Darren(std::istream& is);
//        bubble_Darren(const bubbleD& bub);
    template <class T> bubble_Darren(T&);
    template <class T> bubble_Darren(T*);
        virtual ~bubble_Darren(){}

        virtual C eval(momentum_configuration<double>& mc,const std::vector<int>& ind);
        virtual CHP eval(momentum_configuration<dd_real>& mc,const std::vector<int>& ind);
        virtual CVHP eval(momentum_configuration<qd_real>& mc,const std::vector<int>& ind);

        virtual C eval(const eval_param<R>& ep);
    	virtual CHP eval(const eval_param<RHP>& ep);
    	virtual CVHP eval(const eval_param<RVHP>& ep);

        inline void set_eval(long int mcset, const std::vector<int>& indset){mcID=mcset;indID=indset;}
    	inline void set_eval(unsigned long int mcset){epID=mcset;};
        inline bool is_eval(long int mcset, const std::vector<int>& indset){if((mcset==mcID)&&(indset==indID)){return true;}else{return false;}}
    	inline bool is_eval(unsigned long int mcset){if(mcset==epID){return true;} return false;};

        // Sets up the ind for the different processes
        void GenIndicesBub(const std::vector<int>& IND, std::vector<int*>& pmoml, std::vector<int*>& pmmoml);

    	typedef bubble_Darren_eval_points<BUBPOINTS_CD,2> MomentaEvaluatorType;
        typedef triangle_Darren<cutDbase,7,BUBPOINTS_CD,TRIPOINTS_CD,2> daughter_type;
protected:
        void set_B0coeff(C value){B0coeff=value;}
        void set_B0coeff(CHP value){B0coeff_HP=value;}
        void set_B0coeff(CVHP value){B0coeff_VHP=value;}

        void get_B0coeff(C& value){value=B0coeff;}
        void get_B0coeff(CHP& value){value=B0coeff_HP;}
        void get_B0coeff(CVHP& value){value=B0coeff_VHP;}


        void get_ep(eval_param<R>**& ep){ep=_ep;}
    	void get_ep(eval_param<RHP>**& ep){ep= _ep_HP;}
    	void get_ep(eval_param<RVHP>**& ep){ep= _ep_VHP;}

        //Computes the actual bubble coefficients
    	template <class T> complex<T> get_coeffs(momentum_configuration<T>& mc,const std::vector<int>& ind);
        template <class T> complex<T> get_coeffs(const eval_param<T>& ep);
private:
        //no copy
        bubble_Darren(const bubble_Darren& cd);
        bubble_Darren& operator=(const bubble_Darren& cd);
        void init();
};

#endif

#define ugly_specialization_tri 0

#if ugly_specialization_tri

template <class cutDbase> class triangle_Darren_plusminus<cutDbase,Normal_Triangle_Specification<cutDbase> > : public triangle_Darren<cutDbase,Normal_Triangle_Specification<cutDbase> > {
public:
	typedef Normal_Triangle_Specification<cutDbase> TriangleSpecs;
//        triangle_Darren_plusminus(std::istream& is) : triangle_Darren<cutDbase>(is) {}
//        triangle_Darren_plusminus(const triangleD& tri) : triangle_Darren<cutDbase>(tri) {}
    template <class T> triangle_Darren_plusminus(T& t);
    template <class T> triangle_Darren_plusminus(T* t);
        virtual ~triangle_Darren_plusminus(){};

        virtual C get_sub_terms(momentum_configuration<double>& mc, const std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CHP get_sub_terms(momentum_configuration<dd_real>& mc, const std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CVHP get_sub_terms(momentum_configuration<qd_real>& mc, const std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}

        virtual C get_sub_terms(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CHP get_sub_terms(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CVHP get_sub_terms(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};

private:
        template <class T> complex<T> get_sub_terms_work(momentum_configuration<T>& mc, const std::vector<int>& ind, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
    	template <class T> complex<T> get_sub_terms_work(const eval_param<T>& ep, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
};

template <class cutDbase> class triangle_Darren_3mass<cutDbase,Normal_Triangle_Specification<cutDbase> > : public triangle_Darren<cutDbase,Normal_Triangle_Specification<cutDbase> > {
public:
	typedef Normal_Triangle_Specification<cutDbase> TriangleSpecs;
	//        triangle_Darren_3mass(std::istream& is) : triangle_Darren<cutDbase>(is){}
//        triangle_Darren_3mass(const triangleD& tri) : triangle_Darren<cutDbase>(tri){}

	template <class T> triangle_Darren_3mass(T&);
	template <class T> triangle_Darren_3mass(T*);

        virtual ~triangle_Darren_3mass(){};

        virtual C eval(momentum_configuration<double>& mc,const std::vector<int>& ind){return R(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}
        virtual CHP eval(momentum_configuration<dd_real>& mc,const std::vector<int>& ind){return RHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}
        virtual CVHP eval(momentum_configuration<qd_real>& mc,const std::vector<int>& ind){return RVHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(mc,ind);}

        virtual C eval(const eval_param<R>& ep){return R(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};
    	virtual CHP eval(const eval_param<RHP>& ep){return RHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};
    	virtual CVHP eval(const eval_param<RVHP>& ep){return RVHP(2)*triangle_Darren<cutDbase,TriangleSpecs>::eval(ep);};

        virtual C get_sub_terms(momentum_configuration<double>& mc, const std::vector<int>& ind, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CHP get_sub_terms(momentum_configuration<dd_real>& mc, const std::vector<int>& ind, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}
        virtual CVHP get_sub_terms(momentum_configuration<qd_real>& mc, const std::vector<int>& ind, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(mc,ind,tp);}

        virtual C get_sub_terms(const eval_param<R>& ep, coeffparam<R,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CHP get_sub_terms(const eval_param<RHP>& ep, coeffparam<RHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};
    	virtual CVHP get_sub_terms(const eval_param<RVHP>& ep, coeffparam<RVHP,TriangleSpecs::CPOINTS>& tp){return get_sub_terms_work(ep,tp);};

private:
        template <class T> complex<T> get_sub_terms_work(momentum_configuration<T>& mc, const std::vector<int>& ind, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
        template <class T> complex<T> get_sub_terms_work(const eval_param<T>& ep, coeffparam<T,TriangleSpecs::CPOINTS>& tp);
};

#endif

}





}


}


#endif // CUT_DARREN_H
