
GITTAG = ATOOLS/Math
include ../../git.make

pkglib_LTLIBRARIES       =	libToolsMath.la

libToolsMath_la_SOURCES       =	\
				Git_Info.C \
				MathTools.C \
				Term.C \
				Algebra_Interpreter.C \
				Marsaglia.C \
				Sobol.C \
				Random.C \
				Function_Base.C \
				Poincare.C \
				ZAlign.C \
				Vector.C \
				Matrix.C \
				Gauss_Integrator.C \
				Scaling.C \
				Axis.C \
			        Histogram.C \
				Histogram_2D.C \
				Variable.C \
				Permutation.C \
				PermStore.C \
				Kabbala.C \
				Tensor.C

localinc_HEADERS          =	\
			MathTools.H \
			Term.H \
			Algebra_Interpreter.H \
			Marsaglia.H \
			Sobol.H \
			Random.H \
			Function_Base.H \
			Poincare.H \
			ZAlign.H \
			Vec3.H \
			Vec4.H \
			Vector.H \
			MyComplex.H \
			Matrix.H \
			Gauss_Integrator.H \
			Variable.H \
			Scaling.H \
			Axis.H \
			Histogram.H \
			Histogram_2D.H \
			Permutation.H \
			PermStore.H \
			Cluster_Algorithm.H \
			Cluster_Algorithm.C \
			Kabbala.H \
			Lorentz_Ten2.H \
			Lorentz_Ten3.H \
			Lorentz_Ten4.H \
			Tensor.H \
			Tensor_Build.H \
			Tensor_Contractions.H \
			Tensor_Contractions_Epsilon.H

if GZIP_SUPPORT
Sobol/%.gz: Sobol/%
	gzip < $< > $@
SOBOL_EXT = .gz
else
SOBOL_EXT = 
endif

nobase_dist_pkgdata_DATA         =	\
			Sobol/21201$(SOBOL_EXT)
