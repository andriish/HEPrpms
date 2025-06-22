set(Math_SOURCES MathTools.C
                 Term.C
                 Algebra_Interpreter.C
                 Marsaglia.C
                 Sobol.C
                 Random.C
                 Function_Base.C
                 Poincare.C
                 ZAlign.C
                 BreitBoost.C
                 Vector.C
                 Matrix.C
                 Gauss_Integrator.C
                 Scaling.C
                 Axis.C
                 Histogram.C
                 Histogram_2D.C
                 Variable.C
                 Permutation.C
                 PermStore.C
                 Kabbala.C
                 Tensor.C)

set(Math_HEADERS MathTools.H
                 Term.H
                 Algebra_Interpreter.H
                 Marsaglia.H
                 Sobol.H
                 Random.H
                 Function_Base.H
                 Poincare.H
                 ZAlign.H
                 BreitBoost.H
                 Vec3.H
                 Vec4.H
                 Vector.H
                 MyComplex.H
                 Matrix.H
                 Gauss_Integrator.H
                 Variable.H
                 Scaling.H
                 Axis.H
                 Histogram.H
                 Histogram_2D.H
                 Permutation.H
                 PermStore.H
                 Cluster_Algorithm.H
                 Cluster_Algorithm.C
                 Kabbala.H
                 Lorentz_Ten2.H
                 Lorentz_Ten3.H
                 Lorentz_Ten4.H
                 Tensor.H
                 Tensor_Build.H
                 Tensor_Contractions.H
                 Tensor_Contractions_Epsilon.H
                 Uncertain.H)

sherpa_create_git_info(Math)
 
add_library(ToolsMath SHARED ${Math_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C )
sherpa_handle_shared_library(ToolsMath ToolsMath)
