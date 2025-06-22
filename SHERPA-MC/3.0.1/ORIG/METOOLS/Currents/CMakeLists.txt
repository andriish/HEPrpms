set(Currents_SOURCES C_Scalar.C
                     C_Spinor.C
                     C_Vector.C
                     C_Pseudo.C
                     S_C.C
                     F_C.C
                     V_C.C
                     P_C.C)

set(Currents_HEADERS C_Scalar.H
                     C_Spinor.H
                     C_Vector.H
                     C_Pseudo.H)

sherpa_create_git_info(Currents)
 
add_library(METoolsCurrents SHARED ${Currents_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C )
sherpa_handle_shared_library(METoolsCurrents METoolsCurrents)
