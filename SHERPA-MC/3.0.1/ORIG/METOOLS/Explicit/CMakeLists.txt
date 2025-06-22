set(Explicit_SOURCES C_Object.C
                     Current.C
                     Vertex_Key.C
                     Color_Calculator.C
                     Lorentz_Calculator.C
                     NLO_Counter_Terms.C
                     Dipole_Kinematics.C
                     Dipole_Color.C
                     Dipole_Terms.C
                     Form_Factor.C
                     Vertex.C)

set(Explicit_HEADERS C_Object.H
                     Current.H
                     Vertex_Key.H
                     Color_Calculator.H
                     Lorentz_Calculator.H
                     NLO_Counter_Terms.H
                     Dipole_Info.H
                     Dipole_Kinematics.H
                     Dipole_Color.H
                     Dipole_Terms.H
                     Form_Factor.H
                     Vertex.H)

sherpa_create_git_info(Explicit)
 
add_library(METoolsExplicit SHARED ${Explicit_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C )
sherpa_handle_shared_library(METoolsExplicit METoolsExplicit)
