set(Main_SOURCES Process_Base.C
                 Process_Group.C
                 Single_Process.C
                 Single_Dipole_Term.C
                 Comix.C)

set(Main_HEADERS Process_Base.H
                 Process_Group.H
                 Single_Process.H
                 Single_Dipole_Term.H)

sherpa_create_git_info(Main)
 
add_library(Comix SHARED ${Main_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C )
sherpa_handle_shared_library(Comix Comix)
