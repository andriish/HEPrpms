set(Phasespace_SOURCES PS_Info.C
                       PS_Vertex.C
                       PS_Current.C
                       PS_Generator.C
                       PS_Channel.C)

set(Phasespace_HEADERS PS_Info.H
                       PS_Vertex.H
                       PS_Current.H
                       PS_Generator.H
                       PS_Channel.H)

sherpa_create_git_info(Phasespace)
 
add_library(ComixPhasespace SHARED ${Phasespace_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C )
sherpa_handle_shared_library(ComixPhasespace ComixPhasespace)
