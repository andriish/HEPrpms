set(Amplitude_SOURCES Amplitude.C)

set(Amplitude_HEADERS Amplitude.H)

sherpa_create_git_info(Amplitude)
 
add_library(ComixAmplitude SHARED ${Amplitude_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/Git_Info.C )
sherpa_handle_shared_library(ComixAmplitude ComixAmplitude)
