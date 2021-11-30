%{
#include "PHASIC++/Channels/Single_Channel.H"
#include "PHASIC++/Channels/Rambo.H"
%}

namespace PHASIC {

  class Rambo : public Single_Channel {

  public:

    Rambo(size_t nin, std::vector<double> masses);

    ATOOLS::Vec4D_Vector GeneratePoint(double E);
  };
}

