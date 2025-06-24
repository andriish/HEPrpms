#ifndef ATOOLS_Org_STL_Tools_H
#define ATOOLS_Org_STL_Tools_H

#include "ATOOLS/Org/CXXFLAGS.H"
#include <cstddef>

#include <iostream>
#include <vector>

namespace std {

  template <typename __Tp>
  std::ostream &operator<<(std::ostream &str,const std::vector<__Tp> &v);

}

namespace ATOOLS {

  class String_Sort {
  private:

    std::less<std::string> m_less;

  public:

    bool operator()(const std::string &a,const std::string &b) const;
    
  };// end of class String_Sort

  std::vector<int> ID(size_t id);
  size_t ID(const std::vector<int> &id);

  size_t IdCount(size_t id);

  template <class __Tp>
  class AutoDelete_Vector: public std::vector<__Tp*> {
  public:

    AutoDelete_Vector()
    {
    }

    virtual ~AutoDelete_Vector() 
    {
      while (!this->empty()) {
	delete this->back();
	this->pop_back();
      }
    }

  };// end of class AutoDelete_Vector

  template <class Iterator>
  Iterator AllUnique(Iterator first, Iterator last)
  {
    while (first != last)
    {
      Iterator next(first);
      last = remove(++next, last, *first);
      first = next;
    }
    return last;
  }

}

#endif

