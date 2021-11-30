#include "ATOOLS/Org/MyStrStream.H"
#include <vector>
#include <algorithm>
#include <limits>

#include "ATOOLS/Org/Exception.H"

namespace ATOOLS {

  template<> std::string ToString<std::string>(const std::string& value,
      const size_t precision)
  {
    return value;
  }

  template<> std::string ToType<std::string>(const std::string& value,
                                             const size_t precision)
  {
    return value;
  }

  template<> bool ToType<bool>(const std::string& value,
                               const size_t precision)
  {
    std::string v{ value };
    std::transform(v.begin(), v.end(), v.begin(), ::tolower);
    if (v == "false" || v == "no" || v == "off" || v == "0") {
      return false;
    }
    return true;
  }

  template <> double ToType(const std::string &value,
		            const size_t precision) {
    MyStrStream converter;
    double converted;
    converter.precision(precision);
    converter<<value;
    converter>>converted;
    if (converter.fail()) {
      if (value == "inf")
        converted = std::numeric_limits<double>::infinity();
      else if (value == "-inf")
        converted = -std::numeric_limits<double>::infinity();
      else
        THROW(fatal_error, "Failed to parse " + value);
    }
    return converted;
  }

  std::string StringTrim(const std::string& untrimmed)
  {
    std::string s{ untrimmed };
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
          return !std::isspace(ch);
          }));
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
          return !std::isspace(ch);
          }).base(), s.end());
    return s;
  }

  std::string StringReplace(const std::string &original,
                            const std::string &from, const std::string &to)
  { 
    if(from.length()==0) return original;
    std::string result=original;
    std::vector<int> matches;
    int pos=result.find(from);
    while(pos!=-1) {
      matches.push_back(pos);
      pos=result.find(from,pos+1);
    }

    int offset=0;
    size_t total=matches.size();
    int diff=to.length()-from.length();
    int fromlength=from.length();
    for(size_t i=0;i<total;++i) {
      result.erase(matches[i]+offset,fromlength);
      result.insert(matches[i]+offset,to);
      offset+=diff;
    }
    return result;
  }
  
  std::string SubString(const std::string& original,
                        const std::string& begin, const std::string& end)
  {
    size_t ibegin = original.find(begin);
    if (ibegin == std::string::npos) return "";
    ibegin += begin.length();
    size_t iend = original.find(end);
    return original.substr(ibegin, iend-ibegin);
  }
  
  std::string ReplaceUnits(const std::string& instring)
  {
    // - a unit can not be the first character
    // - a unit can not be preceded by an alphabetic character or an underscore
    // - a unit can not be succeeded by an alphabetic character (exception: B
    //   for byte) or an underscore

    auto converter = [](const std::string& s) -> std::string {
      if      (s == "%")  return "/100.0";
      else if (s == "k")  return "*1000";
      else if (s == "M")  return "*1000000";
      else if (s == "G")  return "*1000000000";
      else if (s == "kB") return "*(1<<10)";
      else if (s == "MB") return "*(1<<20)";
      else if (s == "GB") return "*(1<<30)";
      else                return "";
    };
    auto is_alpha_or_underscore = [](const char c) -> bool {
      return (std::isalpha(c) || c == '_');
    };

    const auto length = instring.length();
    std::string outstring;
    for (size_t i{ 0 }; i < length;) {
      if (std::isblank(instring[i])) {
        ++i;
        continue;
      } else if (i > 0) {
        if (!is_alpha_or_underscore(instring[i - 1])) {
          size_t nchars{ 0 };
          if (i + 1 < length) {
            if (instring[i + 1] == 'B') {
              if (!(i + 2 < length && is_alpha_or_underscore(instring[i + 2])))
                nchars = 2;
            } else if (!is_alpha_or_underscore(instring[i + 1])) {
              nchars = 1;
            }
          } else {
            nchars = 1;
          }
          const auto converted = converter(instring.substr(i, nchars));
          if (converted != "") {
            outstring += converted;
            i += nchars;
            continue;
          }
        }
      }
      outstring += instring[i];
      ++i;
    }

    return outstring;
  }

}// end of namespace ATOOLS;
