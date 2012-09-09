#include <boost/algorithm/string.hpp>
#include "Util.h"

using namespace std;

namespace detail {
	string encode(const string s) {
		if(s == "")
			return "%_";
		else 
			return boost::algorithm::replace_all_copy(s, " ", "%_");
	}


	string decode(const string s) {
		if(s == "%_")
			return "";
		else
			return boost::algorithm::replace_all_copy(s, "%_", " ");
	}
}


/*
namespace detail {
  string encode(const string s) {
    static const char* hex = "0123456789ABCDEF";
    string result;
    result.reserve(s.size() * 2 + 1);
    for(unsigned int i = 0; i < s.size(); i++) {
      result += hex[((unsigned char) s[i]) & 0xF];
      result += hex[((unsigned char) s[i]) >> 4];
    }
    return result;
  }

  int hexToInt(char hex) {
    if(hex >= '0' && hex <= '9')
      return hex - '0';
    else 
      return (hex - 'A') + 10;
  }

  string decode(const string s) {
    string result;
    result.reserve(s.size() / 2 + 1);
    for(unsigned int i = 0; i < s.size(); i += 2)
      result += (char) (hexToInt(s[i]) | (hexToInt(s[i + 1]) << 4));
    return result;
  }
}
*/