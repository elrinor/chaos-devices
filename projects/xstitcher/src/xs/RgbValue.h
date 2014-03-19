#ifndef __XSX_RGB_VALUE_H__
#define __XSX_RGB_VALUE_H__

#include "config.h"
#include <boost/program_options.hpp>
#include <boost/spirit/include/qi.hpp>
#include <vigra/rgbvalue.hxx>

namespace xs {
  typedef vigra::RGBValue<vigra::UInt8> RgbValue;

} // namespace xs

namespace vigra {
  /* ADL won't find our operator in xs namespace, that's why we put it here. */
  inline bool operator< (const xs::RgbValue& l, const xs::RgbValue& r) {
    return l.red() < r.red() || (l.red() == r.red() && (l.green() < r.green() || (l.green() == r.green() && l.blue() < r.blue())));
  }

  /** Boost program_options validation function for RgbValue. */
  inline void validate(boost::any& v, const std::vector<std::string>& values, xs::RgbValue* /* target_type */, int) {
    using namespace boost::program_options;
    using namespace boost::spirit::qi;

    /* Check that it's the only one. */
    validators::check_first_occurrence(v);
    const std::string& s = validators::get_single_string(values);

    /* Match. */
    int r, g, b;
    std::string::const_iterator begin = s.begin();
    if(!parse(begin, s.end(), uint_ >> ':' >> uint_ >> ':' >> uint_, r, g, b))
      throw invalid_option_value(s);

    /* Ok. */
    v = xs::RgbValue(r, g, b);
  }

} // namespace vigra

#endif // __XSX_RGB_VALUE_H__
