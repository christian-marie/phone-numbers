#include "c-phone-numbers.h"
#include <phonenumbers/phonenumberutil.h>
#include <stdlib.h>
#include <cstring>
#include <string>

#include <iostream>

using namespace i18n::phonenumbers;

extern "C" void *_c_phone_number_util_get_instance(void) {
  return PhoneNumberUtil::GetInstance();
}

extern "C" void *_c_phone_number_ctor() {
  return new (std::nothrow) PhoneNumber;
}

extern "C" void _c_phone_number_dtor(void *phone_no) {
  delete (PhoneNumber *)phone_no;
}

extern "C" int _c_phone_number_util_parse(void *util_instance, char *number_str,
                                          size_t number_len, char *region_str,
                                          size_t region_len, void *phone_no) {
  return ((PhoneNumberUtil *)util_instance)
      ->Parse(std::string(number_str, number_len),
              std::string(region_str, region_len), (PhoneNumber *)phone_no);
}

extern "C" bool _c_phone_number_has_country_code(void *phone_no) {
  return ((PhoneNumber *)phone_no)->has_country_code();
}

extern "C" bool _c_phone_number_has_national_number(void *phone_no) {
  return ((PhoneNumber *)phone_no)->has_national_number();
}

extern "C" bool _c_phone_number_has_extension(void *phone_no) {
  return ((PhoneNumber *)phone_no)->has_extension();
}
extern "C" uint64 _c_phone_number_get_country_code(void *phone_no) {
  return ((PhoneNumber *)phone_no)->country_code();
}

extern "C" uint64 _c_phone_number_get_national_number(void *phone_no) {
  return ((PhoneNumber *)phone_no)->national_number();
}

// Up to caller to free
extern "C" char *_c_phone_number_get_extension(void *phone_no) {
  std::string src = ((PhoneNumber *)phone_no)->extension();
  char *dst = (char *)malloc(src.length() + 1);
  std::strcpy(dst, src.c_str());
  return dst;
}

// Updates in place
extern "C" void _c_phone_number_convert_alpha_characters_in_number(
    void *util_instance, char *number_str, size_t number_len) {
  std::string str(number_str, number_len);
  ((PhoneNumberUtil *)util_instance)->ConvertAlphaCharactersInNumber(&str);
  // We have to copy the overwritten data back, now, std::string seems to have
  // to copy :(
  std::memcpy(number_str, str.c_str(), number_len);
}

extern "C" PhoneNumberType _c_phone_number_util_get_number_type(
    void *util_instance, void *phone_no) {
  return (::PhoneNumberType)((PhoneNumberUtil *)util_instance)
      ->GetNumberType(*((PhoneNumber *)phone_no));
}
