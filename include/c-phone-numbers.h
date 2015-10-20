#ifndef _ffi_phone_numbers
#define _ffi_phone_numbers

#ifdef __cplusplus
extern "C"
{
#endif

#include <stddef.h>

void * _c_phone_number_ctor();
void * _c_phone_number_util_get_instance();
int _c_phone_number_util_parse(void *util_instance,
		                 char *number_str, size_t number_len,
		                 char *region_str, size_t region_len,
				 void *phone_no_ptr);

#ifdef __cplusplus
}
#endif

#endif
