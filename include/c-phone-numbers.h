#ifndef _ffi_phone_numbers
#define _ffi_phone_numbers

/* XXX fail
 *
 * The enum below is a nasty copy/paste from libphonenumber's
 * phonenumbers/phonenumberutil.h
 *
 * This could become out of sync in future, with some nasty results.
 *
 * I can't see a particularly good way out of this, though, as it's defined
 * within a is a C++ header, and no haskell binding tools appear to be able to
 * parse C++.
 */

// Type of phone numbers.
enum PhoneNumberType {
        FIXED_LINE,
        MOBILE,
        // In some regions (e.g. the USA), it is impossible to distinguish
        // between fixed-line and mobile numbers by looking at the phone number
        // itself.
        FIXED_LINE_OR_MOBILE,
        // Freephone lines
        TOLL_FREE, PREMIUM_RATE,
        // The cost of this call is shared between the caller and the
        // recipient, and is hence typically less than PREMIUM_RATE calls. See
        // http://en.wikipedia.org/wiki/Shared_Cost_Service for more
        // information.
        SHARED_COST,
        // Voice over IP numbers. This includes TSoIP (Telephony Service over
        // IP).
        VOIP,
        // A personal number is associated with a particular person, and may be
        // routed to either a MOBILE or FIXED_LINE number. Some more
        // information can be found here:
        // http://en.wikipedia.org/wiki/Personal_Numbers
        PERSONAL_NUMBER, PAGER,
        // Used for "Universal Access Numbers" or "Company Numbers". They may
        // be further routed to specific offices, but allow one number to be
        // used for a company.
        UAN,
        // Used for "Voice Mail Access Numbers".
        VOICEMAIL,
        // A phone number is of type UNKNOWN when it does not fit any of the
        // known patterns for a specific region.
        UNKNOWN
};

enum PhoneNumberFormat {
    E164,
    INTERNATIONAL,
    NATIONAL,
    RFC3966
};

enum ErrorType {
                NO_PARSING_ERROR,
                INVALID_COUNTRY_CODE_ERROR,  // INVALID_COUNTRY_CODE in the java version.
                NOT_A_NUMBER,
                TOO_SHORT_AFTER_IDD,
                TOO_SHORT_NSN,
                TOO_LONG_NSN,  // TOO_LONG in the java version.
};

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
