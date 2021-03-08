/*
 *
 * Mini regex-module inspired by Rob Pike's regex code described in:
 *
 * http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
 *
 * Supports:
 * ----------------------------------------
 *   .        Dot, matches any character (unless RE_DOT_NO_NEWLINE is defined)
 *   ^        Start anchor, matches beginning of string
 *   $        End anchor, matches end of string
 *   *        Asterisk, match zero or more (greedy)
 *   +        Plus, match one or more (greedy)
 *   ?        Question, match zero or one (non-greedy)
 *   [abc]    Character class, match if one of {'a', 'b', 'c'}
 *   [^abc]   Inverted class, match if NOT one of {'a', 'b', 'c'} -- NOTE: feature is currently broken!
 *   [a-zA-Z] Character ranges, the character set of the ranges { a-z | A-Z }

 * Some common "character classes":
 * ----------------------------------------
 *   \s       Whitespace, \t \f \r \n \v and spaces
 *   \S       Non-whitespace
 *   \w       Alphanumeric, [a-zA-Z0-9_]
 *   \W       Non-alphanumeric
 *   \d       Digits, [0-9]
 *   \D       Non-digits
 *
 * All of the control characters mentioned above can be escaped with a leading backslash.
 * The following escape sequences are also supported:
 * ----------------------------------------
 *   \\
 *   \t   \n
 *   \-   \|
 *   \(   \)
 *   \[   \]
 *   \{   \}
 */

#define RE_ERRORS(H) \
  H(ENDS_WITH_BACKSLASH) \
  H(MISSING_RIGHT_SQUARE_BRACKET) \
  H(BAD_ESCAPE) \
  H(BRANCH_NOT_IMPLEMENTED)

#define RE_ERROR_AS_ENUM_MEMBER(s) RE_ERROR_##s,
enum re_error { RE_ERROR_NONE, RE_ERRORS(RE_ERROR_AS_ENUM_MEMBER) };
#undef RE_ERROR_AS_ENUM_MEMBER

extern const char *re_error_name_table[];

#ifdef RE_USE_SIZE_T
typedef size_t re_length_t;
#else
typedef unsigned re_length_t;
#endif

struct re_context {
  re_length_t match_length;
  enum re_error error;
  const char *error_location;
};

#ifdef RE_CSTRINGS

// returns 1 on match, 0 for no match
// start of the match is returned in out_match_start
// check context->error for any regex errros
int re_match_cstr(struct re_context *context, const char *regex, const char *text_start, re_length_t *out_match_start);

// returns 1 on match, 0 for no match
// check context->error for any regex errros
int re_match_start_only_cstr(struct re_context *context, const char *regex, const char *text_start);

#else

// returns 1 on match, 0 for no match
// start of the match is returned in out_match_start
// check context->error for any regex errros
int re_match(struct re_context *context, const char *regex, const char *text_start, const char *text_limit, re_length_t *out_match_start);

// returns 1 on match, 0 for no match
// check context->error for any regex errros
int re_match_start_only(struct re_context *context, const char *regex, const char *text_start, const char *text_limit);

#endif
