#include <ctype.h> // for isalpha, isdigit, isspace
#include <string.h> // for strchr

#include <assert.h>
#include "re.h"

#ifndef RE_TRACE
//#include <stdio.h>
//#define RE_TRACE(fmt,...) do { fprintf(stderr, "RE_TRACE: " fmt "\n", ##__VA_ARGS__); } while (0)
#define RE_TRACE(fmt,...)
#endif

// By default, re.c uses 2 pointers to delineate the start and the end of the text being matched.
// This means the user is not required to null-terminate their text.  It also means the \0 character
// can be included in the text to be matched.
//
// The disadvantage of this mechanism is that it requires the user to know the end of the string, and
// 2 pointers are passed around for the text instead of 1 which may have some affect on performance.
// So the RE_CSTRINGS option can be defined to use null-terminated strings.
//
#define text_is_empty(ptr, end) text_is_empty_func(text_call(ptr, end))
#define text_first_char(ptr, end) text_first_char_func(text_call(ptr, end))
#define text_inc(ptr_ref, end) text_inc_func(text_call(ptr_ref, end))

#ifdef RE_CSTRINGS

#define RE_MATCH re_match_cstr
#define RE_MATCH_START_ONLY re_match_start_only_cstr

#define text_decl(ptr, end) const char *ptr
#define text_call(ptr, end) ptr

#define PRITEXT "%s"
#define PRITEXTARGS(ptr, end) ptr

static inline int text_is_empty_func(const char *ptr) { return ptr[0] == '\0'; }
static inline char text_first_char_func(const char *ptr)
{
  assert(ptr[0] != '\0');
  return ptr[0];
}
static inline void text_inc_func(const char **ptr_ref)
{
  assert((*ptr_ref)[0] != '\0');
  *ptr_ref += 1;
}

#else

#define RE_MATCH re_match
#define RE_MATCH_START_ONLY re_match_start_only

#define text_decl(ptr, end) const char *ptr, const char *end
#define text_call(ptr, end) ptr, end

#define PRITEXT "%.*s"
#define PRITEXTARGS(ptr, end) (unsigned)(end-ptr), ptr
static inline int text_is_empty_func(const char *ptr, const char *end)
{
  assert(ptr <= end);
  return ptr == end;
}
static inline char text_first_char_func(const char *ptr, const char *end)
{
  assert(ptr < end);
  return ptr[0];
}
static inline void text_inc_func(const char **ptr_ref, const char *end)
{
  assert(*ptr_ref < end);
  *ptr_ref += 1;
}
#endif

#define AS_STRING_ENTRY(s) #s,
const char *re_error_name_table[] = { "NO_ERROR", RE_ERRORS(AS_STRING_ENTRY) };

// the length of 1 node in the regex
re_length_t nodelen(const char *regex, enum re_error *error_code)
{
    const char c = regex[0];
    if (c == '\0')
      return 0;
    if (c == '\\') {
      if (regex[1] == 0) {
        *error_code = RE_ERROR_ENDS_WITH_BACKSLASH;
        return 0;
      }
      return 2;
    }
    if (c == '*' || c == '+' || c == '.' || c == '$' || c == '^' || c == '?')
      return 1;
    if (c == '[') {
      int in_escape = 0;
      for (re_length_t i = 1;;i++) {
        switch (regex[i]) {
        case ']':
          if (!in_escape)
            return i + 1;
          in_escape = 0;
          break;
        case '\\':
          in_escape = !in_escape;
          break;
        case '\0':
          *error_code = RE_ERROR_MISSING_RIGHT_SQUARE_BRACKET;
          return 0;
        default:
          in_escape = 0;
        }
      }
    }
    return 1;
}

static int isalphanum(char c)
{
  return (c == '_') || isalpha(c) || isdigit(c);
}

// TODO: return -1 on BAD_ESCAPE
static int try_match_escape(char regex_c, char text_c)
{
  switch (regex_c)
  {
  case 'd': return isdigit(text_c)    ? 1 : 0;
  case 'D': return isdigit(text_c)    ? 0 : 1;
  case 'w': return isalphanum(text_c) ? 1 : 0;
  case 'W': return isalphanum(text_c) ? 0 : 1;
  case 's': return isspace(text_c)    ? 1 : 0;
  case 'S': return isspace(text_c)    ? 0 : 1;

  // common escapes
  case 't': return text_c == '\t';
  case 'n': return text_c == '\n';

  case '\\': case '*': case '+':
  case '?': case '.':
  case '$': case '^':
  case '(': case ')':
  case '[': case ']':
  case '{': case '}':
  case '-': case '|':
  return regex_c == text_c;
  }
  return -1; // BAD_ESCAPE
}

#ifdef RE_DOT_NO_NEWLINE
static inline int matchdot(char c){ return c != '\n'; }
#else
#define matchdot(c) 1
#endif

// TODO: return -1 on BAD_ESCAPE
// caller guarantees regex is terminated by ']'
// regex starts after the [ and the first ^ if it exists.
static int try_matchcharset(const char *regex, char c)
{
  while (1) {
    char regex_c = regex[0];
    if (regex_c == ']')
      return 0;

    if (regex[1] == '-' && regex[2] != ']') {
      if (c >= regex_c && c <= regex[2]) {
        return 1;
      }
      regex += 3;
    } else {
      if (regex_c == '\\') {
        int result = try_match_escape(regex[1], c);
        if (result != 0) {
          return result;
        }
        regex += 2;
      } else if (regex_c == '.') {
        // NOTE: why would you have '.' in a char set?
        return matchdot(c);
      } else {
        if (regex_c == c)
          return 1;
        regex++;
      }
    }
  }
}

// inverts the result of a match if it's not an error
static inline int flip_try_match_result(int result)
{
  static const int try_not_table[] = {-1, 1, 0};
  assert(result >= -1);
  assert(result <= 1);
  return try_not_table[result + 1];
}

// returns -1 on invalid escape sequence
static int try_matchone(const char *regex, char text_c)
{
  RE_TRACE("matchone regex='%s' c='%c'", regex, text_c);
  char regex_c = regex[0];
  if (regex_c == '\\')
    return try_match_escape(regex[1], text_c);

  if (regex_c == '.')
    return matchdot(text_c);

  if (regex_c == '[') {
    if (regex[1] ==  '^') {
      return flip_try_match_result(try_matchcharset(regex + 2, text_c));
    }
    return try_matchcharset(regex + 1, text_c);
  }
  return regex_c == text_c;
}

static int matchquestion(struct re_context *context, const char *regex_left, const char *regex_right, text_decl(text_ptr, text_end))
{
  RE_TRACE("matchquestion regex_right='%s' text='"PRITEXT"'", regex_right, PRITEXTARGS(text_ptr, text_end));
  if (RE_MATCH_START_ONLY(context, regex_right, text_call(text_ptr, text_end)))
    return 1;
  if (!text_is_empty(text_ptr, text_end)) {
    int result = try_matchone(regex_left, text_first_char(text_ptr, text_end));
    if (result == -1) {
      context->error = RE_ERROR_BAD_ESCAPE;
      context->error_location = regex_left;
      return 0;
    }
    if (result) {
      text_inc(&text_ptr, text_end);
      if (RE_MATCH_START_ONLY(context, regex_right, text_call(text_ptr, text_end))) {
        context->match_length++;
        return 1;
      }
    }
  }
  return 0;
}

static int matchplus(struct re_context *context, const char *regex_left, const char *regex_right, text_decl(text_ptr, text_end))
{
  RE_TRACE("matchplus regex_right='%s' text='"PRITEXT"'", regex_right, PRITEXTARGS(text_ptr, text_end));
  const char *text_start = text_ptr;

  while (!text_is_empty(text_ptr, text_end)) {
    int result = try_matchone(regex_left, text_ptr[0]);
    if (result == -1) {
      context->error = RE_ERROR_BAD_ESCAPE;
      context->error_location = regex_left;
      return 0;
    }
    if (!result)
      break;
    text_inc(&text_ptr, text_end);
  }
  for (;text_ptr > text_start; text_ptr--) {
    if (RE_MATCH_START_ONLY(context, regex_right, text_call(text_ptr, text_end))) {
      context->match_length += text_ptr - text_start;
      return 1;
    }
  }
  return 0;
}

int RE_MATCH_START_ONLY(struct re_context *context, const char *regex, text_decl(text_ptr, text_end))
{
  RE_TRACE("RE_MATCH_START_ONLY regex='%s' text='"PRITEXT"' (match=%u)", regex, PRITEXTARGS(text_ptr, text_end), (unsigned)context->match_length);
  int save_length = context->match_length;
  while (1) {
    const char current_c = regex[0];
    if (current_c == '\0')
      return 1;
    enum re_error node_length_error = RE_ERROR_NONE;
    const char *next_regex = regex + nodelen(regex, &node_length_error);
    if (node_length_error != RE_ERROR_NONE) {
      context->error = node_length_error;
      context->error_location = regex;
      return 0; // just continue, the user can handle the error if they so chose
    }
    const char next_c = next_regex[0];
    if (next_c == '?') {
      enum re_error ignoreme = 0;
      assert(nodelen(next_regex, &ignoreme) == 1);
      assert(ignoreme == 0);
      return matchquestion(context, regex, next_regex + 1, text_call(text_ptr, text_end));
    }

    if (next_c == '+') {
      enum re_error ignoreme = 0;
      assert(nodelen(next_regex, &ignoreme) == 1);
      assert(ignoreme == 0);
      return matchplus(context, regex, next_regex + 1, text_call(text_ptr, text_end));
    }

    if (current_c == '$' && next_c == '\0') {
      return text_is_empty(text_ptr, text_end);
    }

    if (current_c == '|') {
      context->error = RE_ERROR_BRANCH_NOT_IMPLEMENTED;
      context->error_location = regex;
      return 0;
    }

    if (next_c == '*') {
      enum re_error ignoreme = 0;
      assert(nodelen(next_regex, &ignoreme) == 1);
      assert(ignoreme == 0);
      if (matchplus(context, regex, next_regex + 1, text_call(text_ptr, text_end))) {
        return 1;
      }
      regex = next_regex + 1;
    } else {
      if (text_is_empty(text_ptr, text_end)) {
        break;
      }
      int result = try_matchone(regex, text_first_char(text_ptr, text_end));
      if (result == -1) {
        context->error = RE_ERROR_BAD_ESCAPE;
        context->error_location = regex;
        break;
      }
      if (result == 0)
        break;
      context->match_length++;
      text_inc(&text_ptr, text_end);
      regex = next_regex;
    }
  }

  context->match_length = save_length;
  return 0;
}

int RE_MATCH(struct re_context *context, const char *regex, text_decl(text_ptr, text_end), re_length_t *out_match_start)
{
  RE_TRACE("match '%s' '"PRITEXT"'", regex, PRITEXTARGS(text_ptr, text_end));
  if (regex[0] == '^') {
    context->match_length = 0;
    if (!RE_MATCH_START_ONLY(context, regex + 1, text_call(text_ptr, text_end)))
      return 0;
    *out_match_start = 0;
    return 1;
  }

  const char *text_start = text_ptr;
  for (;; text_inc(&text_ptr, text_end)) {
    context->match_length = 0;
    if (RE_MATCH_START_ONLY(context, regex, text_call(text_ptr, text_end))) {
      *out_match_start = text_ptr - text_start;
      return 1;
    }
    if (text_is_empty(text_ptr, text_end))
      return 0;
  }
}
