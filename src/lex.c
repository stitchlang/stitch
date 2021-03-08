#include <assert.h>
#include <re.h>
#include <tokens.h>
#include "lex.h"

unsigned lex(const char *text, const char *limit, enum token_kind *kind)
{
  for (enum token_kind i = *kind; i < TOKEN_COUNT; i++) {
    struct re_context context = {0};
    int match = re_match_start_only(&context, token_pattern_table[i], text, limit);
    // there should be no errors in the regex's
    assert(!context.error);
    if (match) {
      assert(context.match_length >= 0); // all our regex's should have non-zero lengths
      *kind = i;
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      // TODO: properly handle large re_length_t
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      return (unsigned)context.match_length;
    }
  }
  return 0;
}
