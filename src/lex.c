#include <assert.h>

#include <re.h>
#include <tokens.h>
#include <compiledtokens.h>
#include "lex.h"

unsigned lex(const char *text, enum token_kind *kind)
{
  for (enum token_kind i = *kind; i < TOKEN_COUNT; i++) {
    int match_length = -1;
    int result = re_matchp(regex_table[i], text, &match_length);
    if (result != -1) {
      assert(result == 0); // this must be true because all patterns start with "^"
      assert(result >= 0);
      assert(match_length >= 0);
      *kind = i;
      return (unsigned)match_length;
    }
  }
  return 0;
}
