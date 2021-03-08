#include <re.h>

#include <tokens.h>
#include "lex.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static void test(const char *text, enum token_kind expected_kind)
{
  printf("test '%s'....", text);
  fflush(stdout);
  enum token_kind kind = 0;
  unsigned length = lex(text, &kind);
  if (length == 0) {
    printf("Error: NO MATCH!\n");
  } else {
    // TODO I can test length against strlen(text)
    printf("%s\n", token_name_table[kind]);

    unsigned text_len = (unsigned)strlen(text);
    if (length != text_len) {
      printf("Error: only matched %u out of %u characters\n", length, text_len);
      exit(1);
    }

    enum token_kind next_kind = kind + 1;
    if (lex(text, &next_kind)) {
      printf("Error: text '%s' matched %s and %s\n", text, token_name_table[kind], token_name_table[next_kind]);
      exit(1);
    }

    if (kind != expected_kind) {
      printf("Error: expected kind '%s'\n", token_name_table[expected_kind]);
      exit(1);
    }

  }
}

int main(int argc, char *argv[])
{
  printf("There are %d patterns\n", TOKEN_COUNT);
  printf("--------------------------------------------------------------------------------\n");
  for (unsigned i = 0; i < TOKEN_COUNT; i++) {
    printf("%d: %s: %s\n", i, token_name_table[i], token_pattern_table[i]);
  }
  printf("--------------------------------------------------------------------------------\n");

  test(" ", TOKEN_KIND_INLINE_WHITESPACE);
  test("(", TOKEN_KIND_OPEN_PAREN);
  test(")", TOKEN_KIND_CLOSE_PAREN);
  test("@@", TOKEN_KIND_ESCAPE_SEQUENCE);
  test("@$", TOKEN_KIND_ESCAPE_SEQUENCE);
  test("@echo", TOKEN_KIND_BUILTIN_ID);
  test("a", TOKEN_KIND_ARG);
  test("#", TOKEN_KIND_COMMENT);
  test("#hello", TOKEN_KIND_COMMENT);
  test("\n", TOKEN_KIND_NEWLINE);
  test("#!/usr/bin/env", TOKEN_KIND_COMMENT);
  test("@%|hello|", TOKEN_KIND_DELIMITED_STRING_VERTBAR);
}
