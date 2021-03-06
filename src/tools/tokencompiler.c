#include <re.c>
#include <tokens.h>

#include <stdlib.h>
#include <stdio.h>


static void escape(char quote_char, char c, char *out_buf)
{
  if (c == '\\' || c == quote_char) {
    sprintf(out_buf, "\\%c", c);
  } else if (c == '\n') {
    sprintf(out_buf, "\\n");
  } else if (c == '\t') {
    sprintf(out_buf, "\\t");
  } else {
    sprintf(out_buf, "%c", c);
  }
}

void generate_regex(FILE *file, unsigned kind, regex_t *pattern)
{
  const char* types[] = { "UNUSED", "DOT", "BEGIN", "END", "QUESTIONMARK", "STAR", "PLUS", "CHAR", "CHAR_CLASS", "INV_CHAR_CLASS", "DIGIT", "NOT_DIGIT", "ALPHA", "NOT_ALPHA", "WHITESPACE", "NOT_WHITESPACE", "BRANCH" };

  fprintf(file, "static regex_t %s[] = {\n", token_name_table[kind]);

  int j;
  char c;
  for (int i = 0;; ++i)
  {
    if (i == MAX_REGEXP_OBJECTS) {
      fprintf(stderr, "ERROR: regex too long?");
      exit(1);
    }

    if (pattern[i].type == UNUSED)
    {
      break;
    }

    fprintf(file, "    { .type = %d", pattern[i].type);
    if (pattern[i].type == CHAR_CLASS || pattern[i].type == INV_CHAR_CLASS)
    {
      fprintf(file, ", { .ccl = (unsigned char*)\"");
      for (j = 0; j < MAX_CHAR_CLASS_LEN; ++j)
      {
        c = pattern[i].u.ccl[j];
        if ((c == '\0') || (c == ']'))
        {
          break;
        }
        char escaped[3];
        escape('"', c, escaped);
        fprintf(file, "%s", escaped);
      }
      fprintf(file, "\" }");
    }
    else if (pattern[i].type == CHAR)
    {
      fprintf(file, ", { .ch = '");
      char escaped[3];
      escape('\'', pattern[i].u.ch, escaped);
      fprintf(file, "%s' }", escaped);
    }
    fprintf(file, " }, // %s\n", types[pattern[i].type]);
  }
  fprintf(file, "    { .type = %d }, // UNUSED\n", UNUSED);
  fprintf(file, "};\n");
}

int main(int argc, char *argv[])
{
  if (argc <= 1) {
    printf("Usage: compiler HDR SRC\n");
    return 1;
  }
  argv++;argc--;

  const char *hdr_filename = argv[0];
  const char *src_filename = argv[1];
  {
    FILE *file = fopen(hdr_filename, "w");
    fprintf(file, "#define TOKEN_COUNT %d\n", TOKEN_COUNT);
    fprintf(file, "extern regex_t *regex_table[%d];\n", TOKEN_COUNT);
    fclose(file);
  }

  {
    FILE *file = fopen(src_filename, "w");
    fprintf(file, "#include <re.h>\n");
    for (unsigned kind = 0; kind < TOKEN_COUNT; kind++) {
      struct regex_t *token_re = re_compile(token_pattern_table[kind]);
      if (!token_re) {
        fprintf(stderr, "Error: failed to compile regex %s for token pattern %s\n", token_pattern_table[kind], token_name_table[kind]);
        return 1;
      }
      generate_regex(file, kind, token_re);
    }
    fprintf(file, "regex_t *regex_table[%d] = {\n", TOKEN_COUNT);
    for (unsigned kind = 0; kind < TOKEN_COUNT; kind++) {
      fprintf(file, "    %s,\n", token_name_table[kind]);
    }
    fprintf(file, "};\n");
    fclose(file);
  }
}
