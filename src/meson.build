project(
  'stitch',
  'c',
  default_options : [
    'c_std=c11',
  ],
)

prog_python = import('python').find_installation('python3')
tokens_c = custom_target(
  'tokens_generator',
  input : ['tools/generate-tokens-c', '../tokens.txt'],
  output : ['tokens.h', 'tokens.c'],
  command : [prog_python, '@INPUT0@', '@INPUT1@', '@OUTPUT0@', '@OUTPUT1@']
)

tokencompiler = executable(
  'tokencompiler',
  'tools/tokencompiler.c',
  tokens_c,
  include_directories: [
    'tiny-regex-c',
  ],
)
compiledtokens_c = custom_target(
  'compiledtokens_generator',
  output : ['compiledtokens.h', 'compiledtokens.c'],
  command : [tokencompiler, '@OUTPUT0@', '@OUTPUT1@']
)

executable(
  'testlex',
  'tools/testlex.c',
  'lex.c',
  'tiny-regex-c/re.c',
  tokens_c,
  compiledtokens_c,
  include_directories: [
    'tiny-regex-c',
  ],
)


executable(
  'lexfile',
  'tools/lexfile.c',
  'lex.c',
  'tiny-regex-c/re.c',
  tokens_c,
  compiledtokens_c,
  include_directories: [
    'tiny-regex-c',
  ],
)

