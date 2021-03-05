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
  input : ['tokens/generate-tokens-c', '../tokens.txt'],
  output : ['tokens.h', 'tokens.c'],
  command : [prog_python, '@INPUT0@', '@INPUT1@', '@OUTPUT0@', '@OUTPUT1@']
)

tokencompiler = executable(
  'tokencompiler',
  'tokens/compiler.c',
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
  'testlex.c',
  'lex.c',
  'tiny-regex-c/re.c',
  tokens_c,
  compiledtokens_c,
  include_directories: [
    'tiny-regex-c',
  ],
  c_args: [
    '-DMAX_REGEXP_OBJECTS=30',
    # TODO: I'm not sure what this is for exactly, but apparently I need it because
    #       my regex's won't compile if it gets too small
    '-DMAX_CHAR_CLASS_LEN=20',
  ],
)

