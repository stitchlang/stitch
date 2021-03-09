import re

def getTokensTxtFilename():
    import os
    return os.path.join(os.path.dirname(os.path.abspath(__file__)), "tokens.txt")

TOKENS_FILE_RE = re.compile(b"^([A-Z_]+) *(.*)")

def parseLine(line: bytes):
    match = TOKENS_FILE_RE.match(line)
    if not match:
        raise Exception("TOKENS_FILE_RE '{}' did not match line in tokens.txt: '{}'".format(TOKENS_FILE_RE.pattern.decode('ascii'), line.decode('ascii')))
    return match.group(1), match.group(2)
