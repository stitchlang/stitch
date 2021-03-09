# This module provides code that helps make my
# python code behave more like C code.
#
# This will help in creating a python implementation
# that matches my C implementation more closely.

from typing import TypeVar, Generic

T = TypeVar('T')

class Ref(Generic[T]):
    def __init__(self, value: T):
        self.value = value

# Emulates C string pointer semantics by using Python's bytes type and an int offset
class StringPtr:
    def __init__(self, full_string: bytes, offset: int):
        assert(isinstance(full_string, bytes))
        self.full_string = full_string
        self.offset = offset
    def charAt(self, offset: int) -> int:
        assert(offset >= 0)
        # python will bounds check this for us
        return self.full_string[self.offset + offset]
    def toStringWithLimitOffset(self, limit_offset: int) -> bytes:
        assert(limit_offset <= len(self.full_string))
        assert(self.offset <= limit_offset)
        return self.full_string[self.offset:limit_offset]
    def toStringWithLength(self, length: int) -> bytes:
        assert(length >= 0)
        return self.toStringWithLimitOffset(self.offset + length)
    def toStringWithLimit(self, limit: 'StringPtr') -> bytes:
        assert(self.full_string is limit.full_string)
        assert(self.offset <= limit.offset)
        return self.full_string[self.offset:limit.offset]
    def subtract(self, lower: 'StringPtr') -> int:
        assert(self.full_string is lower.full_string)
        assert(self.offset >= lower.offset)
        return self.offset - lower.offset
