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
