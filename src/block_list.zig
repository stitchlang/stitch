const std = @import("std");

/// A discontiguous list made of blocks of contiguous elements.
///
/// Each block takes a single allocation from the underlying allocator
/// and is doubly-linked to the blocks next to it.
///
/// When the current block is full, an attempt will be made to expand it
/// in place before creating a new block.
///
/// This list is very memory efficient at the cost of making random access O(n)
/// where n is the number of "blocks".  However iteration through the list
/// is still O(1).
///
pub fn BlockList(comptime T: type, comptime options: struct {
    getAllocElementLen: fn () usize = defaultGetAllocElementLen,
}) type {
    return struct {
        const BlockQueue = std.TailQueue(struct {
            alloc_len: usize,
            element_count: usize,
        });
        const BlockNode = BlockQueue.Node;
    
        const elements_offset = @sizeOf(BlockNode) + alignPadding(BlockNode, T);
    
        allocator: *std.mem.Allocator,
        block_queue: BlockQueue = .{},
    
        pub fn deinit(self: *@This()) void {
            var it = self.block_queue.last;
            while (it) |block_node| {
                it = block_node.prev; // do before calling free
                self.allocator.free(getAllocSlice(block_node));
            }
        }
    
        fn getAllocSlice(block_node: *BlockNode) []u8 {
            return @ptrCast([*]u8, block_node)[0 .. block_node.data.alloc_len];
        }
    
        fn getElementPtr(block_node: *BlockNode) [*]T {
            return @intToPtr([*]T, @ptrToInt(block_node) + elements_offset);
        }
    
        fn getElementSlice(block_node: *BlockNode) []T {
            const ptr = getElementPtr(block_node);
            const byte_len = block_node.data.alloc_len - elements_offset;
            return ptr[0.. (@divTrunc(byte_len, @sizeOf(T))) ];
        }
    
        fn allocBlock(self: *@This(), first_element: T) !void {
            const alloc_element_len = options.getAllocElementLen();
            std.debug.assert(alloc_element_len > 0);
            const alloc_len = elements_offset + (alloc_element_len * @sizeOf(T));
    
            const block_mem = @alignCast(
                @alignOf(BlockNode),
                try self.allocator.allocFn(
                    self.allocator,
                    alloc_len,
                    @alignOf(BlockNode),
                    1,
                    @returnAddress()
                )
            );
            //errdefer allocator.free(block_mem);
            
            const block_node = @ptrCast(*BlockNode, block_mem);
            block_node.* = BlockNode { .data = .{
                .alloc_len = block_mem.len,
                .element_count = 1,
            }};
            const elements = getElementSlice(block_node);
            std.debug.assert(elements.len >= alloc_element_len);
            elements[0] = first_element;
            self.block_queue.append(block_node);
        }
    
        pub fn append(self: *@This(), element: T) !void {
            if (self.block_queue.last) |last| {
                const elements = getElementSlice(last);
                if (last.data.element_count < elements.len) {
                    elements.ptr[last.data.element_count] = element;
                    last.data.element_count += 1;
                    return;
                }
                // TODO: try to realloc the block!
                //       this could minimize overhead by reducing allocation count
            }
            try self.allocBlock(element);
        }
    
        pub fn iterator(self: *const @This()) Iterator {
            return Iterator { .block_node = self.block_queue.first };
        }

        pub const Iterator = struct {
            block_node: ?*BlockNode,
            next_index: usize = 0,

            pub fn next(it: *Iterator) ?T {
                if (it.block_node) |first_block_node| {
                    if (it.next_index < first_block_node.data.element_count) {
                        const i = it.next_index;
                        it.next_index += 1;
                        return getElementSlice(first_block_node)[i];
                    }
                    it.block_node = first_block_node.next;
                    it.next_index = 0;
                    if (it.block_node) |next_block_node| {
                        if (next_block_node.data.element_count > 0) {
                            it.next_index = 1;
                            return getElementSlice(next_block_node)[0];
                        }
                        std.debug.assert(next_block_node.next == null);
                    }
                }
                return null;
            }
        };
    };
}

test {
    {
        var b = BlockList(usize, .{}) { .allocator = std.testing.allocator };
        defer b.deinit();
        {
            var i: usize = 0;
            while (i < 100) : (i += 1) {
                try b.append(i);
            }
        }
        {
            var i: usize = 0;
            var it = b.iterator();
            i = 0;
            while (i < 100) : (i += 1) {
                try std.testing.expectEqual(i, it.next().?);
            }
            try std.testing.expectEqual(@as(?usize, null), it.next());
        }
    }
}

pub fn defaultGetAllocElementLen() usize {
    return 300;
}

// The amount of padding needed to align `NextType` if it appears after `FirstType`
fn alignPadding(comptime FirstType: type, comptime NextType: type) usize {
    if (@alignOf(FirstType) > @sizeOf(FirstType)) {
        @compileError("not sure what to do in this case, see https://github.com/ziglang/zig/issues/9588");
    }

    if (@alignOf(NextType) <= @alignOf(FirstType)) {
        comptime {
            // sanity check
            std.debug.assert(@alignOf(FirstType) % @alignOf(NextType) == 0);
        }
        return 0;
    }
    return @alignOf(NextType) - @alignOf(FirstType);
}
