const std = @import("std");
const builtin = std.builtin;

fn nextArg(it: *std.process.ArgIterator) ?[:0]const u8 {
    return (if (builtin.os.tag != .windows) it.nextPosix()
        else @compileError("not impl"));
}

pub fn main() u8 {
    const opts: struct {
        filename: []const u8,
        src: []const u8,
        script_args: []const []const u8
    } = blk: {
        var it = std.process.args();
        const program_arg = nextArg(&it);
        var first_arg = @as(?[:0]const u8, if (program_arg != null) nextArg(&it) else null) orelse {
            std.io.getStdErr().writer().writeAll(
                \\Usage: stitch FILE ARGS...
                \\       stitch -c COMMAND (this is only 1 argument)
                \\
            ) catch unreachable;
            return 1;
        };
        if (std.mem.eql(u8, first_arg, "-c")) {
            const cmd = nextArg(&it) orelse {
                std.log.err("-c requires 1 argument", .{});
                return 1;
            };
            if (nextArg(&it) != null) {
                std.log.err("-c only accepts 1 argument", .{});
                return 1;
            }
            std.debug.print("TODO: execute '{s}'\n", .{cmd});
            break :blk .{
                .filename = "<the -c command>",
                .src = cmd,
                .script_args = &[_][]const u8 { },
            };
        }

        //var script_args_builder = std.ArrayList([]constu8).init();
        break :blk .{
            .filename = first_arg,
            .src = "???",
            .script_args = &[_][]const u8 { },
        };
    };

    std.log.info("opts: {}", .{opts});
    return 0;
}
