const std = @import("std");

// Note: Export is needed to make the function available outside of Zig
// The C calling convention is used by default for exported functions,
// but it's better to be explicit about it (specified by "callconv").
export fn add(a: i32, b: i32) callconv(.C) i32 {
    return a + b;
}

const Allocator = std.mem.Allocator;

const Example = struct {
    field: i32,

    fn create(allocator: Allocator) *Example {
        const obj = allocator.create(Example) catch std.debug.panic("Failed to alloc Example struct", .{});
        obj.field = 42;
        return obj;
    }

    fn deinit(self: *Example) void {
        _ = self;
    }

    fn do_stuff(self: *Example, arg: i32) bool {
        return self.field == arg;
    }
};

export fn example_create() callconv(.C) *Example {
    return Example.create(std.heap.c_allocator);
}

export fn example_destroy(ptr: ?*Example) callconv(.C) void {
    std.debug.assert(ptr != null);
    const obj = ptr.?;
    obj.deinit();
    std.heap.c_allocator.destroy(obj);
}

export fn example_do_stuff(ptr: ?*Example, arg: i32) callconv(.C) bool {
    std.debug.assert(ptr != null);
    return ptr.?.do_stuff(arg);
}
