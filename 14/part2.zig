const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const U36_MAX = std.math.maxInt(u36);

const Mask = struct {
    on: u36 = 0,
    off: u36 = 0,
    float: u36 = 0,
    allocator: *Allocator,
    fn init(allocator: *Allocator) Mask {
        return Mask { .allocator = allocator };
    }
    fn update(self: *Mask, mask: *const [36]u8) void {
        var on: u36 = 0;
        var off: u36 = 0;
        var float: u36 = 0;
        var bit_mask: u36 = 1 << 35;
        for (mask) |digit| {
            if (digit == '1') {
                on |= bit_mask;
            }
            if (digit == '0') {
                off |= bit_mask;
            }
            if (digit == 'X') {
                float |= bit_mask;
            }
            bit_mask >>= 1;
        }
        self.on = on;
        self.off = off;
        self.float = float;
    }
    fn getAddresses(self: *const Mask, address: u36) ![]u36 {
        var list = ArrayList(u36).init(self.allocator);
        const base = (address & self.off) | self.on; 
        try list.append(base);
        var bit_mask: u36 = 1 << 35;
        while (bit_mask != 0) {
            if (self.float & bit_mask != 0) {
                for (list.items) |found| {
                    try list.append(found | bit_mask);
                }
            }
            bit_mask >>= 1;
        }
        return list.toOwnedSlice();
    }
};

const Memory = struct {
    mask: *const Mask,
    values: AutoHashMap(u64, u36),
    // rv: [U36_MAX]u36 = [_]u36{0} ** U36_MAX,
    fn init(allocator: *Allocator, mask: *const Mask) Memory {
        return Memory { .mask = mask, .values = AutoHashMap(u64, u36).init(allocator) };
    }
    fn update(self: *Memory, address: u36, value: u36) !void {
        for (try self.mask.getAddresses(address)) |resolved| {
            try self.values.put(resolved, value);
        }
    }
    fn sum(self: *Memory) u64 {
        var total: u64 = 0;
        var iter = self.values.iterator();
        while (iter.next()) |i| {
            total += i.value;
        }
        return total;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = &arena.allocator;

    var mask = Mask.init(allocator);
    var memory = Memory.init(allocator, &mask);

    const stdin = std.io.getStdIn().inStream();
    var buf: [256]u8 = undefined;
    while (try stdin.readUntilDelimiterOrEof(buf[0..], '\r')) |line| {
        // lol windows
        try stdin.skipUntilDelimiterOrEof('\n');
        if (mem.eql(u8, line[0..4], "mask")) {
            const new_mask = line[7..43]; // 36 characters after "mask = "
            mask.update(new_mask);
        } else {
            const index_start = 4; // after "mem["
            const index_end = index_start + mem.indexOfScalar(u8, line[index_start..], ']').?;
            const value_start = index_end + 4; // after "] = ";

            const index = try fmt.parseInt(u36, line[index_start..index_end], 10);
            const value = try fmt.parseInt(u36, line[value_start..], 10);
            try memory.update(index, value);
        }
    }

    const result = memory.sum();
    print("{}\n", .{result});
}
