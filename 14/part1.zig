const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const print = std.debug.print;

const Mask = struct {
    on: u36 = 0,
    off: u36 = 0,
    fn update(self: *Mask, mask: *const [36]u8) void {
        var on: u36 = 0;
        var off: u36 = 0;
        var bit_mask: u36 = 1 << 35;
        for (mask) |digit| {
            if (digit == '1') {
                on |= bit_mask;
            }
            if (digit == '0') {
                off |= bit_mask;
            }
            bit_mask >>= 1;
        }
        self.on = on;
        self.off = off;
    }
};

const Memory = struct {
    mask: *const Mask,
    rv: [99999]u36 = [_]u36{0} ** 99999,
    fn init(mask: *const Mask) Memory {
        return Memory { .mask = mask };
    }
    fn update(self: *Memory, address: u36, value: u36) !void {
        const real_value = (value | self.mask.on) & ~self.mask.off;
        self.rv[address] = real_value;
    }
    fn sum(self: *Memory) u64 {
        var total: u64 = 0;
        for (self.rv) |i| {
            total += i;
        }
        return total;
    }
};

pub fn main() !void {
    var mask = Mask {};
    var memory = Memory.init(&mask);

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
