const std = @import("std");

const AutoArrayHashMap = std.AutoArrayHashMap;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

fn parseInput(filename: []const u8, allocator: std.mem.Allocator) !std.ArrayList(u64) {
    const file: std.fs.File = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const stat = try file.stat();
    const buffer = try file.readToEndAlloc(allocator, stat.size);
    defer allocator.free(buffer);

    var stoneList = std.ArrayList(u64).init(allocator);
    errdefer stoneList.deinit();

    var stones = std.mem.splitAny(u8, buffer, " \n");
    while (stones.next()) |stone| {
        if (!std.mem.eql(u8, stone, "")) {
            const stoneVal = try std.fmt.parseInt(u64, stone, 10);
            try stoneList.append(stoneVal);
        }
    }
    return stoneList;
}

fn countDigits(value: u64) u8 {
    var valueCopy = value;
    var digits: u8 = 0;
    while (valueCopy > 0) {
        valueCopy /= 10;
        digits += 1;
    }
    return digits;
}

const BlinkGraph = struct {
    const Self = @This();

    stone_counts: AutoArrayHashMap(u64, u64),

    pub fn init(allocator: Allocator) Self {
        return .{
            .stone_counts = AutoArrayHashMap(u64, u64).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.*.stone_counts.deinit();
    }

    pub fn addStone(self: *Self, stone: u64, count: u64) !void {
        const result = try self.*.stone_counts.getOrPut(stone);
        switch (result.found_existing) {
            true => {
                result.value_ptr.* += count;
            },
            false => {
                result.value_ptr.* = count;
            },
        }
    }

    pub fn countStones(self: Self) u64 {
        var count: u64 = 0;
        for (self.stone_counts.values()) |num| {
            count += num;
        }
        return count;
    }

    pub fn blink(self: *Self) !void {
        var old_stone_counts: AutoArrayHashMap(u64, u64) = try self.*.stone_counts.clone();
        defer old_stone_counts.deinit();

        self.*.stone_counts.clearRetainingCapacity();

        for (old_stone_counts.keys()) |stone| {
            const count = old_stone_counts.get(stone).?;

            if (stone == 0) {
                try self.*.addStone(1, count);
            } else {
                const digits: u8 = countDigits(stone);
                if (digits % 2 == 0) {
                    const leftHalf = stone / std.math.pow(u64, 10, digits / 2);
                    const rightHalf = stone % std.math.pow(u64, 10, digits / 2);

                    try self.*.addStone(leftHalf, count);
                    try self.*.addStone(rightHalf, count);
                } else {
                    try self.*.addStone(stone * 2024, count);
                }
            }
        }
    }
};

fn blinkRow(stonesIn: std.ArrayList(u64), stonesOut: *std.ArrayList(u64)) !void {
    for (stonesIn.items) |stone| {
        if (stone == 0) {
            try stonesOut.append(1);
        } else {
            const digits: u8 = countDigits(stone);
            if (digits % 2 == 0) {
                const leftHalf = stone / std.math.pow(u64, 10, digits / 2);
                const rightHalf = stone % std.math.pow(u64, 10, digits / 2);

                try stonesOut.append(leftHalf);
                try stonesOut.append(rightHalf);
            } else {
                try stonesOut.append(stone * 2024);
            }
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

    var inputStones = try parseInput("input.txt", allocator);
    defer inputStones.deinit();

    // part 1
    var fromBuffer1 = true;
    var stoneBuffer1 = try inputStones.clone();
    var stoneBuffer2 = std.ArrayList(u64).init(allocator);
    defer stoneBuffer1.deinit();
    defer stoneBuffer2.deinit();

    for (0..25) |step| {
        if (fromBuffer1) {
            stoneBuffer2.shrinkRetainingCapacity(0);
            try blinkRow(stoneBuffer1, &stoneBuffer2);

            std.debug.print("step {d}: {d}\n", .{ step + 1, stoneBuffer2.items.len });
        } else {
            stoneBuffer1.shrinkRetainingCapacity(0);
            try blinkRow(stoneBuffer2, &stoneBuffer1);

            std.debug.print("step {d}: {d}\n", .{ step + 1, stoneBuffer1.items.len });
        }
        fromBuffer1 = !fromBuffer1;
    }

    // part 2
    var blinkGraph = BlinkGraph.init(allocator);
    defer blinkGraph.deinit();

    for (inputStones.items) |stone| {
        try blinkGraph.addStone(stone, 1);
    }

    for (0..75) |step| {
        try blinkGraph.blink();
        std.debug.print("step {d} blink graph has {d} stones.\n", .{ step + 1, blinkGraph.countStones() });
    }
}
