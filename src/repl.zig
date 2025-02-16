pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var buffer: [512]u8 = undefined;
    while (true) {
        _ = try stdout.write("> ");

        const len = try stdin.read(buffer[0..]);
        if (len == 0) {
            break;
        }
        if (len >= buffer.len - 1) {
            try stdout.print("input too long: {} bytes\n", .{len});
            continue;
        }

        buffer[len] = 0;

        const trimmed = std.mem.trim(u8, buffer[0..len], &std.ascii.whitespace);
        if (trimmed.len == 0) {
            break;
        }

        var lexer = moonlet.lex.Lexer.init(buffer[0..len], "@repl");
        var parser = moonlet.parse.Parser.init(&lexer);
        const tree = parser.parse(allocator) catch |err| {
            try stdout.print("error: {s}\n", .{@errorName(err)});
            if (parser.error_context) |*ctx| {
                const rendered = try ctx.renderAlloc(allocator, &parser);
                defer allocator.free(rendered);
                try stdout.print("{s}\n", .{rendered});
            }
            continue;
        };
        defer tree.deinit();

        try tree.dump(stdout, buffer[0..len]);
    }
}

const std = @import("std");
const moonlet = @import("moonlet");
