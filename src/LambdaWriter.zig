//! A writer that replaces all occurrences of `\` with `λ`, before
//! writing the stream to `backing_writer`.

const std = @import("std");
const assert = std.debug.assert;

const LAMBDA = "λ";

const LambdaWriter = @This();

interface: std.Io.Writer,
backing_writer: *std.Io.Writer,

// When `flush` is called, `backing_writer.flush` will also be called.
//
// Asserts `buffer.len` is non-zero
pub fn init(backing_writer: *std.Io.Writer, buffer: []u8) LambdaWriter {
    assert(buffer.len > 0);
    return .{
        .backing_writer = backing_writer,
        .interface = .{
            .buffer = buffer,
            .vtable = &.{
                .drain = drain,
                .flush = flush,
            },
        },
    };
}

fn drain(w: *std.Io.Writer, data: []const []const u8, _: usize) std.Io.Writer.Error!usize {
    const lambda: *LambdaWriter = @fieldParentPtr("interface", w);
    const buf = w.buffered(); // NOTE: This will not work if the buffer length is 0
    w.end = 0;

    var unwritten_idx: usize = 0;
    while (unwritten_idx < buf.len) {
        const backslash_idx = std.mem.indexOfScalar(u8, buf[unwritten_idx..], '\\') orelse {
            // No remaining backslashes
            try lambda.backing_writer.writeAll(buf[unwritten_idx..buf.len]);
            break;
        };

        try lambda.backing_writer.writeAll(buf[unwritten_idx..backslash_idx]);
        try lambda.backing_writer.writeAll(LAMBDA);
        unwritten_idx = backslash_idx + 1;
    }

    const len = @min(w.buffer.len, data[0].len);
    @memcpy(w.buffer[0..len], data[0]);
    w.end = len;

    return len;
}

fn flush(w: *std.Io.Writer) std.Io.Writer.Error!void {
    try w.defaultFlush();

    const lambda: *LambdaWriter = @fieldParentPtr("interface", w);
    try lambda.backing_writer.flush();
}

pub fn main() !void {
    var stdout_buf: [1024]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&stdout_buf);
    var lambda_buf: [1024]u8 = undefined;
    var lambda: LambdaWriter = .init(&stdout.interface, &lambda_buf);

    try lambda.interface.writeAll(
        \\01234\6789
        \\
    );
    try lambda.interface.flush();
}
