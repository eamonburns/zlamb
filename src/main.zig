//! ,---@z
//!  W-W'

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

pub const std_options: std.Options = .{
    .log_scope_levels = &.{
        .{ .scope = .lined, .level = .err },
    },
};

const lined = @import("lined");
const LambdaWriter = @import("LambdaWriter.zig");

// ===== AST ===== //

const ExprKind = enum {
    variable,
    function,
    application,
};

const VariableName = struct {
    name: []const u8,
    /// 0 is a special value that means the name identifies a free variable
    id: usize,

    pub fn bound(name: []const u8, id: usize) VariableName {
        return .{
            .name = name,
            .id = id,
        };
    }

    pub fn free(name: []const u8) VariableName {
        return .bound(name, 0);
    }
};
// typedef struct {
//     const char *name;
//     size_t id;
// } Var_Name;

// Var_Name var_name_bound(const char *name, size_t id)
// {
//     Var_Name var = {
//         .name = name,
//         .id = id,
//     };
//     return var;
// }

// Var_Name var_name_free(const char *name)
// {
//     return var_name_bound(name, 0);
// }

const ExprFunction = struct {
    arg: VariableName,
    body: *Expr,
};

// typedef struct {
//     Var_Name arg;
//     Expr *body;
// } Expr_Fun;

// TODO: I can probably just use a union(enum)
const Expr = struct {
    kind: ExprKind,
    as: union {
        variable: VariableName,
        function: ExprFunction,
        application: struct {
            lhs: *Expr,
            rhs: *Expr,
        },
    },

    pub fn variable(gpa: Allocator, name: []const u8) *Expr {
        var expr = gpa.create(Expr) catch @panic("OOM");
        expr.kind = .variable;
        expr.as = .{ .variable = .free(name) };
        return expr;
    }

    pub fn function(gpa: Allocator, arg: []const u8, body: *Expr) *Expr {
        var expr = gpa.create(Expr) catch @panic("OOM");
        expr.kind = .function;
        expr.as = .{ .function = .{
            .arg = .free(arg),
            .body = body,
        } };
        return expr;
    }

    pub fn function_bound(gpa: Allocator, arg: VariableName, body: *Expr) *Expr {
        var expr = gpa.create(Expr) catch @panic("OOM");
        expr.kind = .function;
        expr.as = .{ .function = .{
            .arg = arg,
            .body = body,
        } };
        return expr;
    }

    pub fn application(gpa: Allocator, lhs: *Expr, rhs: *Expr) *Expr {
        var expr = gpa.create(Expr) catch @panic("OOM");
        expr.kind = .application;
        expr.as = .{ .application = .{
            .lhs = lhs,
            .rhs = rhs,
        } };
        return expr;
    }

    pub fn format(
        self: Expr,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.kind) {
            .variable => try writer.writeAll(self.as.variable.name),
            .function => try writer.print("\\{s}.{f}", .{ self.as.function.arg.name, self.as.function.body }),
            .application => {
                if (self.as.application.lhs.kind != .variable) {
                    try writer.print("({f})", .{self.as.application.lhs});
                } else {
                    try writer.print("{f}", .{self.as.application.lhs});
                }
                try writer.writeByte(' ');
                if (self.as.application.rhs.kind != .variable) {
                    try writer.print("({f})", .{self.as.application.rhs});
                } else {
                    try writer.print("{f}", .{self.as.application.rhs});
                }
            },
        }
    }
};

// struct Expr {
//     Expr_Kind kind;
//     union {
//         Var_Name var;
//         Expr_Fun fun;
//         struct {
//             Expr *lhs;
//             Expr *rhs;
//         } app;
//     } as;
// };

// Expr *var(const char *name)
// {
//     Expr *expr = malloc(sizeof(*expr));
//     assert(expr != NULL);
//     expr->kind = EXPR_VAR;
//     expr->as.var = var_name_free(name);
//     return expr;
// }

// Expr *fun(const char *arg, Expr *body)
// {
//     Expr *expr = malloc(sizeof(*expr));
//     assert(expr != NULL);
//     expr->kind = EXPR_FUN;
//     expr->as.fun.arg = var_name_free(arg);
//     expr->as.fun.body = body;
//     return expr;
// }

// Expr *fun_bound(Var_Name arg, Expr *body)
// {
//     Expr *expr = malloc(sizeof(*expr));
//     assert(expr != NULL);
//     expr->kind = EXPR_FUN;
//     expr->as.fun.arg = arg;
//     expr->as.fun.body = body;
//     return expr;
// }

// Expr *app(Expr *lhs, Expr *rhs)
// {
//     Expr *expr = malloc(sizeof(*expr));
//     assert(expr != NULL);
//     expr->kind = EXPR_APP;
//     expr->as.app.lhs = lhs;
//     expr->as.app.rhs = rhs;
//     return expr;
// }

// void expr_display(Expr *expr, String_Builder *sb)
// {
//     switch (expr->kind) {
//     case EXPR_VAR:
//         sb_appendf(sb, "%s", expr->as.var.name);
//         break;
//     case EXPR_FUN:
//         sb_appendf(sb, "\\%s.", expr->as.fun.arg.name);
//         expr_display(expr->as.fun.body, sb);
//         sb_appendf(sb, "");
//         break;
//     case EXPR_APP:
//         if (expr->as.app.lhs->kind != EXPR_VAR) sb_appendf(sb, "(");
//         expr_display(expr->as.app.lhs, sb);
//         if (expr->as.app.lhs->kind != EXPR_VAR) sb_appendf(sb, ")");
//         sb_appendf(sb, " ");
//         if (expr->as.app.rhs->kind != EXPR_VAR) sb_appendf(sb, "(");
//         expr_display(expr->as.app.rhs, sb);
//         if (expr->as.app.rhs->kind != EXPR_VAR) sb_appendf(sb, ")");
//         break;
//     default: unreachable;
//     }
// }

// ===== Evaluation ===== //

// TODO: Should these functions be associated with a namespace/struct?

fn replace(gpa: Allocator, arg: VariableName, body: *Expr, value: *Expr) *Expr {
    switch (body.kind) {
        .variable => if (std.mem.eql(u8, body.as.variable.name, arg.name) and body.as.variable.id == arg.id) {
            return value;
        } else {
            return body;
        },
        .function => return .function_bound(
            gpa,
            body.as.function.arg,
            replace(gpa, arg, body.as.function.body, value),
        ),
        .application => return .application(
            gpa,
            replace(gpa, arg, body.as.application.lhs, value),
            replace(gpa, arg, body.as.application.rhs, value),
        ),
    }
}

// Expr *replace(Var_Name arg, Expr *body, Expr *val)
// {
//     switch (body->kind) {
//     case EXPR_VAR:
//         if (strcmp(body->as.var.name, arg.name) == 0 && body->as.var.id == arg.id) {
//             return val;
//         } else {
//             return body;
//         }
//     case EXPR_FUN:
//         return fun_bound(
//             body->as.fun.arg,
//             replace(arg, body->as.fun.body, val));
//     case EXPR_APP:
//         return app(
//             replace(arg, body->as.app.lhs, val),
//             replace(arg, body->as.app.rhs, val));
//     default: unreachable;
//     }
// }

fn apply(gpa: Allocator, function: ExprFunction, value: *Expr) *Expr {
    return replace(gpa, function.arg, function.body, value);
}

// Expr *apply(Expr_Fun fun, Expr *val)
// {
//     return replace(fun.arg, fun.body, val);
// }

fn eval1(gpa: Allocator, expr: *Expr) *Expr {
    switch (expr.kind) {
        .variable => return expr,
        .function => {
            const body = eval1(gpa, expr.as.function.body);
            if (body != expr.as.function.body) { // Application occurred
                return .function_bound(gpa, expr.as.function.arg, body);
            }
            return expr;
        },
        .application => {
            if (expr.as.application.lhs.kind == .function) {
                return apply(gpa, expr.as.application.lhs.as.function, expr.as.application.rhs);
            }

            const lhs = eval1(gpa, expr.as.application.lhs);
            if (lhs != expr.as.application.lhs) { // Application occurred
                return .application(gpa, lhs, expr.as.application.rhs);
            }

            const rhs = eval1(gpa, expr.as.application.rhs);
            if (rhs != expr.as.application.rhs) { // Application occurred
                return .application(gpa, lhs, rhs);
            }

            return expr;
        },
    }
}

// Expr *eval1(Expr *expr)
// {
//     switch (expr->kind) {
//     case EXPR_VAR:
//         return expr;
//     case EXPR_FUN: {
//         Expr *body = eval1(expr->as.fun.body);
//         if (body != expr->as.fun.body) {
//             return fun_bound(expr->as.fun.arg, body);
//         }
//         return expr;
//     }
//     case EXPR_APP:
//         if (expr->as.app.lhs->kind == EXPR_FUN) {
//             return apply(expr->as.app.lhs->as.fun, expr->as.app.rhs);
//         }
//
//         Expr *lhs = eval1(expr->as.app.lhs);
//         if (lhs != expr->as.app.lhs) {
//             return app(lhs, expr->as.app.rhs);
//         }
//
//         Expr *rhs = eval1(expr->as.app.rhs);
//         if (rhs != expr->as.app.rhs) {
//             return app(lhs, rhs);
//         }
//
//         return expr;
//     default: unreachable;
//     }
// }

// NOTE: I don't need this, because I can just do std.debug.print("{f}\n", .{expr})
// void trace_expr(Expr *expr, String_Builder *sb)
// {
//     sb->count = 0;
//     expr_display(expr, sb);
//     sb_append_null(sb);
//     printf("%s\n", sb->items);
// }

fn bind_variable(body: *Expr, variable: VariableName) void {
    switch (body.kind) {
        .variable => if (std.mem.eql(u8, body.as.variable.name, variable.name)) {
            body.as.variable.id = variable.id;
        },
        .function => bind_variable(body.as.function.body, variable),
        .application => {
            bind_variable(body.as.application.lhs, variable);
            bind_variable(body.as.application.rhs, variable);
        },
    }
}

// void bind_var(Expr *body, Var_Name var)
// {
//     switch (body->kind) {
//     case EXPR_VAR: {
//         if (strcmp(body->as.var.name, var.name) == 0) {
//             body->as.var.id = var.id;
//         }
//     } break;
//     case EXPR_FUN: {
//         bind_var(body->as.fun.body, var);
//     } break;
//     case EXPR_APP: {
//         bind_var(body->as.app.lhs, var);
//         bind_var(body->as.app.rhs, var);
//     } break;
//     default: unreachable;
//     }
// }

var id_counter: usize = 1;
fn bind_variables(expr: *Expr) *Expr {
    switch (expr.kind) {
        .variable => return expr,
        .function => {
            assert(expr.as.function.arg.id == 0); // Argument must be a free variable
            expr.as.function.arg.id = id_counter;
            id_counter += 1;
            bind_variable(expr.as.function.body, expr.as.function.arg);
            _ = bind_variables(expr.as.function.body);
            return expr;
        },
        .application => {
            _ = bind_variables(expr.as.application.lhs);
            _ = bind_variables(expr.as.application.rhs);
            return expr;
        },
    }
}

// Expr *bind_vars(Expr *expr)
// {
//     static size_t id_counter = 1;
//     switch (expr->kind) {
//     case EXPR_VAR: return expr;
//     case EXPR_FUN: {
//         assert(expr->as.fun.arg.id == 0);
//         expr->as.fun.arg.id = id_counter++;
//         bind_var(expr->as.fun.body, expr->as.fun.arg);
//         bind_vars(expr->as.fun.body);
//         return expr;
//     } break;
//     case EXPR_APP: {
//         bind_vars(expr->as.app.lhs);
//         bind_vars(expr->as.app.rhs);
//         return expr;
//     } break;
//     default: unreachable;
//     }
// }

test "AST and evaluation" { // TODO: I should probably make this test more thorough
    const expectEqual = std.testing.expectEqual;
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();
    const gpa = arena.allocator();
    // ((\x.\y.x) then) else
    var expr: *Expr = .application(gpa, .application(gpa, .function(gpa, "x", .function(gpa, "y", .variable(gpa, "x"))), .variable(gpa, "then")), .variable(gpa, "else"));
    try expectEqual(ExprKind.application, expr.kind);
    expr = eval1(gpa, expr);
    try expectEqual(ExprKind.application, expr.kind);
    expr = eval1(gpa, expr);
    try expectEqual(ExprKind.variable, expr.kind);
}

// ===== Tokenization ===== //

const TokenKind = enum {
    invalid,
    end,
    // Opening parentheses '('
    oparen,
    // Closing parentheses ')'
    cparen,
    // Start of lambda '\'
    lambda,
    dot,
    colon,
    name,

    pub fn format(
        self: TokenKind,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("{t}", .{self});
    }
};

// typedef enum {
//     TOKEN_INVALID,
//     TOKEN_END,
//     TOKEN_OPAREN,
//     TOKEN_CPAREN,
//     TOKEN_LAMBDA,
//     TOKEN_DOT,
//     TOKEN_COLON,
//     TOKEN_NAME,
// } Token_Kind;

// const char *token_kind_display(Token_Kind kind)
// {
//     switch (kind) {
//     case TOKEN_INVALID: return "TOKEN_INVALID";
//     case TOKEN_END:     return "TOKEN_END";
//     case TOKEN_OPAREN:  return "TOKEN_OPAREN";
//     case TOKEN_CPAREN:  return "TOKEN_CPAREN";
//     case TOKEN_LAMBDA:  return "TOKEN_LAMBDA";
//     case TOKEN_DOT:     return "TOKEN_DOT";
//     case TOKEN_NAME:    return "TOKEN_NAME";
//     case TOKEN_COLON:   return "TOKEN_COLON";
//     default: unreachable);
//     }
// }

/// All fields are 0-indexed
const Cursor = struct {
    /// Position
    pos: usize,
    /// Beginning of line
    bol: usize,
    /// Row
    row: usize,
};

// typedef struct {
//     size_t pos, bol, row;
// } Cur;

const Lexer = struct {
    content: []const u8,
    file_path: ?[]const u8 = null,

    cursor: Cursor,

    /// Kind of current token
    token: TokenKind,
    /// Name of current token
    name: []const u8,
    /// Row of current token
    row: usize,
    /// Column of current token
    col: usize,

    gpa: Allocator,

    pub fn init(gpa: Allocator, file_path: ?[]const u8, content: []const u8) Lexer {
        return .{
            .content = content,
            .file_path = file_path,
            .gpa = gpa,

            .cursor = .{
                .pos = 0,
                .row = 0,
                .bol = 0,
            },

            .token = undefined,
            .name = undefined,
            .row = undefined,
            .col = undefined,
        };
    }

    pub fn printLoc(
        l: Lexer,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (l.file_path) |path| try writer.print("{s}:", .{path});
        try writer.print("{d}:{d}: ", .{ l.row, l.col });
    }

    pub fn currChar(l: *Lexer) ?u8 {
        if (l.cursor.pos >= l.content.len) return null;
        return l.content[l.cursor.pos];
    }

    pub fn nextChar(l: *Lexer) ?u8 {
        if (l.cursor.pos >= l.content.len) return null;
        const x = l.currChar();
        l.cursor.pos += 1;
        if (x == '\n') {
            l.cursor.row += 1;
            l.cursor.bol = l.cursor.pos;
        }
        return x;
    }

    pub fn next(l: *Lexer) bool {
        while (std.ascii.isWhitespace(l.currChar() orelse 0)) {
            _ = l.nextChar();
        }

        // Get location of start of current token, 1-indexed
        l.row = l.cursor.row + 1;
        l.col = l.cursor.pos - l.cursor.bol + 1;

        const x = l.nextChar() orelse {
            l.token = .end;
            return true;
        };

        switch (x) {
            '(' => {
                l.token = .oparen;
                return true;
            },
            ')' => {
                l.token = .cparen;
                return true;
            },
            '\\' => {
                l.token = .lambda;
                return true;
            },
            '.' => {
                l.token = .dot;
                return true;
            },
            ':' => {
                l.token = .colon;
                return true;
            },
            else => {}, // Handle other case below
        }

        if (std.ascii.isAlphanumeric(x)) {
            const start = l.cursor.pos - 1; // FIXME: I don't like the (pos - 1)
            l.token = .name;
            while (l.currChar()) |c| {
                if (!std.ascii.isAlphanumeric(c)) break;
                _ = l.nextChar();
            }
            l.name = l.content[start..l.cursor.pos]; // NOTE: I might want to duplicate this instead of slicing
            return true;
        }

        l.token = .invalid;
        std.debug.print("{f}error: Unknown token starting with '{c}'\n", .{ std.fmt.alt(l.*, .printLoc), x });
        return false;
    }

    pub fn peek(l: *Lexer) bool {
        const saved = l.cursor;
        const result = l.next();
        l.cursor = saved;
        return result;
    }

    pub fn expect(l: *Lexer, expected: TokenKind) bool {
        if (!l.next()) return false;
        if (l.token != expected) {
            std.debug.print("{f}error: Unexpected token '{f}'\n", .{ std.fmt.alt(l.*, .printLoc), l.token });
            return false;
        }
        return true;
    }
};

// typedef struct {
//     const char *content;
//     size_t count;
//     const char *file_path;
//
//     Cur cur;
//
//     Token_Kind token;
//     String_Builder name;
//     size_t row, col;
// } Lexer;

// void lexer_print_loc(Lexer *l, FILE *stream)
// {
//     if (l->file_path) fprintf(stream, "%s:", l->file_path);
//     fprintf(stream, "%zu:%zu: ", l->row, l->col);
// }

// char lexer_curr_char(Lexer *l)
// {
//     if (l->cur.pos >= l->count) return 0;
//     return l->content[l->cur.pos];
// }

// char lexer_next_char(Lexer *l)
// {
//     if (l->cur.pos >= l->count) return 0;
//     char x = l->content[l->cur.pos++];
//     if (x == '\n') {
//         l->cur.row += 1;
//         l->cur.bol = l->cur.pos;
//     }
//     return x;
// }

// bool lexer_next(Lexer *l)
// {
//     while (isspace(lexer_curr_char(l))) {
//         lexer_next_char(l);
//     }
//
//     l->row = l->cur.row + 1;
//     l->col = l->cur.pos - l->cur.bol + 1;
//
//     char x = lexer_next_char(l);
//     if (x == '\0') {
//         l->token = TOKEN_END;
//         return true;
//     }
//
//     switch (x) {
//     case '(':  l->token = TOKEN_OPAREN; return true;
//     case ')':  l->token = TOKEN_CPAREN; return true;
//     case '\\': l->token = TOKEN_LAMBDA; return true;
//     case '.':  l->token = TOKEN_DOT;    return true;
//     case ':':  l->token = TOKEN_COLON;  return true;
//     }
//
//     if (isalnum(x)) {
//         l->token = TOKEN_NAME;
//         l->name.count = 0;
//         da_append(&l->name, x);
//         while (isalnum(lexer_curr_char(l))) {
//             x = lexer_next_char(l);
//             da_append(&l->name, x);
//         }
//         sb_append_null(&l->name);
//         return true;
//     }
//
//     l->token = TOKEN_INVALID;
//     lexer_print_loc(l, stderr);
//     fprintf(stderr, "ERROR: Unknown token starts with `%c`\n", x);
//     return false;
// }

// bool lexer_peek(Lexer *l)
// {
//     Cur saved = l->cur;
//     bool result = lexer_next(l);
//     l->cur = saved;
//     return result;
// }

// bool lexer_expect(Lexer *l, Token_Kind expected)
// {
//     if (!lexer_next(l)) return false;
//     if (l->token != expected) {
//         lexer_print_loc(l, stderr);
//         fprintf(stderr, "ERROR: Unexpected token %s\n", token_kind_display(l->token));
//         return false;
//     }
//     return true;
// }

test "tokenization" {
    const expect = std.testing.expect;
    const expectEqualSlices = std.testing.expectEqualSlices;
    const gpa = std.testing.allocator;
    const content =
        \\:(\x. \y.x) then else"
    ;
    var l: Lexer = .init(gpa, null, content);

    try expect(l.expect(.colon)); //- :
    try expect(l.expect(.oparen)); // (
    try expect(l.expect(.lambda)); // \
    try expect(l.expect(.name)); //-- x
    try expectEqualSlices(u8, "x", l.name);
    try expect(l.expect(.dot)); //--- .
    try expect(l.expect(.lambda)); // \
    try expect(l.expect(.name)); //-- y
    try expectEqualSlices(u8, "y", l.name);
    try expect(l.expect(.dot)); //--- .
    try expect(l.expect(.name)); //-- x
    try expectEqualSlices(u8, "x", l.name);
    try expect(l.expect(.cparen)); // )
    try expect(l.expect(.name)); //-- then
    try expectEqualSlices(u8, "then", l.name);
    try expect(l.expect(.name)); //-- else
    try expectEqualSlices(u8, "else", l.name);
}

// TODO: ===== Parsing ===== //

// Expr *parse_expr(Lexer *l);

fn parse_function(gpa: Allocator, l: *Lexer) ?*Expr {
    if (!l.expect(.name)) return null;
    const arg = gpa.dupe(u8, l.name) catch @panic("OOM");
    if (!l.expect(.dot)) {
        gpa.free(arg);
        return null;
    }

    var a: TokenKind = undefined;
    var b: TokenKind = undefined;
    {
        const saved = l.cursor;
        defer l.cursor = saved;

        if (!l.next()) return null;
        a = l.token;
        if (!l.next()) return null;
        b = l.token;
    }

    const body: ?*Expr = if (a == .name and b == .dot)
        parse_function(gpa, l)
    else
        parse_expr(gpa, l);

    return .function(gpa, arg, body orelse return null);
}

// Expr *parse_fun(Lexer *l)
// {
//     if (!lexer_expect(l, TOKEN_NAME)) return NULL;
//     const char *arg = copy_string(l->name.items);
//     if (!lexer_expect(l, TOKEN_DOT)) return NULL;
//
//     Token_Kind a, b;
//     Cur saved = l->cur; {
//         if (!lexer_next(l)) return NULL;
//         a = l->token;
//         if (!lexer_next(l)) return NULL;
//         b = l->token;
//     } l->cur = saved;
//
//     Expr *body;
//     if (a == TOKEN_NAME && b == TOKEN_DOT) {
//         body = parse_fun(l);
//     } else {
//         body = parse_expr(l);
//     }
//     if (!body) return NULL;
//     return fun(arg, body);
// }

fn parse_primary(gpa: Allocator, l: *Lexer) ?*Expr {
    if (!l.next()) return null;
    switch (l.token) {
        .oparen => {
            const expr = parse_expr(gpa, l) orelse return null;
            if (!l.expect(.cparen)) return null;
            return expr;
        },
        .lambda => return parse_function(gpa, l),
        .name => return .variable(gpa, gpa.dupe(u8, l.name) catch @panic("OOM")),
        else => {
            std.debug.print("{f}error: unexpected token '{f}'\n", .{ std.fmt.alt(l.*, .printLoc), l.token });
            return null;
        },
    }
}

// Expr *parse_primary(Lexer *l)
// {
//     if (!lexer_next(l)) return NULL;
//     switch (l->token) {
//     case TOKEN_OPAREN: {
//         Expr *expr = parse_expr(l);
//         if (!expr) return NULL;
//         if (!lexer_expect(l, TOKEN_CPAREN)) return NULL;
//         return expr;
//     }
//     case TOKEN_LAMBDA: return parse_fun(l);
//     case TOKEN_NAME: return var(copy_string(l->name.items));
//     default:
//         lexer_print_loc(l, stderr);
//         fprintf(stderr, "ERROR: Unexpected token %s\n", token_kind_display(l->token));
//         return NULL;
//     }
// }

fn parse_expr(gpa: Allocator, l: *Lexer) ?*Expr {
    var lhs = parse_primary(gpa, l) orelse return null;
    if (!l.peek()) return null;
    while (l.token != .cparen and l.token != .end) {
        const rhs = parse_primary(gpa, l) orelse return null;
        lhs = .application(gpa, lhs, rhs);
        if (!l.peek()) return null;
    }
    return lhs;
}

const Command = struct {
    name: []const u8,
    signature: []const u8,
    description: []const u8,
};

const Commands = struct {
    commands: std.ArrayList(Command) = .empty,

    pub const init: Commands = .{};

    pub fn deinit(self: *Commands, gpa: Allocator) void {
        self.commands.deinit(gpa);
    }

    pub fn addAndMatch(self: *Commands, gpa: Allocator, input: []const u8, name: []const u8, signature: []const u8, description: []const u8) !bool {
        const command: Command = .{
            .name = name,
            .signature = signature,
            .description = description,
        };

        try self.commands.append(gpa, command);
        return std.mem.eql(u8, input, name);
    }

    pub fn format(
        self: Commands,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll("Available commands:\n");
        var max_name_width: usize = 0;
        var max_sig_width: usize = 0;
        for (self.commands.items) |command| {
            max_name_width = @max(max_name_width, command.name.len);
            max_sig_width = @max(max_sig_width, command.signature.len);
        }

        for (self.commands.items) |command| {
            try writer.print("  :{[n]s: <[nw]} {[s]s: <[sw]} - {[d]s}\n", .{
                .n = command.name,
                .nw = max_name_width,
                .s = command.signature,
                .sw = max_sig_width,
                .d = command.description,
            });
        }
    }
};

// int main(void)
// {
//     static char buffer[1024];
//     static String_Builder sb = {0};
//     static Commands commands = {0};
//
//     size_t limit = 10;
//     printf(",---@.\n");
//     printf(" W-W'\n");
//     printf("Enter :help for more info\n");
//     for (;;) {
//         printf("λ> ");
//         fflush(stdout);
//         if (!fgets(buffer, sizeof(buffer), stdin)) break;
//         const char *source = buffer;
//         Lexer l = {
//             .content = source,
//             .count = strlen(source),
//         };
//
//         if (!lexer_peek(&l)) continue;
//         if (l.token == TOKEN_COLON) {
//             if (!lexer_next(&l)) continue;
//             if (!lexer_expect(&l, TOKEN_NAME)) continue;
//             commands.count = 0;
//             if (command(&commands, l.name.items, "limit", "[number]", "change evaluation limit (0 for no limit)")) {
//                 if (!lexer_peek(&l)) continue;
//                 switch (l.token) {
//                 case TOKEN_NAME:
//                     if (!lexer_expect(&l, TOKEN_NAME)) continue;
//                     limit = strtoul(l.name.items, NULL, 10);
//                     if (limit) {
//                         printf("Setting evaluation limit to %zu\n", limit);
//                     } else {
//                         printf("Evaluation limit is disabled\n");
//                     }
//                     continue;
//                 case TOKEN_END:
//                     if (limit) {
//                         printf("Evaluation limit is %zu\n", limit);
//                     } else {
//                         printf("Evaluation limit is disabled\n");
//                     }
//                     continue;
//                 default:
//                     lexer_print_loc(&l, stderr);
//                     fprintf(stderr, "ERROR: Unexpected token %s\n", token_kind_display(l.token));
//                     continue;
//                 }
//             }
//             if (command(&commands, l.name.items, "quit", "", "quit the REPL")) break;
//             if (command(&commands, l.name.items, "help", "", "print this help message")) {
//                 print_available_commands(&commands);
//                 continue;
//             }
//             print_available_commands(&commands);
//             printf("ERROR: unknown command `%s`\n", l.name.items);
//             continue;
//         }
//
//         Expr *expr = parse_expr(&l);
//         if (!expr) continue;
//         bind_vars(expr);
//
//         trace_expr(expr, &sb);
//         Expr *expr1 = eval1(expr);
//         for (size_t i = 1; (limit == 0 || i < limit) && expr1 != expr; ++i) {
//             expr = expr1;
//             trace_expr(expr, &sb);
//             expr1 = eval1(expr);
//         }
//         if (expr1 != expr) {
//             printf("...\n");
//         }
//     }
//
//     return 0;
// }

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer assert(debug_allocator.deinit() == .ok);
    const gpa = debug_allocator.allocator();

    var stdin_buf: [1024]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&stdin_buf);
    var stderr_buf: [1024]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&stderr_buf);
    var lambda_buf: [1024]u8 = undefined;
    var lambda_writer: LambdaWriter = .init(&stderr.interface, &lambda_buf);

    const input = &stdin.interface;
    const output = &lambda_writer.interface;

    var commands: Commands = .init;
    defer commands.deinit(gpa);

    var limit: u32 = 10;

    var expr_arena: std.heap.ArenaAllocator = .init(gpa);
    defer expr_arena.deinit();
    const expr_gpa = expr_arena.allocator();
    while (true) {
        std.debug.print("λ> ", .{});
        const line = try lined.editLine(gpa, input, output);
        defer gpa.free(line);

        if (line.len == 0) break;

        defer _ = expr_arena.reset(.retain_capacity);
        var l: Lexer = .init(gpa, null, line);

        if (!l.peek()) continue;
        if (l.token == .end) continue;
        if (l.token == .colon) {
            if (!l.next()) continue;
            if (!l.expect(.name)) continue;
            // Commands
            commands.commands.clearRetainingCapacity();
            if (try commands.addAndMatch(gpa, l.name, "limit", "[number]", "change evaluation limit (0 for no limit; if number is omitted, print current limit)")) {
                if (!l.peek()) continue;
                switch (l.token) {
                    .name => {
                        if (!l.expect(.name)) continue;
                        limit = try std.fmt.parseInt(u32, l.name, 10);
                        if (limit != 0) {
                            std.debug.print("Set evaluation limit to {d}\n", .{limit});
                        } else {
                            std.debug.print("Disabled evaluation limit\n", .{});
                        }
                        continue;
                    },
                    .end => {
                        if (limit != 0) {
                            std.debug.print("Evaluation limit is {d}\n", .{limit});
                        } else {
                            std.debug.print("Evaluation limit is disabled\n", .{});
                        }
                        continue;
                    },
                    else => {
                        std.debug.print("{f}error: unexpected token '{f}'\n", .{ std.fmt.alt(l, .printLoc), l.token });
                        continue;
                    },
                }
            }
            if (try commands.addAndMatch(gpa, l.name, "quit", "", "quit the REPL")) break;
            if (try commands.addAndMatch(gpa, l.name, "help", "", "print this help message")) {
                std.debug.print("{f}\n", .{commands});
                continue;
            }
            std.debug.print("{f}\n", .{commands});
            std.debug.print("error: unknown command '{s}'\n", .{l.name});
            continue;
        }

        var expr = parse_expr(expr_gpa, &l) orelse {
            std.debug.print("-> <null>\n", .{});
            continue;
        };
        _ = bind_variables(expr);
        std.debug.print("-> {f}\n", .{expr});

        var i: usize = 0;
        var expr1 = eval1(expr_gpa, expr);
        while ((limit == 0 or i < limit) and expr1 != expr) : (i += 1) {
            expr = expr1;
            std.debug.print(" > {f}\n", .{expr});
            expr1 = eval1(expr_gpa, expr);
        }
        if (expr1 != expr) {
            std.debug.print("...\n", .{});
        }
    }
    std.debug.print("\n", .{});
}

// Copyright 2025 Alexey Kutepov <reximkut@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
