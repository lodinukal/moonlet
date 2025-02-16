const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("lex.zig").Token;

pub const Tree = struct {
    node: *Node,

    /// not owned by the tree
    source: []const u8,

    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,

    pub fn deinit(self: *Tree) void {
        self.arena.promote(self.allocator).deinit();
    }

    pub fn chunk(self: *Tree) *Node.Chunk {
        return @alignCast(@fieldParentPtr("base", self.node));
    }

    pub fn dump(self: *Tree, writer: anytype, source: ?[]const u8) @TypeOf(writer).Error!void {
        try self.node.dump(writer, source, 0);
    }
};

pub const Node = struct {
    id: Id,

    pub const Id = enum {
        chunk,
        call,
        literal,
        identifier,
        assignment_statement,
        compound_assignment_statement,
        field_access,
        index_access,
        if_statement,
        if_clause,
        return_statement,
        while_statement,
        do_statement,
        repeat_statement,
        break_statement,
        for_statement_numeric,
        for_statement_generic,
        function_declaration,
        table_constructor,
        table_field,
        unary_expression,
        binary_expression,
        grouped_expression,

        // :moonlet changes
        type,

        pub fn Type(id: Id) type {
            return switch (id) {
                .chunk => Chunk,
                .call => Call,
                .literal => Literal,
                .identifier => Identifier,
                .assignment_statement => AssignmentStatement,
                .compound_assignment_statement => CompoundAssignmentStatement,
                .field_access => FieldAccess,
                .index_access => IndexAccess,
                .if_statement => IfStatement,
                .if_clause => IfClause,
                .return_statement => ReturnStatement,
                .while_statement => WhileStatement,
                .do_statement => DoStatement,
                .repeat_statement => RepeatStatement,
                .break_statement => BreakStatement,
                .for_statement_numeric => ForStatementNumeric,
                .for_statement_generic => ForStatementGeneric,
                .function_declaration => FunctionDeclaration,
                .table_constructor => TableConstructor,
                .table_field => TableField,
                .unary_expression => UnaryExpression,
                .binary_expression => BinaryExpression,
                .grouped_expression => GroupedExpression,
                // :moonlet changes
                .type => Node.Type,
            };
        }
    };

    pub fn cast(base: *Node, comptime id: Id) ?*id.Type() {
        if (base.id == id) {
            return @alignCast(@fieldParentPtr("base", base));
        }
        return null;
    }

    pub const Chunk = struct {
        base: Node = .{ .id = .chunk },
        body: []*Node,
    };

    pub const Call = struct {
        base: Node = .{ .id = .call },
        expression: *Node,
        arguments: []*Node,
        open_args_token: ?Token,
        close_args_token: ?Token,
        is_statement: bool = false,
    };

    pub const Literal = struct {
        base: Node = .{ .id = .literal },
        /// Can be one of .keyword_nil, .keyword_true, .keyword_false, .number, .string, or .name
        /// (.name is a special case that is only used for table constructor field keys)
        token: Token,
    };

    pub const Identifier = struct {
        base: Node = .{ .id = .identifier },
        token: Token,
    };

    pub const AssignmentStatement = struct {
        base: Node = .{ .id = .assignment_statement },
        variables: []*Node,
        values: []*Node,
        is_local: bool,
        annotation: ?*Node,
    };

    pub const CompoundAssignmentStatement = struct {
        base: Node = .{ .id = .compound_assignment_statement },
        variable: *Node,
        operator: Token,
        value: *Node,
    };

    pub const FieldAccess = struct {
        base: Node = .{ .id = .field_access },
        prefix: *Node,
        field: Token,
        separator: Token,
    };

    pub const IndexAccess = struct {
        base: Node = .{ .id = .index_access },
        prefix: *Node,
        index: *Node,
        open_token: Token,
        close_token: Token,
    };

    pub const IfStatement = struct {
        base: Node = .{ .id = .if_statement },
        clauses: []*Node,
    };

    /// if, elseif, or else
    pub const IfClause = struct {
        base: Node = .{ .id = .if_clause },
        if_token: Token,
        condition: ?*Node,
        body: []*Node,
    };

    pub const ReturnStatement = struct {
        base: Node = .{ .id = .return_statement },
        values: []*Node,
    };

    pub const WhileStatement = struct {
        base: Node = .{ .id = .while_statement },
        condition: *Node,
        body: []*Node,
    };

    pub const DoStatement = struct {
        base: Node = .{ .id = .do_statement },
        body: []*Node,
    };

    pub const RepeatStatement = struct {
        base: Node = .{ .id = .repeat_statement },
        body: []*Node,
        condition: *Node,
    };

    pub const BreakStatement = struct {
        base: Node = .{ .id = .break_statement },
        token: Token,
    };

    pub const ForStatementNumeric = struct {
        base: Node = .{ .id = .for_statement_numeric },
        name: Token,
        start: *Node,
        end: *Node,
        increment: ?*Node,
        body: []*Node,
    };

    pub const ForStatementGeneric = struct {
        base: Node = .{ .id = .for_statement_generic },
        names: []Token,
        expressions: []*Node,
        body: []*Node,
    };

    pub const FunctionDeclaration = struct {
        base: Node = .{ .id = .function_declaration },
        name: ?*Node, // null for anonymous functions
        parameters: []Token,
        body: []*Node,
        is_local: bool,
    };

    pub const TableConstructor = struct {
        base: Node = .{ .id = .table_constructor },
        fields: []*Node,
        open_token: Token,
        close_token: Token,
    };

    pub const TableField = struct {
        base: Node = .{ .id = .table_field },
        key: ?*Node,
        value: *Node,
    };

    pub const UnaryExpression = struct {
        base: Node = .{ .id = .unary_expression },
        operator: Token,
        argument: *Node,
    };

    pub const BinaryExpression = struct {
        base: Node = .{ .id = .binary_expression },
        operator: Token,
        left: *Node,
        right: *Node,
    };

    pub const GroupedExpression = struct {
        base: Node = .{ .id = .grouped_expression },
        open_token: Token,
        expression: *Node,
        close_token: Token,
    };

    // :moonlet changes
    pub const Type = struct {
        pub const Inner = union(enum) {
            // singleton nil
            nil,
            // null -> boolean
            // true -> singleton true
            // false -> singleton false
            boolean: ?bool,
            // null -> f64
            number: enum {
                i32,
                i64,
                f32,
                f64,
            },
            // false -> string
            // true -> singleton string
            string: bool,

            // (Type, Type, ...)
            tuple: struct {
                names: []?Token,
                types: []*Type,
            },

            // <generics> tuple -> tuple
            function: struct {
                /// a tuple
                params: *Type,
                /// a tuple
                returns: *Type,
            },

            /// types can be expressions (type functions)
            expression: *Node,
        };

        base: Node = .{ .id = .type },
        kind: Inner,
        start_token: Token,
    };

    /// Gets the last token of an expression
    /// Needed for detecting ambiguous function calls
    pub fn getLastToken(node: *const Node) Token {
        switch (node.id) {
            .identifier => {
                const casted: *const Node.Identifier = @alignCast(@fieldParentPtr("base", node));
                return casted.token;
            },
            .grouped_expression => {
                const casted: *const Node.GroupedExpression = @alignCast(@fieldParentPtr("base", node));
                return casted.close_token;
            },
            .field_access => {
                const casted: *const Node.FieldAccess = @alignCast(@fieldParentPtr("base", node));
                return casted.field;
            },
            .index_access => {
                const casted: *const Node.IndexAccess = @alignCast(@fieldParentPtr("base", node));
                return casted.close_token;
            },
            .call => {
                const casted: *const Node.Call = @alignCast(@fieldParentPtr("base", node));
                if (casted.close_args_token) |close_token| {
                    return close_token;
                } else {
                    return casted.arguments[casted.arguments.len - 1].getLastToken();
                }
            },
            .literal => {
                const casted: *const Node.Literal = @alignCast(@fieldParentPtr("base", node));
                return casted.token;
            },
            .table_constructor => {
                const casted: *const Node.TableConstructor = @alignCast(@fieldParentPtr("base", node));
                return casted.close_token;
            },
            else => {
                std.debug.print("{}\n", .{node});
                @panic("TODO");
            },
        }
    }

    pub fn dump(
        node: *const Node,
        writer: anytype,
        source: ?[]const u8,
        indent: usize,
    ) @TypeOf(writer).Error!void {
        try writer.writeByteNTimes(' ', indent);
        try writer.writeAll(@tagName(node.id));
        switch (node.id) {
            .chunk => {
                try writer.writeAll("\n");
                const chunk: *const Node.Chunk = @alignCast(@fieldParentPtr("base", node));
                for (chunk.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
            },
            .call => {
                try writer.writeAll("\n");
                const call: *const Node.Call = @alignCast(@fieldParentPtr("base", node));
                try call.expression.dump(writer, source, indent + 1);
                try writer.writeByteNTimes(' ', indent + 1);
                try writer.writeAll("(");
                if (call.arguments.len > 0) {
                    try writer.writeAll("\n");
                    for (call.arguments) |arg_node| {
                        try arg_node.dump(writer, source, indent + 2);
                    }
                    try writer.writeByteNTimes(' ', indent + 1);
                }
                try writer.writeAll(")\n");
            },
            .identifier => {
                const identifier: *const Node.Identifier = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                try writer.writeAll(identifier.token.display(source));
                try writer.writeAll("\n");
            },
            .literal => {
                const literal: *const Node.Literal = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                try writer.writeAll(literal.token.display(source));
                try writer.writeAll("\n");
            },
            .assignment_statement => {
                const assignment: *const Node.AssignmentStatement = @alignCast(@fieldParentPtr("base", node));
                if (assignment.is_local) {
                    try writer.writeAll(" local");
                }
                try writer.writeAll("\n");
                for (assignment.variables) |var_node| {
                    try var_node.dump(writer, source, indent + 1);
                }
                if (assignment.values.len > 0) {
                    if (assignment.is_local) {
                        if (assignment.annotation) |annotation| {
                            try writer.writeByteNTimes(' ', indent);
                            try writer.writeAll(":\n");
                            try annotation.dump(writer, source, indent + 1);
                            // try writer.writeAll("\n");
                        }
                    }
                    try writer.writeAll("\n");
                    try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll("=\n");
                    for (assignment.values) |value_node| {
                        try value_node.dump(writer, source, indent + 1);
                    }
                }
            },
            .compound_assignment_statement => {
                const compound_assignment: *const Node.CompoundAssignmentStatement = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                try compound_assignment.variable.dump(writer, source, indent + 1);
                try writer.writeAll(" ");
                try writer.writeAll(compound_assignment.operator.display(source));
                try writer.writeAll("\n");
                try compound_assignment.value.dump(writer, source, indent + 1);
            },
            .field_access => {
                const field_access: *const Node.FieldAccess = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                try writer.writeAll(field_access.separator.display(source));
                try writer.writeAll(field_access.field.display(source));
                try writer.writeAll("\n");
                try field_access.prefix.dump(writer, source, indent + 1);
            },
            .index_access => {
                const index_access: *const Node.IndexAccess = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                try index_access.prefix.dump(writer, source, indent + 1);
                try index_access.index.dump(writer, source, indent + 1);
            },
            .if_statement => {
                const if_statement: *const Node.IfStatement = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                for (if_statement.clauses) |clause| {
                    try clause.dump(writer, source, indent + 1);
                }
            },
            .if_clause => {
                const if_clause: *const Node.IfClause = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                try writer.writeAll(if_clause.if_token.display(source));
                try writer.writeAll("\n");
                if (if_clause.condition) |condition| {
                    try condition.dump(writer, source, indent + 1);
                    try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll("then\n");
                }
                for (if_clause.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
            },
            .return_statement => {
                const return_statement: *const Node.ReturnStatement = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                for (return_statement.values) |value_node| {
                    try value_node.dump(writer, source, indent + 1);
                }
            },
            .while_statement => {
                const while_statement: *const Node.WhileStatement = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                try while_statement.condition.dump(writer, source, indent + 1);
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll("do\n");
                for (while_statement.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
            },
            .do_statement => {
                const do_statement: *const Node.DoStatement = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                for (do_statement.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
            },
            .repeat_statement => {
                const repeat_statement: *const Node.RepeatStatement = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                for (repeat_statement.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll("until\n");
                try repeat_statement.condition.dump(writer, source, indent + 1);
            },
            .break_statement => {
                try writer.writeAll("\n");
            },
            .for_statement_numeric => {
                const for_statement: *const Node.ForStatementNumeric = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                try for_statement.start.dump(writer, source, indent + 1);
                try for_statement.end.dump(writer, source, indent + 1);
                if (for_statement.increment) |increment| {
                    try increment.dump(writer, source, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll("do\n");
                for (for_statement.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
            },
            .for_statement_generic => {
                const for_statement: *const Node.ForStatementGeneric = @alignCast(@fieldParentPtr("base", node));
                for (for_statement.names) |name_token| {
                    try writer.writeAll(" ");
                    try writer.writeAll(name_token.display(source));
                }
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll("in\n");
                for (for_statement.expressions) |exp_node| {
                    try exp_node.dump(writer, source, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll("do\n");
                for (for_statement.body) |body_node| {
                    try body_node.dump(writer, source, indent + 1);
                }
            },
            .function_declaration => {
                const func: *const Node.FunctionDeclaration = @alignCast(@fieldParentPtr("base", node));
                if (func.is_local) {
                    try writer.writeAll(" local");
                }
                try writer.writeAll("\n");
                if (func.name) |name| {
                    try name.dump(writer, source, indent + 1);
                }
                try writer.writeByteNTimes(' ', indent + 1);
                try writer.writeAll("(");
                for (func.parameters, 0..) |param, i| {
                    if (i != 0) try writer.writeAll(" ");
                    try writer.writeAll(param.display(source));
                }
                try writer.writeAll(")\n");
                for (func.body) |body_node| {
                    try body_node.dump(writer, source, indent + 2);
                }
            },
            .table_constructor => {
                const constructor: *const Node.TableConstructor = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                for (constructor.fields) |field| {
                    try field.dump(writer, source, indent + 1);
                }
            },
            .table_field => {
                const field: *const Node.TableField = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                if (field.key) |key| {
                    try key.dump(writer, source, indent + 1);
                    try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll("=\n");
                }
                try field.value.dump(writer, source, indent + 1);
            },
            .unary_expression => {
                const unary: *const Node.UnaryExpression = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                try writer.writeAll(unary.operator.display(source));
                try writer.writeAll("\n");
                try unary.argument.dump(writer, source, indent + 1);
            },
            .binary_expression => {
                const binary: *const Node.BinaryExpression = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                try writer.writeAll(binary.operator.display(source));
                try writer.writeAll("\n");
                try binary.left.dump(writer, source, indent + 1);
                try binary.right.dump(writer, source, indent + 1);
            },
            .grouped_expression => {
                const grouped: *const Node.GroupedExpression = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll("\n");
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(grouped.open_token.display(source));
                try writer.writeAll("\n");
                try grouped.expression.dump(writer, source, indent + 1);
                try writer.writeByteNTimes(' ', indent);
                try writer.writeAll(grouped.close_token.display(source));
                try writer.writeAll("\n");
            },
            // :moonlet changes
            .type => {
                const ty: *const Node.Type = @alignCast(@fieldParentPtr("base", node));
                try writer.writeAll(" ");
                // try writer.writeAll(ty.token.display(source));
                switch (ty.kind) {
                    .nil => try writer.writeAll("nil"),
                    .boolean => |boolean| {
                        if (boolean) |b| {
                            try writer.writeAll(if (b) "true" else "false");
                        } else {
                            try writer.writeAll("boolean");
                        }
                    },
                    .number => |number| {
                        try writer.writeAll("number");
                        switch (number) {
                            .i32 => try writer.writeAll(" i32"),
                            .i64 => try writer.writeAll(" i64"),
                            .f32 => try writer.writeAll(" f32"),
                            .f64 => try writer.writeAll(" f64"),
                        }
                    },
                    .string => |is_singleton| {
                        if (is_singleton) {
                            try writer.writeAll("string ");
                            try writer.writeAll(ty.start_token.display(source));
                        } else {
                            try writer.writeAll("string");
                        }
                    },
                    .tuple => |tuple| {
                        try writer.writeAll("(\n");
                        for (tuple.names, tuple.types, 0..) |name, typ, index| {
                            // try inner.base.dump(writer, source, indent + 1);
                            try writer.writeByteNTimes(' ', indent + 1);
                            if (name) |n| {
                                try writer.print("{s}", .{n.display(source)});
                            } else {
                                try writer.print("arg{}", .{index});
                            }
                            try writer.writeAll(": ");
                            try typ.base.dump(writer, source, 0);
                            if (index != tuple.names.len - 1) try writer.writeAll(", ");
                            try writer.writeAll("\n");
                        }
                        try writer.writeByteNTimes(' ', indent);
                        try writer.writeAll(")");
                    },
                    .function => |function| {
                        try writer.writeAll("function\n");
                        try function.params.base.dump(writer, source, indent + 1);
                        try writer.writeAll("\n");
                        try writer.writeByteNTimes(' ', indent + 1);
                        try writer.writeAll("->\n");
                        try function.returns.base.dump(writer, source, indent + 1);
                    },
                    .expression => |expression| {
                        try expression.dump(writer, source, 0);
                    },
                }
                // try writer.writeAll("\n");
            },
        }
    }
};
