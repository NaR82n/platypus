# Platypus

## Token
```json
{
    "if":         IF,
    "elif":       ELIF,
    "else":       ELSE,
    "false":      FALSE,
    "identifier": IDENTIFIER,
    "nil":        NIL,
    // "null":       NULL,
    "true":       TRUE,
    "for":        FOR,
    "in":         IN,
    "while":      WHILE,
    "break":      BREAK,
    "continue":   CONTINUE,
    "return":     RETURN,
    "str":        STR,
    "bool":       BOOL,
    "int":        INT,
    "float":      FLOAT,
    "list":       LIST,
    "map":        MAP,
    "struct":     STRUCT,
    "any":        ANY,
    "let":        LET,
    "fn":         FN,
    "->":         RET_SYMB,
    "(":          LEFT_PAREN,
    ")":          RIGHT_PAREN,
    "[":          LEFT_BRACKET,
    "]":          RIGHT_BRACKET,
    "{":          LEFT_BRACE,
    "}":          RIGHT_BRACE,
    ",":          COMMA,
    "=":          EQ,
    "==":         EQEQ,
    ";":          SEMICOLON,
    ".":          DOT,
    "<space>":    SPACE,
    ":":          COLON,
    "-":          SUB,
    "+":          ADD,
    "*":          MUL,
    "%":          MOD,
    "/":          DIV,
    "!=":         NEQ,
    "<=":         LTE,
    "<":          LT,
    ">=":         GTE,
    ">":          GT,
    "&":          BitwiseAND,
    // "^":        XOR,
    "&&":         AND,
    "||":         OR
}
```

## Expr

**Expr**
```yacc
expr: identifier | call_expr
    ;
```

**Identifier**:
```yacc
identifier: QUOTED_STRING
    ;
```

**Type**
```yacc
all_type: basic_type
    | array_type
    | map_type
    | identifier
    | ptr_type
    | LEFT_PAREN all_type RIGHT_PAREN
    ;

basic_type: INT | FLOAT | BOOL | NIL | STR
    ;

array_type: LEFT_BRACKET EQ RIGHT_BRACKET all_type
    | LEFT_BRACKET EQ NUMBER RIGHT_BRACKET all_type
    ;

map_type: MAP LEFT_BRACKET all_type RIGHT_BRACKET all_type
    ;

ptr_type: MUL all_type
    ;

func_arg: identifier
    | identifier EQ expr
    | identifier COLON all_type
    | identifier COLON all_type EQ expr
    ;

func_args: func_arg
    | func_args COMMA func_arg
    ;

func_type: FN LEFT_PAREN func_args RIGHT_PAREN RET_SYMB all_type
    | FN LEFT_PAREN RIGHT_PAREN RET_SYMB all_type
    | FN LEFT_PAREN func_args RIGHT_PAREN
    | FN LEFT_PAREN RIGHT_PAREN
    ;
```

**BasicLiteral**:
```yacc
basic_literal: NUMBER
    | STRING
    | MULTILINE_STRING
    | TRUE
    | FALSE
    | NIL
    ;
```

**ArrayLiteral**:
```yacc
array_literal: array_literal_start EOLS RIGHT_BRACKET
    | array_literal_start RIGHT_BRACKET
    | array_literal_start COMMA RIGTH_BRACKET
    | LEFT_BRACKET RIGHT_BRACKET
    | array_type LEFT_BRACKET RIGHT_BRACKET
    ;

array_literal_start: LEFT_BRACKET expr
    | array_type LEFT_BRACKET expr
    | array_literal_start COMMA expr
    ;
```

**CompositeLiteral**
```yacc
key_value_expr: expr COLON expr
	;

composite_literal: composite_literal_start RIGHT_BRACE
    | composite_literal_start expr RIGHT_BRACE
    | composite_literal_start RIGHT_BRACE
    | empty_block
    | map_type empty_block
    | identifier empty_block
    ;

composite_literal_start: LEFT_BRACE key_value_expr COMMA
    | LEFT_BRACE expr COMMA
    | map_type LEFT_BRACE key_value_expr COMMA
    | map_type LEF_BRACE expr COMMA
    | identifier LEFT_BRACE key_value_expr COMMA
    | identifer LEFT_BRACE expr COMMA
    | composite_literal_start key_value_expr COMMA
    | composite_literal_start expr COMMA
    ;
```


**ParenExpr**
```yacc
paren_expr: LEFT_PRAEN expr RIGHT_PAREN
    | LEFT_PRAEN expr EOLS RIGHT_PRAREN
    ;
```

**AttrExpr**
```yacc
attr_expr: identifier DOT index_expr
    | identifier DOT identifier
    | identifier DOT call_expr
    | index_expr DOT index_expr
    | index_expr DOT identifier
    | index_expr DOT call_expr
    | attr_expr DOT index_expr
    | attr_expr DOT identifier
    | attr_expr DOT call_expr
    ;
```

**IndexExpr/IndexListExpr**
```yacc
index_expr: identifier LEFT_BRACKET expr RIGHT_BRACKET
    // Only for function "json"
    | DOT LEFT_BRACKET expr RIGHT_BRACKET
    | index_expr LEFT_BRACKET expr RIGHT_BRACKET
    ;
```

**SliceExpr**
```yacc
slice_expr: idenfier LEFT_BRACKET expr COLON expr RIGHT_BRACKET
    | identifer LEFT_BRACKET expr COLON expr COLON expr RIGTH_BRACKET
    ;
```

**CallExpr**:

```yacc
call_expr: identifier LEFT_PAREN call_args RIGTH_PAREN
    | identifier LEFT_PAREN RIGTH_PAREN
    ;
```

