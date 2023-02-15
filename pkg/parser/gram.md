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
    | point_type
    | LEFT_PAREN all_type RIGHT_PAREN
    ;

basic_type: INT | FLOAT | BOOL | NIL | STR
    ;

array_type: LEFT_BRACKET EQ RIGHT_BRACKET all_type
    | LEFT_BRACKET EQ NUMBER RIGHT_BRACKET all_type
    ;

map_type: MAP LEFT_BRACKET all_type RIGHT_BRACKET all_type
    ;

point_type: MUL all_type
    ;

func_arg: identifier
    | identifier EQ expr
    | identifier COLON all_type
    | identifier COLON all_type EQ expr
    ;

func_args: func_arg
    | func_args COMMA func_arg

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
array_literal:  

```


**CallExpr**:

```yacc
call_expr: identifier LEFT_PAREN call_args RIGTH_PAREN
    | identifier LEFT_PAREN RIGTH_PAREN
    ;
```

