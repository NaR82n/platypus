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
    "const":      CONST,
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
    "^":           XOR,
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

**CompositeLiteral/KeyValueExpr**
```yacc
key_value_expr: expr COLON expr
	;

composite_literal: composite_literal_start RIGHT_BRACE
    | composite_literal_start expr RIGHT_BRACE
    | composite_literal_start key_value_expr RIGHT_BRACE

    | empty_block

    | map_type empty_block

    | identifier empty_block

    | LEFT_BRACE expr RIGHT_BRACE
    | LEFT_BRACE key_value_expr RIGHT_BRACE

    | identifier LEFT_BRACE key_value_expr RIGHT_BRACE
    | identifier LEFT_BRACE expr RIGHT_BRACE

    | map_type LEFT_BRACE expr RIGHT_BRACE
    | map_type LEFT_BRACE key_value_expr RIGHT_BRACE
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


**AttrExpr/IndexExpr/SliceExpr/CallExpr**
```yacc
suffix_expr: 
    // index_expr
      identifier LEFT_BRACKET expr RIGHT_BRACKET
	| identifier LEFT_PAREN func_call_args RIGHT_PAREN
	| DOT LEFT_BRACKET expr RIGHT_BRACKET
    
	// slice_expr
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty RIGHT_BRACKET
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty COLON expr_or_empty RIGHT_BRACKET

	// attr_expr
	| identifier DOT identifier

	// call_expr
	| identifier LEFT_PAREN RIGHT_PAREN
	| identifier LEFT_PAREN func_call_args EOLS RIGHT_PAREN
	| identifier LEFT_PAREN EOLS RIGHT_PAREN

    // + index_expr
	| suffix_expr LEFT_BRACKET expr RIGHT_BRACKET // index_expr
	| suffix_expr LEFT_PAREN func_call_args RIGHT_PAREN

	// + slice_expr
	| suffix_expr LEFT_BRACKET expr COLON expr RIGHT_BRACKET
	| suffix_expr LEFT_BRACKET expr COLON expr COLON expr RIGHT_BRACKET

	// + attr_expr
	| suffix_expr DOT identifier

	// + call_expr
	| suffix_expr LEFT_PAREN RIGHT_PAREN
	| suffix_expr LEFT_PAREN func_call_args EOLS RIGHT_PAREN
	| suffix_expr LEFT_PAREN EOLS RIGHT_PAREN

	;
```

**BinaryExpr**
```yacc
binary_expr: expr GTE expr
    | expr GT expr
    | expr OR expr
    | expr AND expr
    | expr LT expr
    | expr LTE expr
    | expr NEQ expr
    | expr EQEQ expr
    | expr BitwiseAND expr
    | expr BitwiseXOR expr
    | expr BitwiseOR expr
    | expr ADD expr
    | expr SUB expr
    | expr MUL expr
    | expr DIV expr
    | expr MOD expr
```

**UnaryExpr**
```yacc
unary_expr: MUL expr
	| BitwiseAND expr
    | ADD expr %prec UMINUS
    | SUB expr %prec UMIUNS
    | NOT expr
    | BitwiseNOT expr
	;
```

## Stmt

**Stmt**
```yacc
sep : SEMICOLON
	| EOL
	| sep SEMICOLON
	| sep EOL
    ;

stmt: ifelse_stmt
	| for_in_stmt
	| for_stmt
	| continue_stmt
	| break_stmt
	| return_stmt
	| value_stmt
	| decl_stmt
	| assignment_stmt
	| stmt_block
	;

stmts_list: stmt sep
	| sep
	| stmts_list stmt sep
	;

stmts: stmts_list stmt
	| stmts_list
	| stmt
	;
```

**ValueStmt**
```yacc
value_stmt: expr
    ;
```

**AssignStmt**
```yacc
assignment_stmt: expr EQ expr
    ;
```

**ReturnStmt**
```yacc
return_stmt: RETURN
    ;
```

**BreakStmt**
```yacc
break_stmt: BREAK
    ;
```

**ContinueStmt**
```yacc
continue_stmt: CONTINUE
    ;
```

**ForInStmt**
```
stmt_block_with_empty: empty_block
	| stmt_block
	;

fot_in_stmt: FOT identifier IN expr stmt_block_with_empty
    ;
```

**ForStmt**
```yacc
/*
	for <expr>; <expr>; <expr>  stmt_block_with_empty
	111 (expr, expr, expr) -> 000 ( , , )
*/
for_stmt : FOR expr SEMICOLON expr SEMICOLON expr stmt_block_with_empty
	| FOR expr SEMICOLON expr SEMICOLON stmt_block_with_empty
	| FOR expr SEMICOLON SEMICOLON expr stmt_block_with_empty
	| FOR expr SEMICOLON SEMICOLON stmt_block_with_empty
	| FOR SEMICOLON expr SEMICOLON expr stmt_block_with_empty
	| FOR SEMICOLON expr SEMICOLON stmt_block_with_empty
	| FOR SEMICOLON SEMICOLON expr stmt_block_with_empty
	| FOR SEMICOLON SEMICOLON stmt_block_with_empty
	;
```

**IfStmt**
```yacc
if_stmt: if_flif_list
    | if_elif_list ELSE stmt_block_with_empty
    ;

if_elif_list: if_elem
    | if_elif_list elif_elem
    ;

if_elem: IF expr stmt_block_with_empty
    ;

elif_elem: ELIF expr stmt_block_with_empty
    ;
```


**BlockStmt**
```yacc
stmt_block: stmt_block_start RIGHT_BRACE
	;

stmt_block_start: LEFT_BRACE stmt sep
	| stmt_block_start stmt sep
	;
```

**DeclStmt**
```yacc
decl_stmt: value_decl
    | struct_decl
    | func_decl
    ;
```

## Decl

**ValueDecl**
```yacc
varb_or_const: LET | CONST
	;

value_decl: varb_or_const identifier
	| varb_or_const identifier EQ expr
	| varb_or_const identifier COLON all_type
	| varb_or_const identifier COLON all_type EQ expr
	| value_decl COMMA identifier
	| value_decl COMMA identifier EQ expr
	| value_decl COMMA identifier COLON all_type
	| value_decl COMMA identifier COLON all_type EQ expr
	;
```

**FuncDecl**
```yacc
fn_decl: fn_decl_start RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
	| fn_decl_start EOLS RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
	| fn_decl_start RIGHT_PAREN stmt_block_with_empty
	| fn_decl_start EOLS RIGHT_PAREN stmt_block_with_empty
    ;

fn_decl_start: FN identifier LEFT_PAREN identifier
	| FN identifier LEFT_PAREN identifier COLON all_type
	| FN identifier LEFT_PAREN identifier EQ expr
	| FN identifier LEFT_PAREN identifier COLON all_type EQ expr
    | fn_decl_start COMMA identifier
	| fn_decl_start COMMA identifier COLON all_type
	| fn_decl_start COMMA identifier EQ expr
	| fn_decl_start COMMA identifier COLON all_type EQ expr
    ;
```

**StructDecl**
```yacc
struct_decl: struct_decl_start RIGHT_BRACE
	| struct_decl_start identifier RIGHT_BRACE
	| struct_decl_start identifier COLON all_type RIGHT_BRACE
	| STRUCT identifier LEFT_BRACE identifier RIGHT_BRACE
	| STRUCT identifier LEFT_BRACE identifier COLON all_type RIGHT_BRACE
	| STRUCT identifier empty_block
	;

struct_decl_start : STRUCT identifier LEFT_BRACE identifier COMMA
	| STRUCT identifier LEFT_BRACE identifier COLON all_type COMMA
	| struct_decl_start identifier COMMA
	| struct_decl_start identifier COLON all_type COMMA
	;

```