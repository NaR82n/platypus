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

## Op
```yacc
%right EQ
%left OR
%left AND
%left EQEQ NEQ
%left GTE GT LTE LT
%right BitwiseOR
%right BitwiseXOR
%right BitwiseAND
%left ADD SUB
%left MUL DIV MOD
%right BitwiseNOT NOT UMINUS // ADD SUB MUL BitwiseAND
%left LEFT_BRACKET RIGHT_BRACKET LEFT_PAREN RIGHT_PAREN DOT
```

## Stmts
```yacc
sep : SEMICOLON
	| EOL
	| sep SEMICOLON
	| sep EOL

start	: START_STMTS stmts
	| start EOF
	| error
	;

stmts: stmts_list stmt
	| stmts_list
	| stmt
	;

stmts_list: stmt sep
	| sep
	| stmts_list stmt sep
	;
```


## Stmt

**Stmt**
```yacc
stmt: ifelse_stmt
	| for_in_stmt
	| for_stmt
	| continue_stmt
	| break_stmt
	| return_stmt
	| value_stmt
	| assignment_stmt
	| stmt_block
	| decl_stmt
	;
```

**StmtBlock**
```yacc
stmt_block_with_empty: empty_block
	| stmt_block
	;

stmt_block: stmt_block_start RIGHT_BRACE
		{ $$ = &ast.Node{} }
	;

stmt_block_start: LEFT_BRACE stmt sep
		{ $$ = &ast.Node{} }
	| stmt_block_start stmt sep
		{ $$ = &ast.Node{} }
	;
```

**IfStmt**
```yacc
ifelse_stmt: if_elif_list
	| if_elif_list ELSE stmt_block_with_empty
	;

if_elem: IF expr stmt_block_with_empty
	;

if_elif_list: if_elem
	| if_elif_list elif_elem
	;

elif_elem: ELIF expr stmt_block_with_empty
	;
```


**ForStmt**
```yacc
/*
	for <expr>; <expr>; <expr>  block_smt
	111 (expr, expr, expr) -> 000 ( , , )
*/
for_stmt: FOR expr SEMICOLON expr SEMICOLON expr stmt_block_with_empty
	| FOR expr SEMICOLON expr SEMICOLON stmt_block_with_empty
	| FOR expr SEMICOLON SEMICOLON expr stmt_block_with_empty
	| FOR expr SEMICOLON SEMICOLON stmt_block_with_empty
	| FOR SEMICOLON expr SEMICOLON expr stmt_block_with_empty
	| FOR SEMICOLON expr SEMICOLON stmt_block_with_empty
	| FOR SEMICOLON SEMICOLON expr stmt_block_with_empty
	| FOR SEMICOLON SEMICOLON stmt_block_with_empty
	;
```

**ForInStmt**
```
fot_in_stmt: FOT identifier IN expr stmt_block_with_empty
    ;
```

**ContinueStmt**
```yacc
continue_stmt: CONTINUE
    ;
```

**BreakStmt**
```yacc
break_stmt: BREAK
    ;
```

**ReturnStmt**
```yacc
return_stmt: RETURN
    ;
```

**DeclStmt**
```yacc
decl_stmt: fn_decl
	| value_decl
	| struct_decl
	;
```

**AssignStmt**
```yacc
assignment_stmt: expr EQ expr
    ;
```

**ValueStmt**
```yacc
value_stmt: expr
    ;
```

## Decl

**FuncDecl**
```yacc
fn_decl: FN identifier LEFT_PAREN func_args RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
    | FN identifier LEFT_PAREN RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty

    | FN identifier LEFT_PAREN func_args RIGHT_PAREN stmt_block_with_empty
    | FN identifier LEFT_PAREN RIGHT_PAREN stmt_block_with_empty
    ;


func_arg: identifier
    | identifier EQ expr
    | identifier COLON all_type
    | identifier COLON all_type EQ expr
    ;

func_args_start: func_arg COMMA
    | func_args_start func_arg COMMA
	;

func_args: func_args_start
    | func_args_start func_arg
	| func_arg
	;
```

**ValueDecl**
```yacc
value_decl: varb_or_const value_decl_elem
		{ $$ = &ast.Node{} }
	| value_decl COMMA value_decl_elem
		{ $$ = &ast.Node{} }
	;

varb_or_const: LET | CONST
	;

value_decl_elem: identifier
	| identifier EQ expr
	| identifier COLON all_type EQ expr
	| identifier COLON all_type
	;
```

**StructDecl**
```yacc
struct_decl: STRUCT identifier LEFT_BRACE struct_decl_elems RIGHT_BRACE
	| STRUCT identifier empty_block
	;

struct_decl_elem: identifier
	| identifier COLON all_type
	;

struct_decl_elems_start: struct_decl_elem COMMA
	| struct_decl_elems_start struct_decl_elem COMMA
	;

struct_decl_elems: struct_decl_elems_start
	| struct_decl_elems_start struct_decl_elem
	| struct_decl_elem
	;
```

## Expr

**Expr**
```yacc
expr: identifier 
	| basic_literal
	| array_literal
	| composite_literal
	// | conv_expr 
	| paren_expr
	| suffix_expr
	| unary_expr 
	| binary_expr ;
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
	| pointer_type
	| identifier
	| fn_type
	| LEFT_PAREN all_type RIGHT_PAREN
	;

pointer_type: MUL all_type
	;

basic_type: INT 
	| FLOAT
	| BOOL
	| STR
	| ANY
	;

array_type: LEFT_BRACKET EQ RIGHT_BRACKET all_type
	| LEFT_BRACKET EQ NUMBER RIGHT_BRACKET all_type
	;

map_type : MAP LEFT_BRACKET all_type RIGHT_BRACKET all_type
	;

fn_type: FN LEFT_PAREN func_args RIGHT_PAREN RET_SYMB all_type
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
array_literal: LEFT_BRACKET RIGHT_BRACKET
	| array_type LEFT_BRACKET RIGHT_BRACKET
    | array_literal_start RIGHT_BRACKET
	;

array_literal_start: LEFT_BRACKET expr
	| array_type LEFT_BRACKET expr
    
    | array_elem_start
	
    | array_elem_start expr
	;

array_elem_start :  LEFT_BRACKET expr COMMA
	| array_type LEFT_BRACKET expr COMMA
	| array_elem_start expr COMMA
	;
```

**CompositeLiteral/KeyValueExpr**
```yacc
composite_literal: LEFT_BRACE composite_elems RIGHT_BRACE
	| map_type LEFT_BRACE composite_elems RIGHT_BRACE
	| identifier LEFT_BRACE composite_elems RIGHT_BRACE
	
    | LEFT_BRACE RIGHT_BRACE
	| map_type LEFT_BRACE RIGHT_BRACE
	| identifier LEFT_BRACE RIGHT_BRACE
	;

composite_elem: expr COLON expr
	| expr
	;

composite_elems_start: composite_elem COMMA
	| composite_elems_start composite_elem COMMA
	;

composite_elems: composite_elems_start
	| composite_elems_start composite_elem
	| composite_elem
	;
```

**ParenExpr**
```yacc
paren_expr: LEFT_PRAEN expr RIGHT_PAREN
    ;
```

**AttrExpr/IndexExpr/SliceExpr/CallExpr**
```yacc
// index_expr, attr_expr, call_expr, slice_expr
suffix_expr: 
	// index
	 identifier LEFT_BRACKET expr RIGHT_BRACKET
	| DOT LEFT_BRACKET expr RIGHT_BRACKET

	// attr
	| identifier DOT identifier
		
	// call
	| identifier LEFT_PAREN call_args RIGHT_PAREN
	| identifier LEFT_PAREN RIGHT_PAREN
	
	// slice
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty RIGHT_BRACKET
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty COLON expr_or_empty RIGHT_BRACKET
	
	// + index
	| suffix_expr LEFT_BRACKET expr RIGHT_BRACKET // index_expr
		
	// + attr
	| suffix_expr DOT identifier
	
	// + call
	| suffix_expr LEFT_PAREN call_args RIGHT_PAREN
	| suffix_expr LEFT_PAREN RIGHT_PAREN

	// + slice
	| suffix_expr LEFT_BRACKET expr COLON expr RIGHT_BRACKET
	| suffix_expr LEFT_BRACKET expr COLON expr COLON expr RIGHT_BRACKET
	;

call_arg: identifier
    | identifier EQ expr
    ;

call_args_start: call_arg COMMA
    | call_args_start call_arg COMMA
	;

call_args: call_args_start
    | call_args_start call_arg
	| call_arg
	;

expr_or_empty: expr
	| 
	;
```


**UnaryExpr**
```yacc
unary_expr:  MUL expr %prec UMINUS 
	| BitwiseAND expr %prec UMINUS 
	| ADD expr %prec UMINUS 
	| SUB expr %prec UMINUS 
	| NOT expr
	| BitwiseNOT expr
```

**BinaryExpr**
```yacc
binary_expr	: expr GTE expr
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
    ;
```
