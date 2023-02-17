// Unless explicitly stated otherwise all files in this repository are licensed
// under the MIT License.
// This product includes software developed at Guance Cloud (https://www.guance.com/).
// Copyright 2021-present Guance, Inc.

%{
package parser

import (
	plast "github.com/GuanceCloud/platypus/pkg/ast"
	// pltoken "github.com/GuanceCloud/platypus/pkg/token"
)

%}

%union {
	aststmts   plast.Stmts
	astblock   *plast.BlockStmt

	ifitem     *plast.IfStmtElem
	iflist	   []*plast.IfStmtElem
	node       *plast.Node
	nodes      []*plast.Node
	item       Item

}

%token <item> SEMICOLON COMMA COMMENT DOT EOF ERROR ID NUMBER 
	LEFT_PAREN LEFT_BRACKET LEFT_BRACE RIGHT_BRACE
	RIGHT_PAREN RIGHT_BRACKET SPACE STRING QUOTED_STRING MULTILINE_STRING
	FOR IN WHILE BREAK CONTINUE	RETURN EOL COLON
	STR INT FLOAT BOOL LIST MAP STRUCT ANY LET FN RET_SYMB NOT
	BitwiseXOR BitwiseOR BitwiseNOT BitwiseAND CONST

// operator
%token operatorsStart
%token <item> ADD
	DIV GTE GT
	LT LTE MOD MUL
	NEQ EQ EQEQ SUB
%token operatorsEnd

// keywords
%token keywordsStart
%token <item>
TRUE FALSE IDENTIFIER AND OR 
NIL NULL IF ELIF ELSE
%token keywordsEnd

// start symbols for parser
%token startSymbolsStart
%token START_STMTS
%token startSymbolsEnd

////////////////////////////////////////////////////
// grammar rules
////////////////////////////////////////////////////
%type <item>
	varb_or_const


%type<node>
	stmt_block_with_empty
	empty_block
	stmt_block
	stmt_block_start

%type<aststmts>
	stmts
	stmts_list

%type<ifitem>
	if_elem
	elif_elem

%type<iflist>
	if_elif_list

%type<nodes>
	func_call_args

%type <node>
	stmt
	assignment_stmt
	value_decl_stmt
	for_in_stmt
	for_stmt
	continue_stmt
	return_stmt
	break_stmt
	ifelse_stmt
	expr
	expr_or_empty
	map_type
	basic_type
	all_type
	array_type
	ptr_type
	unary_expr
	/* conv_expr */
	func_arg
	func_args

%type <node>
	identifier
	binary_expr
	paren_expr
	/* expr */
	suffix_expr
	func_type
	composite_literal
	key_value_expr
	composite_literal_start
	array_literal
	array_init_start
	/* array_elem */
	bool_literal
	string_literal
	nil_literal
	number_literal
	value_stmt
	struct_decl_start
	struct_decl
	fn_decl_start
	fn_decl
	decl_stmt
	//columnref

%start start

// operator listed with increasing precedence
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
%%


sep : SEMICOLON
	| EOL
	| sep SEMICOLON
	| sep EOL

start	: START_STMTS stmts
		{ yylex.(*parser).parseResult = $2 }
	| start EOF
	| error
		{ yylex.(*parser).unexpected("", "") }
	;


stmts: stmts_list stmt
		{
			s := $1
			s = append(s, $2)
			$$ = s
		}
	| stmts_list
	| stmt
		{ $$ = plast.Stmts{$1} }	
	;

stmts_list	: stmt sep
		{ $$ = plast.Stmts{$1} }
	| sep
		{ $$ = plast.Stmts{} }
	| stmts_list stmt sep
		{
		s := $1
		s = append(s, $2)
		$$ = s
		}
	;

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

decl_stmt: struct_decl
	| value_decl_stmt
	| fn_decl
	;

value_stmt: expr
	;

all_type: basic_type
	| array_type
	| map_type
	| identifier
	| ptr_type
	| func_type
	| LEFT_PAREN all_type RIGHT_PAREN
		{
			$$ = $2
		}
	;

ptr_type: MUL all_type
	{
		$$ = $2
	}
	;


basic_type: INT 
		{ $$ = &ast.Node{} }
	| FLOAT
    	{ $$ = &ast.Node{} }
	| BOOL
    	{ $$ = &ast.Node{} }
	| STR
	   	{ $$ = &ast.Node{} }
	| ANY
    	{ $$ = &ast.Node{} }
	;


array_type: LEFT_BRACKET EQ RIGHT_BRACKET all_type
		{ $$ = &ast.Node{} }
	| LEFT_BRACKET EQ number_literal RIGHT_BRACKET all_type
		{ $$ = &ast.Node{} }
	;

map_type : MAP LEFT_BRACKET all_type RIGHT_BRACKET all_type
		{ $$ = &ast.Node{} }
	;

func_arg: identifier
		{ $$ = &ast.Node{} }
    | identifier EQ expr
		{ $$ = &ast.Node{} }
    | identifier COLON all_type
		{ $$ = &ast.Node{} }
    | identifier COLON all_type EQ expr
		{ $$ = &ast.Node{} }
    ;

func_args: func_arg
    | func_args COMMA func_arg
		{ $$ = &ast.Node{} }

func_type: FN LEFT_PAREN func_args RIGHT_PAREN RET_SYMB all_type
		{ $$ = &ast.Node{} }
    | FN LEFT_PAREN RIGHT_PAREN RET_SYMB all_type
		{ $$ = &ast.Node{} }
    | FN LEFT_PAREN func_args RIGHT_PAREN
		{ $$ = &ast.Node{} }
    | FN LEFT_PAREN RIGHT_PAREN
		{ $$ = &ast.Node{} }
    ;

/* expression */
expr	: bool_literal
		| string_literal
		| nil_literal
		| number_literal
		| identifier
		/* | conv_expr */
		| unary_expr  | suffix_expr | array_literal | composite_literal | paren_expr | binary_expr ; // arithmeticExpr

/* conv_expr: 	LEFT_PAREN all_type RIGHT_PAREN paren_expr
		{ $$ = $2 }
	; */

unary_expr: MUL expr %prec UMINUS 
		{ $$ = &ast.Node{} }
	| BitwiseAND expr %prec UMINUS 
		{ $$ = &ast.Node{} }
	| ADD expr %prec UMINUS 
		{ $$ = &ast.Node{} }
	| SUB expr %prec UMINUS 
		{ $$ = &ast.Node{} }
	| NOT expr
		{ $$ = &ast.Node{} }
	| BitwiseNOT expr
		{ $$ = &ast.Node{} }
	;

break_stmt: BREAK
		{ $$ = &ast.Node{} }
	;

continue_stmt: CONTINUE
		{ $$ = &ast.Node{} }
	;

return_stmt: RETURN
		{ $$ = &ast.Node{} }
	;

/*
	for identifier IN identifier
	for identifier IN list_init
	for identifier IN string
*/
for_in_stmt : FOR identifier IN expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;


/*
	for <expr>; <expr>; <expr>  block_smt
	111 (expr, expr, expr) -> 000 ( , , )
*/
for_stmt : FOR expr SEMICOLON expr SEMICOLON expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR expr SEMICOLON expr SEMICOLON stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR expr SEMICOLON SEMICOLON expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR expr SEMICOLON SEMICOLON stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR SEMICOLON expr SEMICOLON expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR SEMICOLON expr SEMICOLON stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR SEMICOLON SEMICOLON expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| FOR SEMICOLON SEMICOLON stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;

ifelse_stmt: if_elif_list
		{ $$ = &ast.Node{} }
	| if_elif_list ELSE stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;

if_elem: IF expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;

if_elif_list: if_elem
		{ $$ = &ast.Node{} }
	| if_elif_list elif_elem
		{ $$ = &ast.Node{} }
	;

elif_elem: ELIF expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;


stmt_block_with_empty	: empty_block
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

empty_block: LEFT_BRACE RIGHT_BRACE
		{ $$ = &ast.Node{} }
	;


func_call_args: func_call_args COMMA expr
		{ $$ = &ast.Node{} }
	| func_call_args COMMA
		{ $$ = &ast.Node{} }
	| expr
		{ $$ = &ast.Node{} }
	| identifier EQ expr
		{ $$ = &ast.Node{} }
	;


varb_or_const: LET | CONST
	;

value_decl_stmt: varb_or_const identifier
		{ $$ = &ast.Node{} }
	| varb_or_const identifier EQ expr
		{ $$ = &ast.Node{} }
	| varb_or_const identifier COLON all_type
		{ $$ = &ast.Node{} }
	| varb_or_const identifier COLON all_type EQ expr
		{ $$ = &ast.Node{} }
	| value_decl_stmt COMMA identifier
		{ $$ = &ast.Node{} }
	| value_decl_stmt COMMA identifier EQ expr
		{ $$ = &ast.Node{} }
	| value_decl_stmt COMMA identifier COLON all_type
		{ $$ = &ast.Node{} }
	| value_decl_stmt COMMA identifier COLON all_type EQ expr
		{ $$ = &ast.Node{} }
	;


assignment_stmt: expr EQ expr
        { $$ = &ast.Node{} }
	;

binary_expr	: expr GTE expr
		{ $$ = &ast.Node{} }
	| expr GT expr
		{ $$ = &ast.Node{} }
	| expr OR expr
		{ $$ = &ast.Node{} }
	| expr AND expr
		{ $$ = &ast.Node{} }
	| expr LT expr
		{ $$ = &ast.Node{} }
	| expr LTE expr
		{ $$ = &ast.Node{} }
	| expr NEQ expr
		{ $$ = &ast.Node{} }
	| expr EQEQ expr
		{ $$ = &ast.Node{} }
	| expr BitwiseAND expr
		{ $$ = &ast.Node{} }
	| expr BitwiseXOR expr
		{ $$ = &ast.Node{} }
	| expr BitwiseOR expr
		{ $$ = &ast.Node{} }
	| expr ADD expr
		{ $$ = &ast.Node{} }
	| expr SUB expr
		{ $$ = &ast.Node{} }
	| expr MUL expr
		{ $$ = &ast.Node{} }
	| expr DIV expr
		{ $$ = &ast.Node{} }
	| expr MOD expr
		{ $$ = &ast.Node{} }
	;

// TODO: 支持多个表达式构成的括号表达式
paren_expr: LEFT_PAREN expr RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| LEFT_PAREN expr EOLS RIGHT_PAREN
		{ $$ = &ast.Node{} }
	;


EOLS: EOL
	| EOLS EOL
	;

// index_expr, attr_expr, call_expr, slice_expr
suffix_expr: 
	// index_expr
	  identifier LEFT_BRACKET expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| identifier LEFT_PAREN func_call_args RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| DOT LEFT_BRACKET expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	// slice_expr
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty COLON expr_or_empty RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	// attr_expr
	| identifier DOT identifier
		{ $$ = &ast.Node{} }
	// call expr
	| identifier LEFT_PAREN RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| identifier LEFT_PAREN func_call_args EOLS RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| identifier LEFT_PAREN EOLS RIGHT_PAREN
		{ $$ = &ast.Node{} }
	// index_expr
	| suffix_expr LEFT_BRACKET expr RIGHT_BRACKET // index_expr
		{ $$ = &ast.Node{} }
	| suffix_expr LEFT_PAREN func_call_args RIGHT_PAREN
		{ $$ = &ast.Node{} }
	// slice_expr
	| suffix_expr LEFT_BRACKET expr COLON expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| suffix_expr LEFT_BRACKET expr COLON expr COLON expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	// attr_expr
	| suffix_expr DOT identifier
		{ $$ = &ast.Node{} }
	// call expr
	| suffix_expr LEFT_PAREN RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| suffix_expr LEFT_PAREN func_call_args EOLS RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| suffix_expr LEFT_PAREN EOLS RIGHT_PAREN
		{ $$ = &ast.Node{} }
	;

expr_or_empty: expr
	| 
		{ $$ = nil }
	;

array_literal: array_init_start EOLS RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| array_init_start RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| array_init_start COMMA RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| LEFT_BRACKET RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| array_type LEFT_BRACKET RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	;

array_init_start :  LEFT_BRACKET expr
		{ $$ = &ast.Node{} }
	| array_type LEFT_BRACKET expr
		{ $$ = &ast.Node{} }
	| array_init_start COMMA expr
		{ $$ = &ast.Node{} }
	;

key_value_expr: expr COLON expr
	;

composite_literal : composite_literal_start RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| composite_literal_start expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| composite_literal_start key_value_expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| empty_block
		{ $$ = &ast.Node{} }
	| map_type empty_block
		{ $$ = &ast.Node{} }
	| identifier empty_block
		{ $$ = &ast.Node{} }
	| LEFT_BRACE expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| LEFT_BRACE key_value_expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| identifier LEFT_BRACE key_value_expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| identifier LEFT_BRACE expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| map_type LEFT_BRACE expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| map_type LEFT_BRACE key_value_expr RIGHT_BRACE
		{ $$ = &ast.Node{} }
	;

composite_literal_start: LEFT_BRACE key_value_expr COMMA
		{ $$ = &ast.Node{} }
	| LEFT_BRACE expr COMMA
		{ $$ = &ast.Node{} }
	| map_type LEFT_BRACE key_value_expr COMMA
		{ $$ = &ast.Node{} }
	| map_type LEFT_BRACE expr COMMA
		{ $$ = &ast.Node{} }
	| identifier LEFT_BRACE key_value_expr COMMA
		{ $$ = &ast.Node{} }
	| identifier LEFT_BRACE expr COMMA
		{ $$ = &ast.Node{} }
	| composite_literal_start  key_value_expr  COMMA
		{ $$ = &ast.Node{} }
	| composite_literal_start expr  COMMA
		{ $$ = &ast.Node{} }
	;


bool_literal	: TRUE
		{ $$ = &ast.Node{} }
	| FALSE
		{ $$ = &ast.Node{} }
	;


string_literal	: STRING
		{ $$ = &ast.Node{} }
	| MULTILINE_STRING
		{ $$ = &ast.Node{} }
	;


nil_literal	: NIL
		{ $$ = &ast.Node{} }
	| NULL
		{ $$ = &ast.Node{} }
	;



number_literal	: NUMBER
		{ $$ = &ast.Node{} }
	;

identifier: QUOTED_STRING
		{ $$ = &ast.Node{} }
	;


/* unary_op	: ADD | SUB ; */

struct_decl_start : STRUCT identifier LEFT_BRACE identifier COMMA
		{ $$ = &ast.Node{} }
	| STRUCT identifier LEFT_BRACE identifier COLON all_type COMMA
		{ $$ = &ast.Node{} }
	| struct_decl_start identifier COMMA
		{ $$ = &ast.Node{} }
	| struct_decl_start identifier COLON all_type COMMA
		{ $$ = &ast.Node{} }
	;


struct_decl: struct_decl_start RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| struct_decl_start identifier RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| struct_decl_start identifier COLON all_type RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| STRUCT identifier LEFT_BRACE identifier RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| STRUCT identifier LEFT_BRACE identifier COLON all_type RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| STRUCT identifier empty_block
		{ $$ = &ast.Node{} }
	;

fn_decl: fn_decl_start RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| fn_decl_start EOLS RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| fn_decl_start RIGHT_PAREN stmt_block_with_empty
		{ $$ = &ast.Node{} }
	| fn_decl_start EOLS RIGHT_PAREN stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;

fn_decl_start: FN identifier LEFT_PAREN identifier
		{ $$ = &ast.Node{} }
	| FN identifier LEFT_PAREN identifier COLON all_type
		{ $$ = &ast.Node{} }
	| fn_decl_start COMMA identifier
		{ $$ = &ast.Node{} }
	| fn_decl_start COMMA identifier COLON all_type
		{ $$ = &ast.Node{} }
	| FN identifier LEFT_PAREN identifier EQ expr
		{ $$ = &ast.Node{} }
	| FN identifier LEFT_PAREN identifier COLON all_type EQ expr
		{ $$ = &ast.Node{} }
	| fn_decl_start COMMA identifier EQ expr
		{ $$ = &ast.Node{} }
	| fn_decl_start COMMA identifier COLON all_type EQ expr
		{ $$ = &ast.Node{} }
	;
%%
