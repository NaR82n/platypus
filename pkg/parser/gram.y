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
	node       *plast.Node
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
	if_elem
	elif_elem
	if_elif_list
	stmt
	assignment_stmt
	value_decl
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
	func_args_start
	func_arg
	func_args
	call_args_start
	call_arg
	call_args
	identifier
	binary_expr
	paren_expr
	/* expr */
	suffix_expr
	fn_type
	composite_literal
	composite_elem
	composite_elems
	composite_elems_start
	/* key_value_expr
	composite_literal_start */
	array_literal
	array_literal_start
	basic_literal
	value_stmt
	struct_decl_elem
	struct_decl_elems_start
	struct_decl_elems
	struct_decl
	array_elem_start
	fn_decl
	value_decl_elem
	decl_stmt

%type<aststmts>
	stmts
	stmts_list

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

stmts_list: stmt sep
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

/* expression */
expr: identifier 
	| basic_literal
	| array_literal
	| composite_literal
	// | conv_expr 
	| paren_expr
	| suffix_expr
	| unary_expr 
	| binary_expr ;


// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// stmt

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

empty_block: LEFT_BRACE RIGHT_BRACE
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


/*
	for <expr>; <expr>; <expr>  block_smt
	111 (expr, expr, expr) -> 000 ( , , )
*/
for_stmt: FOR expr SEMICOLON expr SEMICOLON expr stmt_block_with_empty
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

for_in_stmt: FOR identifier IN expr stmt_block_with_empty
		{ $$ = &ast.Node{} }
	;

continue_stmt: CONTINUE
		{ $$ = &ast.Node{} }
	;


break_stmt: BREAK
		{ $$ = &ast.Node{} }
	;


return_stmt: RETURN
		{ $$ = &ast.Node{} }
	| RETURN expr
		{ $$ = &ast.Node{} }
	;


decl_stmt: struct_decl
	| value_decl
	| fn_decl
	;

value_stmt: expr
	;


assignment_stmt: expr EQ expr
        { $$ = &ast.Node{} }
	;


// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// decl

fn_decl: FN identifier LEFT_PAREN func_args RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
		{ $$ = &ast.Node{} }
    | FN identifier LEFT_PAREN RIGHT_PAREN RET_SYMB all_type stmt_block_with_empty
		{ $$ = &ast.Node{} }
    | FN identifier LEFT_PAREN func_args RIGHT_PAREN stmt_block_with_empty
		{ $$ = &ast.Node{} }
    | FN identifier LEFT_PAREN RIGHT_PAREN stmt_block_with_empty
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

func_args_start: func_arg COMMA
		{ $$ = &ast.Node{} }
    | func_args_start func_arg COMMA
		{ $$ = &ast.Node{} }
	;

func_args: func_args_start
    | func_args_start func_arg
	| func_arg
	;


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


struct_decl: STRUCT identifier LEFT_BRACE struct_decl_elems RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| STRUCT identifier empty_block
		{ $$ = &ast.Node{} }
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

// ------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------
// expr

/* conv_expr: 	LEFT_PAREN all_type RIGHT_PAREN paren_expr
		{ $$ = $2 }
	; */


identifier: QUOTED_STRING
		{ $$ = &ast.Node{} }
	;


all_type: basic_type
	| array_type
	| map_type
	| identifier
	| ptr_type
	| fn_type
	| LEFT_PAREN all_type RIGHT_PAREN
		{ $$ = &ast.Node{} }
	;

ptr_type: MUL all_type
		{ $$ = &ast.Node{} }
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
	| LEFT_BRACKET EQ NUMBER RIGHT_BRACKET all_type
		{ $$ = &ast.Node{} }
	;

map_type : MAP LEFT_BRACKET all_type RIGHT_BRACKET all_type
		{ $$ = &ast.Node{} }
	;

fn_type: FN LEFT_PAREN func_args RIGHT_PAREN RET_SYMB all_type
		{ $$ = &ast.Node{} }
    | FN LEFT_PAREN RIGHT_PAREN RET_SYMB all_type
		{ $$ = &ast.Node{} }
    | FN LEFT_PAREN func_args RIGHT_PAREN
		{ $$ = &ast.Node{} }
    | FN LEFT_PAREN RIGHT_PAREN
		{ $$ = &ast.Node{} }
    ;


basic_literal: NUMBER
		{ $$ = &ast.Node{} }
    | STRING
		{ $$ = &ast.Node{} }
    | MULTILINE_STRING
		{ $$ = &ast.Node{} }
    | TRUE
		{ $$ = &ast.Node{} }
    | FALSE
		{ $$ = &ast.Node{} }
    | NIL
		{ $$ = &ast.Node{} }
    ;

array_literal: array_literal_start RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| LEFT_BRACKET RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| array_type LEFT_BRACKET RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	;

array_literal_start: array_elem_start
		{ $$ = &ast.Node{} }
	| array_elem_start expr
		{ $$ = &ast.Node{} }
	|  LEFT_BRACKET expr
		{ $$ = &ast.Node{} }
	| array_type LEFT_BRACKET expr
		{ $$ = &ast.Node{} }
	;

array_elem_start :  LEFT_BRACKET expr COMMA
		{ $$ = &ast.Node{} }
	| array_type LEFT_BRACKET expr COMMA
		{ $$ = &ast.Node{} }
	| array_elem_start expr COMMA
		{ $$ = &ast.Node{} }
	;

composite_literal: LEFT_BRACE composite_elems RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| map_type LEFT_BRACE composite_elems RIGHT_BRACE
	| identifier LEFT_BRACE composite_elems RIGHT_BRACE
	| LEFT_BRACE RIGHT_BRACE
		{ $$ = &ast.Node{} }
	| map_type LEFT_BRACE RIGHT_BRACE
	| identifier LEFT_BRACE RIGHT_BRACE
	;

composite_elem: expr COLON expr
		{ $$ = &ast.Node{} }
	| expr
	;

composite_elems_start: composite_elem COMMA
		{ $$ = &ast.Node{} }
	| composite_elems_start composite_elem COMMA
		{ $$ = &ast.Node{} }
	;

composite_elems: composite_elems_start
		{ $$ = &ast.Node{} }
	| composite_elems_start composite_elem
		{ $$ = &ast.Node{} }
	| composite_elem
	;


paren_expr: LEFT_PAREN expr RIGHT_PAREN
		{ $$ = &ast.Node{} }
	;



call_arg: identifier
		{ $$ = &ast.Node{} }
    | identifier EQ expr
		{ $$ = &ast.Node{} }
    ;

call_args_start: call_arg COMMA
		{ $$ = &ast.Node{} }
    | call_args_start call_arg COMMA
		{ $$ = &ast.Node{} }
	;

call_args: call_args_start
    | call_args_start call_arg
	| call_arg
	;

expr_or_empty: expr
	| 
		{ $$ = nil }
	;

// index_expr, attr_expr, call_expr, slice_expr
suffix_expr: 
	// index
	 identifier LEFT_BRACKET expr RIGHT_BRACKET
	  	{ $$ = &ast.Node{} }
	| DOT LEFT_BRACKET expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }

	// attr
	| identifier DOT identifier
		{ $$ = &ast.Node{} }
		
	// call
	| identifier LEFT_PAREN call_args RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| identifier LEFT_PAREN RIGHT_PAREN
		{ $$ = &ast.Node{} }
	
	// slice
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| identifier LEFT_BRACKET expr_or_empty COLON expr_or_empty COLON expr_or_empty RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	
	// + index
	| suffix_expr LEFT_BRACKET expr RIGHT_BRACKET // index_expr
		{ $$ = &ast.Node{} }
		
	// + attr
	| suffix_expr DOT identifier
		{ $$ = &ast.Node{} }
	
	// + call
	| suffix_expr LEFT_PAREN call_args RIGHT_PAREN
		{ $$ = &ast.Node{} }
	| suffix_expr LEFT_PAREN RIGHT_PAREN
		{ $$ = &ast.Node{} }

	// +slice
	| suffix_expr LEFT_BRACKET expr COLON expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	| suffix_expr LEFT_BRACKET expr COLON expr COLON expr RIGHT_BRACKET
		{ $$ = &ast.Node{} }
	;


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


%%
