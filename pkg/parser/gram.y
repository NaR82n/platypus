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
	STR INT FLOAT BOOL LIST MAP STRUCT ANY LET FN RET_SYMB

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
	unary_op


%type<astblock>
	stmt_block
	empty_block


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
	assignment_expr
	varb_decl_stmt
	for_in_stmt
	for_stmt
	continue_stmt
	break_stmt
	ifelse_stmt
	call_expr
	map_type
	list_type
	base_type
	all_type

%type <node>
	identifier
	binary_expr
	conditional_expr
	arithmeticExpr
	paren_expr
	index_expr
	attr_expr
	expr
	map_init
	map_init_start
	list_init
	list_init_start
	array_elem
	bool_literal
	string_literal
	nil_literal
	number_literal
	value_stmt
	struct_type_decl_start
	struct_type_decl_stmt
	fn_decl_start
	fn_decl_stmt
	//columnref

%start start

// operator listed with increasing precedence
%right EQ
%left OR
%left AND
%left GTE GT NEQ EQEQ LTE LT
%left ADD SUB
%left MUL DIV MOD

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

stmt	: ifelse_stmt
	| for_in_stmt
	| for_stmt
	| continue_stmt
	| break_stmt
	| value_stmt
	| struct_type_decl_stmt
	| map_type
	| varb_decl_stmt
	| fn_decl_stmt
	;


value_stmt: expr
	;

/* expression */
expr	: array_elem | assignment_expr | list_init | map_init | paren_expr | call_expr | binary_expr | attr_expr | index_expr ; // arithmeticExpr


break_stmt: BREAK
			{ $$ = yylex.(*parser).newBreakStmt($1.Pos) }
		;

continue_stmt: CONTINUE
			{ $$ = yylex.(*parser).newContinueStmt($1.Pos) }
		;

/*
	for identifier IN identifier
	for identifier IN list_init
	for identifier IN string
*/
for_in_stmt : FOR identifier IN expr stmt_block
			{ $$ = yylex.(*parser).newForInStmt($2, $4, $5, $1, $3) }
		;


/*
	for init expr; cond expr; loop expr  block_smt
	for init expr; cond expr; 			 block_stmt
	for 		 ; cond expr; loop expr  block_stmt
	for 		 ; cond expr; 		     block_stmt
*/
for_stmt : FOR expr SEMICOLON expr SEMICOLON expr stmt_block
		{ $$ = yylex.(*parser).newForStmt($2, $4, $6, $7) }
	| FOR expr SEMICOLON expr SEMICOLON stmt_block
		{ $$ = yylex.(*parser).newForStmt($2, $4, nil, $6) }
	| FOR SEMICOLON expr SEMICOLON expr stmt_block
		{ $$ = yylex.(*parser).newForStmt(nil, $3, $5, $6) }
	| FOR SEMICOLON expr SEMICOLON stmt_block
		{ $$ = yylex.(*parser).newForStmt(nil, $3, nil, $5) }

	| FOR expr SEMICOLON SEMICOLON expr stmt_block
		{ $$ = yylex.(*parser).newForStmt($2, nil, $5, $6) }
	| FOR expr SEMICOLON SEMICOLON stmt_block
		{ $$ = yylex.(*parser).newForStmt($2, nil, nil, $5) }
	| FOR SEMICOLON SEMICOLON expr stmt_block
		{ $$ = yylex.(*parser).newForStmt(nil, nil, $4, $5) }
	| FOR SEMICOLON SEMICOLON stmt_block
		{ $$ = yylex.(*parser).newForStmt(nil, nil, nil, $4) }
	;

ifelse_stmt: if_elif_list
		{
			$$ = yylex.(*parser).newIfElifStmt($1)
		}
	| if_elif_list ELSE stmt_block
		{
			$$ = yylex.(*parser).newIfElifelseStmt($1, $2, $3)
		}
	;

if_elem: IF expr stmt_block
	{ $$ = yylex.(*parser).newIfElem($1, $2, $3) } 
	;

if_elif_list: if_elem
		{ $$ = []*plast.IfStmtElem{ $1 } }
	| if_elif_list elif_elem
		{ $$ = append($1, $2) }
	;

elif_elem: ELIF expr stmt_block
		{ $$ = yylex.(*parser).newIfElem($1, $2, $3) }
	;


stmt_block	: empty_block
	| LEFT_BRACE stmts RIGHT_BRACE
		{ $$ = yylex.(*parser).newBlockStmt($1, $2, $3) }
	;

empty_block : LEFT_BRACE RIGHT_BRACE
		{ $$ = yylex.(*parser).newBlockStmt($1, plast.Stmts{} , $2) }
	;


call_expr : identifier LEFT_PAREN func_call_args RIGHT_PAREN
		{
			$$ = yylex.(*parser).newCallExpr($1, $3, $2, $4)
		}
	| identifier LEFT_PAREN RIGHT_PAREN
		{
			$$ = yylex.(*parser).newCallExpr($1, nil, $2, $3)
		}
	| identifier LEFT_PAREN func_call_args EOLS RIGHT_PAREN
		{
			$$ = yylex.(*parser).newCallExpr($1, $3, $2, $5)
		}
	| identifier LEFT_PAREN EOLS RIGHT_PAREN
		{
			$$ = yylex.(*parser).newCallExpr($1, nil, $2, $4)
		}
	;


func_call_args	: func_call_args COMMA expr
			{
			$$ = append($$, $3)
			}
		| func_call_args COMMA
		| expr
			{ $$ = []*plast.Node{$1} }
		;


binary_expr: conditional_expr | arithmeticExpr ;

varb_decl_stmt: LET identifier
		{ 
			$$ = yylex.(*parser).newVarbDeclStmt($1) 
			$$ = yylex.(*parser).varbDeclAppend($$, $2, nil, nil) 
		}	
	| LET identifier EQ expr
		{ 
			$$ = yylex.(*parser).newVarbDeclStmt($1) 
			$$ = yylex.(*parser).varbDeclAppend($$, $2, nil, $4) 
		}	
	| LET identifier COLON all_type EQ expr
		{ 
			$$ = yylex.(*parser).newVarbDeclStmt($1) 
			$$ = yylex.(*parser).varbDeclAppend($$, $2, $4, $6) 
		}	
	| varb_decl_stmt COMMA identifier
		{
			$$ = yylex.(*parser).varbDeclAppend($$, $3, nil, nil) 
		}
	| varb_decl_stmt COMMA identifier EQ expr
		{
			$$ = yylex.(*parser).varbDeclAppend($$, $3, nil, $5) 
		}
	| varb_decl_stmt COMMA identifier COLON all_type EQ expr
		{
			$$ = yylex.(*parser).varbDeclAppend($$, $3, $5, $7) 
		}

	;

assignment_expr: index_expr EQ expr
        { $$ = yylex.(*parser).newAssignmentExpr($1, $3, $2) }	
	| attr_expr EQ expr
        { $$ = yylex.(*parser).newAssignmentExpr($1, $3, $2) }
	| identifier EQ expr
		{ $$ = yylex.(*parser).newAssignmentExpr($1, $3, $2) }	
	;

conditional_expr	: expr GTE expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr GT expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr OR expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr AND expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr LT expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr LTE expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr NEQ expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			| expr EQEQ expr
				{ $$ = yylex.(*parser).newConditionalExpr($1, $3, $2) }
			;


arithmeticExpr	: expr ADD expr
				{ $$ = yylex.(*parser).newArithmeticExpr($1, $3, $2) }
			| expr SUB expr
				{ $$ = yylex.(*parser).newArithmeticExpr($1, $3, $2) }
			| expr MUL expr
				{ $$ = yylex.(*parser).newArithmeticExpr($1, $3, $2) }
			| expr DIV expr
				{ $$ = yylex.(*parser).newArithmeticExpr($1, $3, $2) }
			| expr MOD expr
				{ $$ = yylex.(*parser).newArithmeticExpr($1, $3, $2) }
			;

// TODO: 支持多个表达式构成的括号表达式
paren_expr: LEFT_PAREN expr RIGHT_PAREN
			{ $$ = yylex.(*parser).newParenExpr($1, $2, $3) }
		| LEFT_PAREN expr EOLS RIGHT_PAREN
			{ $$ = yylex.(*parser).newParenExpr($1, $2, $4) }
		;


EOLS: EOL
	| EOLS EOL
	;

index_expr	: identifier LEFT_BRACKET expr RIGHT_BRACKET
			{ $$ = yylex.(*parser).newIndexExpr($1, $2 ,$3, $4) }
		| DOT LEFT_BRACKET expr RIGHT_BRACKET	
			// 兼容原有语法，仅作为 json 函数的第二个参数
			{ $$ = yylex.(*parser).newIndexExpr(nil, $2, $3, $4) }
		| index_expr LEFT_BRACKET expr RIGHT_BRACKET
			{ $$ = yylex.(*parser).newIndexExpr($1, $2, $3, $4) }
		;


// TODO 实现结构体或类，当前不进行取值操作
// 仅用于 json 函数
attr_expr	: identifier DOT index_expr
			{ 
				$$ =  yylex.(*parser).newAttrExpr($1, $3)
			}
		| identifier DOT identifier
			{ 
				$$ =  yylex.(*parser).newAttrExpr($1, $3)
			}
		| index_expr DOT index_expr
			{ 
				$$ = yylex.(*parser).newAttrExpr($1, $3)
			}
	  	| index_expr DOT identifier
			{ 
				$$ =  yylex.(*parser).newAttrExpr($1, $3)
			}
		| attr_expr DOT index_expr
			{ 
				$$ = yylex.(*parser).newAttrExpr($1, $3)
			}
		| attr_expr DOT identifier
			{ 
				$$ =  yylex.(*parser).newAttrExpr($1, $3)
			}
		;


list_init :list_init_start RIGHT_BRACKET
			{
				$$ = yylex.(*parser).newListInitEndExpr($$, $2.Pos)
			}
		| list_init_start COMMA RIGHT_BRACKET
			{
				$$ = yylex.(*parser).newListInitEndExpr($$, $2.Pos)
			}
		| LEFT_BRACKET RIGHT_BRACKET
			{ 
				$$ = yylex.(*parser).newListInitStartExpr($1.Pos, nil)
				$$ = yylex.(*parser).newListInitEndExpr($$, $2.Pos)
			}
		;

list_init_start :  LEFT_BRACKET expr
			{ 
				$$ = yylex.(*parser).newListInitStartExpr($1.Pos, nil)
				$$ = yylex.(*parser).newListInitAppendExpr($$, $2)
			}
		| list_type LEFT_BRACKET expr
			{ 
				$$ = yylex.(*parser).newListInitStartExpr($2.Pos, $1)
				$$ = yylex.(*parser).newListInitAppendExpr($$, $3)
			}
		| list_init_start COMMA expr
				{				
					$$ = yylex.(*parser).newListInitAppendExpr($$, $3)
				}
		| list_init_start EOL
	;


map_init : map_init_start RIGHT_BRACE
			{
				$$ = yylex.(*parser).newMapInitEndExpr($$, $2.Pos)
			}
		| map_init_start COMMA RIGHT_BRACE
			{
				$$ = yylex.(*parser).newMapInitEndExpr($$, $3.Pos)
			}
		| empty_block
			{ 
				$$ = yylex.(*parser).newMapInitStartExpr($1.LBracePos.Pos, nil)
				$$ = yylex.(*parser).newMapInitEndExpr($$, $1.RBracePos.Pos)
			}
		;

map_init_start: LEFT_BRACE expr COLON expr
		{ 
			$$ = yylex.(*parser).newMapInitStartExpr($1.Pos, nil)
			$$ = yylex.(*parser).newMapInitAppendExpr($$, $2, $4)
		}
	| map_type LEFT_BRACE expr COLON expr
		{ 
			$$ = yylex.(*parser).newMapInitStartExpr($2.Pos, $1)
			$$ = yylex.(*parser).newMapInitAppendExpr($$, $3, $5)
		} 
	| map_init_start COMMA expr COLON expr
		{
			$$ = yylex.(*parser).newMapInitAppendExpr($1, $3, $5)
		}
	| map_init_start EOL
	;


array_elem	: bool_literal
		| string_literal
		| nil_literal
		| number_literal
		| identifier
		;

/*
	literal:
		bool
		number (int float)
		nil
*/
bool_literal	: TRUE
			{ $$ = yylex.(*parser).newBoolLiteral($1.Pos, true) }
		| FALSE
			{ $$ =  yylex.(*parser).newBoolLiteral($1.Pos, false) }
		;


string_literal	: STRING
			{ 
				$1.Val = yylex.(*parser).unquoteString($1.Val)
				$$ = yylex.(*parser).newStringLiteral($1) 
			}
		| MULTILINE_STRING
			{
				$1.Val = yylex.(*parser).unquoteMultilineString($1.Val)
				$$ = yylex.(*parser).newStringLiteral($1)
			}
		;


nil_literal	: NIL
			{ $$ = yylex.(*parser).newNilLiteral($1.Pos) }
		| NULL
			{ $$ = yylex.(*parser).newNilLiteral($1.Pos) }
		;



number_literal	: NUMBER
			{ $$ =  yylex.(*parser).newNumberLiteral($1) }
		| unary_op NUMBER
			{
			num :=  yylex.(*parser).newNumberLiteral($2) 
			switch $1.Typ {
			case ADD: // pass
			case SUB:
				if num.NodeType == plast.TypeFloatLiteral {
					num.FloatLiteral.Val = -num.FloatLiteral.Val
					num.FloatLiteral.Start = yylex.(*parser).posCache.LnCol($1.Pos)
				} else {
					num.IntegerLiteral.Val = -num.IntegerLiteral.Val
					num.IntegerLiteral.Start = yylex.(*parser).posCache.LnCol($1.Pos)

				}
			}
			$$ = num
			}
		;

identifier: ID
			{
				$$ = yylex.(*parser).newIdentifier($1)
			}
		| QUOTED_STRING
			{
				$1.Val = yylex.(*parser).unquoteString($1.Val) 
				$$ = yylex.(*parser).newIdentifier($1)
			}
		;


unary_op	: ADD | SUB ;


all_type: base_type | list_type | map_type ;

base_type: BOOL
		{
			$$ = yylex.(*parser).newNamedType($1.Val, 0)
		}
	| INT
		{
			$$ = yylex.(*parser).newNamedType($1.Val, 0)
		}
	| FLOAT
		{
			$$ = yylex.(*parser).newNamedType($1.Val, 0)
		}
	| STR
		{
			$$ = yylex.(*parser).newNamedType($1.Val, 0)
		}
	| ANY
		{
			$$ = yylex.(*parser).newNamedType($1.Val, 0)
		}
	| identifier
		{
			$$ = yylex.(*parser).newNamedType($1.Identifier.Name, 0)
		}
	;


list_type: LEFT_BRACKET RIGHT_BRACKET all_type
		{	
			$$ = yylex.(*parser).newListType($3)
		}
	;

map_type : MAP LEFT_BRACKET base_type RIGHT_BRACKET all_type
		{	
			$$ = yylex.(*parser).newMapType($3, $5)
		}
	;


struct_type_decl_start : STRUCT identifier LEFT_BRACE identifier
		{
			$$ = yylex.(*parser).newStructTypeDecl($2)
			$$ = yylex.(*parser).structTypeAppendField($$, $4, nil)
		}
	| STRUCT identifier LEFT_BRACE identifier COLON all_type
		{
			$$ = yylex.(*parser).newStructTypeDecl($2)
			$$ = yylex.(*parser).structTypeAppendField($$, $4, $6)
		}
	| struct_type_decl_start COMMA identifier
		{
			$$ = yylex.(*parser).structTypeAppendField($$, $3, nil)
		}
	| struct_type_decl_start COMMA identifier COLON all_type
		{
			$$ = yylex.(*parser).structTypeAppendField($$, $3, $5)
		}
	;


struct_type_decl_stmt: struct_type_decl_start RIGHT_BRACE
		{
			$$ = $1
		}
	| struct_type_decl_start COMMA RIGHT_BRACE
		{
			$$ = $1
		}
	| STRUCT identifier empty_block
		{
			$$ = yylex.(*parser).newStructTypeDecl($2)
		}
	;

fn_decl_stmt: fn_decl_start RIGHT_PAREN RET_SYMB all_type stmt_block
		{
			$$ = yylex.(*parser).fnDeclAppenReturn($1, $4)
			$$ = yylex.(*parser).fnDeclEnd($$, $5)
		}
	| fn_decl_start COMMA RIGHT_PAREN RET_SYMB all_type stmt_block
		{
			$$ = yylex.(*parser).fnDeclAppenReturn($1, $5)
			$$ = yylex.(*parser).fnDeclEnd($$, $6)
		}
	| fn_decl_start RIGHT_PAREN stmt_block
		{
			$$ = yylex.(*parser).fnDeclEnd($$, $3)
		}
	| fn_decl_start COMMA RIGHT_PAREN stmt_block
		{
			$$ = yylex.(*parser).fnDeclEnd($$, $4)
		}
	;

fn_decl_start: FN identifier LEFT_PAREN identifier
		{
			$$ = yylex.(*parser).newFnDecl($2)
			$$ = yylex.(*parser).fnDeclAppenParam($$, $4, nil, nil)
		}
	| FN identifier LEFT_PAREN identifier COLON all_type
		{
			$$ = yylex.(*parser).newFnDecl($2)
			$$ = yylex.(*parser).fnDeclAppenParam($$, $4, $6, nil)
		}
	| fn_decl_start COMMA identifier
		{
			$$ = yylex.(*parser).fnDeclAppenParam($$, $3, nil, nil)
		}
	| fn_decl_start COMMA identifier COLON all_type
		{
			$$ = yylex.(*parser).fnDeclAppenParam($$, $3, $5, nil)
		}
	| FN identifier LEFT_PAREN identifier EQ expr
		{
			$$ = yylex.(*parser).newFnDecl($2)
			$$ = yylex.(*parser).fnDeclAppenParam($$, $4, nil, $6)
		}
	| FN identifier LEFT_PAREN identifier COLON all_type EQ expr
		{
			$$ = yylex.(*parser).newFnDecl($2)
			$$ = yylex.(*parser).fnDeclAppenParam($$, $4, $6, $8)
		}
	| fn_decl_start COMMA identifier EQ expr
		{
			$$ = yylex.(*parser).fnDeclAppenParam($$, $3, nil, $5)
		}
	| fn_decl_start COMMA identifier COLON all_type EQ expr
		{
			$$ = yylex.(*parser).fnDeclAppenParam($$, $3, $5, $7)
		}
	;
%%
