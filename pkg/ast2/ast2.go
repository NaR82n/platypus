package ast2

import "go/token"

type Node interface {
	Start() token.Pos
	End() token.Pos
}

type Decl interface {
	decl()

	Start() token.Token
	End() token.Token
}

type Expr interface {
	expr()

	Start() token.Pos
	End() token.Pos
}

type Stmt interface {
	stmt()

	Start() token.Pos
	End() token.Pos
}

// ---------------------- Decl ---------------------------
// -------------------------------------------------------

type ValueDecl struct {
	// for varb and const
}

// TODO:
// type TypeDecl struct{}

type StructDecl struct{}

type FuncDecl struct{}

// ------------------ Type And Expr ----------------------
// -------------------------------------------------------

type Identifier struct {
	Name string

	Start token.Pos
	End   token.Pos
}

type BasicType struct {
	// int, float, bool, str
}

type ArrayType struct {
	// array, list
}

type MapType struct {
}

type StructType struct {
}

type PointType struct {
	Start token.Pos
	Expr  Expr
}

type Field struct {
	Name *Identifier
	Type Expr
}

type FuncType struct {
	Func token.Pos

	Rparen token.Pos

	Param []*Field

	Lparen token.Pos
}

type IntLiteral struct{}

type FloatLiteral struct{}

type BoolLiteral struct{}

type StrLiteral struct{}

type NilLiteral struct{}

type ArrayLiteral struct{}

type CompositeLiteral struct {
	// for map, array(list) and struct
}

type ParenExpr struct {
}

type AttrExpr struct {
}

type IndexExpr struct {
}

type IndexListExpr struct {
}

type SliceExpr struct {
}

type CallExpr struct {
}

type BinaryExpr struct {
}

type UnaryExpr struct {
}

// ---------------------- Stmt ---------------------------
// -------------------------------------------------------
type EmptyStmt struct{}

type AssignStmt struct{}

type BlockStmt struct{}

type IfStmt struct {
}

type ForStmt struct{}

type ContinueStmt struct{}

type BreakStmt struct{}

type ReturnStmt struct{}

// ValueStmt
type ExprStmt struct{}
