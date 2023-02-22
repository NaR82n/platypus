package ast2

import "go/token"

type Node interface {
	Start() token.Pos
	End() token.Pos
}

type Decl interface {
	declNode()

	Start() token.Token
	End() token.Token
}

type Expr interface {
	exprNode()

	Start() token.Pos
	End() token.Pos
}

type Stmt interface {
	stmtNode()

	Start() token.Pos
	End() token.Pos
}

type Type interface {
	typeNode()

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
	Kind  TypeKind
	Value string

	Start token.Pos
	End   token.Pos
}

type Field struct {
	Name *Identifier
	Type Type
}

type StructType struct {
	Fields []Field

	Start token.Pos
	End   token.Pos
}

type PointType struct {
	Expr Expr

	Start token.Pos
	End   token.Pos
}

type AnyType struct {
	Start token.Pos
	End   token.Pos
}

type FuncType struct {
	Param      []*Field
	ReturnType []Type

	FuncToken token.Pos
	Rparen    token.Pos
	Lparen    token.Pos
	RetSymb   token.Pos
}

type ArrayType struct {
	IsList  bool
	Len     int
	ValType Type

	Start token.Pos
	End   token.Pos
}

type MapType struct {
	KeyType   Type
	ValueType Type
	MapToken  token.Pos
}

// -----------------------------------

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
