package ast2

type TypeKind int

const (
	UnknownT TypeKind = iota
	IntT
	FloatT
	StrT
	BoolT
	PointerT
	StructT
	ArrayT
	FuncT
	MapT
)
