package runtime

import "github.com/GuanceCloud/platypus/pkg/ast"

type SymbolKind uint8

const (
	SFunc SymbolKind = iota + 1
	SType
	SVarb
	SConst
)

type SymbolInfo struct {
	Name string

	SKind SymbolKind

	DType ast.DType

	Spec TypeSpec
}

type SymbolTable struct {
	Symbol []*SymbolInfo
}
