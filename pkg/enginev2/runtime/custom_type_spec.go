package runtime

import "github.com/GuanceCloud/platypus/pkg/ast"

const (
	__operator_index_get__ = "__operator_index_get__"
	__operator_index_set__ = "__operator_index_set__"

	__operator_equal__ = "__operator_equal__"
)

type TypeSpec struct {
	Dtype ast.DType

	CustomTypeSpec *CustomTypeSpec
}

type ParamInclude uint

const (
	NormalParam ParamInclude = iota
	DefaultValueParam
	VariableParam
)

type ParamSpec struct {
	Name string

	TypeSpec TypeSpec
}

type FuncSpec struct {
	Name        string
	Param       []ParamSpec
	ReturnValue []TypeSpec

	ParamInclude ParamInclude
	Offset       int // VariableParam only
}

type FieldSpec struct {
	Name string

	DType ast.DType

	StructSpec *TypeSpec
}

type CustomTypeSpec struct {
	Name   string
	Fields map[string]FieldSpec
	Method map[string]FuncSpec
}
