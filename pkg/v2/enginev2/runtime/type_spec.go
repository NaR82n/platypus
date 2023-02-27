package runtime

import (
	"github.com/GuanceCloud/platypus/pkg/ast"
)

const (
	__operator_index_get__ = "__operator_index_get__"
	__operator_index_set__ = "__operator_index_set__"
	__operator_index_del__ = "__operator_index_del__"

	__operator_equal__ = "__operator_equal__"
)

type TypeSpec struct {
	Dtype ast.DType

	CustomTypeSpec *StructTypeSpec
	FuncSpec       *FuncSpec
}

type ParamType uint

const (
	NormalParam ParamType = iota
	DefaultValueParam
	VariableParam
)

type ParamSpec struct {
	Name string

	TypeSpec TypeSpec
}

type FuncSpec struct {
	Name string

	TypeInstance *FieldSpec

	Param       []ParamSpec
	ReturnValue []TypeSpec

	ParamTypeInclue ParamType
	Offset          int // position of the first default value parameter
}

type FieldSpec struct {
	Name string

	DType ast.DType

	StructSpec *TypeSpec
}

type StructTypeSpec struct {
	Name string

	Fields map[string]*FieldSpec

	Method map[string]*FuncSpec
}
