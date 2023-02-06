// Unless explicitly stated otherwise all files in this repository are licensed
// under the MIT License.
// This product includes software developed at Guance Cloud (https://www.guance.com/).
// Copyright 2021-present Guance, Inc.

package ast

import (
	"fmt"
)

const TabStr = "    "

type TypeCls int

const (
	TClassUnknown TypeCls = iota
	TClassMap
	TClassList
	TClassArray
	TClassNamed
)

func (t TypeCls) String() string {
	switch t {
	case TClassUnknown:
		return "unknown"
	case TClassList:
		return "list"
	case TClassArray:
		return "array"
	case TClassMap:
		return "map"
	case TClassNamed:
		return "named"
	}
	return "unknown"
}

type TIface interface {
	TStr() string
	TCls() TypeCls
}

type MapType struct {
	KeyT   *Node
	ValueT *Node
}

func (e *MapType) TCls() TypeCls {
	return TClassMap
}

func (e *MapType) TStr() string {
	return fmt.Sprintf("map[%s]%s", e.KeyT.String(), e.ValueT.String())
}

type ListType struct {
	ValueT *Node
}

func (e *ListType) TCls() TypeCls {
	return TClassList
}

func (e *ListType) TStr() string {
	return "[]" + e.ValueT.String()
}

type ArrayType struct {
	Len    int
	ValueT *Node
}

func (e *ArrayType) TCls() TypeCls {
	return TClassArray
}

func (e *ArrayType) TStr() string {
	return fmt.Sprintf("[%d]%s", e.Len, e.ValueT.String())
}

type NamedType struct {
	// include base type
	Name string

	// TODO
	TypeNode *Node
}

func (e *NamedType) TCls() TypeCls {
	return TClassNamed
}

func (e *NamedType) TStr() string {
	return e.Name
}

type Type struct {
	T TIface
}

func (t *Type) Format() []string {
	if t.T != nil {
		return []string{t.T.TStr()}
	}
	return nil
}

func (t *Type) String() string {
	if t.T != nil {
		return t.T.TStr()
	} else {
		return ""
	}
}

func (t *Type) TCls() TypeCls {
	if t.T != nil {
		return t.T.TCls()
	} else {
		return TClassUnknown
	}
}
