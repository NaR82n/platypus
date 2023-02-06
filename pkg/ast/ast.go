// Unless explicitly stated otherwise all files in this repository are licensed
// under the MIT License.
// This product includes software developed at Guance Cloud (https://www.guance.com/).
// Copyright 2021-present Guance, Inc.

// Package ast pipeline ast node
package ast

import (
	"fmt"
	"sort"
	"strings"

	"github.com/GuanceCloud/platypus/pkg/token"
)

type NodeType uint

const (
	TypeInvalid NodeType = iota

	// Expr.
	TypeIdentifier
	TypeStringLiteral
	TypeIntegerLiteral
	TypeFloatLiteral
	TypeBoolLiteral
	TypeNilLiteral

	TypeListInitExpr
	TypeMapInitExpr

	TypeParenExpr

	TypeAttrExpr
	TypeIndexExpr

	TypeArithmeticExpr
	TypeConditionalExpr
	TypeAssignmentExpr

	TypeCallExpr

	// Stmt.
	TypeBlockStmt
	TypeIfelseStmt
	TypeForStmt
	TypeForInStmt
	TypeContinueStmt
	TypeBreakStmt

	// Decl.
	TypeStructTypeDecl
	TypeVarbDecl
	TypeFuncDeclStmt

	// Type.
	TypeType
)

func (t NodeType) String() string {
	switch t {
	case TypeInvalid:
		return "Invalid"
	case TypeIdentifier:
		return "Identifier"
	case TypeStringLiteral:
		return "StringLiteral"
	case TypeIntegerLiteral:
		return "IntLiteral"
	case TypeFloatLiteral:
		return "FloatLiteral"
	case TypeBoolLiteral:
		return "BoolLiteral"
	case TypeNilLiteral:
		return "NilLiteral"
	case TypeListInitExpr:
		return "ListInitExpr"
	case TypeMapInitExpr:
		return "MapInitExpr"
	case TypeParenExpr:
		return "ParenExpr"
	case TypeAttrExpr:
		return "AttrExpr"
	case TypeIndexExpr:
		return "IndexExpr"
	case TypeArithmeticExpr:
		return "ArithmeticExpr"
	case TypeConditionalExpr:
		return "ConditionalExpr"
	case TypeAssignmentExpr:
		return "AssignmentExpr"
	case TypeCallExpr:
		return "CallExpr"
	case TypeBlockStmt:
		return "BlockStmt"
	case TypeIfelseStmt:
		return "IfelseStmt"
	case TypeForStmt:
		return "ForStmt"
	case TypeForInStmt:
		return "ForInStmt"
	case TypeContinueStmt:
		return "ContinueStmt"
	case TypeBreakStmt:
		return "BreakStmt"
	case TypeFuncDeclStmt:
		return "FuncDeclStmt"
	case TypeStructTypeDecl:
		return "StructTypeDeclStmt"
	case TypeVarbDecl:
		return "VarbDeclStmt"
	case TypeType:
		return "Type"
	}
	return "Undefined"
}

type Stmts []*Node

type KwArgs map[string]*Node

type FuncArgList []*Node

func (e KwArgs) String() string {
	keys := []string{}
	for k := range e {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	arr := []string{}
	for _, key := range keys {
		arr = append(arr, fmt.Sprintf("%s = %s", key, e[key]))
	}
	return strings.Join(arr, ", ")
}

func (e Stmts) String() string {
	arr := []string{}
	for _, x := range e {
		arr = append(arr, x.String())
	}
	return strings.Join(arr, "\n")
}

type Node struct {
	// node type
	NodeType NodeType

	// expr
	Identifier *Identifier

	StringLiteral  *StringLiteral
	IntegerLiteral *IntegerLiteral
	FloatLiteral   *FloatLiteral
	BoolLiteral    *BoolLiteral
	NilLiteral     *NilLiteral

	ListInitExpr *ListInitExpr
	MapInitExpr  *MapInitExpr

	ParenExpr *ParenExpr

	AttrExpr  *AttrExpr
	IndexExpr *IndexExpr

	ArithmeticExpr  *ArithmeticExpr
	ConditionalExpr *ConditionalExpr
	AssignmentExpr  *AssignmentExpr

	CallExpr *CallExpr

	// stmt
	BlockStmt *BlockStmt

	IfelseStmt   *IfelseStmt
	ForStmt      *ForStmt
	ForInStmt    *ForInStmt
	ContinueStmt *ContinueStmt
	BreakStmt    *BreakStmt

	// decl
	StructTypeDeclStmt *StructDeclStmt
	VarbDeclStmt       *VarbDeclStmt
	FnDeclStmt         *FuncDeclStmt
	// type
	Type *Type
}

func (node *Node) Format() []string {
	switch node.NodeType { //nolint:exhaustive
	case TypeIdentifier:
		return node.Identifier.Format()
	case TypeStringLiteral:
		return node.StringLiteral.Format()
	case TypeIntegerLiteral:
		return node.IntegerLiteral.Format()
	case TypeFloatLiteral:
		return node.FloatLiteral.Format()
	case TypeBoolLiteral:
		return node.BoolLiteral.Format()
	case TypeNilLiteral:
		return node.NilLiteral.Format()
	case TypeListInitExpr:
		return node.ListInitExpr.Format()
	case TypeMapInitExpr:
		return node.MapInitExpr.Format()
	case TypeParenExpr:
		return node.ParenExpr.Format()
	case TypeAttrExpr:
		return node.AttrExpr.Format()
	case TypeIndexExpr:
		return node.IndexExpr.Format()
	case TypeArithmeticExpr:
		return node.ArithmeticExpr.Format()
	case TypeConditionalExpr:
		return node.ConditionalExpr.Format()
	case TypeAssignmentExpr:
		return node.AssignmentExpr.Format()
	case TypeCallExpr:
		return node.CallExpr.Format()
	case TypeIfelseStmt:
		return node.IfelseStmt.Format()
	case TypeForStmt:
		return node.ForStmt.Format()
	case TypeForInStmt:
		return node.ForInStmt.Format()
	case TypeContinueStmt:
		return node.ContinueStmt.Format()
	case TypeBreakStmt:
		return node.BreakStmt.Format()
	case TypeStructTypeDecl:
		return node.StructTypeDeclStmt.Format()
	case TypeFuncDeclStmt:
		return node.FnDeclStmt.Format()
	case TypeVarbDecl:
		return node.VarbDeclStmt.Format()
	case TypeType:
		return node.Type.Format()
	}
	return []string{"node format failed"}
}
func (node *Node) String() string {
	switch node.NodeType { //nolint:exhaustive
	case TypeIdentifier:
		return node.Identifier.String()
	case TypeStringLiteral:
		return node.StringLiteral.String()
	case TypeIntegerLiteral:
		return node.IntegerLiteral.String()
	case TypeFloatLiteral:
		return node.FloatLiteral.String()
	case TypeBoolLiteral:
		return node.BoolLiteral.String()
	case TypeNilLiteral:
		return node.NilLiteral.String()
	case TypeListInitExpr:
		return node.ListInitExpr.String()
	case TypeMapInitExpr:
		return node.MapInitExpr.String()
	case TypeParenExpr:
		return node.ParenExpr.String()
	case TypeAttrExpr:
		return node.AttrExpr.String()
	case TypeIndexExpr:
		return node.IndexExpr.String()
	case TypeArithmeticExpr:
		return node.ArithmeticExpr.String()
	case TypeConditionalExpr:
		return node.ConditionalExpr.String()
	case TypeAssignmentExpr:
		return node.AssignmentExpr.String()
	case TypeCallExpr:
		return node.CallExpr.String()
	case TypeIfelseStmt:
		return node.IfelseStmt.String()
	case TypeForStmt:
		return node.ForStmt.String()
	case TypeForInStmt:
		return node.ForInStmt.String()
	case TypeContinueStmt:
		return node.ContinueStmt.String()
	case TypeBreakStmt:
		return node.BreakStmt.String()
	case TypeStructTypeDecl:
		return node.StructTypeDeclStmt.String()
	case TypeFuncDeclStmt:
		return node.FnDeclStmt.String()
	case TypeVarbDecl:
		return node.VarbDeclStmt.String()
	case TypeType:
		return node.Type.String()
	}
	return "node conv to string failed"
}

func WrapIdentifier(node *Identifier) *Node {
	return &Node{
		NodeType:   TypeIdentifier,
		Identifier: node,
	}
}

func WrapStringLiteral(node *StringLiteral) *Node {
	return &Node{
		NodeType:      TypeStringLiteral,
		StringLiteral: node,
	}
}

func WrapIntegerLiteral(node *IntegerLiteral) *Node {
	return &Node{
		NodeType:       TypeIntegerLiteral,
		IntegerLiteral: node,
	}
}

func WrapFloatLiteral(node *FloatLiteral) *Node {
	return &Node{
		NodeType:     TypeFloatLiteral,
		FloatLiteral: node,
	}
}

func WrapBoolLiteral(node *BoolLiteral) *Node {
	return &Node{
		NodeType:    TypeBoolLiteral,
		BoolLiteral: node,
	}
}

func WrapNilLiteral(node *NilLiteral) *Node {
	return &Node{
		NodeType:   TypeNilLiteral,
		NilLiteral: node,
	}
}

func WrapListInitExpr(node *ListInitExpr) *Node {
	return &Node{
		NodeType:     TypeListInitExpr,
		ListInitExpr: node,
	}
}

func WrapMapInitExpr(node *MapInitExpr) *Node {
	return &Node{
		NodeType:    TypeMapInitExpr,
		MapInitExpr: node,
	}
}

func WrapParenExpr(node *ParenExpr) *Node {
	return &Node{
		NodeType:  TypeParenExpr,
		ParenExpr: node,
	}
}

func WrapAttrExpr(node *AttrExpr) *Node {
	return &Node{
		NodeType: TypeAttrExpr,
		AttrExpr: node,
	}
}

func WrapIndexExpr(node *IndexExpr) *Node {
	return &Node{
		NodeType:  TypeIndexExpr,
		IndexExpr: node,
	}
}

func WrapArithmeticExpr(node *ArithmeticExpr) *Node {
	return &Node{
		NodeType:       TypeArithmeticExpr,
		ArithmeticExpr: node,
	}
}

func WrapConditionExpr(node *ConditionalExpr) *Node {
	return &Node{
		NodeType:        TypeConditionalExpr,
		ConditionalExpr: node,
	}
}

func WrapAssignmentExpr(node *AssignmentExpr) *Node {
	return &Node{
		NodeType:       TypeAssignmentExpr,
		AssignmentExpr: node,
	}
}

func WrapCallExpr(node *CallExpr) *Node {
	return &Node{
		NodeType: TypeCallExpr,
		CallExpr: node,
	}
}

func WrapIfelseStmt(node *IfelseStmt) *Node {
	return &Node{
		NodeType:   TypeIfelseStmt,
		IfelseStmt: node,
	}
}

func WrapForStmt(node *ForStmt) *Node {
	return &Node{
		NodeType: TypeForStmt,
		ForStmt:  node,
	}
}

func WrapForInStmt(node *ForInStmt) *Node {
	return &Node{
		NodeType:  TypeForInStmt,
		ForInStmt: node,
	}
}

func WrapContinueStmt(node *ContinueStmt) *Node {
	return &Node{
		NodeType:     TypeContinueStmt,
		ContinueStmt: node,
	}
}

func WrapBreakStmt(node *BreakStmt) *Node {
	return &Node{
		NodeType:  TypeBreakStmt,
		BreakStmt: node,
	}
}

func WrapeBlockStmt(node *BlockStmt) *Node {
	return &Node{
		NodeType:  TypeBlockStmt,
		BlockStmt: node,
	}
}

func WrapStructTypeDecl(node *StructDeclStmt) *Node {
	return &Node{
		NodeType:           TypeStructTypeDecl,
		StructTypeDeclStmt: node,
	}
}

func WrapVarbDecl(node *VarbDeclStmt) *Node {
	return &Node{
		NodeType:     TypeVarbDecl,
		VarbDeclStmt: &VarbDeclStmt{},
	}
}

func WrapFnDecl(node *FuncDeclStmt) *Node {
	return &Node{
		NodeType:   TypeFuncDeclStmt,
		FnDeclStmt: node,
	}
}

func WrapType(node *Type) *Node {
	return &Node{
		NodeType: TypeType,
		Type:     node,
	}
}

func (node *Node) StartPos() token.LnColPos {
	return NodeStartPos(node)
}

func NodeStartPos(node *Node) token.LnColPos {
	if node == nil {
		return token.InvalidLnColPos
	}
	switch node.NodeType {
	case TypeInvalid:
		return token.InvalidLnColPos
	case TypeIdentifier:
		return node.Identifier.Start
	case TypeStringLiteral:
		return node.StringLiteral.Start
	case TypeIntegerLiteral:
		return node.IntegerLiteral.Start
	case TypeFloatLiteral:
		return node.FloatLiteral.Start
	case TypeBoolLiteral:
		return node.BoolLiteral.Start
	case TypeNilLiteral:
		return node.NilLiteral.Start

	case TypeListInitExpr:
		return node.ListInitExpr.LBracket
	case TypeMapInitExpr:
		return node.MapInitExpr.LBrace

	case TypeParenExpr:
		return node.ParenExpr.LParen

	case TypeAttrExpr:
		return node.AttrExpr.Start

	case TypeIndexExpr:
		return node.IndexExpr.Obj.Start

	case TypeArithmeticExpr:
		return node.ArithmeticExpr.LHS.StartPos()
	case TypeConditionalExpr:
		return node.ConditionalExpr.LHS.StartPos()
	case TypeAssignmentExpr:
		return node.AssignmentExpr.LHS.StartPos()
	case TypeCallExpr:
		return node.CallExpr.NamePos

	case TypeBlockStmt:
		return node.BlockStmt.LBracePos

	case TypeIfelseStmt:
		if len(node.IfelseStmt.IfList) > 0 {
			return node.IfelseStmt.IfList[0].Start
		} else {
			return token.InvalidLnColPos
		}

	case TypeForStmt:
		return node.ForStmt.ForPos
	case TypeForInStmt:
		return node.ForInStmt.ForPos
	case TypeContinueStmt:
		return node.ContinueStmt.Start
	case TypeBreakStmt:
		return node.BreakStmt.Start
	}
	return token.InvalidLnColPos
}

func nodeFAppendTab(src, dst []string) []string {
	for i := range dst {
		src = append(src, TabStr+dst[i])
	}
	return src
}

func nodeFAppendConnect(src, dst []string) []string {
	for i := range dst {
		if i == 0 && len(src) != 0 {
			src[len(src)-1] = src[len(src)-1] + dst[i]
		} else {
			src = append(src, dst[i])
		}
	}
	return src
}
