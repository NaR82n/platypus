// Unless explicitly stated otherwise all files in this repository are licensed
// under the MIT License.
// This product includes software developed at Guance Cloud (https://www.guance.com/).
// Copyright 2021-present Guance, Inc.

package ast

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/GuanceCloud/grok"
	"github.com/GuanceCloud/platypus/pkg/token"
)

type Op string

const (
	ADD Op = "+"
	SUB Op = "-"
	MUL Op = "*"
	DIV Op = "/"
	MOD Op = "%"

	// XOR Op = "^"
	// ~~~ POW Op = "^" ~~~.

	EQEQ Op = "=="
	NEQ  Op = "!="
	LTE  Op = "<="
	LT   Op = "<"
	GTE  Op = ">="
	GT   Op = ">"

	AND Op = "&&"
	OR  Op = "||"

	EQ Op = "="
)

type Identifier struct {
	Name  string
	Start token.LnColPos
}

func (e *Identifier) Format() []string {
	return []string{e.Name}
}

func (e *Identifier) String() string {
	return e.Name
}

type StringLiteral struct {
	Val   string
	Start token.LnColPos
}

func (e *StringLiteral) Format() []string {
	return []string{fmt.Sprintf("'%s'", e.Val)}
}

func (e *StringLiteral) String() string {
	return fmt.Sprintf("'%s'", e.Val)
}

type IntegerLiteral struct {
	Val   int64
	Start token.LnColPos
}

func (e *IntegerLiteral) Format() []string {
	return []string{fmt.Sprintf("%d", e.Val)}
}

func (e *IntegerLiteral) String() string {
	return fmt.Sprintf("%d", e.Val)
}

type FloatLiteral struct {
	Val   float64
	Start token.LnColPos
}

func (e *FloatLiteral) Format() []string {
	return []string{fmt.Sprintf("%f", e.Val)}
}

func (e *FloatLiteral) String() string {
	return fmt.Sprintf("%f", e.Val)
}

type BoolLiteral struct {
	Val   bool
	Start token.LnColPos
}

func (e *BoolLiteral) Format() []string {
	return []string{fmt.Sprintf("%v", e.Val)}
}

func (e *BoolLiteral) String() string {
	return fmt.Sprintf("%v", e.Val)
}

type NilLiteral struct {
	Start token.LnColPos
}

func (e *NilLiteral) Format() []string {
	return []string{"nil"}
}

func (e *NilLiteral) String() string {
	return "nil"
}

type MapInitExpr struct {
	Type *Node

	KeyValeList [][2]*Node // key,value list
	LBrace      token.LnColPos
	RBrace      token.LnColPos
}

func (e *MapInitExpr) Format() []string {
	lines := []string{}

	if e.Type != nil {
		lines = e.Type.Format()
	}

	lines = nodeFAppendConnect(lines, []string{"{"})

	for _, item := range e.KeyValeList {
		lines = append(lines, TabStr+item[0].String()+": "+item[1].String()+",")
	}
	lines = append(lines, "}")

	return lines
}

func (e *MapInitExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type ListInitExpr struct {
	Type *Node

	List     []*Node
	LBracket token.LnColPos
	RBracket token.LnColPos
}

func (e *ListInitExpr) Format() []string {
	lines := []string{"["}
	for i, elem := range e.List {
		if i == 0 {
			lines = nodeFAppendConnect(lines, elem.Format())
		} else {
			lines = nodeFAppendConnect(lines, []string{", "})
			lines = nodeFAppendConnect(lines, elem.Format())
		}
	}
	lines = nodeFAppendConnect(lines, []string{"]"})
	return lines
}

func (e *ListInitExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type ConditionalExpr struct {
	Op       Op
	LHS, RHS *Node
	OpPos    token.LnColPos
}

func (e *ConditionalExpr) Format() []string {
	lines := e.LHS.Format()
	lines = nodeFAppendConnect(lines, []string{" " + string(e.Op) + " "})
	lines = nodeFAppendConnect(lines, e.RHS.Format())
	return lines
}

func (e *ConditionalExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type ArithmeticExpr struct {
	Op       Op
	LHS, RHS *Node
	OpPos    token.LnColPos
}

func (e *ArithmeticExpr) Format() []string {
	lines := e.LHS.Format()
	lines = nodeFAppendConnect(lines, []string{" " + string(e.Op) + " "})
	lines = nodeFAppendConnect(lines, e.RHS.Format())
	return lines
}

func (e *ArithmeticExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type AttrExpr struct {
	Obj   *Node
	Attr  *Node
	Start token.LnColPos
}

func (e *AttrExpr) Format() []string {
	lines := []string{}

	if e.Attr != nil {
		lines = e.Attr.Format()
		lines = nodeFAppendConnect(lines, []string{"."})
	}

	if e.Obj != nil {
		lines = nodeFAppendConnect(lines, e.Obj.Format())
	}

	return lines
}

func (e *AttrExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type IndexExpr struct {
	Obj      *Identifier
	Index    []*Node // int float string bool
	LBracket []token.LnColPos
	RBracket []token.LnColPos
}

func (e *IndexExpr) Format() []string {
	lines := []string{}

	if e.Obj != nil {
		lines = e.Obj.Format()
		lines = nodeFAppendConnect(lines, []string{"."})
	}

	for _, elem := range e.Index {
		lines = nodeFAppendConnect(lines, []string{"["})
		lines = nodeFAppendConnect(lines, elem.Format())
		lines = nodeFAppendConnect(lines, []string{"]"})
	}

	return lines
}

func (e *IndexExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type ParenExpr struct {
	Param  *Node
	LParen token.LnColPos
	RParen token.LnColPos
}

func (e ParenExpr) Format() []string {
	lines := []string{"("}
	lines = nodeFAppendConnect(lines, e.Param.Format())
	lines = nodeFAppendConnect(lines, []string{")"})
	return lines
}

func (e *ParenExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type CallExpr struct {
	// TODO
	// Name *ast.Node

	// temporary record function name location
	NamePos token.LnColPos // as 'Start' (token.FilePos)

	LParen token.LnColPos
	RParen token.LnColPos

	Name string

	Param []*Node

	PrivateData interface{}

	// ParamIndex []int

	Grok *grok.GrokRegexp
	Re   *regexp.Regexp
}

func (e *CallExpr) Format() []string {
	lines := []string{e.Name + "("}
	for i, p := range e.Param {
		if i != 0 {
			lines = nodeFAppendConnect(lines, []string{", "})
		}
		lines = nodeFAppendConnect(lines, p.Format())
	}
	lines = nodeFAppendConnect(lines, []string{")"})
	return lines
}

func (e *CallExpr) String() string {
	return strings.Join(e.Format(), "\n")
}

type AssignmentExpr struct {
	LHS, RHS *Node
	OpPos    token.LnColPos
}

func (e *AssignmentExpr) Format() []string {
	lines := []string{}
	if e.LHS != nil {
		lines = e.LHS.Format()
	}
	lines = nodeFAppendConnect(lines, []string{" = "})
	if e.RHS != nil {
		lines = nodeFAppendConnect(lines, e.RHS.Format())
	}
	return lines
}

func (e *AssignmentExpr) String() string {
	return strings.Join(e.Format(), "\n")
}
