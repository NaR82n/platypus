// Unless explicitly stated otherwise all files in this repository are licensed
// under the MIT License.
// This product includes software developed at Guance Cloud (https://www.guance.com/).
// Copyright 2021-present Guance, Inc.

package ast

import (
	"fmt"
	"strings"

	"github.com/GuanceCloud/platypus/pkg/token"
)

type IfelseStmt struct {
	IfList IfList
	Else   *BlockStmt

	ElsePos token.LnColPos
}

func (e *IfelseStmt) IsExpr() bool {
	return false
}

func (e *IfelseStmt) Format() []string {
	var lines []string
	if e.IfList != nil {
		lines = e.IfList.Format()
	} else {
		return lines
	}

	if e.Else != nil {
		lines = nodeFAppendConnect(lines, []string{" else "})
		lines = nodeFAppendConnect(lines, e.Else.Format())
	}

	return lines
}

func (e *IfelseStmt) String() string {
	return strings.Join(e.Format(), "\n")
}

// IfList index [0] is IF, [1..end] is ELIF.
type IfList []*IfStmtElem

func (e IfList) Format() []string {
	lines := []string{}
	if len(e) == 0 {
		return lines
	}
	lines = append(lines, "if ")
	lines = nodeFAppendConnect(lines, e[0].Format())

	for i := 1; i < len(e); i++ {
		lines = nodeFAppendConnect(lines, []string{" elif "})
		lines = nodeFAppendConnect(lines, e[i].Format())
	}

	return lines
}

func (e IfList) String() string {
	return strings.Join(e.Format(), "\n")
}

type IfStmtElem struct {
	Condition *Node
	Block     *BlockStmt

	Start token.LnColPos
}

func (e *IfStmtElem) Format() []string {
	lines := e.Condition.Format()
	lines = nodeFAppendConnect(lines, []string{" "})
	if e.Block != nil {
		lines = nodeFAppendConnect(lines, e.Block.Format())
	} else {
		lines = nodeFAppendConnect(lines, []string{"{\n}"})
	}
	return lines
}

func (e *IfStmtElem) String() string {
	return strings.Join(e.Format(), "\n")
}

type BreakStmt struct {
	Start token.LnColPos
}

func (e *BreakStmt) Format() []string {
	return []string{"break"}
}

func (e *BreakStmt) String() string {
	return "break"
}

type ContinueStmt struct {
	Start token.LnColPos
}

func (e *ContinueStmt) Format() []string {
	return []string{"continue"}
}

func (e *ContinueStmt) String() string {
	return "continue"
}

type ForInStmt struct {
	Varb *Node
	Iter *Node
	Body *BlockStmt

	ForPos token.LnColPos
	InPos  token.LnColPos
}

func (e *ForInStmt) Format() []string {
	lines := []string{"for "}

	if e.Varb != nil {
		lines = nodeFAppendConnect(lines, e.Varb.Format())
	}
	lines[len(lines)-1] += " in "

	if e.Iter != nil {
		lines = nodeFAppendConnect(lines, e.Iter.Format())
	}

	lines[len(lines)-1] += " {"
	lines = nodeFAppendTab(lines, e.Body.Format())
	lines = append(lines, "}")

	return lines
}

func (e *ForInStmt) String() string {
	return strings.Join(e.Format(), "\n")
}

type ForStmt struct {
	// init
	Init *Node

	// step1: -> step2 or break
	Cond *Node

	// step3: -> step1
	Loop *Node

	// step2: -> step3
	Body *BlockStmt

	ForPos token.LnColPos
}

func (e *ForStmt) Format() []string {
	lines := []string{"for "}

	if e.Init != nil {
		lines = nodeFAppendConnect(lines, e.Init.Format())
	}
	lines[len(lines)-1] += "; "

	if e.Cond != nil {
		lines = nodeFAppendConnect(lines, e.Cond.Format())
	}
	lines[len(lines)-1] += "; "

	if e.Loop != nil {
		lines = nodeFAppendConnect(lines, e.Loop.Format())
		lines[len(lines)-1] += " "
	}

	lines[len(lines)-1] += " {"
	lines = nodeFAppendTab(lines, e.Body.Format())
	lines = append(lines, "}")

	return lines
}

func (e *ForStmt) String() string {
	return strings.Join(e.Format(), "\n")
}

type BlockStmt struct {
	LBracePos token.LnColPos
	RBracePos token.LnColPos
	Stmts     Stmts
}

func (block *BlockStmt) Format() []string {
	var lines []string
	lines = append(lines, "{")
	for _, v := range block.Stmts {
		lines = nodeFAppendTab(lines, v.Format())
	}
	lines = append(lines, "}")
	return lines
}

func (block *BlockStmt) String() string {
	return strings.Join(block.Format(), "\n")
}

type VarbDeclStmt struct {
	LetPos token.LnColPos

	VarbDeclAndAssi [][3]*Node
}

func (e *VarbDeclStmt) Format() []string {
	lines := []string{""}

	for i, v := range e.VarbDeclAndAssi {
		if i == 0 {
			lines = nodeFAppendConcStr(lines, "let ")
		} else {
			lines = nodeFAppendConcStr(lines, ", ")
		}

		if v[0] != nil {
			lines = nodeFAppendConnect(lines, v[0].Format())
		}
		if v[1] != nil {
			lines = nodeFAppendConcStr(lines, ": ")
			lines = nodeFAppendTabConnect(lines, v[1].Format())
		}
		if v[2] != nil {
			lines = nodeFAppendConcStr(lines, " = ")
			lines = nodeFAppendTabConnect(lines, v[2].Format())
		}
	}

	return lines
}

func (e *VarbDeclStmt) String() string {
	return strings.Join(e.Format(), "\n")
}

type StructDeclStmt struct {
	Name string

	Fields []*StructField

	LBrace token.LnColPos
	RBrace token.LnColPos
}

func (decl *StructDeclStmt) Format() []string {
	v := []string{}
	if decl.Name != "" {
		v = append(v, fmt.Sprintf("struct %s {", decl.Name))
	} else {
		v = append(v, "struct {")
	}
	for i, f := range decl.Fields {
		line := TabStr + f.Name
		if decl.Fields[i].Type != nil {
			line += ": " + decl.Fields[i].Type.String()
		}
		if i < len(decl.Fields)-1 {
			line += ","
		}
		v = append(v, line)
	}
	v = append(v, "}")
	return v
}

func (decl *StructDeclStmt) String() string {
	return strings.Join(decl.Format(), "\n")
}

type StructField struct {
	Name string
	Type *Node
}

type FuncDeclStmt struct {
	Name  string
	Block *BlockStmt

	Param []*FnParam

	ReturnType []*Node

	FnPos   token.LnColPos
	NamePos token.LnColPos
	LParen  token.LnColPos
	RParen  token.LnColPos
}

type FnParam struct {
	Name       string
	Type       *Node
	DefaultVal *Node
}

func (p *FnParam) Format() []string {
	lines := []string{p.Name}
	if p.Type != nil {
		lines = nodeFAppendConcStr(lines, ": ")
		lines = nodeFAppendConnect(lines, p.Type.Format())
	}

	if p.DefaultVal != nil {
		lines = nodeFAppendConcStr(lines, " = ")
		lines = nodeFAppendConnect(lines, p.DefaultVal.Format())
	}

	return lines
}

func (p *FnParam) String() string {
	return strings.Join(p.Format(), "\n")
}

func (decl *FuncDeclStmt) Format() []string {
	lines := []string{"fn " + decl.Name + "("}

	for i, v := range decl.Param {
		if i != 0 {
			lines = nodeFAppendConcStr(lines, ", ")
		}
		lines = nodeFAppendConnect(lines, v.Format())
	}
	lines = nodeFAppendConcStr(lines, ") ")

	if len(decl.ReturnType) > 1 {
		lines = nodeFAppendConcStr(lines, "-> (")
		for i, v := range decl.ReturnType {
			if i != 0 {
				lines = nodeFAppendConcStr(lines, ", ")
			}
			lines = nodeFAppendConnect(lines, v.Format())
		}
		lines = nodeFAppendConcStr(lines, ") ")
	} else if len(decl.ReturnType) == 1 {
		lines = nodeFAppendConcStr(lines, "-> ")
		lines = nodeFAppendConnect(lines, decl.ReturnType[0].Format())
		lines = nodeFAppendConcStr(lines, " ")
	}

	lines = nodeFAppendConnect(lines, decl.Block.Format())

	return lines
}

func (decl *FuncDeclStmt) String() string {
	return strings.Join(decl.Format(), "\n")
}
