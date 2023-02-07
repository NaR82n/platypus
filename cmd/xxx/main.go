package main

import (
	"fmt"

	"github.com/GuanceCloud/platypus/pkg/parser"
)

var scripts = []string{
	`let a: int= 2, c = {"a": 1}, b: map[string][]int = {}, d: [][]map[type_a]float = nil`,
	`struct a {x, v: int, b: a, z:any, d: map[string][]map[string]int}`,
	`fn test(a: int, b, c = 2, d: []int= [1,2]) -> ac_t {
	print("a")
}

struct ac_t {
	a: int,
	b: float,
	c: any,
	d,
	e: []map[int]x
}

`,
}

func main() {
	for i, v := range scripts {
		s, e := parser.ParsePipeline(fmt.Sprintf("%d.p", i), v)
		if e != nil {
			fmt.Println(e)
			fmt.Print("\n\n")
			return
		}

		fmt.Println("input: -->\n" + v)
		fmt.Println("format: -->")
		fmt.Println(s.String())
		fmt.Print("\n\n")

	}
}
