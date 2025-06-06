// Unless explicitly stated otherwise all files in this repository are licensed
// under the MIT License.
// This product includes software developed at Guance Cloud (https://www.guance.com/).
// Copyright 2021-present Guance, Inc.

package funcs

import (
	"testing"
	"time"

	"github.com/GuanceCloud/platypus/pkg/inimpl/guancecloud/input"
	"github.com/stretchr/testify/assert"
)

func TestUse(t *testing.T) {
	case1 := map[string]string{
		"a.ppl": "if true {use(\"b.ppl\")}",
		"b.ppl": "add_key(b,1)",
		"d.ppl": "use(\"c.ppl\")",
		"c.ppl": "use(\"a.ppl\") use(\"d.ppl\") use(\"fcName.ppl\")",
	}

	ret := [2][]string{
		{"a.ppl", "b.ppl"},
		{"d.ppl", "c.ppl"},
	}

	timenow := time.Now()

	retCheck := input.Point{
		Measurement: "default",
		Tags: map[string]string{
			"ax": "1",
		},
		Fields: map[string]interface{}{
			"b": int64(1),
		},
		Drop: false,
	}

	ret1, ret2 := NewTestingRunner2(case1)
	assert.Equal(t, len(ret[0]), len(ret1))
	assert.Equal(t, len(ret[1]), len(ret2))

	for _, v := range ret[0] {
		if _, ok := ret1[v]; !ok {
			t.Error(v)
		}
	}

	for _, v := range ret[1] {
		if _, ok := ret2[v]; !ok {
			t.Error(v)
		}
	}

	for _, name := range ret[0] {
		m, tags, f, _, drop, err := runScript(ret1[name], "default", map[string]string{
			"ax": "1",
		}, nil, timenow)
		if err != nil {
			t.Error(err)
			continue
		}
		assert.Equal(t, retCheck.Tags, tags)
		assert.Equal(t, retCheck.Fields, f)
		assert.Equal(t, retCheck.Drop, drop)
		assert.Equal(t, retCheck.Measurement, m)
	}
}
