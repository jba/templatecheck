// Copyright 2023 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	"io"
	"testing"
	ttmpl "text/template"
)

type S2 struct {
	I int
	F float64
}

func TestNewChecked(t *testing.T) {
	tt := ttmpl.Must(ttmpl.New("").Parse(`{{.F}}`))
	ct := MustChecked[S2](tt)
	if err := ct.Execute(io.Discard, S2{}); err != nil {
		t.Fatal(err)
	}
}
