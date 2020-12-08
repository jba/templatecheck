// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"strings"
	"testing"
	ttmpl "text/template"
	"text/template/parse"
)

var debug = flag.Bool("debug", false, "display extra debug output")

type S struct {
	B     bool
	Bs    []byte
	I     int
	P     *S
	R     io.Reader
	F     interface{}
	unexp int
}

func (s S) Add(x int) int { return s.I + x }

func TestCheck(t *testing.T) {
	const (
		noX          = "can't use field X"
		noI          = "can't use field I in type"
		undef        = "undefined variable"
		conservative = "CONSERVATIVE"
	)

	newChan := func() chan S {
		c := make(chan S, 1)
		c <- S{}
		close(c)
		return c
	}

	funcs := ttmpl.FuncMap{
		"pluralize": func(i int, s string) string { return "" },
		"add1":      func(x int) int { return x + 1 },
		"variadic":  func(x int, ys ...string) string { return "" },
		"nilary":    func() *S { return &S{P: &S{}} },
	}

	for _, test := range []struct {
		name     string
		contents string
		dot      interface{}
		want     string // if non-empty, error should contain it
	}{
		{"field", `{{.B}}`, S{}, ""},
		{"field ptr", `{{.B}}`, &S{}, ""},
		{"field iface", `{{.B}}`, *new(interface{}), ""},
		{"no field", `{{.X}}`, S{}, noX},
		{"no field ptr", `{{.X}}`, &S{}, noX},
		{"unexported", `{{.unexp}}`, S{}, "unexported field"},
		{"method ok", `{{.Add 1}}`, S{}, ""},
		{"method too few", `{{.Add}}`, S{}, "want 1, got 0"},
		{"method too many", `{{.Add 1 2}}`, S{}, "want 1, got 2"},
		{"method interface ok", `{{.R.Read .Bs}}`, S{R: strings.NewReader("x")}, ""},
		{"method interface too few", `{{.R.Read}}`, S{}, "want 1, got 0"},
		{"method interface too many", `{{.R.Read 1 true ""}}`, S{}, "want 1, got 3"},
		{"not a struct", `{{.B.I}}`, S{}, noI},
		{"not a func", `{{.I 1}}`, S{}, "cannot be invoked"},
		{"nested", `{{.P.P.P.X}}`, S{}, noX},
		{"map key ok", `{{.X.I}}`, map[string]S{}, ""},
		{"map key no field", `{{.X.X}}`, map[string]S{}, noX},
		{"map key arg", `{{.X 1}}`, map[string]S{}, "is not a method but has arguments"},
		{"map key bad type", `{{.X}}`, map[bool]string{}, "bad"},
		{"decl ok", `{{$x := .}}{{$x.B}}`, S{}, ""},
		{"decl", `{{$x := .}}{{$x.X}}`, S{}, noX},
		{"decl bool", `{{$x := true}}{{$x.I}}`, S{}, noI},
		{"decl int", `{{$x := 1}}{{$x.I}}`, S{}, noI},
		{"not a func var", `{{$x := 1}}{{$x 1}}`, S{}, "can't give argument to non-function"},
		{"not a func pipe", `{{$x := 1}}{{1 | $x}}`, S{}, "can't give argument to non-function"},
		{"with", `{{with .P}}{{.X}}{{end}}`, S{P: &S{}}, noX},
		{"with else", `{{with .P}}{{.B}}{{else}}{{.X}}{{end}}`, S{}, noX},
		{"with dollar", `{{with .B}}{{$.X}}{{end}}`, S{B: true}, noX},
		{"if", `{{if .P}}{{.P.X}}{{end}}`, S{P: &S{}}, noX},
		{"ifelse", `{{if .P}}{{.B}}{{else}}{{.X}}{{end}}`, S{}, noX},
		{"range slice ok", `{{range .}}{{.B}}{{end}}`, make([]S, 1), ""},
		{"range slice", `{{range .}}{{.X}}{{end}}`, make([]S, 1), noX},
		{"range map", `{{range .}}{{.X}}{{end}}`, map[string]S{"X": S{}}, noX},
		{"range chan ok", `{{range .}}{{.I}}{{end}}`, newChan(), ""},
		{"range chan", `{{range .}}{{.X}}{{end}}`, newChan(), noX},
		{"range chan send", `{{range .}}{{end}}`, make(chan<- S), "over send-only channel"},
		{"range one var", `{{range $e := .}}{{$e.X}}{{end}}`, make([]S, 1), noX},
		{"range two vars", `{{range $k, $e := .}}{{$e.X}}{{end}}`, map[string]S{"x": S{}}, noX},
		{"range two vars 2", `{{range $k, $e := .}}{{$k.I}}{{end}}`, map[bool]string{true: "x"}, noI},
		{"range bad type", `{{range 1}}{{end}}`, nil, "can't iterate over type"},
		{"range iface", `{{range .F}}{{end}}`, S{F: 1}, conservative},
		{"chain ok", `{{(.P).I}}`, S{P: &S{}}, ""},
		{"chain bool", `{{(true).I}}`, S{}, noI},
		{"chain no field", `{{(.P).X}}`, S{}, noX},
		{"chain no struct", `{{(.B).I}}`, S{}, noI},
		{"chain map ok", `{{(.K).I}}`, map[string]S{}, ""},
		{"chain map", `{{(.K).X}}`, map[string]S{}, noX},
		{"chain pipe", `{{((.B) | printf).I}}`, S{}, noI},
		{"chain ident ok", `{{nilary.P.I}}`, nil, ""},
		{"chain ident", `{{nilary.P.X}}`, nil, noX},
		{"assign same type", `{{$v := 1}}{{$v = 2}}{{$v.I}}`, nil, noI},
		{"assign diffrent type", `{{$v := 1}}{{$v = ""}}{{$v.I}}`, nil, noI},
		{"func args few", `{{and}}`, nil, "want at least 1, got 0"},
		{"func args many", `{{le 1 2 3}}`, nil, "want 2, got 3"},
		{"len", `{{(len .).I}}`, map[string]S{}, noI},
		{"len arg too many", `{{add1 (len 1 2)}}`, nil, "want 1, got 2"},
		{"userfunc ok", `{{add1 3}}`, nil, ""},
		{"userfunc too few", `{{add1}}`, nil, "want 1, got 0"},
		{"userfunc wrong type", `{{$v := "y"}}{{add1 $v}}`, nil, "expected int; found string"},
		{"variadic", `{{variadic 1 2}}`, nil, "expected string; found 2"},
		{"undefined", `{{$x = 1}}`, nil, undef}, // parser catches references, but not assignments
		{"arg var", `{{$v := 1}}{{add1 $v}}`, nil, ""},
		{"arg nil", `{{add1 nil}}`, nil, "cannot assign nil to int"},
		{"arg reflect.Value ok", `{{add1 (and 1)}}`, nil, ""},
		{"arg reflect.Value", `{{add1 (and "x")}}`, nil, conservative},
		{
			"nested decl", // variable redeclared in an inner scope; doesn't affect outer scope
			`
				{{$v := 1}}
				{{if .}}
					{{$v := .}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			noI,
		},
		{
			"if assign", // assignments from the `if` pipeline stick
			`
				{{$v := .}}
				{{if $v = 1}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			noI,
		},
		{
			"if decl", // declarations from the `if` pipeline do not
			`
				{{$v := .}}
				{{if $v := 1}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			"",
		},
		{
			"if assign same type", // variable assigned to same type in conditional
			`
				{{$v := 1}}
				{{if .}}
					{{$v = 2.5}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			noI,
		},
		{
			"if assign different type", // variable assigned to different type in conditional
			`
				{{$v := 1}}
				{{if .}}
					{{$v = true}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			conservative, // be conservative, do not warn even though it's an error
		},
		{
			"if assign same type else",
			`
				{{$v := 1}}
				{{if .}}
					{{$v = 2.5}}
				{{else}}
					{{$v = 3i}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			noI,
		},
		{
			"if assign different type else",
			`
				{{$v := 1}}
				{{if .}}
					{{$v = 2.5}}
				{{else}}
					{{$v = true}}
				{{end}}
				{{$v.I}}
			`,
			S{},
			conservative,
		},
		{
			"range assign same type",
			`
				{{$v := 1}}
				{{range .}}
					{{$v = 2i}}
				{{end}}
				{{$v.I}}
			`,
			map[string]S{},
			noI,
		},
		{
			"range assign different type",
			`
				{{$v := 1}}
				{{range .}}
					{{$v = ""}}
				{{end}}
				{{$v.I}}
			`,
			map[string]S{},
			conservative,
		},
		{
			"range else same type",
			`
				{{$v := 1}}
				{{range .}}
					{{$v = 2}}
				{{else}}
					{{$v = 3}}
				{{end}}
				{{$v.I}}
			`,
			map[string]S{},
			noI,
		},
		{
			"range else different type",
			`
				{{$v := 1}}
				{{range .}}
					{{$v = 2}}
				{{else}}
					{{$v = ""}}
				{{end}}
				{{$v.I}}
			`,
			map[string]S{},
			conservative,
		},
		{
			"template call ok",
			`
				{{template "foo" .}}
				{{define "foo"}}{{.I}}{{end}}
			`,
			S{},
			"",
		},
		{
			"template call no x",
			`
				{{template "foo" .}}
				{{define "foo"}}{{.X}}{{end}}
			`,
			S{},
			noX,
		},
		{
			"template call undef",
			`
				{{template "bar" .}}
				{{define "foo"}}{{.X}}{{end}}
			`,
			S{},
			`template "bar" not defined`,
		},
		{
			"recursion",
			`
				{{template "links" .}}
				{{define "links"}}{{if .}}{{template "link" .}}{{end}}{{end}}
				{{define "link"}}{{.I}} {{template "links" .P}}{{end}}
			`,
			S{I: 1, P: &S{I: 2}},
			"",
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			tmpl, err := ttmpl.New(test.name).
				Funcs(funcs).
				Option("missingkey=zero").
				Parse(test.contents)
			if err != nil {
				t.Fatal("while parsing:", err)
			}
			if *debug {
				fmt.Printf("%s =>\n", test.contents)
				dump(tmpl.Root, 0)
			}
			err = CheckText(tmpl, test.dot, funcs)
			if err != nil {
				if test.want == "" || test.want == conservative {
					t.Fatalf("got %v, wanted no error", err)
				}
				if !strings.Contains(err.Error(), test.want) {
					t.Fatalf("%q not contained in %q", test.want, err)
				}
			} else if test.want != "" && test.want != conservative {
				t.Fatalf("got nil, want error containing %q", test.want)
			}
			// Execute the template to make sure we get the same error state.
			terr := safeExec(tmpl, test.dot)
			if err == nil && terr != nil && test.want != conservative {
				t.Fatalf("Check suceeded but Execute failed with %v", terr)
			}
			if err != nil && terr == nil {
				t.Fatalf("Execute succeeded but Check failed with %v", err)
			}
			if err == nil && terr == nil && test.want == conservative {
				t.Fatal("Check conservatively succeeded but Execute did too, and should have failed")
			}
		})
	}
}

func safeExec(tmpl *ttmpl.Template, dot interface{}) (err error) {
	// Execute panics on a send-only channel: golang/go#43065
	defer func() {
		if e := recover(); e != nil {
			err = fmt.Errorf("panic: %v", e)
		}
	}()

	return tmpl.Execute(ioutil.Discard, dot)
}

func TestUndefinedFunction(t *testing.T) {
	// If the user doesn't pass the same FuncMap to Check, then it could
	// return an "undefined function" error. Otherwise, the parser will catch it.
	tmpl, err := ttmpl.New("").Funcs(ttmpl.FuncMap{"f": func() int { return 0 }}).Parse(`{{f}}`)
	if err != nil {
		t.Fatal("while parsing:", err)
	}
	err = CheckText(tmpl, nil)
	got := err.Error()
	const want = "is not a defined function"
	if !strings.Contains(got, want) {
		t.Errorf("%q not contained in %q", want, got)
	}
}

func dump(n parse.Node, level int) {
	for i := 0; i < level; i++ {
		fmt.Print("  ")
	}
	fmt.Printf("%T", n)
	switch n := n.(type) {
	case *parse.ActionNode:
		fmt.Println()
		dump(n.Pipe, level+1)
	case *parse.ChainNode:
		fmt.Printf(" %q\n", n.Field)
		dump(n.Node, level+1)
	case *parse.CommandNode:
		fmt.Println()
		for _, arg := range n.Args {
			dump(arg, level+1)
		}
	case *parse.ListNode:
		fmt.Println()
		for _, c := range n.Nodes {
			dump(c, level+1)
		}
	case *parse.PipeNode:
		fmt.Printf(" IsAssign: %t, decl: %v\n", n.IsAssign, n.Decl)
		for _, c := range n.Cmds {
			dump(c, level+1)
		}
	case *parse.VariableNode:
		fmt.Printf(" %q\n", n.Ident)
	default:
		fmt.Printf(" %s\n", n)
	}

}
