// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	"flag"
	"fmt"
	"go/version"
	htmpl "html/template"
	"io"
	"os"
	"regexp"
	"runtime"
	"strings"
	"testing"
	ttmpl "text/template"
	"text/template/parse"

	stmpl "github.com/google/safehtml/template"
)

var debug = flag.Bool("debug", false, "display extra debug output")

type S struct {
	B     bool
	Bs    []byte
	I     int
	P     *S
	R     io.Reader
	Any   any
	A     [4]int
	unexp int
}

func (s S) Add(x int) int { return s.I + x }

type testCase struct {
	name     string
	contents string
	dot      any
	want     string // if non-empty, error should contain it
}

const conservative = "CONSERVATIVE" // Check succeeds but Execute fails

var funcs = ttmpl.FuncMap{
	"add1":         func(x int) int { return x + 1 },
	"args":         func(bool, uint, float64, complex128) int { return 0 },
	"intptr":       func(x *int) int { return 0 },
	"variadic":     func(x int, ys ...string) string { return "" },
	"nilary":       func() *S { return &S{P: &S{}} },
	"float":        func(x any) float64 { return x.(float64) },
	"emptyiface":   func(any) int { return 0 },
	"iface":        func(io.Reader) int { return 0 },
	"structure":    func(S) int { return 0 },
	"stdin":        func() io.Reader { return os.Stdin },
	"stringReader": func(s string) *strings.Reader { return strings.NewReader(s) },
}

func TestCheck(t *testing.T) {
	const (
		noX          = "can't use field X"
		noI          = "can't use field I in type"
		conservative = "CONSERVATIVE" // Check succeeds but Execute fails
	)

	newChan := func() chan S {
		c := make(chan S, 1)
		c <- S{}
		close(c)
		return c
	}

	for _, test := range []testCase{
		{"field", `{{.B}}`, S{}, ""},
		{"field ptr", `{{.B}}`, &S{}, ""},
		{"field unknown", `{{.B}}`, *new(any), ""},
		{"field iface", `{{.Any.B}}`, S{Any: 1}, conservative},
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
		{"range map", `{{range .}}{{.X}}{{end}}`, map[string]S{"X": {}}, noX},
		{"range chan ok", `{{range .}}{{.I}}{{end}}`, newChan(), ""},
		{"range chan", `{{range .}}{{.X}}{{end}}`, newChan(), noX},
		{"range chan send", `{{range .}}{{end}}`, make(chan<- S), "over send-only channel"},
		{"range one var", `{{range $e := .}}{{$e.X}}{{end}}`, make([]S, 1), noX},
		{"range two vars", `{{range $k, $e := .}}{{$e.X}}{{end}}`, map[string]S{"x": {}}, noX},
		{"range two vars 2", `{{range $k, $e := .}}{{$k.I}}{{end}}`, map[bool]string{true: "x"}, noI},
		{"range bad type", `{{range 1.5}}{{end}}`, nil, "can't iterate over type"},
		{"range iface", `{{range .Any}}{{end}}`, S{Any: 1.5}, conservative},
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

		{"func ok", `{{add1 3}}`, nil, ""},
		{"func too few", `{{add1}}`, nil, "want 1, got 0"},
		{"func wrong type", `{{$v := "y"}}{{add1 $v}}`, nil, "expected int; found string"},
		{"undefined", `{{$x = 1}}`, nil, "undefined variable"}, // parser catches references, but not assignments

		{"break", `{{range .A}}{{break}}{{end}}`, S{}, ""},
		{"continue", `{{range .A}}{{if false}}{{continue}}{{end}}{{end}}`, S{}, ""},
		// function arguments
		{"arg var", `{{$v := 1}}{{add1 $v}}`, nil, ""},
		{"arg ptr", `{{add1 .}}`, new(int), ""},
		{"arg addr", `{{intptr .I}}`, &S{}, ""},
		{"arg reflect.Value ok", `{{add1 (and 1)}}`, nil, ""},
		{"arg reflect.Value", `{{add1 (and "x")}}`, nil, conservative},
		{"arg reflect.Value nil", `{{and nil}}`, nil, ""},
		{"arg ident ok", `{{and nilary}}`, nil, ""},
		{"arg ident", `{{add1 nilary}}`, nil, "expected int; found *templatecheck.S"},
		{"arg chain ok", `{{add1 nilary.I}}`, nil, ""},
		{"arg chain", `{{add1 nilary.B}}`, nil, "expected int; found bool"},
		{"arg nil", `{{add1 nil}}`, nil, "cannot assign nil to int"},
		{"arg nil ok", `{{intptr nil}}`, nil, ""},
		{"arg int", `{{add1 true}}`, nil, "expected int; found true"},
		{"arg bool", `{{args 0 0 0 0}}`, nil, "expected bool; found 0"},
		{"arg uint", `{{args true false 0 0}}`, nil, "expected uint; found false"},
		{"arg float", `{{args true 0 false 0}}`, nil, "expected float64; found false"},
		{"arg complex", `{{args true 0 0 false}}`, nil, "expected complex128; found false"},
		{"arg complex ok", `{{args true 0 0 1i}}`, nil, ""},
		{"arg string", `{{variadic 1 2}}`, nil, "expected string; found 2"},
		{"arg string ok", `{{variadic 1 "x"}}`, nil, ""},
		{"arg iface", `{{add1 .Any}}`, S{}, conservative},
		{"arg empty iface formal", `{{emptyiface 1}}`, nil, ""},    // any arg is OK
		{"arg nonempty iface", `{{iface 1}}`, nil, "can't handle"}, // non-empty iface args never OK
		{"arg struct", `{{structure 1}}`, nil, "can't handle"},     // struct args never OK

		// len builtin
		{"len", `{{(len .).I}}`, map[string]S{}, noI},
		{"len arg too many", `{{add1 (len 1 2)}}`, nil, "want 1, got 2"},
		{"len nil", `{{len nil}}`, nil, "len of nil"},
		{"len num", `{{len 3+2i}}`, nil, "len of 3+2i"},
		{"len ptr", `{{len .}}`, &[1]int{0}, ""},
		{"len struct", `{{len .}}`, S{}, "len of type templatecheck.S"},
		{"len iface", `{{len .Any}}`, S{Any: 1}, conservative},

		// index builtin
		{"index no indexes", `{{index 1}}`, nil, ""},       // anything OK if no indexes...
		{"index nil", `{{index nil}}`, nil, "untyped nil"}, // except literal nil
		{"index index nil", `{{index "x" nil}}`, nil, "cannot index slice/array with nil"},
		{"index bad type", `{{index . 0}}`, S{}, "can't index item of type templatecheck.S"},
		{"index string ok", `{{index "x" 0}}`, nil, ""},
		{"index string bool", `{{index "x" true}}`, nil, "cannot index slice/array with type bool"},
		{"index string float", `{{index "x" 1.0}}`, nil, "cannot index slice/array with type float64"},
		{"index slice ok", `{{index . 0}}`, []int{1}, ""},
		{"index slice 2 ok", `{{index . 0 0}}`, [][]int{{1}}, ""},
		{"index slice 2", `{{index . 0 "a"}}`, [][]int{{1}}, "cannot index"},
		{"index map ok ", `{{index . "x"}}`, map[string]int{}, ""},
		{"index map", `{{index . 1}}`, map[string]int{}, "has type int; should be string"},
		{"index map 2", `{{index . "x" 1}}`, map[string]int{}, "can't index item of type"},
		{"index map slice", `{{index . "x" 0}}`, map[string][]int{"x": {1}}, ""},
		{"index map nil", `{{index . nil}}`, map[string]int{}, "value is nil; should be"},
		{"index map nil ok", `{{index . nil}}`, map[*string]int{}, ""},
		{"index indirect", `{{index . 0}}`, &[1]int{1}, ""},
		{"index int conversion", `{{index . (len "")}}`, map[uint]int{1.0: 1}, ""},

		// slice builtin
		{"slice no indexes ok", `{{slice "x"}}`, nil, ""},
		{"slice bad type", `{{slice 1}}`, nil, "can't slice item of type int"},
		{"slice nil", `{{slice nil}}`, nil, "index of untyped nil"},
		{"slice too many", `{{slice "x" 1 2 3 4}}`, nil, "too many slice indexes"},
		{"slice ok", `{{slice "x" 1 2.0}}`, nil, "cannot index"},
		{"slice string 3", `{{slice "x" 1 2 3}}`, nil, "cannot 3-index slice a string"},
		{"slice array", `{{slice .A 1 2 3}}`, &S{}, ""},
		{"slice bad index", `{{slice "x" "y"}}`, nil, "cannot index slice/array with type string"},

		// eq/neq builtins
		{"eq ok", `{{eq 1 2 }}`, nil, ""},
		{"eq too few", `{{eq 1}}`, nil, "missing"},
		{"eq func type", `{{eq . 1}}`, func() {}, conservative}, // we could notice that other arg cannot be nil
		{"eq struct type", `{{eq . .}}`, S{}, "uncomparable type"},

		// ordered comparisons
		{"le ok", `{{le 1 2}}`, nil, ""},
		{"le nil", `{{le 1 nil}}`, nil, "cannot compare values of type untyped nil"},
		{"le bad type", `{{le "x" 2+3i}}`, nil, "cannot compare values of type complex"},
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
			// Since $v has the same type (numberType: we don't distinguish kinds of numbers)
			// on both execution paths, we know its type afterwards and can detect that
			// a field reference will fail.
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
			// Since $v is assigned different types on both execution paths, we know nothing
			// about its type afterwards.
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
			// Again, same type on both paths. The original type of $v doesn't matter.
			`
				{{$v := .}}
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
				{{$v := .}}
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
				{{$v := .}}
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
		{
			"unknown arg type",
			`
				{{$v := 1}}
				{{if .}}
					{{$v = true}}
				{{end}}  {{/* v's type unknown here */}}
				{{add1 $v}}
			`,
			true,
			conservative,
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			testTemplate(t, test, false)
		})
	}
}

func testTemplate(t *testing.T, test testCase, strict bool) {
	t.Helper()
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
	if strict {
		err = CheckTextStrict(tmpl, test.dot)
	} else {
		err = CheckText(tmpl, test.dot)
	}
	if err != nil {
		if test.want == "" || test.want == conservative {
			t.Fatalf("failed with %v, wanted success", err)
		}
		if !strings.Contains(err.Error(), test.want) {
			t.Fatalf("%q not contained in %q", test.want, err)
		}
	} else if test.want != "" && test.want != conservative {
		t.Fatalf("succeeded, want error containing %q", test.want)
	}
	// Execute the template to make sure we get the same error state.
	terr := safeExec(tmpl, test.dot)
	if err == nil && terr != nil && test.want != conservative {
		t.Fatalf("Check suceeded but Execute failed with %v", terr)
	}
	if !strict {
		if err != nil && terr == nil {
			t.Fatalf("Execute succeeded but Check failed with %v", err)
		}
		if err == nil && terr == nil && test.want == conservative {
			t.Fatal("Check conservatively succeeded but Execute did too, and should have failed")
		}
	}
}

func safeExec(tmpl *ttmpl.Template, dot any) (err error) {
	// Execute panics on a send-only channel: golang/go#43065
	defer func() {
		if e := recover(); e != nil {
			err = fmt.Errorf("panic: %v", e)
		}
	}()

	return tmpl.Execute(io.Discard, dot)
}

func TestCheckStrict(t *testing.T) {
	const (
		noX      = "can't use field X"
		noI      = "can't use field I in type"
		noField  = "cannot access field"
		incompat = "incompatible types for comparison"
	)

	newChan := func() chan S {
		c := make(chan S, 1)
		c <- S{}
		close(c)
		return c
	}

	for _, test := range []testCase{
		{"field", `{{.B}}`, S{}, ""},
		{"field ptr", `{{.B}}`, &S{}, ""},
		{"field unknown", `{{.B}}`, *new(any), noField},
		{"field iface", `{{.Any.B}}`, S{Any: 1}, noField},
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
		{"range map", `{{range .}}{{.X}}{{end}}`, map[string]S{"X": {}}, noX},
		{"range chan ok", `{{range .}}{{.I}}{{end}}`, newChan(), ""},
		{"range chan", `{{range .}}{{.X}}{{end}}`, newChan(), noX},
		{"range chan send", `{{range .}}{{end}}`, make(chan<- S), "over send-only channel"},
		{"range one var", `{{range $e := .}}{{$e.X}}{{end}}`, make([]S, 1), noX},
		{"range two vars", `{{range $k, $e := .}}{{$e.X}}{{end}}`, map[string]S{"x": {}}, noX},
		{"range two vars 2", `{{range $k, $e := .}}{{$k.I}}{{end}}`, map[bool]string{true: "x"}, noI},
		{"range bad type", `{{range 1.5}}{{end}}`, nil, "can't iterate over type"},
		{"range iface", `{{range .Any}}{{end}}`, S{Any: 1}, "range can't iterate"},

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
		{"assign different type", `{{$v := 1}}{{$v = ""}}{{$v.I}}`, nil, "cannot assign"},
		{"assign assignable type", `{{$v := stdin}}{{$v = stringReader ""}}`, nil, ""},
		{"assign assignable type 2", `{{$v := stdin}}{{$v = 1}}`, nil, "cannot assign"},
		{"func args few", `{{and}}`, nil, "want at least 1, got 0"},
		{"func args many", `{{le 1 2 3}}`, nil, "want 2, got 3"},

		{"func ok", `{{add1 3}}`, nil, ""},
		{"func not ok var", `{{$v := 3.1}}{{add1 $v}}`, nil, "expected int; found float64"},
		{"func too few", `{{add1}}`, nil, "want 1, got 0"},
		{"func wrong type", `{{$v := "y"}}{{add1 $v}}`, nil, "expected int; found string"},
		{"undefined", `{{$x = 1}}`, nil, "undefined variable"}, // parser catches references, but not assignments

		{"break", `{{range .A}}{{break}}{{end}}`, S{}, ""},
		{"continue", `{{range .A}}{{if false}}{{continue}}{{end}}{{end}}`, S{}, ""},
		// function arguments
		{"arg var", `{{$v := 1}}{{add1 $v}}`, nil, ""},
		{"arg ptr", `{{add1 .}}`, new(int), ""},
		{"arg ptr int8", `{{add1 .}}`, new(int8), "expected int; found *int8"},
		{"arg addr", `{{intptr .I}}`, &S{}, ""},
		{"arg reflect.Value ok", `{{add1 (and 1)}}`, nil, ""},
		{"arg reflect.Value", `{{add1 (and "x")}}`, nil, "wrong type"},
		{"arg reflect.Value nil", `{{and nil}}`, nil, ""},
		{"arg ident ok", `{{and nilary}}`, nil, ""},
		{"arg ident", `{{add1 nilary}}`, nil, "expected int; found *templatecheck.S"},
		{"arg chain ok", `{{add1 nilary.I}}`, nil, ""},
		{"arg chain", `{{add1 nilary.B}}`, nil, "expected int; found bool"},
		{"arg nil", `{{add1 nil}}`, nil, "cannot assign nil to int"},
		{"arg nil ok", `{{intptr nil}}`, nil, ""},
		{"arg int", `{{add1 true}}`, nil, "expected int; found true"},
		{"arg bool", `{{args 0 0 0 0}}`, nil, "expected bool; found 0"},
		{"arg uint", `{{args true false 0 0}}`, nil, "expected uint; found false"},
		{"arg float", `{{args true 0 false 0}}`, nil, "expected float64; found false"},
		{"arg complex", `{{args true 0 0 false}}`, nil, "expected complex128; found false"},
		{"arg complex ok", `{{args true 0 0 1i}}`, nil, ""},
		{"arg string", `{{variadic 1 2}}`, nil, "expected string; found 2"},
		{"arg string ok", `{{variadic 1 "x"}}`, nil, ""},
		{"arg iface", `{{add1 .Any}}`, S{}, "wrong type"},
		{"arg empty iface formal", `{{emptyiface 1}}`, nil, ""},    // any arg is OK
		{"arg nonempty iface", `{{iface 1}}`, nil, "can't handle"}, // non-empty iface args never OK
		{"arg struct", `{{structure 1}}`, nil, "can't handle"},     // struct args never OK

		// len builtin
		{"len", `{{(len .).I}}`, map[string]S{}, noI},
		{"len arg too many", `{{add1 (len 1 2)}}`, nil, "want 1, got 2"},
		{"len nil", `{{len nil}}`, nil, "len of nil"},
		{"len num", `{{len 3+2i}}`, nil, "len of 3+2i"},
		{"len ptr", `{{len .}}`, &[1]int{0}, ""},
		{"len struct", `{{len .}}`, S{}, "len of type templatecheck.S"},
		{"len iface", `{{len .Any}}`, S{Any: 1}, "len of interface {}"},

		// index builtin
		{"index no indexes", `{{index 1}}`, nil, ""},       // anything OK if no indexes...
		{"index nil", `{{index nil}}`, nil, "untyped nil"}, // except literal nil
		{"index index nil", `{{index "x" nil}}`, nil, "cannot index slice/array with nil"},
		{"index bad type", `{{index . 0}}`, S{}, "can't index item of type templatecheck.S"},
		{"index string ok", `{{index "x" 0}}`, nil, ""},
		{"index string bool", `{{index "x" true}}`, nil, "cannot index slice/array with type bool"},
		{"index string float", `{{index "x" 1.0}}`, nil, "cannot index slice/array with type float64"},
		{"index slice ok", `{{index . 0}}`, []int{1}, ""},
		{"index slice 2 ok", `{{index . 0 0}}`, [][]int{{1}}, ""},
		{"index slice 2", `{{index . 0 "a"}}`, [][]int{{1}}, "cannot index"},
		{"index map ok ", `{{index . "x"}}`, map[string]int{}, ""},
		{"index map", `{{index . 1}}`, map[string]int{}, "has type int; should be string"},
		{"index map 2", `{{index . "x" 1}}`, map[string]int{}, "can't index item of type"},
		{"index map slice", `{{index . "x" 0}}`, map[string][]int{"x": {1}}, ""},
		{"index map nil", `{{index . nil}}`, map[string]int{}, "value is nil; should be"},
		{"index map nil ok", `{{index . nil}}`, map[*string]int{}, ""},
		{"index indirect", `{{index . 0}}`, &[1]int{1}, ""},
		{"index int conversion", `{{index . (len "")}}`, map[uint]int{1.0: 1}, ""},

		// slice builtin
		{"slice no indexes ok", `{{slice "x"}}`, nil, ""},
		{"slice bad type", `{{slice 1}}`, nil, "can't slice item of type int"},
		{"slice nil", `{{slice nil}}`, nil, "index of untyped nil"},
		{"slice too many", `{{slice "x" 1 2 3 4}}`, nil, "too many slice indexes"},
		{"slice ok", `{{slice "x" 1 2.0}}`, nil, "cannot index"},
		{"slice string 3", `{{slice "x" 1 2 3}}`, nil, "cannot 3-index slice a string"},
		{"slice array", `{{slice .A 1 2 3}}`, &S{}, ""},
		{"slice bad index", `{{slice "x" "y"}}`, nil, "cannot index slice/array with type string"},

		// eq/neq builtins
		{"eq ok", `{{eq 1 2 }}`, nil, ""},
		{"eq too few", `{{eq 1}}`, nil, "missing"},
		{"eq int and float", `{{eq (add1 1) (float 2.0)}}`, nil, incompat},
		{"eq int and float literals", `{{eq 1 2.0}}`, nil, incompat}, // eq doesn't observe the Go rules for literals
		{"eq float and string", `{{eq "x" 2.0}}`, nil, incompat},
		{"eq nil nil", `{{eq nil nil}}`, nil, ""},
		{"eq string and nil", `{{eq "x" nil}}`, nil, ""},
		{"eq func type", `{{eq . 1}}`, func() {}, incompat},
		{"eq func type nil", `{{eq . nil}}`, func() {}, ""},
		{"eq nil nil", `{{eq nil nil}}`, nil, ""},
		{"eq str str", `{{eq "a" "b"}}`, nil, ""},
		{"eq nil func type", `{{eq nil .}}`, func() {}, ""},
		{"eq func type nil 3", `{{eq . nil nil nil}}`, func() {}, ""},
		{"eq func type nil 2 1", `{{eq . nil nil 1}}`, func() {}, incompat},
		{"eq struct type", `{{eq . .}}`, S{}, "uncomparable type"},

		// ordered comparisons
		{"le ok", `{{le 1 2}}`, nil, ""},
		{"le nil", `{{le 1 nil}}`, nil, "cannot compare values of type untyped nil"},
		{"le bad type", `{{le "x" 2+3i}}`, nil, "cannot compare values of type complex"},
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
			"if assign",
			`
				{{$v := .}}
				{{if $v = 1}}
				{{end}}
			`,
			S{},
			"cannot assign type",
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
			// Since $v has the same type, on both execution paths, we know its type afterwards
			// and can detect that  a field reference will fail.
			`
				{{$v := 1}}
				{{if .}}
					{{$v = 2}}
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
			"cannot assign type",
		},
		{
			"if assign same type else",
			// Again, same type on both paths.
			`
				{{$v := 1}}
				{{if .}}
					{{$v = 2}}
				{{else}}
					{{$v = 3}}
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
			"cannot assign type",
		},
		{
			"range assign same type",
			`
				{{$v := 1}}
				{{range .}}
					{{$v = 2}}
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
			"cannot assign",
		},

		{
			"range else same type",
			`
				{{$v := .}}
				{{range .}}
					{{$v = 2}}
				{{else}}
					{{$v = 3}}
				{{end}}
				{{$v.I}}
			`,
			map[string]S{},
			"cannot assign",
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
			"cannot assign",
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
			&S{I: 1, P: &S{I: 2}},
			"",
		},
		{
			"bad arg type TODO:dup?",
			`
				{{$v := ""}}
				{{add1 $v}}
			`,
			true,
			"wrong type",
		},

		// stricter semantics
		{
			"and args same type",
			`{{and 1 ""}}`,
			nil,
			"must have same type",
		},
		{
			"or args same type",
			`{{or 1 ""}}`,
			nil,
			"must have same type",
		},
		{
			"and in if any type",
			`{{if (and 1 "")}}{{end}}`,
			nil,
			"",
		},
		{
			"and or in if any type",
			`{{if (and 1 (or "" 2))}}{{end}}`,
			nil,
			"",
		},

		{
			"and in if nested",
			`{{if (and (add1 (or 1 "a"))  "b")}}{{end}}`,
			nil,
			"must have same type",
		},
		{
			"template calls different types",
			`
				{{template "t" 1}}
				{{template "t" 1.0}}
				{{define "t"}}{{.}}{{end}}
			`,
			nil,
			"inconsistent types",
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			testTemplate(t, test, true)
		})
	}
}

func TestNewRangeTypes(t *testing.T) {
	t.Skip("template execution sometimes fails for range-over-int, range-over-func")
	// New features in Go 1.22 and Go 1.23. These should be errors in earlier versions.
	seq1 := func(func(string) bool) {}
	seq2 := func(func(string, int) bool) {}

	for _, test := range []testCase{
		{"range int", `{{range .}}{{end}}`, 5, "can't iterate over type"},
		{"range Seq1", `{{range .}}{{end}}`, seq1, "can't iterate over type"},
		{"range Seq2", `{{range .}}{{end}}`, seq2, "can't iterate over type"},
	} {
		if version.Compare(version.Lang(runtime.Version()), "go1.22") >= 0 {
			test.want = ""
		}
		t.Run(test.name, func(t *testing.T) {
			testTemplate(t, test, false)
			testTemplate(t, test, true)
		})
	}
}

func TestOtherTemplates(t *testing.T) {
	f := func(string) int { return 0 }
	const contents = `{{block "foo" .}}{{.B}}{{f "x"}}{{end}}`

	htm := htmpl.Must(htmpl.New("").Funcs(htmpl.FuncMap{"f": f}).Parse(contents))
	err := CheckHTML(htm, S{})
	if err != nil {
		t.Fatal(err)
	}

	stm := stmpl.Must(stmpl.New("").Funcs(stmpl.FuncMap{"f": f}).Parse(contents))
	err = CheckSafe(stm, S{})
	if err != nil {
		t.Fatal(err)
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

func TestCheckLanguageVersion(t *testing.T) {
	for _, test := range []struct {
		have, want string
		match      string
	}{
		{"go1.23", "go1.23", "^$"},
		{"go1.23", "go1.22", "^$"},
		{"go1.23", "go1.24", "have.*need"},
		{"1.23", "go1.22", `language version "" \(which is invalid\)`},
		{"go1.23-20240626-RC01 cl/646990413 +5a18e79687 X:fieldtrack,boringcrypto linux/amd64", "go1.23", ""},
		{"go1.23-20240626-RC01 cl/646990413 +5a18e79687 X:fieldtrack,boringcrypto linux/amd64", "go1.24", "have.*need"},
		{"devel go1.24-5fe3b31cf8 Fri Sep 27 16:45:09 2024 +0000 linux/amd64", "go1.23", "invalid"},
	} {
		got := checkLangVersion(test.have, test.want)
		re := regexp.MustCompile(test.match)
		if !re.MatchString(got) {
			t.Errorf("checkLangVersion(%q, %q) = %q, want match of %s", test.have, test.want, got, test.match)
		}
	}
}
