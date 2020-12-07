package templatecheck

import (
	"flag"
	"fmt"
	"io"
	"reflect"
	"strings"
	"testing"
	ttmpl "text/template"
	"text/template/parse"
)

var debug = flag.Bool("debug", false, "display extra debug output")

type checkStruct struct {
	B     bool
	Bs    []byte
	I     int
	P     *checkStruct
	R     io.Reader
	unexp int
}

func (c checkStruct) Add(x int) int { return c.I + x }

func TestCheck(t *testing.T) {
	const (
		noX   = "can't use field X"
		noI   = "can't use field I in type"
		undef = "undefined variable"
	)

	var (
		csType    = reflect.TypeOf(checkStruct{})
		csMapType = reflect.MapOf(stringType, csType)
	)
	funcs := ttmpl.FuncMap{
		"pluralize": func(i int, s string) string { return "" },
		"variadic":  func(x int, ys ...string) string { return "" },
	}

	for _, test := range []struct {
		name     string
		contents string
		dotType  reflect.Type
		want     string // if non-empty, error should contain it
	}{
		{"field", `{{.B}}`, csType, ""},
		{"field ptr", `{{.B}}`, reflect.PtrTo(csType), ""},
		{"field iface", `{{.B}}`, emptyInterfaceType, ""},
		{"no field", `{{.X}}`, csType, noX},
		{"no field ptr", `{{.X}}`, reflect.PtrTo(csType), noX},
		{"unexported", `{{.unexp}}`, csType, "unexported field"},
		{"method ok", `{{.Add 1}}`, csType, ""},
		{"method too few", `{{.Add}}`, csType, "want 1, got 0"},
		{"method too many", `{{.Add 1 2}}`, csType, "want 1, got 2"},
		{"method interface ok", `{{.R.Read .Bs}}`, csType, ""},
		{"method interface too few", `{{.R.Read}}`, csType, "want 1, got 0"},
		{"method interface too many", `{{.R.Read 1 true ""}}`, csType, "want 1, got 3"},
		{"not a struct", `{{.B.I}}`, csType, noI},
		{"not a func", `{{.I 1}}`, csType, "cannot be invoked"},
		{"nested", `{{.P.P.P.X}}`, csType, noX},
		{"map key ok", `{{.X.I}}`, csMapType, ""},
		{"map key no field", `{{.X.X}}`, csMapType, noX},
		{"map key arg", `{{.X 1}}`, csMapType, "is not a method but has arguments"},
		{"map key bad type", `{{.X}}`, reflect.MapOf(boolType, stringType), "bad"},
		{"decl ok", `{{$x := .}}{{$x.B}}`, csType, ""},
		{"decl", `{{$x := .}}{{$x.X}}`, csType, noX},
		{"decl bool", `{{$x := true}}{{$x.I}}`, csType, noI},
		{"decl int", `{{$x := 1}}{{$x.I}}`, csType, noI},
		{"not a func var", `{{$x := 1}}{{$x 1}}`, csType, "can't give argument to non-function"},
		{"not a func pipe", `{{$x := 1}}{{1 | $x}}`, csType, "can't give argument to non-function"},
		{"with", `{{with .P}}{{.X}}{{end}}`, csType, noX},
		{"with else", `{{with .P}}{{.B}}{{else}}{{.X}}{{end}}`, csType, noX},
		{"with dollar", `{{with .B}}{{$.X}}{{end}}`, csType, noX},
		{"if", `{{if .P}}{{.P.X}}{{end}}`, csType, noX},
		{"ifelse", `{{if .P}}{{.B}}{{else}}{{.X}}{{end}}`, csType, noX},
		{"range slice ok", `{{range .}}{{.P.B}}{{end}}`, reflect.SliceOf(csType), ""},
		{"range slice", `{{range .}}{{.X}}{{end}}`, reflect.SliceOf(csType), noX},
		{"range map", `{{range .}}{{.X}}{{end}}`, csMapType, noX},
		{"range chan ok", `{{range .}}{{.I}}{{end}}`, reflect.ChanOf(reflect.BothDir, csType), ""},
		{"range chan", `{{range .}}{{.X}}{{end}}`, reflect.ChanOf(reflect.BothDir, csType), noX},
		{"range chan send", `{{range .}}{{end}}`, reflect.ChanOf(reflect.SendDir, csType), "over send-only channel"},
		{"range one var", `{{range $e := .}}{{$e.X}}{{end}}`, reflect.SliceOf(csType), noX},
		{"range two vars", `{{range $k, $e := .}}{{$e.X}}{{end}}`, reflect.MapOf(stringType, csType), noX},
		{"range two vars 2", `{{range $k, $e := .}}{{$k.I}}{{end}}`, reflect.MapOf(boolType, stringType), noI},
		{"range bad type", `{{range 1}}{{end}}`, nil, "can't iterate over type"},
		{"range unknown", `{{range .}}{{end}}`, unknownType, ""},
		{"range iface", `{{range .}}{{end}}`, emptyInterfaceType, ""},
		{"chain ok", `{{(.P).I}}`, csType, ""},
		{"chain bool", `{{(true).I}}`, csType, noI},
		{"chain no field", `{{(.P).X}}`, csType, noX},
		{"chain no struct", `{{(.B).I}}`, csType, noI},
		{"chain map ok", `{{(.K).I}}`, csMapType, ""},
		{"chain map", `{{(.K).X}}`, csMapType, noX},
		{"chain pipe", `{{((.B) | printf).I}}`, csType, noI},
		{"assign same type", `{{$v := 1}}{{$v = 2}}{{$v.I}}`, nil, noI},
		{"assign diffrent type", `{{$v := 1}}{{$v = ""}}{{$v.I}}`, nil, noI},
		{"func args few", `{{and}}`, nil, "want at least 1, got 0"},
		{"func args many", `{{le 1 2 3}}`, nil, "want 2, got 3"},
		{"len", `{{(len .).I}}`, csMapType, noI},
		{"len arg too many", `{{pluralize (len 1 2) .}}`, csMapType, "want 1, got 2"},
		{"userfunc ok", `{{pluralize 3 "x"}}`, nil, ""},
		{"userfunc too few", `{{pluralize 3}}`, nil, "want 2, got 1"},
		{"userfunc wrong type", `{{pluralize (pluralize 1 "x") "y"}}`, nil, "expected int; found string"},
		{"variadic", `{{variadic 1 2}}`, nil, "expected string; found 2"},
		{"var arg", `{{$v := 1}}{{pluralize $v "x"}}`, nil, ""},
		{"undefined", `{{$x = 1}}`, nil, undef}, // parser catches references, but not assignments
		{
			"nested decl", // variable redeclared in an inner scope; doesn't affect outer scope
			`
				{{$v := 1}}
				{{if .}}
					{{$v := .}}
				{{end}}
				{{$v.I}}
			`,
			csType,
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
			csType,
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
			csType,
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
			csType,
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
			csType,
			"", // be conservative, do not warn
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
			csType,
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
			csType,
			"",
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
			csMapType,
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
			csMapType,
			"", // conservative
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
			csMapType,
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
			csMapType,
			"",
		},
		{
			"template call ok",
			`
				{{template "foo" .}}
				{{define "foo"}}{{.I}}{{end}}
			`,
			csType,
			"",
		},
		{
			"template call no x",
			`
				{{template "foo" .}}
				{{define "foo"}}{{.X}}{{end}}
			`,
			csType,
			noX,
		},
		{
			"template call undef",
			`
				{{template "bar" .}}
				{{define "foo"}}{{.X}}{{end}}
			`,
			csType,
			`template "bar" not defined`,
		},
		{
			"recursion",
			`
				{{define "foos"}}{{if .}}{{template "foo" .P}}{{end}}{{end}}
				{{define "foo"}}{{.I}}{{template "foos" .P}}{{end}}
				{{template "foos" .}}
			`,
			csType,
			"",
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			tmpl, err := ttmpl.New(test.name).Funcs(funcs).Parse(test.contents)
			if err != nil {
				t.Fatal("while parsing:", err)
			}
			if *debug {
				dump(tmpl.Root, 0)
			}
			err = check(textTemplate{tmpl}, test.dotType, []map[string]interface{}{funcs})
			if err != nil {
				if test.want == "" {
					t.Fatalf("got %v, wanted no error", err)
				}
				if !strings.Contains(err.Error(), test.want) {
					t.Fatalf("%q not contained in %q", test.want, err)
				}
			} else if test.want != "" {
				t.Fatalf("got nil, want error containing %q", test.want)
			}
		})
	}
}

func dump(n parse.Node, level int) {
	for i := 0; i < level; i++ {
		fmt.Print("  ")
	}
	fmt.Printf("%T", n)
	switch n := n.(type) {
	case *parse.ListNode:
		fmt.Println()
		for _, c := range n.Nodes {
			dump(c, level+1)
		}
	case *parse.ActionNode:
		fmt.Println()
		dump(n.Pipe, level+1)
	case *parse.PipeNode:
		fmt.Printf(" IsAssign: %t, decl: %v\n", n.IsAssign, n.Decl)
		for _, c := range n.Cmds {
			dump(c, level+1)
		}
	case *parse.CommandNode:
		fmt.Println()
		for _, arg := range n.Args {
			dump(arg, level+1)
		}
	case *parse.VariableNode:
		fmt.Printf(" %q\n", n.Ident)
	default:
		fmt.Printf(" %s\n", n)
	}

}
