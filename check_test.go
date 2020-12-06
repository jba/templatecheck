package templatecheck

import (
	"flag"
	"fmt"
	"reflect"
	"strings"
	"testing"
	"text/template"
	"text/template/parse"
)

var debug = flag.Bool("debug", false, "display extra debug output")

type checkStruct struct {
	B bool
	I int
	P *checkStruct
}

var csType = reflect.TypeOf(checkStruct{})

func TestCheck(t *testing.T) {
	const (
		noX   = "can't use field X"
		noI   = "can't use field I in type"
		undef = "undefined variable"
	)
	for _, test := range []struct {
		name     string
		contents string
		dotType  reflect.Type
		want     string // if non-empty, error should contain it
	}{
		{"field", `{{.B}}`, csType, ""},
		{"field ptr", `{{.B}}`, reflect.PtrTo(csType), ""},
		{"no field", `{{.X}}`, csType, noX},
		{"no field ptr", `{{.X}}`, reflect.PtrTo(csType), noX},
		{"not a struct", `{{.B.I}}`, csType, noI},
		{"nested", `{{.P.P.P.X}}`, csType, noX},
		{"decl ok", `{{$x := .}}{{$x.B}}`, csType, ""},
		{"decl", `{{$x := .}}{{$x.X}}`, csType, noX},
		{"decl bool", `{{$x := true}}{{$x.I}}`, csType, noI},
		{"decl int", `{{$x := 1}}{{$x.I}}`, csType, noI},
		{"with", `{{with .P}}{{.X}}{{end}}`, csType, noX},
		{"with else", `{{with .P}}{{.B}}{{else}}{{.X}}{{end}}`, csType, noX},
		{"with dollar", `{{with .B}}{{$.X}}{{end}}`, csType, noX},
		{"if", `{{if .P}}{{.P.X}}{{end}}`, csType, noX},
		{"ifelse", `{{if .P}}{{.B}}{{else}}{{.X}}{{end}}`, csType, noX},
		{"range slice ok", `{{range .}}{{.P.B}}{{end}}`, reflect.SliceOf(csType), ""},
		{"range slice", `{{range .}}{{.X}}{{end}}`, reflect.SliceOf(csType), noX},
		{"range map", `{{range .}}{{.X}}{{end}}`, reflect.MapOf(stringType, csType), noX},
		{"range chan ok", `{{range .}}{{.I}}{{end}}`, reflect.ChanOf(reflect.BothDir, csType), ""},
		{"range chan", `{{range .}}{{.X}}{{end}}`, reflect.ChanOf(reflect.BothDir, csType), noX},
		{"range chan send", `{{range .}}{{end}}`, reflect.ChanOf(reflect.SendDir, csType), "over send-only channel"},
		{"range one var", `{{range $e := .}}{{$e.X}}{{end}}`, reflect.SliceOf(csType), noX},
		{"range two vars", `{{range $k, $e := .}}{{$e.X}}{{end}}`, reflect.MapOf(stringType, csType), noX},
		{"range two vars 2", `{{range $k, $e := .}}{{$k.X}}{{end}}`, reflect.MapOf(csType, stringType), noX},
		{"assign same type", `{{$v := 1}}{{$v = 2}}{{$v.I}}`, nil, noI},
		{"assign diffrent type", `{{$v := 1}}{{$v = ""}}{{$v.I}}`, nil, noI},
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
			"conditional assign same type", // variable assigned to same type in conditional
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
			"conditional assign different type", // variable assigned to different type in conditional
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
			"conditional assign same type else",
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
			"conditional assign different type else",
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
	} {
		t.Run(test.name, func(t *testing.T) {
			tmpl, err := template.New(test.name).Parse(test.contents)
			if err != nil {
				t.Fatal("while parsing:", err)
			}
			if *debug {
				dump(tmpl.Root, 0)
			}
			err = Check(tmpl, test.dotType)
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
