// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

// Package templatecheck checks Go templates for problems. It can detect many
// errors that are normally caught only during execution. Use templatecheck
// to find template errors early, and along template execution paths that
// might only rarely be reached.
//
// # Type-Checked Templates
//
// Given a template t from one of the text/template, html/template, or
// github.com/google/safehtml/template packages, calling NewChecked[T](t)
// type-checks t and returns a CheckedTemplate[T]. The only things you can do
// with a CheckedTemplate[T] are get its name, and execute it with data of type
// T.
//
// The execution will be free of type errors. To make that guarantee, the
// semantics of templates are restricted. [CheckHTMLStrict] has details, but the
// general idea is that types must be known so they can be checked. For example,
// the template
//
//	{{.F}}
//
// will succeed as long as dot contains a field or zero-argument method named "F".
// The non-strict CheckXXX functions of this package will check that if the
// type of dot is known, but if it is unknown or an interface type, they will
// not complain, since template execution might succeed.
// By contrast, strict checking will fail unless it can be sure
// that the reference to "F" is valid, implying that it must know the type of dot.
//
// When all of a program's templates are CheckedTemplates, it is not necessary
// to write tests to check templates as described below. All type-checking
// happens when the CheckedTemplates are created.
//
// # Checks
//
// The rest of this documentation describes the original templatecheck mechanism,
// accessed through the CheckXXX functions.
// It may still be useful, especially for existing programs, but CheckedTemplate[T]
// is preferable because it is safer.
//
// A template must be invoked with the same Go type each time to use
// the templatecheck.CheckXXX functions.
// Passing that type to templatecheck gives it enough information
// to verify that all the field references in the template are valid.
// templatecheck can also verify that functions are called with the right number
// and types of arguments, that the argument to a range statement can actually
// be ranged over, and a few other things.
//
// Consider a web server that parses a template for its home page:
//
//	import "html/template"
//
//	var tmpl = template.Must(template.ParseFiles("index.tmpl"))
//
//	type homePage struct { ... }
//
//	func handler(w http.ResponseWriter, r *http.Request) {
//	    ...
//	    var buf bytes.Buffer
//	    err := tmpl.Execute(&buf, homePage{...})
//	    ...
//	}
//
// Use templatecheck to catch errors in tests, instead of during serving:
//
//	func TestTemplates(t *testing.T) {
//	    if err := templatecheck.CheckHTML(tmpl, homePage{}); err != nil {
//	        t.Fatal(err)
//	    }
//	}
//
// # Checking Associated Templates
//
// To check associated templates, use Template.Lookup. This can be necessary for
// non-strict checking if full type information isn't available to the main
// template. (It isn't needed for strict checking, because all calls to
// associated templates are type-checked.)
//
// For example, here the base template is always invoked with a basePage,
// but the type of its Details field differs depending on the value of IsTop.
//
//	type basePage struct {
//	    IsTop bool
//	    Details any
//	}
//
//	type topDetails ...
//	type bottomDetails ...
//
// The template text is
//
//	{{if .IsTop}}
//	  {{template "top" .Details}}
//	{{else}}
//	  {{template "bottom" .Details}}
//	{{end}}
//
//	{{define "top"}}...{{end}}
//	{{define "bottom"}}...{{end}}
//
// Checking only the main template will not provide much information about the
// two associated templates, because their data types are unknown. All three
// templates should be checked, like so:
//
//	t := template.Must(template.New("").Parse(base))
//	if err := templatecheck.CheckText(t, basePage{}) ...
//	if err := templatecheck.CheckText(t.Lookup("top"), topDetails{}) ...
//	if err := templatecheck.CheckText(t.Lookup("bottom"), bottomDetails{}) ...
package templatecheck
