// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

// Package templatecheck checks Go templates for problems. It can detect many
// errors that are normally caught only during execution. Use templatecheck in
// tests to find template errors early, and along template execution paths that
// might only rarely be reached.
//
// A template must be invoked with the same Go type each time to use
// templatecheck. Passing that type to templatecheck gives it enough information
// to verify that all the field references in the template are valid.
// templatecheck can also verify that functions are called with the right number
// and types of arguments, that the argument to a range statement can actually
// be ranged over, and a few other things.
//
// Consider a web server that parses a template for its home page:
//
//   import "html/template"
//
//   var tmpl = template.Must(template.ParseFiles("index.tmpl"))
//
//   type homePage struct { ... }
//
//   func handler(w http.ResponseWriter, r *http.Request) {
//       ...
//       var buf bytes.Buffer
//       err := tmpl.Execute(&buf, homePage{...})
//       ...
//   }
//
// Use templatecheck to catch errors in tests, instead of during serving:
//
//   func TestTemplates(t *testing.T) {
//       if err := templatecheck.CheckHTML(tmpl, homePage{}); err != nil {
//           t.Fatal(err)
//       }
//   }
//
//
// Checking Associated Templates
//
// To check associated templates, use Template.Lookup. This can be necessary
// if full type information isn't available to the main template.
//
// For example, here the base template is always invoked with a basePage,
// but the type of its Details field differs depending on the value of IsTop.
//
//   type basePage struct {
//       IsTop bool
//       Details interface{}
//   }
//
//   type topDetails ...
//   type bottomDetails ...
//
// The template text is
//   {{if .IsTop}}
//     {{template "top" .Details}}
//   {{else}}
//     {{template "bottom" .Details}}
//   {{end}}
//
//   {{define "top"}}...{{end}}
//   {{define "bottom"}}...{{end}}
//
// Checking only the main template will not provide much information about the
// two associated templates, because their data types are unknown. All three
// templates should be checked, like so:
//
//   t := template.Must(template.New("").Parse(base))
//   if err := templatecheck.CheckText(t, basePage{}) ...
//   if err := templatecheck.CheckText(t.Lookup("top"), topDetails{}) ...
//   if err := templatecheck.CheckText(t.Lookup("bottom"), bottomDetails{}) ...
package templatecheck
