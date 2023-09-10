# templatecheck

Check Go templates for validity.

Using `CheckedTemplate[T]`:

```
t := template.Must(template.ParseFiles("index.tmpl"))
ct, err := templatecheck.NewChecked[IndexData](t)
if err != nil { ... }
// ct cannot have type errors.
data := IndexData{...}
...
// ct.Execute's second argument must be of type IndexData.
if err := ct.Execute(w, data); err != nil { ... }
```

Using `CheckedTemplate[T]` is recommended, but may require changes
in how you write your templates and provide data to them.
For example, if your template contains

    {{.F}}

you can execute it with any value that has an `F` field or method.
Two different calls to `Template.Execute` can pass in two different
types, as long as each has `F`. With checked templates, you
must fix a single type to use for execution.


Using the `CheckXXX` functions:

```
t := template.Must(template.ParseFiles("index.tmpl"))
if err := templatecheck.CheckHTML(t, homePage{}); err != nil {
    log.Fatal(err)
}
```

See the [package documentation](https://pkg.go.dev/github.com/jba/templatecheck)
for details.
