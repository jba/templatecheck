# templatecheck

Check Go templates for validity.

```
t := template.Must(template.ParseFiles("index.tmpl"))
if err := templatecheck.CheckHTML(t, homePage{}); err != nil {
    log.Fatal(err)
}
```

See the [package documentation](https://pkg.go.dev/github.com/jba/templatecheck)
for details.

## strict mode

Everything must have a definite type. Type errors are impossible at runtime.
