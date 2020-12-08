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
