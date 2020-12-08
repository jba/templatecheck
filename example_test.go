// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck_test

import (
	"fmt"
	"text/template"

	"github.com/jba/templatecheck"
)

func Example() {
	type data struct {
		Greeting string
		Name     string
	}

	const contents = `{{.Greetings}}, {{.Name}} and welcome!`

	t := template.Must(template.New("greet").Parse(contents))
	err := templatecheck.CheckText(t, data{})
	fmt.Println(err)

	// Output: template: greet:1:2: checking "greet" at <.Greetings>: can't use field Greetings in type templatecheck_test.data
}
