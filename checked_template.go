// Copyright 2023 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	htmpl "html/template"
	"io"
	ttmpl "text/template"

	stmpl "github.com/google/safehtml/template"
)

type CheckedTemplate[T any] struct {
	t template
}

type tmpl[T any] interface {
	*ttmpl.Template | *htmpl.Template | *stmpl.Template
	Clone() (T, error)
}

// NewChecked type-checks t using one of the CheckXXXStrict functions.
// If the check succeeds, it returns a CheckedTemplate[T] that behaves
// like the argument template.
//
// Subsequent changes to the argument template do not affect the
// CheckedTemplate.
func NewChecked[T any, M tmpl[M]](t M) (*CheckedTemplate[T], error) {
	c, err := t.Clone()
	if err != nil {
		return nil, err
	}
	ct := makeTemplate[M](c)
	var z T
	if err := check(ct, z, true); err != nil {
		return nil, err
	}
	return &CheckedTemplate[T]{ct}, nil
}

// MustChecked is like NewChecked, but panics if type-checking fails.
func MustChecked[T any, M tmpl[M]](t M) *CheckedTemplate[T] {
	ct, err := NewChecked[T](t)
	if err != nil {
		panic(err)
	}
	return ct
}

func makeTemplate[T tmpl[T]](t T) template {
	switch t := any(t).(type) {
	case *ttmpl.Template:
		return textTemplate{t}
	case *htmpl.Template:
		return htmlTemplate{t}
	case *stmpl.Template:
		return safeTemplate{t}
	default:
		panic("bad type")
	}
}

// Name returns the name of the template.
func (t *CheckedTemplate[T]) Name() string {
	return t.t.Name()
}

// Execute behaves like Template.Execute.
func (t *CheckedTemplate[T]) Execute(wr io.Writer, data T) error {
	return t.t.Execute(wr, data)
}
