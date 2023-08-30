// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	"reflect"
	"testing"
	"text/template/parse"
)

func TestCheckSlice(t *testing.T) {
	// Verify return types for slice.

	type (
		stringType string
		sliceType  []int
		arrayType  [1]int
	)

	for _, test := range []struct {
		item, want any
	}{
		{"x", ""},
		{[]int{}, []int{}},
		{stringType(""), stringType("")},
		{sliceType{}, sliceType{}},
		{[1]int{}, []int{}},    // slice(array) => slice
		{arrayType{}, []int{}}, // ditto, defined type is lost
	} {
		got := checkSlice(&state{}, reflect.TypeOf(test.item), []parse.Node{&parse.DotNode{}})
		want := reflect.TypeOf(test.want)
		if got != want {
			t.Errorf("%T: got %s, want %s", test.item, got, want)
		}
	}
}
