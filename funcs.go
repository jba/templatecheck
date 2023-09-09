// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	"reflect"
	"text/template/parse"
)

// checkLen checks a call to the "len" built-in function.
// it returns the type of the result, which is always int.
// The number of args has already been checked, so we know len(args) == 1.
func checkLen(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	validateLen(s, dot, args[0])
	return intType
}

func validateLen(s *state, dot reflect.Type, arg parse.Node) {
	argType, isLiteral := s.evalArg(dot, arg, false)
	if isLiteral {
		if argType != stringType {
			s.errorf("len of %s", arg)
		}
		return
	}
	argType = indirectType(argType)
	if argType == unknownType {
		if s.strict {
			s.errorf("len of unknown type")
		} else {
			return
		}
	}
	switch argType.Kind() {
	case reflect.Array, reflect.Chan, reflect.Map, reflect.Slice, reflect.String:
	case reflect.Interface:
		// We can't assume anything about an interface type.
		if s.strict {
			s.errorf("len of %s", typeString(argType))
		}
	default:
		s.errorf("len of type %s", typeString(argType))
	}
}

func checkIndex(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	item := args[0]
	itemType, _ := s.evalArg(dot, item, false)
	if itemType == nil {
		s.errorf("index of untyped nil")
	}
	for _, index := range args[1:] {
		itemType = indirectType(itemType)
		indexType, _ := s.evalArg(dot, index, false)
		switch itemType.Kind() {
		case reflect.Array, reflect.Slice, reflect.String:
			checkIndexArg(s, indexType)
			if itemType.Kind() == reflect.String {
				itemType = byteType
			} else {
				itemType = itemType.Elem()
			}
		case reflect.Map:
			checkMapArg(s, indexType, itemType.Key())
			itemType = itemType.Elem()
		default:
			s.errorf("can't index item of type %s", typeString(itemType))
		}
	}
	return itemType
}

// In strict mode, all must have the same type.
func checkAndOr(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	if !s.strict {
		// Any number of args, of any types.
		return reflectValueType
	}
	// All args must have the same type.
	t, _ := s.evalArg(dot, args[0], s.onlyTruthMatters)
	for _, arg := range args[1:] {
		t2, _ := s.evalArg(dot, arg, s.onlyTruthMatters)
		if !s.onlyTruthMatters && t != t2 {
			s.errorf("and/or args must have same type; got %s and %s", t, t2)
		}
	}
	return t
}

func checkIndexArg(s *state, typ reflect.Type) {
	if typ == nil {
		s.errorf("cannot index slice/array with nil")
	}
	if !(typ == intType || typ == numberType) {
		s.errorf("cannot index slice/array with type %s", typ)
	}
}

func checkMapArg(s *state, indexType, keyType reflect.Type) {
	if indexType == nil {
		if !canBeNil(keyType) {
			s.errorf("value is nil; should be of type %s", typeString(keyType))
		}
		return
	}
	if indexType.AssignableTo(keyType) {
		return
	}
	if isIntegerType(indexType) && isIntegerType(keyType) && indexType.ConvertibleTo(keyType) {
		return
	}
	s.errorf("index has type %s; should be %s", typeString(indexType), typeString(keyType))
}

func checkSlice(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	item := args[0]
	indexes := args[1:]
	itemType, _ := s.evalArg(dot, item, false)
	if itemType == nil {
		s.errorf("index of untyped nil")
	}
	if len(indexes) > 3 {
		s.errorf("too many slice indexes: %d", len(indexes))
	}
	var resultType reflect.Type
	switch itemType.Kind() {
	case reflect.String:
		if len(indexes) == 3 {
			s.errorf("cannot 3-index slice a string")
		}
		resultType = itemType
	case reflect.Array:
		resultType = reflect.SliceOf(itemType.Elem())
	case reflect.Slice:
		resultType = itemType
	default:
		s.errorf("can't slice item of type %s", typeString(itemType))
	}
	for _, index := range indexes {
		indexType, _ := s.evalArg(dot, index, false)
		checkIndexArg(s, indexType)
	}
	return resultType
}

// TODO: revisit this, matching the actual definition if eq in template/funcs.go.
// - Call the equivalent of indirectInterface.
// - Use basicKind.
func checkEq(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	if len(args) == 1 {
		s.errorf("missing argument for comparison")
	}
	for _, arg := range args {
		typ, _ := s.evalArg(dot, arg, false)
		if definitelyNotComparable(typ) {
			s.errorf("uncomparable type: %s", typeString(typ))
		}
	}
	if s.strict {
		typ0, _ := s.evalArg(dot, args[0], false)
		for _, arg := range args[1:] {
			typ, _ := s.evalArg(dot, arg, false)
			if !comparisonCompatible(typ0, typ) {
				s.errorf("incompatible types for comparison: %s and %s", typeString(typ0), typeString(typ))
			}
		}
	}
	return boolType
}

// definitelyNotComparable returns true if values of type t can never be compared.
// Only non-comparable struct types have that property.
func definitelyNotComparable(t reflect.Type) bool {
	return t != nil && t.Kind() == reflect.Struct && !t.Comparable()
}

func comparisonCompatible(t1, t2 reflect.Type) bool {
	// Comparison with untyped nil; always OK (?)
	if t1 == nil || t2 == nil {
		return true
	}
	if t1.Kind() == t2.Kind() && t1.Comparable() {
		return true
	}
	if isIntegerType(t1) && isIntegerType(t2) {
		return true
	}
	if isFloatType(t1) && isFloatType(t2) {
		return true
	}
	if isComplexType(t1) && isComplexType(t2) {
		return true
	}
	return false
}

// Can this otherwise non-comparable type be compared to a literal nil?
func isNilComparable(t reflect.Type) bool {
	switch t.Kind() {
	case reflect.Slice, reflect.Map, reflect.Func:
		return true
	default:
		return false
	}
}

// check le, gt, etc.
func checkOrderedComparison(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	for _, arg := range args {
		if t, _ := s.evalArg(dot, arg, false); !isOrderable(t) {
			s.errorf("cannot compare values of type %s", typeString(t))
		}
	}
	return boolType
}

func isOrderable(t reflect.Type) bool {
	if t == nil {
		return false
	}
	return t.Kind() == reflect.String || isIntegerType(t) || isFloatType(t)
}

func checkNot(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	// Any type is OK.
	return boolType
}

func typeString(t reflect.Type) string {
	if t == nil {
		return "untyped nil"
	}
	return t.String()
}
