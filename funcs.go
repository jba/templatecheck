// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

package templatecheck

import (
	"reflect"
	"text/template/parse"
)

func checkLen(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	arg := args[0]
	argType, isLiteral := s.evalArg(dot, arg)
	if isLiteral {
		if argType == stringType {
			return intType
		}
		s.errorf("len of %s", arg)
	}
	argType = indirectType(argType)
	if argType == unknownType {
		return intType
	}
	switch argType.Kind() {
	case reflect.Array, reflect.Chan, reflect.Map, reflect.Slice, reflect.String:
		return intType
	case reflect.Interface:
		// We can't assume anything about an interface type.
		return intType
	default:
		s.errorf("len of type %s", typeString(argType))
	}
	panic("not reached")
}

func checkIndex(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	item := args[0]
	itemType, _ := s.evalArg(dot, item)
	if itemType == nil {
		s.errorf("index of untyped nil")
	}
	for _, index := range args[1:] {
		itemType = indirectType(itemType)
		indexType, _ := s.evalArg(dot, index)
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
	if intLike(indexType.Kind()) && intLike(keyType.Kind()) && indexType.ConvertibleTo(keyType) {
		return
	}
	s.errorf("index has type %s; should be %s", typeString(indexType), typeString(keyType))
}

func intLike(typ reflect.Kind) bool {
	switch typ {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return true
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return true
	}
	return false
}

func checkSlice(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	item := args[0]
	indexes := args[1:]
	itemType, _ := s.evalArg(dot, item)
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
		indexType, _ := s.evalArg(dot, index)
		checkIndexArg(s, indexType)
	}
	return resultType
}

func checkEq(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	if len(args) == 1 {
		s.errorf("missing argument for comparison")
	}
	for _, arg := range args {
		typ, _ := s.evalArg(dot, arg)
		if definitelyNotComparable(typ) {
			s.errorf("uncomparable type: %s", typeString(typ))
		}
	}
	return boolType
}

// definitelyNotComparable returns true if values of type t can never be compared.
// Only non-comparable struct types have that property.
func definitelyNotComparable(t reflect.Type) bool {
	return t != nil && t.Kind() == reflect.Struct && !t.Comparable()
}

// check le, gt, etc.
func checkOrderedComparison(s *state, dot reflect.Type, args []parse.Node) reflect.Type {
	for _, arg := range args {
		if t, _ := s.evalArg(dot, arg); !isOrderable(t) {
			s.errorf("cannot compare values of type %s", typeString(t))
		}
	}
	return boolType
}

func isOrderable(t reflect.Type) bool {
	if t == nil {
		return false
	}
	if intLike(t.Kind()) {
		return true
	}
	switch t.Kind() {
	case reflect.Float32, reflect.Float64, reflect.String:
		return true
	default:
		return false
	}
}

func typeString(t reflect.Type) string {
	if t == nil {
		return "untyped nil"
	}
	return t.String()
}
