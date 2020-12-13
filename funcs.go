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
	switch argType.Kind() {
	case reflect.Array, reflect.Chan, reflect.Map, reflect.Slice, reflect.String:
		return intType
	default:
		s.errorf("len of type %s", argType)
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
			s.errorf("can't index item of type %s", itemType)
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
			s.errorf("value is nil; should be of type %s", keyType)
		}
		return
	}
	if indexType.AssignableTo(keyType) {
		return
	}
	if intLike(indexType.Kind()) && intLike(keyType.Kind()) && indexType.ConvertibleTo(keyType) {
		return
	}
	s.errorf("index has type %s; should be %s", indexType, keyType)
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
