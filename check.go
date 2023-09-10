// Copyright 2020 Jonathan Amsterdam.
// Use of this source code is governed by an MIT
// license that can be found in the LICENSE file.

// TODO: increase coverage

package templatecheck

import (
	"fmt"
	htmpl "html/template"
	"io"
	"reflect"
	"strings"
	ttmpl "text/template"
	"text/template/parse"

	stmpl "github.com/google/safehtml/template"
)

type template interface {
	Name() string
	Tree() *parse.Tree
	Lookup(string) template
	FuncMap() reflect.Value
	Execute(io.Writer, any) error
}

// CheckHTML checks an html/template for problems. The second argument is the
// type of dot passed to template.Execute.
//
// CheckHTML assumes that the "missingkey" option for the template is "zero",
// meaning that a missing key in a map returns the zero value for the map's
// element type. This is not the default value for "missingkey", but it allows
// more checks.
func CheckHTML(t *htmpl.Template, typeValue any) error {
	return check(htmlTemplate{t}, typeValue, false)
}

// CheckHTMLStrict checks an html/template for problems. The second argument is the
// type of dot passed to template.Execute.
//
// CheckHTMLStrict makes the same assumption about the "missingkey" option as
// [CheckHTML] does.
//
// CheckHTMLStrict guarantees that no type errors will occur if t is executed
// with a value of the given type. To do so, it restricts the semantics of
// the template language as follows:
//
//   - All calls to templates created with "define" or "block" must pass the same
//     type for dot.
//   - Variables have a consistent type, determined when the variable is declared with ":=".
//     If a variable is assigned a new value, the value's type must be assignable to the type
//     used to declare it.
//   - All types in an expression must be known. That includes field access, function calls
//     and arguments to "range".
//   - If the "and" or "or" functions are used for their value, then all their arguments
//     must be of the same type. That restriction does not apply if they are used
//     as an argument to "if", where only the truth value of the result matters.
func CheckHTMLStrict(t *htmpl.Template, typeValue any) error {
	return check(htmlTemplate{t}, typeValue, true)
}

type htmlTemplate struct {
	tmpl *htmpl.Template
}

func (t htmlTemplate) Name() string      { return t.tmpl.Name() }
func (t htmlTemplate) Tree() *parse.Tree { return t.tmpl.Tree }

func (t htmlTemplate) Lookup(name string) template {
	if u := t.tmpl.Lookup(name); u != nil {
		return htmlTemplate{u}
	}
	return nil
}

func (t htmlTemplate) FuncMap() reflect.Value {
	return textFuncMap(reflect.ValueOf(*t.tmpl).FieldByName("text"))
}

func (t htmlTemplate) Execute(w io.Writer, data any) error {
	return t.tmpl.Execute(w, data)
}

// CheckText checks a text/template for problems. See CheckHTML for details.
func CheckText(t *ttmpl.Template, typeValue any) error {
	return check(textTemplate{t}, typeValue, false)
}

// CheckTextStrict does strict checking. See [CheckHTMLStrict] for details.
func CheckTextStrict(t *ttmpl.Template, typeValue any) error {
	return check(textTemplate{t}, typeValue, true)
}

type textTemplate struct {
	tmpl *ttmpl.Template
}

func (t textTemplate) Name() string      { return t.tmpl.Name() }
func (t textTemplate) Tree() *parse.Tree { return t.tmpl.Tree }

func (t textTemplate) Lookup(name string) template {
	if u := t.tmpl.Lookup(name); u != nil {
		return textTemplate{u}
	}
	return nil
}

func (t textTemplate) FuncMap() reflect.Value {
	return textFuncMap(reflect.ValueOf(t.tmpl))
}

func (t textTemplate) Execute(w io.Writer, data any) error {
	return t.tmpl.Execute(w, data)
}

func textFuncMap(textTmplPtr reflect.Value) reflect.Value {
	return textTmplPtr.Elem().FieldByName("parseFuncs")
}

// CheckSafe checks a github.com/google/safehtml/template for problems. See [CheckHTML] for details.
func CheckSafe(t *stmpl.Template, typeValue any) error {
	return check(safeTemplate{t}, typeValue, false)
}

// CheckSafeStrict does strict checking. See [CheckHTMLStrict] for details.
func CheckSafeStrict(t *stmpl.Template, typeValue any) error {
	return check(safeTemplate{t}, typeValue, true)
}

type safeTemplate struct {
	tmpl *stmpl.Template
}

func (t safeTemplate) Name() string      { return t.tmpl.Name() }
func (t safeTemplate) Tree() *parse.Tree { return t.tmpl.Tree }

func (t safeTemplate) Lookup(name string) template {
	if u := t.tmpl.Lookup(name); u != nil {
		return safeTemplate{u}
	}
	return nil
}
func (t safeTemplate) FuncMap() reflect.Value {
	return textFuncMap(reflect.ValueOf(*t.tmpl).FieldByName("text"))
}

func (t safeTemplate) Execute(w io.Writer, data any) error {
	return t.tmpl.Execute(w, data)
}

type state struct {
	tmpl             template
	node             parse.Node
	vars             []variable
	userFuncTypes    map[string]reflect.Type
	seen             map[string]bool         // template names seen, to avoid recursion
	tmplType         map[string]reflect.Type // dot types for associated templates (strict mode only)
	strict           bool                    // Ensure no type errors at exec time.
	onlyTruthMatters bool                    // see checkAndOr
}

type (
	// A type representing a template number, which does not correspond to any one
	// Go numeric type.
	number struct{}

	// A type representing an unknown type.
	unknown struct{}
)

var (
	byteType           = reflect.TypeOf(byte(0))
	boolType           = reflect.TypeOf(true)
	stringType         = reflect.TypeOf("")
	intType            = reflect.TypeOf(int(0))
	float64Type        = reflect.TypeOf(float64(0))
	complex128Type     = reflect.TypeOf(complex128(0))
	numberType         = reflect.TypeOf(number{})
	unknownType        = reflect.TypeOf(unknown{})
	emptyInterfaceType = reflect.TypeOf((*any)(nil)).Elem()
	reflectValueType   = reflect.TypeOf((*reflect.Value)(nil)).Elem()
	errorType          = reflect.TypeOf((*error)(nil)).Elem()
)

type checkError struct {
	err error
}

// check does the actual work. Its structure mirrors that of the template
// interpreter in text/template/exec.go. Much of the code was copied from there
// and other parts of the text/template implementation, and heavily modified.
// Roughly speaking, the changes involved replacing reflect.Value with
// reflect.Type.
func check(t template, dot any, strict bool) (err error) {
	defer func() {
		if e := recover(); e != nil {
			if cerr, ok := e.(checkError); ok {
				err = cerr.err
			} else {
				panic(e)
			}
		}
	}()

	dotType := reflect.TypeOf(dot)
	// A nil interface value has a nil reflect.Type. Use unknownType instead.
	if dot == nil {
		dotType = unknownType
	}

	s := &state{
		tmpl:          t,
		vars:          []variable{{"$", dotType}},
		seen:          map[string]bool{},
		tmplType:      map[string]reflect.Type{},
		userFuncTypes: map[string]reflect.Type{},
		strict:        strict,
	}
	tree := t.Tree()
	if tree == nil || tree.Root == nil {
		s.errorf("%q is an incomplete or empty template", t.Name())
	}

	iter := t.FuncMap().MapRange()
	for iter.Next() {
		s.userFuncTypes[iter.Key().String()] = iter.Value().Elem().Type()
	}

	s.walk(dotType, tree.Root)
	return nil
}

func (s *state) walk(dot reflect.Type, node parse.Node) {
	s.at(node)
	switch node := node.(type) {
	case *parse.ActionNode:
		// Do not pop variables so they persist until next end.
		_ = s.evalPipeline(dot, node.Pipe, false)

	case *parse.IfNode:
		s.walkIfOrWith(parse.NodeIf, dot, node.Pipe, node.List, node.ElseList)
	case *parse.ListNode:
		for _, node := range node.Nodes {
			s.walk(dot, node)
		}
	case *parse.RangeNode:
		s.walkRange(dot, node)
	case *parse.TemplateNode:
		s.walkTemplate(dot, node)
	case *parse.TextNode:
	case *parse.WithNode:
		s.walkIfOrWith(parse.NodeWith, dot, node.Pipe, node.List, node.ElseList)
	case *parse.BreakNode:
	case *parse.ContinueNode:
	default:
		s.errorf("internal error: unknown node: %s", node)
	}
}

// walkIfOrWith walks an 'if' or 'with' node. The two control structures
// are identical in behavior except that 'with' sets dot.
func (s *state) walkIfOrWith(ntype parse.NodeType, dot reflect.Type, pipe *parse.PipeNode, list, elseList *parse.ListNode) {
	mark := s.mark()
	defer s.pop(mark)

	// Evaluate the argument to if/with.
	// We still need to do this even in the case of `if`, for effects (var decls
	// and assignments). Decls in the pipeline are popped, but assignments
	// affect the state.
	typ := s.evalPipeline(dot, pipe, ntype == parse.NodeIf)

	if ntype == parse.NodeIf {
		typ = dot
	}
	ifVars := s.walkCopy(typ, list)
	var elseVars []variable
	if elseList != nil {
		elseVars = s.walkCopy(dot, elseList)
	}
	if !s.strict {
		// Join the two or three variable stacks, but don't go past where we're
		// going to pop anyway.
		s.joinVars(s.vars[:mark], ifVars, elseVars)
	}
}

// walkCopy walks node with dot on a copy of the variable stack, and returns the copy.
func (s *state) walkCopy(dot reflect.Type, node parse.Node) []variable {
	origVars := s.vars
	newVars := make([]variable, len(origVars))
	copy(newVars, origVars)
	s.vars = newVars
	s.walk(dot, node)
	s.vars = origVars
	return newVars
}

// joinVars joins the types of each variable in origVars with the corresponding
// variable in ifVars and elseVars. If the types are the same, nothing is done.
// Otherwise, the type in origVars is marked as unknown.
//
// Since variables are never removed or reordered, and the latter two slices
// were copied from origVars, the three slices have the same variable at the
// same index.
func (s *state) joinVars(origVars, ifVars, elseVars []variable) {
	// Check alignment.
	for i, ov := range origVars {
		if ov.name != ifVars[i].name || (elseVars != nil && ov.name != elseVars[i].name) {
			panic("name mismatch")
		}
	}
	// Only two of these three slices matter for comparisions:
	// - If elseVars is nil, there was no "else": one execution path kept the
	//   original type, the other used ifVars.
	// - If elseVars is non-nil, then one execution path used ifVars and one
	//   used elseVars; the original variable type doesn't matter.
	// However, we always write changes back to origVars, since that's the slice we'll
	// use subsequently.
	otherVars := elseVars
	if otherVars == nil {
		otherVars = origVars
	}
	for i := range origVars {
		it := ifVars[i].typ
		if it == otherVars[i].typ {
			origVars[i].typ = it
		} else {
			if s.strict {
				s.errorf("different types %s and %s for variable %s", it, otherVars[i].typ, ifVars[i].name)
			} else {
				origVars[i].typ = unknownType
			}
		}
	}
}

func (s *state) walkRange(dot reflect.Type, r *parse.RangeNode) {
	s.at(r)
	origMark := s.mark()
	defer s.pop(origMark)
	typ := indirectType(s.evalPipeline(dot, r.Pipe, false))

	if typ == unknownType {
		if s.strict {
			s.errorf("range can't iterate over unknown type")
		} else {
			return
		}
	}
	// mark top of stack before any variables in the body are pushed.
	mark := s.mark()
	checkBody := func(index, elem reflect.Type) []variable {
		// Set top var (lexically the second if there are two) to the element.
		if len(r.Pipe.Decl) > 0 {
			s.setTopVar(1, elem)
		}
		// Set next var (lexically the first if there are two) to the index.
		if len(r.Pipe.Decl) > 1 {
			s.setTopVar(2, index)
		}
		vars := s.walkCopy(elem, r.List)
		s.pop(mark)
		return vars
	}

	var rangeVars, elseVars []variable
	switch typ.Kind() {
	case reflect.Array, reflect.Slice:
		rangeVars = checkBody(intType, typ.Elem())
	case reflect.Map:
		rangeVars = checkBody(typ.Key(), typ.Elem())
	case reflect.Chan:
		if typ.ChanDir() == reflect.SendDir {
			s.errorf("range can't iterate over send-only channel %v", typ)
		}
		rangeVars = checkBody(intType, typ.Elem())
	case reflect.Interface:
		if s.strict {
			s.errorf("range can't iterate over type %v", typ)
		} else {
			// We can't assume anything about an interface type.
			return
		}
	default:
		s.errorf("range can't iterate over type %v", typ)
	}
	if r.ElseList != nil {
		elseVars = s.walkCopy(dot, r.ElseList)
	}
	if !s.strict {
		s.joinVars(s.vars[:origMark], rangeVars, elseVars)
	}
}

func (s *state) walkTemplate(dot reflect.Type, t *parse.TemplateNode) {
	if s.seen[t.Name] && !s.strict {
		return
	}
	s.at(t)
	s.seen[t.Name] = true
	tmpl := s.tmpl.Lookup(t.Name)
	if tmpl == nil {
		s.errorf("template %q not defined", t.Name)
	}
	// Variables declared by the pipeline persist.
	dot = s.evalPipeline(dot, t.Pipe, false)
	if s.strict {
		// All calls to the template must have the same type.
		if tt, ok := s.tmplType[t.Name]; ok {
			if dot != tt {
				s.errorf("inconsistent types for template %s: %s and %s", t.Name, typeString(tt), typeString(dot))
			}
			// The template argument types are the same, and we checked
			// the body previously, so nothing more to do.
			return
		}
		s.tmplType[t.Name] = dot
	}

	newState := *s
	newState.tmpl = tmpl
	// No dynamic scoping: template invocations inherit no variables.
	newState.vars = []variable{{"$", dot}}
	newState.walk(dot, tmpl.Tree().Root)
}

func (s *state) evalPipeline(dot reflect.Type, pipe *parse.PipeNode, onlyTruthMatters bool) reflect.Type {
	// If onlyTruthMatters is true, then strict mode doesn't need to ensure that
	// all args to `and` and `or` have the same type.
	if pipe == nil {
		return nil
	}
	s.at(pipe)
	var typ reflect.Type
	for _, cmd := range pipe.Cmds {
		typ = s.evalCommand(dot, cmd, typ, onlyTruthMatters) // previous type is this one's final arg
	}
	for _, variable := range pipe.Decl {
		if pipe.IsAssign {
			s.setVar(variable.Ident[0], typ)
		} else {
			s.push(variable.Ident[0], typ)
		}
	}
	return typ
}

func (s *state) evalCommand(dot reflect.Type, cmd *parse.CommandNode, final reflect.Type, onlyTruthMatters bool) reflect.Type {
	firstWord := cmd.Args[0]
	switch n := firstWord.(type) {
	case *parse.FieldNode:
		return s.evalFieldNode(dot, n, cmd.Args, final)
	case *parse.ChainNode:
		return s.evalChainNode(dot, n, cmd.Args, final, onlyTruthMatters)
	case *parse.IdentifierNode:
		// Must be a function.
		return s.evalFunction(dot, n, cmd, cmd.Args, final, onlyTruthMatters)
	case *parse.PipeNode:
		// Parenthesized pipeline. The arguments are all inside the pipeline; final must be absent.
		s.notAFunction(cmd.Args, final)
		return s.evalPipeline(dot, n, onlyTruthMatters)
	case *parse.VariableNode:
		return s.evalVariableNode(dot, n, cmd.Args, final)
	}
	s.at(firstWord)
	s.notAFunction(cmd.Args, final)
	switch word := firstWord.(type) {
	case *parse.BoolNode:
		return boolType
	case *parse.DotNode:
		return dot
	case *parse.NilNode:
		s.errorf("nil is not a command")
	case *parse.NumberNode:
		if s.strict {
			return s.idealConstantType(word)
		}
		return numberType
	case *parse.StringNode:
		return stringType
	}
	s.errorf("can't evaluate command %q", firstWord)
	panic("not reached")
}

func (s *state) notAFunction(args []parse.Node, final reflect.Type) {
	if len(args) > 1 || final != nil {
		s.errorf("can't give argument to non-function %s", args[0])
	}
}

func (s *state) evalFieldNode(dot reflect.Type, field *parse.FieldNode, args []parse.Node, final reflect.Type) reflect.Type {
	s.at(field)
	return s.evalFieldChain(dot, dot, field, field.Ident, args, final)
}

// evalFieldChain evaluates .X.Y.Z possibly followed by arguments.
// dot is the environment in which to evaluate arguments, while
// receiver is the value being walked along the chain.
func (s *state) evalFieldChain(dot, receiver reflect.Type, node parse.Node, ident []string, args []parse.Node, final reflect.Type) reflect.Type {
	n := len(ident)
	for i := 0; i < n-1; i++ {
		receiver = s.evalField(dot, ident[i], node, nil, nil, receiver)
	}
	// Now if it's a method, it gets the arguments.
	return s.evalField(dot, ident[n-1], node, args, final, receiver)
}

func (s *state) evalFunction(dot reflect.Type, node *parse.IdentifierNode, cmd parse.Node, args []parse.Node, final reflect.Type, onlyTruthMatters bool) reflect.Type {
	s.at(node)
	name := node.Ident
	fi := s.lookupFuncInfo(name)
	if fi == nil {
		s.errorf("%q is not a defined function", name)
	}
	return s.evalCall(dot, fi, cmd, name, args, final, onlyTruthMatters)
}

func (s *state) evalField(dot reflect.Type, fieldName string, node parse.Node, args []parse.Node, final, receiver reflect.Type) reflect.Type {
	if receiver == unknownType {
		if s.strict {
			s.errorf("cannot access field %q of unknown type", fieldName)
		} else {
			return unknownType
		}
	}
	receiver = indirectType(receiver)
	// Unless it's an interface, need to get to a value of type *T to guarantee
	// we see all methods of T and *T.
	ptr := receiver
	if ptr.Kind() != reflect.Interface && ptr.Kind() != reflect.Ptr {
		ptr = reflect.PtrTo(ptr)
	}
	if method, ok := ptr.MethodByName(fieldName); ok {
		// If method.Func is nil, method.Type describes the function
		// without a receiver, which is what evalCall wants.
		// If method.Func is not nil, then method.Type has the receiver
		// as a first arg, so create a new function type without the first arg.
		mt := method.Type
		if method.Func.IsValid() {
			ins := make([]reflect.Type, mt.NumIn()-1)
			for i := 1; i < mt.NumIn(); i++ {
				ins[i-1] = mt.In(i)
			}
			outs := make([]reflect.Type, mt.NumOut())
			for i := 0; i < mt.NumOut(); i++ {
				outs[i] = mt.Out(i)
			}
			mt = reflect.FuncOf(ins, outs, mt.IsVariadic())
		}
		return s.evalCall(dot, &funcInfo{typ: mt}, node, fieldName, args, final, false)
	}
	hasArgs := len(args) > 1 || final != nil
	// It's not a method; must be a field of a struct or an element of a map.
	switch receiver.Kind() {
	case reflect.Struct:
		tField, ok := receiver.FieldByName(fieldName)
		if ok {
			if tField.PkgPath != "" { // field is unexported
				s.errorf("%s is an unexported field of struct type %s", fieldName, receiver)
			}
			// If it's a function, we must call it.
			if hasArgs {
				s.errorf("%s has arguments but cannot be invoked as function", fieldName)
			}
			return tField.Type
		}
	case reflect.Map:
		// If it's a map, attempt to use the field name as a key.
		if stringType.AssignableTo(receiver.Key()) {
			if hasArgs {
				s.errorf("%s is not a method but has arguments", fieldName)
			}
			return receiver.Elem()
		}

	case reflect.Interface:
		// We can't assume anything about what's in an interface.
		if s.strict {
			s.errorf("cannot access field or map element of interface type %s", typeString(receiver))
		} else {
			return unknownType
		}
		// A reflect.Ptr case appears in the template interpreter, but can't
		// happen here because indirectType never returns a Ptr.
	}
	s.errorf("can't use field %s in type %s", fieldName, receiver)
	panic("not reached")
}

func (s *state) evalChainNode(dot reflect.Type, chain *parse.ChainNode, args []parse.Node, final reflect.Type, onlyTruthMatters bool) reflect.Type {
	s.at(chain)
	if len(chain.Field) == 0 {
		s.errorf("internal error: no fields in evalChainNode")
	}
	// (pipe).Field1.Field2 has pipe as .Node, fields as .Field. Eval the pipeline, then the fields.
	// The only other possibility is ident.Field1..., that is, a nilary function call.
	var typ reflect.Type
	switch n := chain.Node.(type) {
	case *parse.PipeNode:
		typ = s.evalPipeline(dot, n, onlyTruthMatters)
	case *parse.IdentifierNode:
		typ = s.evalFunction(dot, n, n, nil, nil, onlyTruthMatters)
	default:
		s.errorf("internal error: chain.Node has type %T, not PipeNode or IdentifierNode", n)
	}
	return s.evalFieldChain(dot, typ, chain, chain.Field, args, final)
}

func (s *state) evalVariableNode(dot reflect.Type, variable *parse.VariableNode, args []parse.Node, final reflect.Type) reflect.Type {
	// $x.Field has $x as the first ident, Field as the second. Eval the var, then the fields.
	s.at(variable)
	typ := s.varType(variable.Ident[0])
	if len(variable.Ident) == 1 {
		s.notAFunction(args, final)
		return typ
	}
	return s.evalFieldChain(dot, typ, variable, variable.Ident[1:], args, final)
}

// evalCall checks a function or method call. If it's a method, fun already has the receiver bound, so
// it looks just like a function call. The arg list, if non-nil, includes (in the manner of the shell), arg[0]
// as the function itself.
func (s *state) evalCall(dot reflect.Type, fi *funcInfo, node parse.Node, name string, args []parse.Node, final reflect.Type, onlyTruthMatters bool) reflect.Type {
	if args != nil {
		args = args[1:] // Zeroth arg is function name/node; not passed to function.
	}
	numIn := len(args)
	if final != nil {
		numIn++
	}
	numFixed := len(args)
	if fi.typ.IsVariadic() {
		numFixed = fi.typ.NumIn() - 1 // last arg is the variadic one.
		if numIn < numFixed {
			s.errorf("wrong number of args for %s: want at least %d, got %d", name, fi.typ.NumIn()-1, len(args))
		}
	} else if numIn != fi.typ.NumIn() {
		s.errorf("wrong number of args for %s: want %d, got %d", name, fi.typ.NumIn(), numIn)
	}
	// Call custom arg-checker if there is one.
	if fi.checkArgs != nil {
		// See checkAndOr for onlyTruthMatters.
		defer func(b bool) { s.onlyTruthMatters = b }(s.onlyTruthMatters)
		s.onlyTruthMatters = onlyTruthMatters
		return fi.checkArgs(s, dot, args)
	}
	// Args must be checked. Fixed args first.
	i := 0
	for ; i < numFixed && i < len(args); i++ {
		s.checkArg(dot, fi.typ.In(i), args[i])
	}
	// Now the ... args.
	if fi.typ.IsVariadic() {
		argType := fi.typ.In(fi.typ.NumIn() - 1).Elem() // Argument is a slice.
		for ; i < len(args); i++ {
			s.checkArg(dot, argType, args[i])
		}
	}

	return fi.typ.Out(0)
}

// validateType guarantees that the argument type is assignable to the formal type.
func (s *state) validateType(argType, formalType reflect.Type) {
	if formalType == nil || formalType == unknownType {
		s.errorf("internal error: bad formalType %v", formalType)
	}
	if !s.strict {
		// If we don't know the argument type, assume we can assign.
		if argType == unknownType {
			return
		}
		// If the argument is of interface type, we can't tell here whether the
		// assignment will succeed. Be conservative.
		if argType.Kind() == reflect.Interface {
			return
		}
		// If the argument is numberType, be conservative and assume it can be
		// converted to any numeric formal type.
		if argType == numberType && isNumericType(formalType) {
			return
		}
		// If either the argument or the formal is reflect.Value, be conservative.
		if argType == reflectValueType || formalType == reflectValueType {
			return
		}
	}
	if argType.AssignableTo(formalType) {
		return
	}
	// If the argument is a pointer, it will be dereferenced.
	if argType.Kind() == reflect.Ptr && argType.Elem().AssignableTo(formalType) {
		return
	}
	// If a pointer to the argument is assignable, then its address will be taken.
	if pt := reflect.PtrTo(argType); pt.AssignableTo(formalType) {
		return
	}
	s.errorf("wrong type: expected %s; found %s", formalType, argType)
}

// evalArg evaluates n as a function argument. It returns the resulting type and
// whether the node was a literal (Nil, Bool, String or Number).
func (s *state) evalArg(dot reflect.Type, n parse.Node, onlyTruthMatters bool) (reflect.Type, bool) {
	s.at(n)
	switch n := n.(type) {
	case *parse.DotNode:
		return dot, false
	case *parse.FieldNode:
		return s.evalFieldNode(dot, n, []parse.Node{n}, nil), false
	case *parse.VariableNode:
		return s.evalVariableNode(dot, n, nil, nil), false
	case *parse.PipeNode:
		return s.evalPipeline(dot, n, onlyTruthMatters), false
	case *parse.IdentifierNode:
		return s.evalFunction(dot, n, n, nil, nil, onlyTruthMatters), false
	case *parse.ChainNode:
		return s.evalChainNode(dot, n, nil, nil, onlyTruthMatters), false
	case *parse.NilNode:
		return nil, true
	case *parse.BoolNode:
		return boolType, true
	case *parse.StringNode:
		return stringType, true
	case *parse.NumberNode:
		return s.idealConstantType(n), true
	}
	s.errorf("internal error: unexpected node type %T in evalNonLiteralArg", n)
	panic("not reached")
}

// idealConstantType is called to return the type of a number in a context where
// we don't know the type. In that case, the syntax of the number tells us its
// type, and we use Go rules to resolve. Note there is no such thing as a uint
// ideal constant in this situation - the value must be of int type.
func (s *state) idealConstantType(constant *parse.NumberNode) reflect.Type {
	// These are ideal constants but we don't know the type
	// and we have no context.  (If it was a method argument,
	// we'd know what we need.) The syntax guides us to some extent.
	s.at(constant)
	switch {
	case constant.IsComplex:
		return complex128Type

	case constant.IsFloat &&
		!isHexInt(constant.Text) && !isRuneInt(constant.Text) &&
		strings.ContainsAny(constant.Text, ".eEpP"):
		return float64Type

	case constant.IsInt:
		n := int(constant.Int64)
		if int64(n) != constant.Int64 {
			s.errorf("%s overflows int", constant.Text)
		}
		return intType

	case constant.IsUint:
		s.errorf("%s overflows int", constant.Text)
	}
	return numberType
}

func isRuneInt(s string) bool {
	return len(s) > 0 && s[0] == '\''
}

func isHexInt(s string) bool {
	return len(s) > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X') && !strings.ContainsAny(s, "pP")
}

// checkArg checks an argument to a function.
func (s *state) checkArg(dot, formalType reflect.Type, arg parse.Node) {
	s.at(arg)
	argType, isLiteral := s.evalArg(dot, arg, false)
	if !isLiteral {
		s.validateType(argType, formalType)
		return
	}
	if argType == nil { // literal nil
		if !canBeNil(formalType) {
			s.errorf("cannot assign nil to %s", formalType)
		}
		return
	}
	// Handle literals like Go untyped constants: a bool literal can be assigned
	// to any type whose underlying type is bool, etc.
	switch formalType.Kind() {
	case reflect.Bool:
		if _, ok := arg.(*parse.BoolNode); !ok {
			s.wrongTypeErr(formalType, arg)
		}
		return
	case reflect.String:
		if _, ok := arg.(*parse.StringNode); !ok {
			s.wrongTypeErr(formalType, arg)
		}
		return
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if nn, ok := arg.(*parse.NumberNode); !ok || !nn.IsInt {
			s.wrongTypeErr(formalType, arg)
		}
		return
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		if nn, ok := arg.(*parse.NumberNode); !ok || !nn.IsUint {
			s.wrongTypeErr(formalType, arg)
		}
		return
	case reflect.Float32, reflect.Float64:
		if nn, ok := arg.(*parse.NumberNode); !ok || !nn.IsFloat {
			s.wrongTypeErr(formalType, arg)
		}
		return
	case reflect.Complex64, reflect.Complex128:
		if nn, ok := arg.(*parse.NumberNode); !ok || !nn.IsComplex {
			s.wrongTypeErr(formalType, arg)
		}
		return
	case reflect.Interface: // Any argument can be assigned to an any.
		if formalType.NumMethod() == 0 {
			return
		}
	case reflect.Struct: // The only acceptable formal struct type is reflect.Value.
		if formalType == reflectValueType {
			return
		}
	}
	s.errorf("can't handle %s for arg of type %s", arg, formalType)
}

func (s *state) wrongTypeErr(typ reflect.Type, n parse.Node) {
	s.at(n)
	s.errorf("wrong type: expected %s; found %s", typ, n)
}

// canBeNil reports whether an untyped nil can be assigned to the type. See reflect.Zero.
func canBeNil(typ reflect.Type) bool {
	switch typ.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Ptr, reflect.Slice:
		return true
	case reflect.Struct:
		return typ == reflectValueType
	}
	return false
}

func isNumericType(t reflect.Type) bool {
	return isIntegerType(t) || isFloatType(t) || isComplexType(t)
}

func isIntegerType(t reflect.Type) bool {
	if t == nil {
		return false
	}
	switch t.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return true
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return true
	default:
		return false
	}
}

func isFloatType(t reflect.Type) bool {
	if t == nil {
		return false
	}
	switch t.Kind() {
	case reflect.Float32, reflect.Float64:
		return true
	default:
		return false
	}
}

func isComplexType(t reflect.Type) bool {
	if t == nil {
		return false
	}
	switch t.Kind() {
	case reflect.Complex64, reflect.Complex128:
		return true
	default:
		return false
	}
}

func indirectType(t reflect.Type) reflect.Type {
	if t == unknownType {
		return unknownType
	}
	for t.Kind() == reflect.Ptr {
		t = t.Elem()
	}
	return t
}

// variable holds the type of a variable such as $, $x etc.
type variable struct {
	name string
	typ  reflect.Type
}

// push pushes a new variable on the stack.
func (s *state) push(name string, typ reflect.Type) {
	s.vars = append(s.vars, variable{name, typ})
}

// mark returns the length of the variable stack.
func (s *state) mark() int {
	return len(s.vars)
}

// pop pops the variable stack up to the mark.
func (s *state) pop(mark int) {
	s.vars = s.vars[0:mark]
}

// setVar overwrites the last declared variable with the given name.
// Used by variable assignments.
func (s *state) setVar(name string, typ reflect.Type) {
	for i := s.mark() - 1; i >= 0; i-- {
		v := s.vars[i]
		if v.name == name {
			if s.strict && !typ.AssignableTo(v.typ) {
				s.errorf("cannot assign type %s to variable %s of type %s", typ, v.name, v.typ)
			}
			s.vars[i].typ = typ
			return
		}
	}
	s.errorf("undefined variable: %s", name)
}

// setTopVar overwrites the top-nth variable on the stack. Used by range iterations.
func (s *state) setTopVar(n int, typ reflect.Type) {
	s.vars[len(s.vars)-n].typ = typ
}

// varType returns the type of the named variable.
func (s *state) varType(name string) reflect.Type {
	for i := s.mark() - 1; i >= 0; i-- {
		if s.vars[i].name == name {
			return s.vars[i].typ
		}
	}
	s.errorf("undefined variable: %s", name)
	return unknownType
}

// at marks the state to be on node n, for error reporting.
func (s *state) at(node parse.Node) {
	s.node = node
}

// errorf records an ExecError and terminates processing.
func (s *state) errorf(format string, args ...any) {
	name := doublePercent(s.tmpl.Name())
	if s.node == nil {
		format = fmt.Sprintf("template: %s: %s", name, format)
	} else {
		location, context := s.tmpl.Tree().ErrorContext(s.node)
		format = fmt.Sprintf("template: %s: checking %q at <%s>: %s", location, name, doublePercent(context), format)
	}
	panic(checkError{fmt.Errorf(format, args...)})
}

// doublePercent returns the string with %'s replaced by %%, if necessary,
// so it can be used safely inside a Printf format string.
func doublePercent(str string) string {
	return strings.ReplaceAll(str, "%", "%%")
}

type funcInfo struct {
	typ       reflect.Type                                          // function signature
	checkArgs func(*state, reflect.Type, []parse.Node) reflect.Type // if non-nil, call to check arg types
}

// lookupFuncInfo returns the funcInfo for the function with the given name, or nil if
// there is no such function.
func (s *state) lookupFuncInfo(name string) *funcInfo {
	if t := s.userFuncTypes[name]; t != nil {
		return &funcInfo{typ: t}
	}
	return builtinFuncInfos[name]
}

var (
	oneOrMoreValues = []reflect.Type{reflectValueType, reflect.SliceOf(reflectValueType)}
	valueOrError    = []reflect.Type{reflectValueType, errorType}
	boolOrError     = []reflect.Type{boolType, errorType}

	boolFuncType       = reflect.FuncOf(oneOrMoreValues, []reflect.Type{reflectValueType}, true)
	comparisonFuncType = reflect.FuncOf([]reflect.Type{reflectValueType, reflectValueType}, boolOrError, false)
)

// Can't initialize directly because of a reference loop with checkLen.
var builtinFuncInfos map[string]*funcInfo

func init() {
	builtinFuncInfos = map[string]*funcInfo{
		"and": &funcInfo{
			typ:       boolFuncType,
			checkArgs: checkAndOr,
		},
		"or": &funcInfo{
			typ:       boolFuncType,
			checkArgs: checkAndOr,
		},
		"call": &funcInfo{typ: reflect.FuncOf(oneOrMoreValues, valueOrError, true)},
		"index": &funcInfo{
			typ:       reflect.FuncOf(oneOrMoreValues, valueOrError, true),
			checkArgs: checkIndex,
		},
		// TODO(#2): Use more knowledge about slice.
		"slice": &funcInfo{
			typ:       reflect.FuncOf([]reflect.Type{reflectValueType, reflect.SliceOf(numberType)}, valueOrError, true),
			checkArgs: checkSlice,
		},
		"len": &funcInfo{
			typ:       reflect.TypeOf(func(reflect.Value) (int, error) { return 0, nil }),
			checkArgs: checkLen,
		},
		"not": &funcInfo{
			typ:       reflect.TypeOf(func(reflect.Value) bool { return false }),
			checkArgs: checkNot,
		},
		"html":     &funcInfo{typ: reflect.TypeOf(ttmpl.HTMLEscaper)},
		"js":       &funcInfo{typ: reflect.TypeOf(ttmpl.JSEscaper)},
		"print":    &funcInfo{typ: reflect.TypeOf(fmt.Sprint)},
		"printf":   &funcInfo{typ: reflect.TypeOf(fmt.Sprintf)},
		"println":  &funcInfo{typ: reflect.TypeOf(fmt.Sprintln)},
		"urlquery": &funcInfo{typ: reflect.TypeOf(ttmpl.URLQueryEscaper)},

		// Comparisons
		// TODO(#2): Use more knowledge about comparison functions.
		"eq": &funcInfo{
			typ:       reflect.FuncOf(oneOrMoreValues, boolOrError, true),
			checkArgs: checkEq,
		},
		"ne": &funcInfo{typ: comparisonFuncType, checkArgs: checkEq},
		"ge": &funcInfo{typ: comparisonFuncType, checkArgs: checkOrderedComparison},
		"gt": &funcInfo{typ: comparisonFuncType, checkArgs: checkOrderedComparison},
		"le": &funcInfo{typ: comparisonFuncType, checkArgs: checkOrderedComparison},
		"lt": &funcInfo{typ: comparisonFuncType, checkArgs: checkOrderedComparison},
	}
}
