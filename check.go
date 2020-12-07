/* TODO
   - Increase coverage.
   - Look at chain nodes that occur as operands. See template/parse/parse.go:Tree.operand.
   - We only care about the result of evalArg in evalChainNode, where the formal type is always nil. In that case validateType always
     returns argType. So we can split evalArg into two funcs, one where the formal is nil and we care about the result, and
     one where it isn't and we don't.
   - Test a chain node with a nil, like `{{(nil).X}}`.
     The nil case in evalArg returns typ, which would be nil here, which is bad.
     I'm not sure how to generate a nil in that position; I get the error "nil
     is not a command" when I try.
*/

package templatecheck

import (
	"fmt"
	htmpl "html/template"
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
}

func CheckText(t *ttmpl.Template, typeValue interface{}, funcMaps ...map[string]interface{}) error {
	return check(textTemplate{t}, reflect.TypeOf(typeValue), funcMaps)
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

func CheckHTML(t *htmpl.Template, typeValue interface{}, funcMaps ...map[string]interface{}) error {
	return check(htmlTemplate{t}, reflect.TypeOf(typeValue), funcMaps)
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

func CheckSafe(t *stmpl.Template, typeValue interface{}, funcMaps ...map[string]interface{}) error {
	return check(safeTemplate{t}, reflect.TypeOf(typeValue), funcMaps)
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

type state struct {
	tmpl      template
	node      parse.Node
	vars      []variable
	funcTypes map[string]reflect.Type
	seen      map[string]bool // template names seen, to avoid recursion
}

type (
	// A type representing a template number, which does not correspond to any one
	// Go numeric type.
	number struct{}

	// A type representing an unknown type.
	unknown struct{}
)

var (
	intType            = reflect.TypeOf(int(0))
	boolType           = reflect.TypeOf(true)
	stringType         = reflect.TypeOf("")
	numberType         = reflect.TypeOf(number{})
	unknownType        = reflect.TypeOf(unknown{})
	emptyInterfaceType = reflect.TypeOf((*interface{})(nil)).Elem()
	reflectValueType   = reflect.TypeOf((*reflect.Value)(nil)).Elem()
	errorType          = reflect.TypeOf((*error)(nil)).Elem()
	boolFuncType       = reflect.TypeOf(func(reflect.Value, ...reflect.Value) reflect.Value { return reflect.Value{} })
)

type checkError struct {
	err error
}

func check(t template, dot reflect.Type, funcMaps []map[string]interface{}) (err error) {
	defer func() {
		if e := recover(); e != nil {
			if cerr, ok := e.(checkError); ok {
				err = cerr.err
			} else {
				panic(e)
			}
		}
	}()

	s := &state{
		tmpl: t,
		vars: []variable{{"$", dot}},
		seen: map[string]bool{},
	}
	tree := t.Tree()
	if tree == nil || tree.Root == nil {
		s.errorf("%q is an incomplete or empty template", t.Name())
	}
	if len(funcMaps) > 0 {
		s.funcTypes = map[string]reflect.Type{}
		for _, m := range funcMaps {
			for name, f := range m {
				s.funcTypes[name] = reflect.TypeOf(f)
			}
		}
	}

	s.walk(dot, tree.Root)
	return nil
}

func (s *state) walk(dot reflect.Type, node parse.Node) {
	s.at(node)
	switch node := node.(type) {
	case *parse.ActionNode:
		// Do not pop variables so they persist until next end.
		_ = s.evalPipeline(dot, node.Pipe)

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
	default:
		s.errorf("unknown node: %s", node)
	}
}

// walkIfOrWith walks an 'if' or 'with' node. The two control structures
// are identical in behavior except that 'with' sets dot.
func (s *state) walkIfOrWith(ntype parse.NodeType, dot reflect.Type, pipe *parse.PipeNode, list, elseList *parse.ListNode) {
	mark := s.mark()
	defer s.pop(mark)

	// Evaluate the argument to if/walk.
	// We still need to do this even in the case of `if`, for effects (var decls
	// and assignments). Decls in the pipeline are popped, but assignments
	// affect the state.
	typ := s.evalPipeline(dot, pipe)

	if ntype == parse.NodeIf {
		typ = dot //
	}
	ifVars := s.walkCopy(typ, list)
	var elseVars []variable
	if elseList != nil {
		elseVars = s.walkCopy(dot, elseList)
	}
	// Join the two or three variable stacks, but don't go past where we're
	// going to pop anyway.
	joinVars(s.vars[:mark], ifVars, elseVars)
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
func joinVars(origVars, ifVars, elseVars []variable) {
	// Check alignment.
	for i, ov := range origVars {
		if ov.name != ifVars[i].name || (elseVars != nil && ov.name != elseVars[i].name) {
			panic("name mismatch")
		}
	}
	for i := range origVars {
		ot := origVars[i].typ
		if ifVars[i].typ != ot || (elseVars != nil && elseVars[i].typ != ot) {
			origVars[i].typ = unknownType
		}
	}
}

func (s *state) walkRange(dot reflect.Type, r *parse.RangeNode) {
	s.at(r)
	origMark := s.mark()
	defer s.pop(origMark)
	typ := indirectType(s.evalPipeline(dot, r.Pipe))

	if typ == unknownType {
		return
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
	case reflect.Invalid:
		// An invalid value is likely a nil map, etc. and acts like an empty map.
	case reflect.Interface:
		// We can't assume anything about an interface type.
		return
	default:
		s.errorf("range can't iterate over type %v", typ)
	}
	if r.ElseList != nil {
		elseVars = s.walkCopy(dot, r.ElseList)
	}
	joinVars(s.vars[:origMark], rangeVars, elseVars)
}

func (s *state) walkTemplate(dot reflect.Type, t *parse.TemplateNode) {
	if s.seen[t.Name] {
		return
	}
	s.seen[t.Name] = true
	s.at(t)
	tmpl := s.tmpl.Lookup(t.Name)
	if tmpl == nil {
		s.errorf("template %q not defined", t.Name)
	}
	// Variables declared by the pipeline persist.
	dot = s.evalPipeline(dot, t.Pipe)
	newState := *s
	newState.tmpl = tmpl
	// No dynamic scoping: template invocations inherit no variables.
	newState.vars = []variable{{"$", dot}}
	newState.walk(dot, tmpl.Tree().Root)
}

func (s *state) evalPipeline(dot reflect.Type, pipe *parse.PipeNode) reflect.Type {
	if pipe == nil {
		return nil
	}
	s.at(pipe)
	var typ reflect.Type
	for _, cmd := range pipe.Cmds {
		typ = s.evalCommand(dot, cmd, typ) // previous type is this one's final arg
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

func (s *state) evalCommand(dot reflect.Type, cmd *parse.CommandNode, final reflect.Type) reflect.Type {
	firstWord := cmd.Args[0]
	switch n := firstWord.(type) {
	case *parse.FieldNode:
		return s.evalFieldNode(dot, n, cmd.Args, final)
	case *parse.ChainNode:
		return s.evalChainNode(dot, n, cmd.Args, final)
	case *parse.IdentifierNode:
		// Must be a function.
		return s.evalFunction(dot, n, cmd, cmd.Args, final)
	case *parse.PipeNode:
		// Parenthesized pipeline. The arguments are all inside the pipeline; final must be absent.
		s.notAFunction(cmd.Args, final)
		return s.evalPipeline(dot, n)
	case *parse.VariableNode:
		return s.evalVariableNode(dot, n, cmd.Args, final)
	}
	s.at(firstWord)
	s.notAFunction(cmd.Args, final)
	switch firstWord.(type) {
	case *parse.BoolNode:
		return boolType
	case *parse.DotNode:
		return dot
	case *parse.NilNode:
		s.errorf("nil is not a command")
	case *parse.NumberNode:
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

func (s *state) evalFunction(dot reflect.Type, node *parse.IdentifierNode, cmd parse.Node, args []parse.Node, final reflect.Type) reflect.Type {
	s.at(node)
	name := node.Ident
	ft := s.lookupFuncType(name)
	if ft == nil {
		s.errorf("%q is not a defined function", name)
	}
	return s.evalCall(dot, ft, cmd, name, args, final)
}

func (s *state) evalField(dot reflect.Type, fieldName string, node parse.Node, args []parse.Node, final, receiver reflect.Type) reflect.Type {
	if receiver == unknownType {
		return unknownType
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
		return s.evalCall(dot, mt, node, fieldName, args, final)
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
		return unknownType

		// A reflect.Ptr case appears in the template interpreter, but can't
		// happen here because indirectType never returns a Ptr.
	}
	s.errorf("can't use field %s in type %s", fieldName, receiver)
	panic("not reached")
}

func (s *state) evalChainNode(dot reflect.Type, chain *parse.ChainNode, args []parse.Node, final reflect.Type) reflect.Type {
	s.at(chain)
	if len(chain.Field) == 0 {
		s.errorf("internal error: no fields in evalChainNode")
	}
	if chain.Node.Type() == parse.NodeNil {
		s.errorf("indirection through explicit nil in %s", chain)
	}
	// (pipe).Field1.Field2 has pipe as .Node, fields as .Field. Eval the pipeline, then the fields.
	pipe := s.evalArg(dot, nil, chain.Node)
	return s.evalFieldChain(dot, pipe, chain, chain.Field, args, final)
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

// evalCall checks  a function or method call. If it's a method, fun already has the receiver bound, so
// it looks just like a function call. The arg list, if non-nil, includes (in the manner of the shell), arg[0]
// as the function itself.
func (s *state) evalCall(dot, typ reflect.Type, node parse.Node, name string, args []parse.Node, final reflect.Type) reflect.Type {
	if args != nil {
		args = args[1:] // Zeroth arg is function name/node; not passed to function.
	}
	numIn := len(args)
	if final != nil {
		numIn++
	}
	numFixed := len(args)
	if typ.IsVariadic() {
		numFixed = typ.NumIn() - 1 // last arg is the variadic one.
		if numIn < numFixed {
			s.errorf("wrong number of args for %s: want at least %d, got %d", name, typ.NumIn()-1, len(args))
		}
	} else if numIn != typ.NumIn() {
		s.errorf("wrong number of args for %s: want %d, got %d", name, typ.NumIn(), numIn)
	}
	// Args must be checked. Fixed args first.
	i := 0
	for ; i < numFixed && i < len(args); i++ {
		_ = s.evalArg(dot, typ.In(i), args[i])
	}
	// Now the ... args.
	if typ.IsVariadic() {
		argType := typ.In(typ.NumIn() - 1).Elem() // Argument is a slice.
		for ; i < len(args); i++ {
			_ = s.evalArg(dot, argType, args[i])
		}
	}

	return typ.Out(0)
}

// validateType guarantees that the argument type is assignable to the formal type.
func (s *state) validateType(argType, formalType reflect.Type) reflect.Type {
	// If we don't know the formal or arg's type, assume we can assign.
	if formalType == nil || formalType == unknownType || argType == unknownType {
		return argType
	}
	if argType.AssignableTo(formalType) {
		return argType
	}
	// If the argument is of interface type, we can't tell here whether the
	// assignment will succeed. Be conservative.
	if argType.Kind() == reflect.Interface {
		return argType
	}
	// If the argument is numberType, be conservative and assume it can be
	// converted to any numeric formal type.
	if argType == numberType && isNumericType(formalType) {
		return argType
	}
	// If the formal is reflect.Value, the argument will be reflected.
	if formalType == reflectValueType {
		return reflectValueType
	}
	// If the argument is a pointer, it will be dereferenced.
	if argType.Kind() == reflect.Ptr && argType.Elem().AssignableTo(formalType) {
		return argType.Elem()
	}
	// If a pointer to the argument is assignable, then its address will be taken.
	if pt := reflect.PtrTo(argType); pt.AssignableTo(formalType) {
		return pt
	}
	s.errorf("wrong type: expected %s; found %s", formalType, argType)
	panic("not reached")
}

// evalArg evaluates an argument to a function. It is also used (in
// evalChainNode) to evaluation a general expression. typ is the type of the
// formal parameter, or nil if there isn't one (as in evalChainNode).
func (s *state) evalArg(dot, typ reflect.Type, n parse.Node) reflect.Type {
	s.at(n)
	switch arg := n.(type) {
	case *parse.DotNode:
		return s.validateType(dot, typ)
	case *parse.NilNode:
		if canBeNil(typ) {
			return typ
		}
		s.errorf("cannot assign nil to %s", typ)
	case *parse.FieldNode:
		return s.validateType(s.evalFieldNode(dot, arg, []parse.Node{n}, nil), typ)
	case *parse.VariableNode:
		return s.validateType(s.evalVariableNode(dot, arg, nil, nil), typ)
	case *parse.PipeNode:
		return s.validateType(s.evalPipeline(dot, arg), typ)
	case *parse.IdentifierNode:
		return s.validateType(s.evalFunction(dot, arg, arg, nil, unknownType), typ)
	case *parse.ChainNode:
		return s.validateType(s.evalChainNode(dot, arg, nil, nil), typ)
	}
	switch typ.Kind() {
	case reflect.Bool:
		_, ok := n.(*parse.BoolNode)
		return s.evalPrim(typ, n, ok)
	case reflect.String:
		_, ok := n.(*parse.StringNode)
		return s.evalPrim(typ, n, ok)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		nn, ok := n.(*parse.NumberNode)
		return s.evalPrim(typ, n, ok && nn.IsInt)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		nn, ok := n.(*parse.NumberNode)
		return s.evalPrim(typ, n, ok && nn.IsUint)
	case reflect.Float32, reflect.Float64:
		nn, ok := n.(*parse.NumberNode)
		return s.evalPrim(typ, n, ok && nn.IsFloat)
	case reflect.Complex64, reflect.Complex128:
		nn, ok := n.(*parse.NumberNode)
		return s.evalPrim(typ, n, ok && nn.IsComplex)
	case reflect.Interface:
		if typ.NumMethod() == 0 {
			return s.evalEmptyInterface(dot, n)
		}
	case reflect.Struct:
		if typ == reflectValueType {
			return reflectValueType
		}
	}
	s.errorf("can't handle %s for arg of type %s", n, typ)
	panic("not reached")
}

func (s *state) evalPrim(formalType reflect.Type, n parse.Node, ok bool) reflect.Type {
	s.at(n)
	if ok {
		return formalType
	}
	s.errorf("wrong type: expected %s; found %s", formalType, n)
	panic("not reached")
}

func (s *state) evalEmptyInterface(dot reflect.Type, n parse.Node) reflect.Type {
	s.at(n)
	switch n := n.(type) {
	case *parse.BoolNode:
		return boolType
	case *parse.DotNode:
		return dot
	case *parse.FieldNode:
		return s.evalFieldNode(dot, n, nil, nil)
	case *parse.IdentifierNode:
		return s.evalFunction(dot, n, n, nil, nil)
	case *parse.NilNode:
		// NilNode is handled in evalArg, the only place that calls here.
		s.errorf("evalEmptyInterface: nil (can't happen)")
	case *parse.NumberNode:
		return numberType
	case *parse.StringNode:
		return stringType
	case *parse.VariableNode:
		return s.evalVariableNode(dot, n, nil, nil)
	case *parse.PipeNode:
		return s.evalPipeline(dot, n)
	}
	s.errorf("can't handle assignment of %s to empty interface argument", n)
	panic("not reached")
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
	switch t.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return true
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return true
	case reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128:
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
		if s.vars[i].name == name {
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
func (s *state) errorf(format string, args ...interface{}) {
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

var comparisonFuncType = reflect.FuncOf([]reflect.Type{reflectValueType, reflectValueType}, []reflect.Type{boolType, errorType}, false)

var builtinFuncTypes = map[string]reflect.Type{
	"and": boolFuncType,
	"or":  boolFuncType,
	"call": reflect.FuncOf(
		[]reflect.Type{reflectValueType, reflect.SliceOf(reflectValueType)},
		[]reflect.Type{reflectValueType, errorType},
		true),
	"html": reflect.TypeOf(ttmpl.HTMLEscaper),
	// TODO: Use more knowledge about index and slice.
	"index": reflect.FuncOf(
		[]reflect.Type{reflectValueType, reflect.SliceOf(reflectValueType)},
		[]reflect.Type{reflectValueType, errorType},
		true),
	"slice": reflect.FuncOf(
		[]reflect.Type{reflectValueType, reflect.SliceOf(numberType)},
		[]reflect.Type{reflectValueType, errorType},
		true),
	"js":       reflect.TypeOf(ttmpl.JSEscaper),
	"len":      reflect.TypeOf(func(reflect.Value) (int, error) { return 0, nil }),
	"not":      reflect.TypeOf(func(reflect.Value) bool { return false }),
	"print":    reflect.TypeOf(fmt.Sprint),
	"printf":   reflect.TypeOf(fmt.Sprintf),
	"println":  reflect.TypeOf(fmt.Sprintln),
	"urlquery": reflect.TypeOf(ttmpl.URLQueryEscaper),

	// Comparisons
	// TODO: Use more knowledge about comparison functions.
	"eq": reflect.FuncOf(
		[]reflect.Type{reflectValueType, reflect.SliceOf(reflectValueType)},
		[]reflect.Type{boolType, errorType},
		true),
	"ge": comparisonFuncType,
	"gt": comparisonFuncType,
	"le": comparisonFuncType,
	"lt": comparisonFuncType,
	"ne": comparisonFuncType,
}

// lookupFuncType returns the function with the given name. It returns nil if there is no such function.
func (s *state) lookupFuncType(name string) reflect.Type {
	if t := s.funcTypes[name]; t != nil {
		return t
	}
	return builtinFuncTypes[name]
}
