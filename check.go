/* TODO

   Use walkCopy in range/else.
*/

package templatecheck

import (
	"fmt"
	"reflect"
	"strings"
	"text/template"
	"text/template/parse"
)

type state struct {
	tmpl *template.Template
	node parse.Node
	vars []variable
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
	return nil
}

// A type representing a template number, which does not correspond to any one
// Go numeric type.
type number struct{}

var (
	intType    = reflect.TypeOf(int(0))
	boolType   = reflect.TypeOf(true)
	stringType = reflect.TypeOf("")
	numberType = reflect.TypeOf(number{})
)

type checkError struct {
	err error
}

func Check(t *template.Template, dot reflect.Type) (err error) {
	defer func() {
		if e := recover(); e != nil {
			if cerr, ok := e.(checkError); ok {
				err = cerr.err
			} else {
				panic(e)
			}
		}
	}()

	s := &state{tmpl: t, vars: []variable{{"$", dot}}}
	if t.Tree == nil || t.Root == nil {
		s.errorf("%q is an incomplete or empty template", t.Name())
	}
	s.walk(dot, t.Root)
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
	m := s.mark()
	defer s.pop(m)

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
	joinVars(s.vars[:m], ifVars, elseVars)
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
			origVars[i].typ = nil
		}
	}
}

func (s *state) walkRange(dot reflect.Type, r *parse.RangeNode) {
	s.at(r)
	defer s.pop(s.mark())
	typ := indirectType(s.evalPipeline(dot, r.Pipe))
	// mark top of stack before any variables in the body are pushed.
	mark := s.mark()
	checkBody := func(index, elem reflect.Type) {
		// Set top var (lexically the second if there are two) to the element.
		if len(r.Pipe.Decl) > 0 {
			s.setTopVar(1, elem)
		}
		// Set next var (lexically the first if there are two) to the index.
		if len(r.Pipe.Decl) > 1 {
			s.setTopVar(2, index)
		}
		s.walk(elem, r.List)
		s.pop(mark)
	}
	switch typ.Kind() {
	case reflect.Array, reflect.Slice:
		checkBody(intType, typ.Elem())
	case reflect.Map:
		checkBody(typ.Key(), typ.Elem())
	case reflect.Chan:
		if typ.ChanDir() == reflect.SendDir {
			s.errorf("range can't iterate over send-only channel %v", typ)
		}
		checkBody(intType, typ.Elem())
	case reflect.Invalid:
		// An invalid value is likely a nil map, etc. and acts like an empty map.
	default:
		s.errorf("range can't iterate over type %v", typ)
	}
	if r.ElseList != nil {
		s.walk(dot, r.ElseList)
	}
}

func (s *state) walkTemplate(dot reflect.Type, t *parse.TemplateNode) {
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
	newState.walk(dot, tmpl.Root)
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
	// case *parse.ChainNode:
	// 	return s.evalChainNode(dot, n, cmd.Args, final)
	// case *parse.IdentifierNode:
	// 	// Must be a function.
	// 	return s.evalFunction(dot, n, cmd, cmd.Args, final)
	// case *parse.PipeNode:
	// 	// Parenthesized pipeline. The arguments are all inside the pipeline; final must be absent.
	// 	s.notAFunction(cmd.Args, final)
	// 	return s.evalPipeline(dot, n)
	case *parse.VariableNode:
		return s.evalVariableNode(dot, n, cmd.Args, final)
	}
	s.at(firstWord)
	s.notAFunction(cmd.Args, final)
	switch /*word :=*/ firstWord.(type) {
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

func (s *state) evalField(dot reflect.Type, fieldName string, node parse.Node, args []parse.Node, final, receiver reflect.Type) reflect.Type {
	if receiver == nil {
		return nil
	}
	receiver = indirectType(receiver)
	// Unless it's an interface, need to get to a value of type *T to guarantee
	// we see all methods of T and *T.
	ptr := receiver
	if ptr.Kind() != reflect.Interface && ptr.Kind() != reflect.Ptr {
		ptr = reflect.PtrTo(ptr)
	}
	if method, ok := ptr.MethodByName(fieldName); ok {
		_ = method
		return nil //s.evalCall(dot, method, node, fieldName, args, final)
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
		nameVal := reflect.ValueOf(fieldName)
		if nameVal.Type().AssignableTo(receiver.Key()) {
			if hasArgs {
				s.errorf("%s is not a method but has arguments", fieldName)
			}
			return receiver.Key()
		}
	case reflect.Ptr:
		etyp := receiver.Elem()
		if etyp.Kind() == reflect.Struct {
			if _, ok := etyp.FieldByName(fieldName); !ok {
				// If there's no such field, say "can't evaluate"
				// instead of "nil pointer evaluating".
				break
			}
		}
	}
	s.errorf("can't use field %s in type %s", fieldName, receiver)
	panic("not reached")
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

func indirectType(t reflect.Type) reflect.Type {
	if t == nil {
		return nil
	}
	for t.Kind() == reflect.Ptr {
		t = t.Elem()
	}
	return t
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
		location, context := s.tmpl.ErrorContext(s.node)
		format = fmt.Sprintf("template: %s: checking %q at <%s>: %s", location, name, doublePercent(context), format)
	}
	panic(checkError{fmt.Errorf(format, args...)})
}

// doublePercent returns the string with %'s replaced by %%, if necessary,
// so it can be used safely inside a Printf format string.
func doublePercent(str string) string {
	return strings.ReplaceAll(str, "%", "%%")
}
