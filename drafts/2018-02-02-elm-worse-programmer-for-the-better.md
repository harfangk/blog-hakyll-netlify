---
layout: post
ref: 
date: 2017-02-25 00:00:00 +0900
title: 
lang: en
---

tip 1

start with unit tests - properties are discernible from multiple use cases

tip 2

writing more than 6 inputs

tip 3

writing own fuzzer and/or generator

tip 4

file naming ocnvention


Elm: DSL for web frontend, compiles to HTMl/CSS/JS

Why created? Web Dev was painful.

HTMl had reusability issue - continued copy and paste

JS... lots of issues

Learning JS comes down to learning about pitfalls to avoid and the ilbraries of the day that provide functionalities i want

I want to focus on runtime errors for now.

Runtime error vs compiletime error

JS has no compile time...

so everything is runtime!

YAY!

70 shades of runtime errors in JS

Irrelevant
Error: Permission denied to access property "x"
Warning: 08/09 is not a legal ECMA-262 octal constant
Warning: -file- is being assigned a //# sourceMappingURL, but already has one
Warning: Date.prototype.toLocaleFormat is deprecated
Warning: JavaScript 1.6's for-each-in loops are deprecated
Warning: String.x is deprecated; use String.prototype.x instead
Warning: expression closures are deprecated
Warning: unreachable code after return statement
X.prototype.y called on incompatible type

Exists
InternalError: too much recursion

Through type (Maybe/Result)
RangeError: argument is not a valid code point
RangeError: invalid array length
RangeError: invalid date
RangeError: precision is out of range
RangeError: radix must be an integer
RangeError: repeat count must be less than infinity
RangeError: repeat count must be non-negative

Compiler
ReferenceError: "x" is not defined
ReferenceError: assignment to undeclared variable "x"
ReferenceError: can't access lexical declaration`X' before initialization
ReferenceError: deprecated caller or arguments usage
ReferenceError: invalid assignment left-hand side
ReferenceError: reference to undefined property "x"

SyntaxError: "0"-prefixed octal literals and octal escape seq. are deprecated
SyntaxError: "use strict" not allowed in function with non-simple parameters
SyntaxError: "x" is a reserved identifier
SyntaxError: JSON.parse: bad parsing
SyntaxError: Malformed formal parameter
SyntaxError: Unexpected token
SyntaxError: Using //@ to indicate sourceURL pragmas is deprecated. Use //# instead
SyntaxError: a declaration in the head of a for-of loop can't have an initializer
SyntaxError: applying the 'delete' operator to an unqualified name is deprecated
SyntaxError: for-in loop head declarations may not have initializers
SyntaxError: function statement requires a name
SyntaxError: identifier starts immediately after numeric literal
SyntaxError: illegal character
SyntaxError: invalid regular expression flag "x"
SyntaxError: missing ) after argument list
SyntaxError: missing ) after condition
SyntaxError: missing : after property id
SyntaxError: missing ; before statement
SyntaxError: missing = in const declaration
SyntaxError: missing ] after element list
SyntaxError: missing formal parameter
SyntaxError: missing name after . operator
SyntaxError: missing variable name
SyntaxError: missing } after function body
SyntaxError: missing } after property list
SyntaxError: redeclaration of formal parameter "x"
SyntaxError: return not in function
SyntaxError: test for equality (==) mistyped as assignment (=)?
SyntaxError: unterminated string literal

TypeError: cyclic object value

Through type signature
TypeError: "x" has no properties
TypeError: "x" is (not) "y"
TypeError: "x" is not a constructor
TypeError: "x" is not a function
TypeError: "x" is not a non-null object
TypeError: "x" is read-only
TypeError: More arguments needed
TypeError: can't access dead object
TypeError: invalid 'in' operand "x"
TypeError: invalid Array.prototype.sort argument
TypeError: invalid arguments
URIError: malformed URI sequence

Immutability
TypeError: can't define property "x": "obj" is not extensible
TypeError: can't delete non-configurable array element
TypeError: can't redefine non-configurable property "x"
TypeError: invalid assignment to const "x"
TypeError: property "x" is non-configurable and can't be deleted
TypeError: setting getter-only property "x"

Silently fails...
TypeError: variable "x" redeclares argument

Maybe/Result to account for expression that can fail
All type/syntax errors are handled at compile time

Logic error

No unexpected mutation -> no need to worry about synchronization
Use types for precise definition of domain logic -> no need to worry about unexpected values

error?
* Error: Permission denied to access property "x"
* InternalError: too much recursion
* RangeError: argument is not a valid code point
* RangeError: invalid array length
* RangeError: invalid date
* RangeError: precision is out of range
* RangeError: radix must be an integer
* RangeError: repeat count must be less than infinity
* RangeError: repeat count must be non-negative
* ReferenceError: "x" is not defined
* ReferenceError: assignment to undeclared variable "x"
* ReferenceError: can't access lexical declaration`X' before initialization
* ReferenceError: deprecated caller or arguments usage
* ReferenceError: invalid assignment left-hand side
* ReferenceError: reference to undefined property "x"
* SyntaxError: "0"-prefixed octal literals and octal escape seq. are deprecated
* SyntaxError: "use strict" not allowed in function with non-simple parameters
* SyntaxError: "x" is a reserved identifier
* SyntaxError: JSON.parse: bad parsing
* SyntaxError: Malformed formal parameter
* SyntaxError: Unexpected token
* SyntaxError: Using //@ to indicate sourceURL pragmas is deprecated. Use //# instead
* SyntaxError: a declaration in the head of a for-of loop can't have an initializer
* SyntaxError: applying the 'delete' operator to an unqualified name is deprecated
* SyntaxError: for-in loop head declarations may not have initializers
* SyntaxError: function statement requires a name
* SyntaxError: identifier starts immediately after numeric literal
* SyntaxError: illegal character
* SyntaxError: invalid regular expression flag "x"
* SyntaxError: missing ) after argument list
* SyntaxError: missing ) after condition
* SyntaxError: missing : after property id
* SyntaxError: missing ; before statement
* SyntaxError: missing = in const declaration
* SyntaxError: missing ] after element list
* SyntaxError: missing formal parameter
* SyntaxError: missing name after . operator
* SyntaxError: missing variable name
* SyntaxError: missing } after function body
* SyntaxError: missing } after property list
* SyntaxError: redeclaration of formal parameter "x"
* SyntaxError: return not in function
* SyntaxError: test for equality (==) mistyped as assignment (=)?
* SyntaxError: unterminated string literal
* TypeError: "x" has no properties
* TypeError: "x" is (not) "y"
* TypeError: "x" is not a constructor
* TypeError: "x" is not a function
* TypeError: "x" is not a non-null object
* TypeError: "x" is read-only
* TypeError: More arguments needed
* TypeError: can't access dead object
* TypeError: can't define property "x": "obj" is not extensible
* TypeError: can't delete non-configurable array element
* TypeError: can't redefine non-configurable property "x"
* TypeError: cyclic object value
* TypeError: invalid 'in' operand "x"
* TypeError: invalid Array.prototype.sort argument
* TypeError: invalid arguments
* TypeError: invalid assignment to const "x"
* TypeError: property "x" is non-configurable and can't be deleted
* TypeError: setting getter-only property "x"
* TypeError: variable "x" redeclares argument
* URIError: malformed URI sequence
* Warning: 08/09 is not a legal ECMA-262 octal constant
* Warning: -file- is being assigned a //# sourceMappingURL, but already has one
* Warning: Date.prototype.toLocaleFormat is deprecated
* Warning: JavaScript 1.6's for-each-in loops are deprecated
* Warning: String.x is deprecated; use String.prototype.x instead
* Warning: expression closures are deprecated
* Warning: unreachable code after return statement
* X.prototype.y called on incompatible type
