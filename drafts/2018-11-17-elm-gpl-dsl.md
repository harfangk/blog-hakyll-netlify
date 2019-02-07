---
layout: post
ref: elm-fast-and-steady
date: 2018-11-17 00:00:00 +0900
title: How to Make Elm Move Fast Yet Steady
lang: en
---

I've been using Elm in production for a year, and I really like the reliability it gives - I'll always choose it over vanilla JavaScript for writing web applications. But Elm is still a small language that lacks a lot of features that are available in JavaScript, which is only natural. JavaScript has much larger community and more financial support. So Elm can't provide all the features that JavaScript ecosystem provides.

To inter-operate with JavaScript to leverage its vast ecosystem, Elm provides an interface called `Port`, which allows unidirectional message passing between Elm and JavaScript. This design was intentionally chosen to guarantee run-time reliability of Elm, and the design rationale is provided [here](https://guide.elm-lang.org/interop/ports.html). Unfortunately, the design has trade-offs. It treats all function calls asynchronous without exception, making it clunky to use synchronous functions; in addition, composing `Port` function calls between JavaScript and Elm is impossible as all messages are treated as one-way messages without guaranteed responses.

I was not the only one who found `Port` too cumbersome to use. People found out how to directly call "Native" JavaScript code from Elm functions to bypass `Port` interface by reverse-engineering Elm core implementation. This hacky solution was shut down with the release of Elm 0.19, and the rationale behind it can be read [here](https://discourse.elm-lang.org/t/native-code-in-0-19/826). Removing a feature of a language is bound to be controversial. But removing a community-led, although unsanctioned, solution to a widely felt problem without providing an alternative solution caused much bad blood in the community. Most of recent doomsday criticisms against Elm are written in response to this change. As for me, I wholeheartedly agree with the removal of "Native" JavaScript codes but was disappointed to learn that the root cause remained unaddressed.

There are a few known methods to address the issue, such as setting timeout timer to guarantee termination and tagging requests and responses with unique identifiers to pair them. The core team is of course [aware of those approaches](https://gist.github.com/alpacaaa/13335246234042395813d97af029b10f). But so far they've decided not to implement those solutions - I guess they had explored those options but found them wanting. I'm interested in what they found out, but couldn't find out much information about it and I am not knowledgeable enough to propose a better general solution. So we're back to `Port` as the only interface for JavaScript inter-operation.

As far as one-size-fits-all approaches go, `Port` is a pretty good one. It's just that it's not the best fit for some use cases, especially those synchronous Web APIs that are frequently used but not implemented in Elm. So instead of overhauling the `Port` interface, filling out those holes could be a practical, good enough solution. Sounds simple enough, except it's not. 

Designing a good API is not easy. In this [video](https://youtu.be/uGlzRt-FYto?t=1367), Evan talks about the iterations he went through while implementing `Http` and `Browser.Dom` modules. Directly porting JavaScript API is rarely the best course of action, as can be seen in discussion about `Browser.Dom`. Sometimes it's almost impossible to directly port into Elm. Web Crypto API is such an example, as discussed [here](https://discourse.elm-lang.org/t/supporting-the-webcrypto-api/2289/11). Even [Web Storage API](https://html.spec.whatwg.org/multipage/webstorage.html#webstorage), which I thought was straightforward enough to be implemented as it is, would [need more comprehensive work](https://github.com/elm/projects/blob/master/roadmap.md#where-is-the-localstorage-package).





I applaud the core team for standing by what makes Elm great: reliability and code quality guarantee over long term. Yet they failed to solve the root cause of the whole issue, and as far as I can tell , but left much bad blood and bitter taste in the community.

On the other hand, they couldn't come up with the solution to the root of the problem - inter-operation with JavaScript remains too bothersome.

The Elm core team knows this and has adopted a sound strategy for interaction with JavaScript. The core team focuses on improving Elm provides a general foreign function interface for communicating with JavaScript, thereby while they work on core language features and incorporate critical web platform APIs into Elm. This is a sound strategy that I agree to. 

There has been yet another discussion on Elm slack about Elm being a domain-specific language (DSL), which led to an interesting discussion about definitions of general-purpose languages (GPL) and DSL. Apparently this topic has been raised often enough that the core team wrote an article about it: [On “General-Purpose” Languages](https://github.com/elm/projects/blob/master/notes/on-general-purpose.md). The article points out that languages establish their niches before branching out to other domains, and those niches are usually what the languages are known to be great at.

Elm chose web frontend as its domain and carved some niche there, but it's by no means complete. There are several important Web APIs that are yet to be implemented in native Elm and must be used through foreign function interface to JavaScript. Most importantly, the core team is clearly set out to make the language better and better. So they will continue to work on making Elm great in web frontend.

But the core team is not the only ones who can expand the boundary of a language. Members of the community have often been an important driving force behind explorations and innovations. Regarding explorations into other domains with Elm, the aforementioned article writes:
> If you want to experiment with Elm in other domains, please (1) let folks know on elm-dev as soon as possible and (2) label your work as experimental very clearly. This way it can be clearer to everyone how to use their time efficiently.

This is strange. This is where all experimental work that's not supported by Elm language goes.

That request to discuss exploration plans with the core team at `#elm-dev` channel sounds reasonable enough. After all, taking a language to a different domain is not a simple undertaking. Yet it's amazing how much ruckus that caused. The issue is that here the definition of the word `domain` is rather narrow.

Elm can be expanded in two directions: first one is expanding within web frontend by bringing in more JavaScript features into native Elm; second one is moving outside the web frontend to use Elm in command line or backend. Few would argue about the second case. It's the first case that's the issue. It seems that many developers consider that the first direction should not require coordination with the core team and should be open to the public. The core team is of different opinion. There has been a post about it: [Native Code in 0.19](https://discourse.elm-lang.org/t/native-code-in-0-19/826).

I agree with the post. But there exists an institutional barrier there. [Building Trust: What Has Worked](https://discourse.elm-lang.org/t/building-trust-what-has-worked/975) outlines a successful case, but it is nevertheless a barrier, whether psychological or bureaucratic. It will necessarily slow down individual contributions.

Is it good? Bad? I'm not sure. On one hand, Elm's core ecosystem moves in a surprisingly well-coordinated manner thanks to this. All of Elm standard libraries are of high quality. It is clear that Elm moves with a different strategy as the rest of the JavaScript community. [What is Success?](https://youtu.be/uGlzRt-FYto)
[The Hard Parts of Open Source](https://youtu.be/o_4EX4dPppA)




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
Error: Permission denied to access property x
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
