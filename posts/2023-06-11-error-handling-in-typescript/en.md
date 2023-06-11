---
title: Error handling in Typescript
---

# Summary

- Define and use custom error types by extending the built-in `Error` type.
- Always handle errors within the functions where they occur, and return errors as values to let the Typescript compiler handle them.
- Do not introduce `Result` type as its benefit is not worth the cost.

<!--more-->

# Error Handling in JavaScript

Error handling in Typescript starts with the tools provided in Javascript. 

```typescript
/* 
* Custom error types. Extending built-in Error class is great for 
* interoperability, but works only after ES5.
*/
class CustomError extends Error {
  constructor(errorCode, message) {
    super(message);
    this.name = this.constructor.name;
    this.errorCode = errorCode
  }
}

class InsufficientBalanceError extends CustomError {
  constructor() {
    super(13, "The account has insufficient balance to execute the transaction.")
  }
}

class SuspendedAccountError extends CustomError {
  constructor() {
    super(21, "The account is suspended.")
  }
}

class OffBusinessHoursError extends CustomError {
  constructor() {
    super(53, "The branch is off business hours and cannot execute the transaction.")
  }
}

function checkBalance(account) {
  if (!account.active) {
    throw new SuspendedAccountError()
  } else {
    return account.balance
  }
}

// Try... catch statement for failable operation and handling errors
function failableFunction(account) {
	try {
    // Do something that may throw error
    const balance = checkBalance(account)
    return balance
	} catch(e) {
	  // Handle thrown error
    console.error(e)
	  if (e instanceof InsufficientBalanceError) {
	    return e
	  } else if (e instanceof SuspendedAccountError) {
	    return e
	  } else if (e instanceof OffBusinessHoursError) {
	    return e
	  } else {
	    return undefined
	  }
	} finally {
	  // Do this regardless of whether there's an error or not
	  console.log("Hello, world!") 
	}
}
```

# Three key improvements by Typescript

1. Return type can be written down for better readibility. This also encourages programmers to pay more attention to error handling, especially if return types are inconsistent or convoluted like in the following example. 
    
    ```typescript
    function failableFunction(account: string): number | InsufficientBalanceError | SuspendedAccountError | OffBusinessHoursError | undefined {
      ...
    }
    ```
    
2. Union types can be used to group errors free of prototype chain.
    
    ```typescript
    type AccountError = InsufficientBalanceError | SuspendedAccountError
    function failableFunction(account: string): number | AccountError | OffBusinessHoursError | undefined {
      ...
    }
    ```
    
3. Custom type guards allow more precise handling of error types than Javascript’s type guards of `typeof` and `instanceof`
    - `typeof` checks for only the most basic types - `boolean`, `string`, `bigint`, `symbol`, `undefined`, `function`, `number`, `object` - and is not suited for handling error types
    - `A instanceof B` simply checks if `B.prototype` exists anywhere in the prototype chain of `A`, which can result in unexpected behavior.
        
        ```typescript
        class AccountError extends CustomError {
          constructor(message) {
            super(10, message);
          }
        }
        
        const e = new AccountError("test")
        
        if (e instanceof CustomError) {
          console.log("CustomError")
        } else if (e instanceof AccountError) {
          console.log("AccountError")
        }
        
        /*
        * This outputs "CustomError". 
        * When using instanceof typeguard, you should keep track of 
        * the prototype chain and handle more specific errors first.
        */ 
        ```
        
    - Custom type guard can specify the exact error type you want to handle.
        
        ```typescript
        function isInsufficientBalanceError(o: unknown): o is InsufficientBalanceError {
        	return typeof o === "object" && o !== null && "name" in o && o.name === "InsufficientBalanceError"
        }
        ```
        

# Introducing `Result` type

Typescript also enables adopting `Result` type to handle errors. `Result`, also often called `Either`, is not built into Typescript. Defining the type itself is easy, but defining the API around it is quite a lot of work so I recommend using libraries. There are several, ranging from simple ones such as [vultix/ts-result](https://github.com/vultix/ts-results) or [badrap/result](https://github.com/badrap/result) to full suites such as [mobily/ts-belt](https://github.com/mobily/ts-belt) or [gcanti/fp-ts](https://github.com/gcanti/fp-ts).

A basic definition and usage of `Return` type is as following:

```typescript
class Ok<T> {
  constructor(private value: T) {}
}

class Err<E> {
  constructor(private value: E) {}
}

type Result<T, E> = Ok<T> | Err<E>

class ParseError extends CustomError {
    constructor(input: any) {
      const message = `Could not parse the given input: ${input}`
      super(message)
    }
}

const SEASONS = ["spring", "summer", "fall", "winter"] as const

type Season = typeof SEASONS[number]

function isSeason(o: unknown): o is Season {
  return typeof o === "string" && !!SEASONS.find((season) => o === season);
}

function parseSeason(s: string): Result<Season, ParseError> {
  if (isSeason(s)) {
    return new Ok(s as Season)
  } else {
    return new Err(new ParseError(s))
  }
}
```

## Benefits of `Result` type

1. This pattern requires all errors to be caught within the functions where they can occur. Otherwise the return types for both successful and failed operations cannot be correctly specified.
2. This pattern also leads to type-safe errors. Javascript can `throw` anything, not just `Error` type - this is why caught errors have `unknown` type in Typescript.  
3. All failable operations can be represented as a single unified abstraction, improving code readability and composability. A chain of failable functions can quickly grow out of hand.
    
    ```typescript
    function stepOne(): string | undefined {
      ...
    }
    
    function stepTwo(s: string): number | StepTwoError {
      ...
    }
    
    function stepThree(n: number): string | StepThreeError {
      ...
    }
    
    function operation() {
      const stepOneResult = stepOne()
      if (stepOneResult !== undefined) {
        const stepTwoResult = stepTwo(stepOneResult)
        if (typeof stepTwoResult === "number") { 
          const stepThreeResult = stepThree(stepTwoResult)
          return stepThreeResult
        } else {
          ...
        }          
      } else { 
        ... 
      }
    }
    ```
    
    `Result` type has an established pattern of API that makes such operation much easier. Specific implementation may differ among libraries, but it generally looks like this.
    
    ```typescript
    function stepOne(): Result<string | undefined> {
      ...
    }
    
    function stepTwo(s: string): Result<number | StepTwoError> {
      ...
    }
    
    function stepThree(n: number): Result<string | StepThreeError> {
      ...
    }
    
    function operation() {
      const result = stepOne().andThen(stepTwo).andThen(stepThree)
      return result
    }
    ```
    

## Downsides of `Result` type

1. Introducing an unwieldy functional programming pattern and complexity just for error handling is often hard to justify, unless robustness is particularly important for the business domain. And if that is the case, then maybe the first question should be if Typescript is the right language for it.
2. Wrapping and unwrapping values into and out of `Result` type is tiring, especially when the pattern is almost never supported by the broader Javascript ecosystem.
3. This pattern encourages explicitly handling all possible exceptions at compile time, which quickly becomes extremely tedious. For years, there has been a lot of [skepticism](https://www.artima.com/articles/the-trouble-with-checked-exceptions) in the practicality of this approach.
4. Using `Result` type still cannot guarantee the absence of runtime error at compile time. If you forget to handle a potential error, Typescript won’t remind you of it since `throw` is not represented in Typescript’s type system. For example, `JSON.parse` can `throw` a `SyntaxError`, but its type signature is just `JSON.parse(text: string, reviver?: ((this: any, key: string, value: any) => any)`. Unless you remember to wrap it in `Result`, the program will still crash at runtime.

# Conclusion

Error handling in Typescript is better than Javascript’s but still is not really great. Here’s my conclusion as of now.

- Define and use custom error types by extending the built-in `Error` type.
    
    This is simply a standard practice. Typescript’s union type allows a very flexible definition of error types which is pleasant to use.
    
- Always handle errors within the functions where they occur, and return errors as values to let the Typescript compiler handle them.
    
    All Javascript errors are runtime errors, which are difficult to catch and reason about. Typescript allows programmers to manually turn them into compile time errors, which should be taken advantage of as much as possible. Unfortunately, this means that the programmer’s skill and understanding of the domain will remain the deciding factor of the program’s robustness. 
    
- Do not introduce `Result` type as its benefit is not worth the cost.
    
    I loved using `Result` in Elm and Haskell, and missed it when working with Typescript. Trying it out in Typescript, however, was an unpleasant experience. The Typescript ecosystem is not compatible with it, and you have to constantly fight against it to make `Result` work. And I believe that if you’re fighting against the environment, it’s a losing game. Unless the language itself starts natively supporting the `Result` type, I won’t be using it.
