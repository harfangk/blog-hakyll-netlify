---
title: A Short Guide to Function Operators in Elm (|>, <|, >>, <<)
---

## Summary

Once we get through the introduction to Elm, we start to encounter some odd-looking operators in Elm codes. I'm talking about the ones like `>>`, `<<`, `|>`, and `<|`, which modify how functions are composed and applied. I will give more details about how and when I use them in this post, but here's my rule of thumb:

1. Use `|>` and `<|` to visually describe the flow of data. It's their main advantage over nested parantheses, which provide equivalent functionalities.

2. Use `>>` and `<<` to describe function compositions independent of the data flow. In practice, I usually define a new function through composition and use it with `|>` and `<|`.

3. Use either the pair of `|>` and `>>` or `<|` and `<<` without mixing them. The shapes of these operators hold inherent directional meanings, so mixing different directions taxes our cognitive resources.

<!--more-->

## Do We Really Need These Weird Operators?

We don't. Elm will still work fine even if we removed these operators, because there are other perfectly fine ways to do the same thing. After all, they are just syntactic sugars that stand in for standard functions. But these operators let us write more understandable code by adding extra visual information about the flow of data and structure of code. Let's see some examples.

## Examples

### `|>` Operator

Here's an example task that I want to accomplish: given a list of random integers, I want to get the first digit of each integer as a list. Here's my initial implementation without function operators:

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    let
        stringList = List.map toString list
        firstStringDigits = List.map (String.left 1) stringList
        firstResultIntDigits = List.map (String.toInt) firstStringDigits
        firstIntDigits = List.map (Result.withDefault 0) firstResultIntDigits
    in
        firstIntDigits
```

I had to assign the result of each smaller operation to a variable and pass it over to the next operation. This is pretty normal - we see this kind of code often in imperative languages. But `|>`, commonly known as `pipe operator`, can make this code much easier to understand.

```elm
-- In multiple lines:
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list
    |> List.map toString
    |> List.map (String.left 1)
    |> List.map (String.toInt)
    |> List.map (Result.withDefault 0)

-- In one line:
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list |> List.map toString |> List.map (String.left 1) |> List.map (String.toInt) |> List.map (Result.withDefault 0)
```

Thanks to `|>`, we no longer have to assign intermediate results to pass them to the next step. Moreover, `|>` gives a visual hint about where the data will flow to.

How does it work? `|>` passes its first argument to its second argument, which is a function. We can see that in its type signature: `(|>) : a -> (a -> b) -> b`. In practice, we can keep passing the result of previous operation to the next operation, expressing it as a single pipeline of connected operations.

As I said, `|>` is just a syntactic sugar, so the above function can be expressed like this without the operator:

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    (List.map (Result.withDefault 0) (List.map (String.toInt) (List.map (String.left 1) (List.map toString list))))
```

But we use the style using `|>` because it saves us from writing nested parantheses or manually keeping track of which operation comes next. In general, `|>` is useful when we want to express a series of connected operations.

### `<|` Operator

`<|` is the reverse of `|>` as it pipes its second argument into its first argument. Its definition is `(<|) : (a -> b) -> a -> b`. Using `<|`, the above function can be writen like this:

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    List.map (Result.withDefault 0) <| List.map (String.toInt) <| List.map (String.left 1) <| List.map toString <| list
```

As you can see, `<|` lets us visually express the flow from right to left. If you are used to mathematical notations, which flow from right to left, or a right-to-left language, you'll feel at home. Personally I find that the right-to-left flow conflicts with the overall top-left-to-bottom-right flow that I'm used to, so I rarely use this operator. Still, I do find it useful in certain cases. For example, I like it when the last argument of a function is a function that spans over multiple lines like this:

```elm
-- with <|
test : Test.Test
test "subtraction should have identity property" <|
    \_ ->
        let
            a = negate (2 ^ 50 + 32)
            b = 0
        in
            Expect.equal (a - b) a

-- with parentheses
test : Test.Test
test "subtraction should have identity property"
    (\_ ->
        let
            a = negate (2 ^ 50 + 32)
            b = 0
        in
            Expect.equal (a - b) a
    )
```

I find that `<|` provides a nice visual separation, telling me that I don't have to think about the code that comes before `<|` until I'm done with the code that comes after it.

### `>>` and `<<` Operators

These two operators let us compose two functions into one larger function. Here are some examples:

```elm
toString : a -> String

String.length : String -> Int

toString >> String.length : a -> Int

String.length << toString : a -> Int
```

As you can tell from the function signatues, `>>` and `<<` combine two functions into one. Their definitions show this in a more abstract way.

```elm
(<<) : (b -> c) -> (a -> b) -> (a -> c)

(>>) : (a -> b) -> (b -> c) -> (a -> c)
```

Functionally they are identical, except that they work in opposite directions. The direction of arrows align with the direction of function composition. But if they are almost identical, which one should we use? I think this is more about personal taste. Using either composition operators with pipe operators, our example function can be expressed like this:

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    List.map (Result.withDefault 0) << List.map (String.toInt) << List.map (String.left 1) << List.map toString <| list

getFirstDigits : List Int -> List Int
getFirstDigits list =
    list |> List.map toString >> List.map (String.left 1) >> List.map (String.toInt) >> List.map (Result.withDefault 0)
```

If you prefer right-to-left flow, go with the pair of `<<` and `<|`. On the other hand, if you prefer left-to-right flow, go with the pair of `>>` and `|>`. Just avoid mixing operators with different flow direction - that adds unnecessary burden to our brains.

But how are these operators useful? Abstractly, they let us mix and match smaller functions to create a larger one, giving us an almost infinite possiblity for creating new functions out of other ones. Practically, this gives us a lot of freedom when writing funtions. Let's get back to our example function.

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list
    |> List.map toString
    |> String.left 1
    |> List.map (String.toInt >> Result.withDefault 0)
```

Here I used `>>` to group two small functions into one for transforming string to integer, because I thought that those two functions are more meaningful when composed together. I find function composition most useful when grouping logically coupled functions. We can also separate them into a new function in that case, but `>>` gives us a quick inline option.

## Things to Watch Out When Using Function Operators

Depending on the context, there are dozens of different ways to express this code. Here's another one:

```elm
-- Split to separate function
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list
    |> List.map toString
    |> String.left 1
    |> List.map stringToInt

stringToInt : String -> Int
stringToInt s = String.toInt << Result.withDefault <| s
```

Overall, I gave 9 different ways to write this function in this post. If we didn't have function operators, we would have been restricted to using nested parentheses, assigning intermediate variables, and defining new functions. As we've seen here, function operators can greatly expand our freedom of expression when writing code.

Nevertheless, they can lead to chaos when abused. Mixing functions written in different styles makes it harder for others to read and understand our code - which include our future selves. So it's important to have self-discipline and clear conventions to use these operators in a consistent way.

## Conclusion

We've looked at the four function operators in Elm: `>>`, `<<`, `|>`, and `<|`, and how they are used within an example function. I've deliberately left out formal discussions about them because there are already great resources about those topics. Besides, I found that discussions about when and how to use these operators were almost nonexistent, so I wanted to help fill that gap. I hope that this post was helpful in that respect.
