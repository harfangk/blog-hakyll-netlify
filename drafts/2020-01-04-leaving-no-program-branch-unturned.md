---
title: Leaving No Program Branch Unturned With Elm
---

One of the best characteristics of Elm is how it nudges you to exhaustively think through all program branches, resulting in significantly fewer bugs and undefined behaviors in the programs. Let's compare the process of writing a trivial program in both Javascript and Elm. We'll be parsing a JSON response that contains one of the four directions and return the corresponding arrow symbol. For example, given the input `{"dir": "up"}`, the return value should be `↑`.

I'd write it in this order:
1. Parse JSON
2. Access the field containing the direction in string
3. Convert that direction string into a symbol
4. Return the result

```javascript
# A Javascript implementation
function dirToSym(dir) {
  switch (dir) {
    case 'left':
      return '←'
    case 'up':
      return '↑'
    case 'right':
      return '→'
    case 'down':
      return '↓'
    default:
      return 'Invalid input string'
  }
}

let result
try {
  let resp = JSON.parse(json)
  let dir = resp.dir
  if (dir) {
    result = dirToSym(dir)
  } else {
    result = 'Invalid JSON format'
  }
} catch(e) {
  result = 'Failed to parse JSON'
}
return result
```

This implementation does the specified job while handling some common errors. Unfortunately, it's also possible to write an implementation that still does the same thing but would blow up all the time.

```javascript
# A terrible Javascript implementation
function dirToSym(dir) {
  if (dir === 'left') {
    return '←'
  } else if (dir === 'up')
    return '↑'
  } else if (dir === 'right')
    return '→'
  } else {
    return '↓'
  }
}

let resp = JSON.parse(json)
let dir = resp.dir
let result = dirToSym(dir)
return result
```

Most programming languages, including Javascript, does little to stop you from writing bad programs. Its design allows, and even nudges, you to write programs that only deal with happy paths. Your painful experiences from such haphazardly written programs are the only deterrent to writing such programs.

On the other hand, Elm encourages you to consider all program paths. Here's a description of my thought process when writing the same program in Elm.

```elm
import Json.Decode exposing (Decoder, decodeString, field, string)

# 1. Define a data type for valid data
type Dir = Left | Up | Right | Down

# 2. Define a parser for converting string to Dir
parseDir : String -> Result Error Dir
parseDir s =
  case s of
    "left" -> Ok Left
    "up" -> Ok Up
    "right" -> Ok Right
    "down" -> Ok Down
    _ -> Err (InvalidInputString s)

# 3. Define a JSON decoder to parse JSON into string to feed into parseDir
decoder : Decoder Dir
decoder = field "dir" string
  
# 4. Define a function to convert Dir into symbols
dirToSym : Dir -> String
dirToSym d =
  case d of
    Left -> "←"
    Up -> "↑"
    Right -> "→"
    Down -> "↓"
    
# 5. Define error type and refactor parseDir to handle it
type Error = InvalidInputString String 
           | InvalidJson Json.Decode.Error
    
# 5. Run the program
json |> decodeString decoder |> Result.mapError InvalidJson |> Result.andThen parseDir
```

Elm encourages starting from the definition of valid data, in this case `Dir` type. When writing a function to convert `String` into `Dir`, I had to think about what to do with strings that cannot be converted. I decided to use `Result Error Dir` type to represent both results. It was the same when parsing JSON data. Here `decodeString` returns `Result Json.Decode.Error Dir`, reminding me to handle failures when parsing JSON data.

Elm teaches you to think through all program branches. Haskell does similar, but it's much more complex and less gentle. Other languages force you to learn by trial of fire - by mercilessly crashing at runtime with vengeance. Shoving your failure in your face.

Elm does this by
1. Start by thinking about the definition of valid data - custom type
2. Explicitly handle all branches of functions - both success and failure
