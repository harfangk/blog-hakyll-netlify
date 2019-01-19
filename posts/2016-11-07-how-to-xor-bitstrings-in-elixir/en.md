---
title: How to XOR Bitstrings in Elixir
---

### tl;dr

Use `:crypto.exor/2` from [crypto](http://erlang.org/doc/man/crypto.html) module in Erlang standard library.

### Chronicle of My Decent into Rabbit Hole

It started with a simple quiz from [Cryptography](https://www.coursera.org/learn/crypto) course in Coursera. I was given a plain text (`pt`) and a cipher text (`ct`) encrypted with one-time pad, so I just had to do an XOR of `ct` and `pt` to figure out the one-time pad key.

I didn't know how to do that in Elixir, so I decided to figure it out.

<!--more-->

The plain text was encoded in hexadecimal and has to be converted to binary first. I searched and found that `Base` module had `decode16/2` function. 

```elixir
{:ok, ct} = Base.decode16("7427a99a4e7a45683a")
** (MatchError) no match of right hand side value: :error

# Why is this function case sensitive?

{:ok, ct} = Base.decode16("7427a99a4e7a45683a", case: :lower)
{:ok, <<116, 39, 169, 154, 78, 122, 69, 104, 58>>}

pt = "menagerie"
"menagerie"
```

I tried straight-up XOR.

```elixir
use Bitwise
Bitwise
ct ^^^ pt
** (ArithmeticError) bad argument in arithmetic expression
    :erlang.bxor(ct, pt)
```

Well, that didn't work. I now had to figure out how to XOR each of corresponding bytes in the pair of bitstrings. `Enum` is the obvious first choice for iteration, but bitstring is not an `Enumerable` so that was out of option. I decided to use [generators](http://elixir-lang.org/getting-started/comprehensions.html) instead.

```elixir
for << a::8 <- ct, b::8 <- pt >>, do: a ^^^ b
** (CompileError) iex:60: undefined function <-/2
    (elixir) src/elixir_bitstring.erl:33: :elixir_bitstring.expand_bitstr/4
    (elixir) src/elixir_bitstring.erl:10: :elixir_bitstring.expand/3
    (elixir) src/elixir_for.erl:44: :elixir_for.expand/2
    (stdlib) lists.erl:1354: :lists.mapfoldl/3
    (elixir) src/elixir_for.erl:31: :elixir_for.expand/3
```

No multiple generators for bitstrings, I guess. Even if that worked, it wouldn't have done what I wanted. Next idea was to turn bitstrings into lists and operate on them. Like this.

```elixir
defmodule XorLists do
  def xor_lists(list1, list2) do
    _xor_lists(list1, list2, [])
    |> Enum.reverse() 
    |> :binary.list_to_bin
  end
  
  defp _xor_lists([], _, acc), do: acc
  defp _xor_lists(_, [], acc), do: acc
  defp _xor_lists([h1|t1], [h2|t2], acc), do: _xor_lists(t1, t2, [acc|h1 ^^^ h2])
end

list_ct = for<<a::bytes-size(1) <- ct >>, do: a
[116, 39, 169, 154, 78, 122, 69, 104, 58]
list_pt = String.to_charlist(pt)
'menagerie'
XorLists.xor_lists(list_ct, list_pt)
<<25, 66, 199, 251, 41, 31, 55, 1, 95>>
```

It works well and I got the one-time pad key. But at this point, someone in IRC told me to look into `:crypto.exor`.

```elixir
:crypto.exor(ct, pt)
<<25, 66, 199, 251, 41, 31, 55, 1, 95>>
```

Well, it seems that I didn't have to go through all of that. At least I got it right and learned much more about binaries in Elixir, though.
