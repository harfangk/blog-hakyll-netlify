---
title: How to Find Full Function Name in Elixir
---

Sometimes you want to know the full name of an imported function, including the name of the module it is defined in. 

Capture the function name with `&` and pass it to `IO.inspect/1` to reveal its full name.

Another way is to rummage through `__ENV__.functions/0` which lists all the modules and functions loaded into your compile time environment. Information about `__ENV__` can be found [here](https://hexdocs.pm/elixir/Macro.Env.html).

<!--more-->

Here's an example.

```elixir
defmodule Foo do
  import Enum, only: [any?: 2]
  
  def bar, do: IO.inspect(&any?/2) 
  def baz, do: __ENV__.functions |> IO.inspect()
end

iex> Foo.bar
&Enum.any?/2
iex> Foo.baz
[{Enum,
  [any?: 2]},
 {IEx.Helpers,
  [c: 1, c: 2, cd: 1, clear: 0, flush: 0, h: 0, i: 1, l: 1, ls: 0, ls: 1, nl: 1,
   nl: 2, pid: 1, pid: 3, pwd: 0, r: 1, recompile: 0, respawn: 0, v: 0, v: 1]},
 {Kernel,
  [!=: 2, !==: 2, *: 2, +: 1, +: 2, ++: 2, -: 1, -: 2, --: 2, /: 2, <: 2, <=: 2,
   ==: 2, ===: 2, =~: 2, >: 2, >=: 2, abs: 1, apply: 2, apply: 3,
   binary_part: 3, bit_size: 1, byte_size: 1, div: 2, elem: 2, exit: 1,
   function_exported?: 3, get_and_update_in: 3, get_in: 2, hd: 1, inspect: 1,
   inspect: 2, is_atom: 1, is_binary: 1, is_bitstring: 1, is_boolean: 1,
   is_float: 1, is_function: 1, is_function: 2, is_integer: 1, is_list: 1,
   is_map: 1, is_number: 1, is_pid: 1, is_port: 1, is_reference: 1, is_tuple: 1,
   ...]}]
```

## Don't Import or Alias Too Many Modules

If you find yourself needing to do that, you've probably imported or aliased too many modules. It'd be best to avoid getting into such a situation by splitting modules accordingly.
