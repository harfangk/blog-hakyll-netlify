---
title: 엘릭서에서 모듈 이름을 포함한 함수의 전체 이름 찾기
---

가끔 임포트한 함수의 전체 이름을 알고 싶을 때가 있습니다. 즉, 그 함수가 원래 정의되어 있던 모듈 이름을 알고 싶을 때 말입니다.

그 함수 이름을 `&`를 사용해서 캡쳐해서 `IO.inspect/1`에 넘겨주면 전체 이름을 알 수가 있습니다.

다른 방법은 `__ENV__.functions/0`를 실행했을 때 나오는 정보를 살펴보는 것입니다. 해당 함수는 컴파일 타임 환경에 로드된 모듈과 함수를 전부 열거합니다. `__ENV__` 관련 문서는 [여기](https://hexdocs.pm/elixir/Macro.Env.html)에서 확인해볼 수 있습니다.

<!--more-->

예시를 하나 봅시다.

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

## 너무 많은 모듈을 임포트하거나 앨리어스하지 마세요

이런 상황에 처했다면 너무 많은 모듈을 임포트하거나 앨리어스했다는 의미입니다. 애초에 그런 상황에 처하지 않도록 모듈을 적절히 분리해주세요.
