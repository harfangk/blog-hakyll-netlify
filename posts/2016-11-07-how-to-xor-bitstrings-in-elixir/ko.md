---
title: 엘릭서에서 비트스트링 XOR 하기
---

### 한줄 요약

얼랭 스탠다드 라이브러리의 [crypto](http://erlang.org/doc/man/crypto.html) 모듈에 있는 `:crypto.exor/2` 함수를 사용하면 됩니다.

### 삽질의 기록

코세라에서 제공하는 [Cryptography](https://www.coursera.org/learn/crypto) 수업에 있는 간단한 퀴즈를 풀려고 하다가 모든 것이 시작되었습니다. 평문(`pt`)과 One-Time Pad로 암호화된 암호문(`ct`)이 주어졌으니 간단하게 `ct`와 `pt`를 XOR하기만 하면 One-Time Pad 키를 알아낼 수 있습니다.

그런데 엘릭서로는 아직 비트 연산자 작업을 안 해봐서 이 참에 엘릭서로 해보기로 했습니다.

<!--more-->

퀴즈에서 암호문이 16진수로 인코딩해서 주어졌으니 먼저 2진수로 인코딩해야겠죠? 찾아보니 `Base` 모듈의 `decode16/2` 함수가 적절하겠군요.

```elixir
{:ok, ct} = Base.decode16("7427a99a4e7a45683a")
** (MatchError) no match of right hand side value: :error

# 대체 왜 이 함수가 대소문자를 구분하는 것인가...

{:ok, ct} = Base.decode16("7427a99a4e7a45683a", case: :lower)
{:ok, <<116, 39, 169, 154, 78, 122, 69, 104, 58>>}

pt = "menagerie"
"menagerie"
```

일단 냅다 XOR를 시전해봅니다.

```elixir
use Bitwise
Bitwise
ct ^^^ pt
** (ArithmeticError) bad argument in arithmetic expression
    :erlang.bxor(ct, pt)
```

안 먹히는군요. 비트스트링 2개에서 각각 대응하는 바이트끼리 어떻게 XOR를 시킬지 알아내야 합니다. 아이터레이션에는 당연히 `Enum`을 써야겠죠? 그런데 비트스트링은 `Enumerable`이 아니라서 사용할 수가 없군요. 대신 [제너레이터](http://elixir-lang.org/getting-started/comprehensions.html)를 사용하기로 합니다.

```elixir
for << a::8 <- ct, b::8 <- pt >>, do: a ^^^ b
** (CompileError) iex:60: undefined function <-/2
    (elixir) src/elixir_bitstring.erl:33: :elixir_bitstring.expand_bitstr/4
    (elixir) src/elixir_bitstring.erl:10: :elixir_bitstring.expand/3
    (elixir) src/elixir_for.erl:44: :elixir_for.expand/2
    (stdlib) lists.erl:1354: :lists.mapfoldl/3
    (elixir) src/elixir_for.erl:31: :elixir_for.expand/3
```

비트스트링에는 다중 제너레이터가 지원이 안 되는군요. 어차피 작동했어도 원하는 결과가 나오진 않았을 것이니 상관은 없습니다. 다음 떠오른 방법은 비트스트링을 리스트로 바꿔서 아이터레이트 하는 것입니다. 이렇게요.

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

잘 돌아가네요. One-Time Pad 키도 알아냈습니다. 그런데 다 하고 나니까 IRC에서 누군가가 `:crypto.exor`를 찾아보라고 합니다. 음...

```elixir
:crypto.exor(ct, pt)
<<25, 66, 199, 251, 41, 31, 55, 1, 95>>
```

이렇게 삽질할 필요가 전혀 없었군요. 그래도 바이너리랑 비트 작업에 관련된 엘릭서와 얼랭 모듈에 대해서 더 잘 알게 되었으니 괜찮은 삽질이었다고 치겠습니다.
