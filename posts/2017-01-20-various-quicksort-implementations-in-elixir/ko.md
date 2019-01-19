---
title: 엘릭서로 퀵소트 구현하기
---

엘릭서에는 언어 차원에서 정렬용으로 제공하는 `Enum.sort/1`와 `Enum.sort/2` 함수가 있습니다. 리스트 타입의 경우 머지 소트 알고리즘을 사용하는 얼랭 `:lists.sort/1`와 `:lists.sort/2`를 호출하고, `Enumerable` 프로토콜을 구현하는 다른 엘릭서 타입의 경우 엘릭서 자체적으로 머지 소트 알고리즘을 구현합니다. [소스 코드 링크](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/enum.ex#L2111)

엘릭서 리스트는 싱글 링크드 리스트 방식으로 구현되어 있기 때문에 정렬 용으로는 머지 소트가 가장 적합합니다. 하지만 링크드 리스트로 퀵소트를 직접 구현해본 적이 없기 때문에 엘릭서로 한 번 만들어보기로 했습니다. 

<!--more-->

## 주의사항!

링크드 리스트를 사용해서 퀵소트를 구현해도 사실 장점이 거의 없습니다. 정렬이 필요하면 그냥 엘릭서에서 제공하는 머지 소트 정렬 함수를 사용하시면 되겠습니다.

## 기초적 퀵소트

Programming Erlang 책에 Joe Armstrong 본인이 예시로 제공한 얼랭 퀵소트 예제 코드가 있습니다. 다음은 그걸 엘릭서로 그대로 변환한 코드입니다.

```elixir
defmodule Quicksort do
  def quicksort_naive([]), do: []
  def quicksort_naive([pivot|t]) do
    quicksort_naive(for x <- t, x < pivot, do: x)
    ++ [pivot] ++
    quicksort_naive(for x <- t, x >= pivot, do: x)
  end
end
```

간결하게 필요한 기능을 구현했지만 리스트의 첫 요소를 피벗으로 사용한 것이 문제입니다. 이러면 이미 정렬된 리스트에서는 항상 퀵소트 최악의 경우인 O(n^2)로 실행됩니다.

## 랜덤 피벗 퀵소트

리스트에서 무작위로 요소를 선택해서 피벗으로 사용하면 이 문제가 발생할 확률을 대폭 낮출 수 있습니다. 

```elixir
defmodule Quicksort do
  def quicksort_random([]), do: []
  def quicksort_random(list) do
    list
    |> List.pop_at(random_position_in(list))
    |> _quicksort_random()
  end

  defp _quicksort_random({nil, _}), do: []
  defp _quicksort_random({pivot, []}), do: [pivot]
  defp _quicksort_random({pivot, sublist}) do
    smaller_elements = for x <- sublist, x < pivot, do: x
    larget_elements = for x <- sublist, x >= pivot, do: x
    _quicksort_random(List.pop_at(smaller_elements, 
                                  random_position_in(smaller_elements)
                                  ))
    ++ [pivot] ++
    _quicksort_random(List.pop_at(larget_elements, 
                                  random_position_in(larget_elements))
                                  )
  end

  defp random_position_in(list) when length(list) > 1, do: :rand.uniform(length(list)) - 1
  defp random_position_in(_), do: 0
end
```

이번에는 코드가 그렇게 간결하지도 않고, 링크드 리스트로 퀵소트를 구현했을 때 발생하는 문제가 보이기 시작합니다. 

`random_position_in/1`가 무작위로 인덱스를 선택하기 위해서는 일단 `Kernel.length/1`을 호출해서 리스트의 전체 길이를 확인해야 하는데, 이러면 전체 리스트를 한 번 순회해야 합니다.

`List.pop_at/3`는 주어진 인덱스에 있는 요소를 리스트에서 팝하고, 그 요소와 그 요소가 제거된 리스트를 튜플로 반환합니다. 이 함수도 주어진 인덱스에 도달할 때까지 리스트를 순회합니다.

`Enum.random/1`를 사용해서 인덱스가 아니라 요소를 바로 반환받아서 피벗으로 사용하는 방법도 있지만, 그러면 피벗과 값이 같은 요소가 리스트에 존재할 경우를 고려해서 처리해야 하기 때문에 덜 귀찮아 보이는 방법을 선택했습니다. `Enum.random/1`도 어차피 무작위로 선택한 인덱스에 도달할 때까지 리스트를 순회하는 부분은 동일하거든요.

## 중앙값 피벗 퀵소트

퀵소트는 리스트의 중앙값을 계산해서 피벗으로 사용할 경우에 가장 효율적이지만 중앙값은 빠르게 계산하기 어렵습니다. Sedgewick은 리스트의 중앙값의 근사값을 빠르게 구하는 방법으로 리스트에서 요소 세 개를 뽑아서 그 세 요소의 중앙값을 계산하고, 그 값을 피벗으로 사용하기를 권장합니다. 이러면 무작위로 피벗을 선택할 때보다 더 안정적인 성능이 나옵니다. 

```elixir
defmodule Quicksort do
  def quicksort_median([]), do: []
  def quicksort_median(list) do
    list
    |> List.pop_at(index_of_median_of_three_random_elements_from(list))
    |> _quicksort_median()
  end

  defp _quicksort_median({nil, _}), do: []
  defp _quicksort_median({pivot, []}), do: [pivot]
  defp _quicksort_median({pivot, sublist}) do
    smaller_elements = for x <- sublist, x < pivot, do: x
    larget_elements = for x <- sublist, x >= pivot, do: x
    _quicksort_median(List.pop_at(smaller_elements, 
                                  index_of_median_of_three_random_elements_from(smaller_elements)))
    ++ [pivot] ++
    _quicksort_median(List.pop_at(larget_elements, 
                                  index_of_median_of_three_random_elements_from(larget_elements)))
  end

  defp index_of_median_of_three_random_elements_from(list) when length(list) < 3, do: 0
  defp index_of_median_of_three_random_elements_from(list = [_|_]) do
    median =
      list
      |> Enum.take_random(3)
      |> case do
        [a, b, c] when (a >= b and a <= c) or (a <= b and a >= c) -> a
        [a, b, c] when (b >= a and b <= c) or (b <= a and b >= c) -> b
        [_, _, c] -> c
      end
    Enum.find_index(list, fn(x) -> x == median end)
  end
end
```

`index_of_median_of_three_random_elements_from/1` 함수를 정의해서 사용하는 것을 제외하면 무작위 피벗 퀵소트와 거의 동일합니다. `Enum.take_random/2`는 리스트를 순회하면서 주어진 갯수의 요소를 무작위로 선택합니다. Sedgewick은 그냥 어레이의 첫 번째, 중간, 그리고 마지막 요소를 사용하는 것을 권장하지만 무작위로 선택해도 상관 없을 것 같습니다.

## 듀얼 피벗 퀵소트

[야로슬라브스키의 논문](http://codeblab.com/wp-content/uploads/2009/09/DualPivotQuicksort.pdf)에서 2009년에 처음 제시된 개념인 듀얼 피벗 퀵소트는 피벗을 하나만 사용하는 기존 퀵소트보다 빠른 것으로 알려져 있습니다. 자바 7 어레이 클래스에서 듀얼 피벗 퀵소트를 최적화한 알고리즘을 사용하고 있습니다.

```elixir
defmodule Quicksort do
  def quicksort_dual_pivot([]), do: []
  def quicksort_dual_pivot([h]), do: [h]
  def quicksort_dual_pivot([h,t]) do
    cond do
      h < t -> [h,t]
      true -> [t,h]
    end
  end
  def quicksort_dual_pivot(list) do
    {first_pivot, temp_sublist} = List.pop_at(list,
     random_position_in(list))
    {second_pivot, target_sublist} = List.pop_at(temp_sublist,
     random_position_in(temp_sublist))
    
    smaller_pivot = Enum.min([first_pivot, second_pivot])
    larger_pivot = Enum.max([first_pivot, second_pivot])
    
    quicksort_dual_pivot(for x <- target_sublist, x < smaller_pivot, do: x) 
    ++ [smaller_pivot] ++
    quicksort_dual_pivot(for x <- target_sublist, x >= smaller_pivot, x < larger_pivot, do: x)
    ++ [larger_pivot] ++
    quicksort_dual_pivot(for x <- target_sublist, x >= larger_pivot, do: x)
  end
  
  defp random_position_in(list) when length(list) > 1, do: :rand.uniform(length(list)) - 1
  defp random_position_in(_), do: 0
end
```

싱글 피벗 퀵소트는 피벗이 리스트의 중앙값에 가까울 수록 성능이 좋아지지만 듀얼 피벗 퀵소트에서는 그렇지 않습니다. 검증해본 것은 아니지만 직관적으로 생각해볼 때 각 피벗이 전체 리스트의 33%와 66% 지점에 위치할 때 가장 효율적일 것이라 추측합니다.

야로슬라브스키의 예제 슈도코드에서는 리스트의 첫번째와 마지막 요소를 피벗으로 사용했지만, 그럴 경우 이미 정렬된 리스트에서는 항상 퀵소트 최악의 경우인 O(n^2)로 실행됩니다. 

좀 더 성능이 안정적으로 나올 것이라 추측했기에 무작위로 두 피벗을 선택하도록 했지만 검증해본 것은 아닙니다.
