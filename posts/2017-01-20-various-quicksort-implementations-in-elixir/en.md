---
title: Various Quicksort Implementations in Elixir
---

Elixir has built-in sort functions `Enum.sort/1` and `Enum.sort/2`. For list type, it calls Erlang `:lists.sort/1` and `:lists.sort/2` that use merge sort algorithm. For other Elixir types that implement `Enumerable` protocol, Elixir implements its own merge sort. Here's the link to the [source code](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/enum.ex#L2111). 

Since Elixir list is implemented as singly linked list, merge sort is the obvious choice for sorting. But I've never implemented quicksort with a linked list before, so I decided to give it a try with Elixir.

<!--more-->

## Warning

There's little practical value in implementing quicksort with linked list. Just use the merge sort implementation provided by Elixir if you need sorting. 

## Naive implementation

There is an Erlang example provided by Joe Armstrong himself in Programming Erlang. Here is its Elixir equivalent.

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

It's a succint implementation that does its job. Unfortunately, it uses the head of the list as its pivot, which would always lead to the worst case performance of quicksort O(n^2) for already sorted lists.

## Random pivot implementation

This problem can be mitigated by taking a random element in the list as the pivot. 

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

Not so succint this time. The problems inherent in linked list start to appear. 

In order to pick a random index, `random_position_in/1` has to call `Kernel.length/1`, which has to traverse the entire list once and adds a constant to the O(nlogn) performance.

`List.pop_at/3` pops the pivot element at the given index and returns a tuple that contains the pivot element and the rest of the list. It also traverses the list until it reaches the given index.

I could have written an alternative implementation by using `Enum.random/1` to immediately get a random element from the list instead of an index and use it as the pivot, but that would have required me to deal with elements identical to the randomly chosen pivot element. I decided to take a seemingly simpler route. `Enum.random/1` has the same performance issue because it traverses the list until it reaches the randomly chosen index, anyway.

## Median pivot implementation

Quicksort is most efficient when it uses the true median of the list as its pivot, but it's costly to calculate the true median. Sedgewick recommends using a median of three elements of the list as a pivot for a highly efficient approximation of the true median. This enables better and more stable performance than when using a random pivot.

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

This is almost identical to the previous solution, except that I defined `index_of_median_of_three_random_elements_from/1`. It uses `Enum.take_random/2`, which traverses the given list to randomly pick three elements from it. Sedgewick recommends simply using first, last, and middle elements of the array, but I think random choice would be good enough.

## Dual pivot implementation

First proposed by [Yaroslavskiy's paper](http://codeblab.com/wp-content/uploads/2009/09/DualPivotQuicksort.pdf) in 2009, dual pivot quicksort has been found to be faster than single pivot quicksort. Its carefully optimized variant is implemented in Java 7 Array class. 

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

Whereas single pivot quicksort gets more efficient the closer the pivot is to the true median, that strategy is suboptimal for dual pivot quicksort. My unverified intuition is that dual pivot quicksort operates most efficiently when each pivot is located around 33% and 66% percentile of the list. 

Yaroslavskiy's sample pseudocode uses first and last elements as the pivots, but that would also lead to O(n^2) worse case performance for already sorted lists.

I decided to use random pivots for a hopefully more stable performance, although I didn't benchmark it.
