---
title: 루비에서 꼬리 호출 최적화 사용하기
---

독자가 재귀라는 개념과 루비를 안다는 것을 가정합니다.

## 꼬리 호출 최적화란?

*꼬리 호출 최적화*(TCO: tail call optimization)는 꼬리 재귀(tail-recursive) 프로시져를 최적화하는 방법입니다. 재귀적인 프로시져의 콜 스택 사이즈를 하나로 줄여주어서 재귀를 사용할 때 스택 오버플로가 발생하는 것을 막아 줍니다.

<!--more-->

## TCO는 어떻게 동작하는가?

TCO가 최적화하는 대상이 콜 스택의 크기이니 콜 스택에 대해서 이야기 하는 것으로 시작하겠습니다. 콜 스택은 프로그램이 현재 자신이 실행하고 있는 서브루틴에 대한 정보를 저장하는 공간입니다. 

프로시져를 호출한 프로그램은 그 프로시져의 반환 주소를 콜 스택에 푸시합니다. 그리고 그 프로시져가 종료되면 프로시져를 호출했던 프로그램은 그 프로시져의 반환 주소를 콜 스택에서 팝한 뒤, 프로시져의 반환 값을 가지고 그 주소로 돌아갑니다. 그리고 프로시져를 호출한 이후로 이어지는 메인 프로그램을 마저 실행합니다. 

프로시져를 호출할 때마다 스택에 새로운 스택 프레임이 추가됩니다. 에러가 발생해서 디버깅할 때 보는 스택 트레이스가 바로 이 콜 스택에 쌓인 스택 프레임으로, 에러가 발생한 스택 프레임까지 이어지는 모든 스택 프레임을 보여줍니다. 콜 스택에 허용된 크기 이상으로 스택 프레임의 수가 늘어나면 스택 오버플로우가 일어나고 프로그램이 강제종료됩니다.

콜 스택에 대한 설명은 간략히 이 정도로 정리하겠습니다.

이제 꼬리 호출이 무엇인지 살펴봅시다. 꼬리 호출은 프로시져가 종료되기 직전에 호출한 마지막 서브루틴을 말합니다. 종료되기 직전에 마지막으로 호출했다는 것은 프로시져의 스택 프레임에서 꼬리 호출 이전에 사용된 부분은 이제 필요가 없어졌으며, 꼬리 호출에 사용된 새로운 스택 프레임으로 대체해도 큰 상관이 없다는 것을 의미합니다. 이렇게 기존 스택 프레임을 대체하면 메모리와 CPU를 더 효율적으로 사용할 수 있게 됩니다. 새로운 스택 프레임을 만드는 대신 이렇게 기존 스택 프레임을 대체하는 것을 *꼬리 호출 제거*(tail call elimination)이라고 부릅니다. 그리고 꼬리 호출 제거를 하는 프로그램이 *꼬리 호출 최적화*를 한다고 표현합니다. 

재귀가 아닌 프로시져의 경우, 사용하는 공간이나 시간을 TCO가 크게 줄여주지는 않기 때문에  그리 중요하지 않습니다. 하지만 재귀적인 프로시져의 경우 TCO는 프로그램의 동작 여부를 좌우하는 수준이며 매우 중요합니다.

## 재귀에는 왜 TCO가 중요한가?

재귀적으로 호출되는 프로시져는 매번 호출될 때마다 콜 스택에 새로운 스택 프레임을 추가하는데, 이 스택 프레임의 수가 순식간에 감당하지 못 할만큼 불어나서 스택 오버플로를 일으킬 수도 있습니다. 하지만 TCO는 재귀적인 프로시져가 몇 번이나 호출되건 상관 없이 단 하나의 스택 프레임만 사용하도록 해줍니다. 달리 표현하자면 재귀적인 프로시져가 n번 호출되면 필요한 콜 스택의 크기가 O(n)이지만, TCO는 그 것을 O(1)으로 줄여줍니다.

모든 재귀적인 프로시져에서 TCO가 가능하지는 않습니다. TCO가 기능하려면 프로시져가 *꼬리 재귀*(tail-recursive) 형태여야 하는데, 이는 프로시져가 꼬리 호출로 자기 자신을 호출하는 것을 말합니다. 

## 루비에서 TCO를 사용할때 주의할 점

TCO는 루비 언어에서 금지된 것은 아니지만 그렇다고 필수적이지도 않습니다. YARV나 Rubinius는 TCO를 지원하지만 JRuby는 이를 지원하지 않습니다. 따라서 TCO를 필요로 하는 재귀적인 프로시져를 사용할 경우, 그 코드는 JRuby에서는 제대로 동작하지 않을 것이라는 것을 알아둬야 합니다. 

## 팩토리얼

```ruby
class Fact
  def self.iterator(n)
    (1..n).reduce(:*)
  end

  def self.non_tail_recursive(n)
    1 if n <= 1 
    n * non_tail_recursive(n - 1)
  end

  def self.tail_recursive(n, acc = 1)
    return acc if n <= 1
    tail_recursive(n - 1, acc * n)
  end
end 
```

팩토리얼을 사용해서 꼬리 재귀 최적화를 실험해 봅시다. 세 가지 버전을 준비했습니다. 이터레이터를 쓰는 메서드, 꼬리 재귀 메서드, 그리고 꼬리 재귀가 아닌 재귀 메서드입니다.

실행해보도록 하지요.

```ruby
puts 'Iterator result'
=> Iterator result
puts Fact.iterator(100000).to_f
=> Infinity

puts 'Non-tail-recursive result'
=> Tail-recursive result
puts Fact.non_tail_recursive(100000).to_f
=> fact.rb:6:in `non_tail_recursive': stack level too deep (SystemStackError) `

puts 'Tail-recursive result'
=> Tail-recursive result
puts Fact.tail_recursive(100000).to_f
=> fact.rb:11:in `tail_recursive': stack level too deep (SystemStackError) `
```

이터레이터 메서드만 동작하고, 두 재귀 메서드에서는 `SystemStackError`가 발생합니다. TCO를 작동시키고 다시 실행해 봅시다. 

## 루비에서 TCO를 작동시키는 방법

```ruby
RubyVM::InstructionSequence.compile_option = {
  :tailcall_optimization => true,
  :trace_instruction => false
}
```

`RubyVM::InstructionSequence`의 `compile_option`에서 TCO가 작동하도록 설정할 수 있습니다. 매우 간단해 보이지만 한 가지 문제가 있는데, `RubyVM`에 대한 변경점은 런타임에 적용되기 때문에 `compile_option` 변경 코드를 파일에 그냥 추가하는 것 만으로는 작동하지 않습니다. 이를 해결하는 방법은 두 가지가 있습니다.

```ruby
RubyVM::InstructionSequence.compile_option = {
  :tailcall_optimization => true,
  :trace_instruction => false
}

class Fact
  def self.iterator(n)
    (1..n).reduce(:*)
  end

  eval <<END
    def self.non_tail_recursive(n)
      1 if n <= 1 
      n * non_tail_recursive(n - 1)
    end

    def self.tail_recursive(n, acc = 1)
      return acc if n <= 1
      tail_recursive(n - 1, acc * n)
    end
  END
end 
```

이게 첫 번째 방법입니다. 컴파일 타임에 이미 파싱된 메서드 정의를 오버라이드하기 위해서  `eval`을 사용해서 메서드 정의를 런타임에 다시 정의합니다. 

```ruby
# fact.rb
class Fact
  def self.iterator(n)
    (1..n).reduce(:*)
  end

  def self.non_tail_recursive(n)
    1 if n <= 1 
    n * non_tail_recursive(n - 1)
  end
	
  def self.tail_recursive(n, acc = 1)
    return acc if n <= 1
    tail_recursive(n - 1, acc * n)
  end
end 

# ruby_vm_option.rb
RubyVM::InstructionSequence.compile_option = {
  :tailcall_optimization => true,
  :trace_instruction => false
}

require_relative 'fact.rb'
```

이게 두 번째 방법입니다. `ruby_vm_option.rb`에서 컴파일 옵션이 변경되었기 때문에 `fact.rb`를 비롯하여 이후에 파싱된 코드에는 TCO가 적용됩니다.

TCO를 적용해서 다시 실행해 봅시다.

```ruby
puts 'Iterator result'
=> Iterator result
puts Fact.iterator(100000).to_f
=> Infinity

puts 'Non-tail-recursive result'
=> Tail-recursive result
puts Fact.non_tail_recursive(100000).to_f
=> fact.rb:6:in `non_tail_recursive': stack level too deep (SystemStackError) `

puts 'Tail-recursive result'
=> Tail-recursive result
puts Fact.tail_recursive(100000).to_f
=> Infinity
```

이번에는 꼬리 재귀 메서드가 동작하는 것을 볼 수 있습니다. 하지만 꼬리 재귀가 아닌 재귀 메서드에서는 여전히 `SystemStackError`가 발생합니다. 

## 마무리

꼬리 재귀 최적화가 무엇인지, 그리고 루비에서 어떻게 작동시키는 지를 간략히 살펴봤습니다. 기본 YARV와 Rubinius에서는 TCO가 가능하지만, JVM에서 TCO를 지원하지 않기 때문에 JRuby에는 TCO가 없습니다. 사용하실 때 이런 부분을 꼭 유의해주세요!
