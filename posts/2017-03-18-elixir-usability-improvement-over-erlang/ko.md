---
title: 엘릭서에서 얼랭보다 사용성이 개선된 9가지 측면
---

이 글은 [Sergii Boiko](http://railsware.com/blog/author/sergii-boiko/)가 [railsware](http://railsware.com/)에 올린 [Important overhaul Elixir did to Erlang to become appealing web-development tool](http://railsware.com/blog/2016/08/23/important-overhaul-elixir-did-to-erlang-to-become-appealing-web-development-tool) 라는 글의 전문을 번역한 글입니다.  

<!--more-->

---

엘릭서가 얼랭 VM에서 돌아간다면 그냥 엘릭서 대신 얼랭을 써서 웹 어플리케이션을 만들면 되지 않나요? 라는 질문을 종종 듣곤 합니다. 짧게 답하자면 순수한 얼랭만으로는 피닉스/엑토 같은 것을 만드는 것은 불가능합니다.

그렇다면 엘릭서가 완전히 다른 언어라는 의미일까요? 그렇지는 않습니다. 제 생각에 엘릭서는 80%가 얼랭이고, 나머지 20%는 특히 웹 어플리케이션을 만들 때 언어의 사용성을 극적으로 개선하는 중요한 요소로 이루어져 있습니다. 재미 있는 것은 조 암스트롱 본인도 얼랭은 웹 개발에 적합하지 않다고 말한 적이 있다는 것입니다. 하지만 저라면 얼랭으로 웹 어플리케이션을 만들려면 코드가 장황해진다고 표현할 것입니다. 

엘릭서와 얼랭의 차이점 중 가장 중요한 것들을 한 번 살펴봅시다.

## 1. 문자열 문제 해결

얼랭의 문자열에는 문제가 많습니다. 얼랭이 최초에 사용한 문자열 타입은 그냥 char 리스트였는데, 이건 프롤로그 언어에서 물려받은 방식으로 매우 비효율적이었습다. 요즘은 아무도 이걸 쓰지 않고 대신 바이너리를 사용합니다. 하지만 바이너리 문법은 이상하게 생겼습니다. 간단한 문자열을 하나 작성하려고 하는데도 다음과 같이 작성해야 한다고 생각해보세요.

{% highlight erlang %}
<<"Hello world">> % binary-string in Erlang
{% endhighlight %}

전 얼랭으로 개발을 할 때 이 엉망진창인 문자들을 타이핑하지 않으려는 일념 하에 빔에 특수한 단축키를 설정해야 했습니다.

바이너리의 또다른 문제는 얼랭에는 문자열 라이브러리와는 달리 바이너리를 다루는 쓸만한 라이브러리가 없다는 점입니다. 물론 직접 솔루션을 만들 수도 있지만 그건 매우 번거로울 뿐 아니라 심각한 파편화도 초래합니다. 직접 만든 라이브러리의 API를 아는 사람이 없을 테니까요. 또한 얼랭은 정규식을 라이브러리 형태로만 지원합니다. 연산자도 없고, 문법에서 퍼스트 클래스로 지원해주지도 않습니다.

{% highlight elixir %}
"Hello world" # the same string backed by binary but in Elixir
{% endhighlight %}

엘릭서는 문자열을 매우 잘 지원합니다. 문법도 그냥 간단한 따옴표를 사용하고요. 문자열 모듈의 API는 매우 우수하며, 심지어 `String.pad_leading`을 통해서 "left_pad"도 지원합니다. 정규식용 특수한 리터럴도 있고, 비교 연산자인 `=~`도 있으며 기능이 개선된 `Regexp` 모듈도 있습니다.

## 2. 우수한 표준 라이브러리

역사적으로 얼랭의 표준 라이브러리는 매우 일관성이 없고 빈약했습니다. 예를 들어 `lists` 모듈에는 중요한 함수가 많이 부족합니다. 리스트에서 10보다 큰 첫 번째 요소를 찾으려고 한다고 해봅시다. 얼랭 코드는 이렇습니다.

{% highlight erlang %}
case lists:dropwhile(fun(X) -> X =< 10 end, [1, 3, 8, 15, 7, 100]) of
  [] -> nil;
  [Y | _] -> Y
end.
%=> 15
{% endhighlight %}

엘릭서로 작성한 동일한 솔루션입니다.

{% highlight elixir %}
Enum.find([1, 3, 8, 15, 7, 100], fn(x) -> x > 10 end) #=> 15
{% endhighlight %}

엘릭서는 각 함수를 (특히 루비를 배웠다면) 매우 타당한 이름의 모듈 안에 넣어 놓았습니다. 또한 우수한 모듈인 `Enum`과 `Stream`은 리스트, 맵, 그리고 다른 iterable을 동일한 방식으로 다룰 수 있도록 해줍니다. 얼랭 표준 라이브러리는 이 모듈들이 제공하는 기능의 일부만 제공합니다.

## 3. 네임스페이스 추가

별 것 아닌 것처럼 보이지만 꽤나 중요한 부분입니다. 얼랭 모듈은 모두 하나의 평면적인 공간에 존재하며, 네임스페이스나 패키지 개념이 없습니다. 그래서 다른 패키지와의 충돌을 방지하려면 모듈에 고유한 태그를 항상 접두사로 붙여야 합니다. 그리고 이 접두사를 프로젝트 전체에 걸쳐 사용하게 되는데, 완전 쓰레기 같습니다. 예를 들어 프로젝트 이름이 Cowboy면 모든 모듈에 "cowboy\_" 접두사를 붙여서 "cowboy\_router", "cowboy\_stream" 처럼 명명하는 것이 베스트 프랙티스입니다.

그리고 어떤 모듈 안에 정의된 함수를 호출하려면 그 모듈 이름을 사용해야만 합니다.

{% highlight erlang %}
cowboy_req:reply(400, Req).
{% endhighlight %}

엘릭서도 여전히 ErlangVM에서 돌아가기 때문에 alias를 사용해서 네임스페이스와 서브모듈 기능을 흉내냅니다. `TenMinutesBlog` 같은 기묘한 이름의 프로젝트에 `TenMinutesBlog.User`라는 모델이 있으면 그 이름을 전부 사용할 필요가 없습니다. `alias` 덕분이죠.

{% highlight elixir %}
alias TenMinutesBlog.User
{% endhighlight %}

이제 이 모듈을 `User`라는 간결한 이름으로 사용할 수 있습니다.

## 4. 스트럭트 추가

레코드는 사람들이 얼랭에서 참 싫어하는 부분입니다. 제대로 된 계획 없이 만들어졌을 뿐 아니라 문법도 장황하죠. 얼랭에서 `Person` 레코드를 사용해서 만들려면 다음과 같이 해야 합니다.

{% highlight erlang %}
-module(using_record).
-record(person, {fname, lname, phone, address}).

full_name(Person) ->
  Person#person.fname ++ " " ++ Person#person.lname.
{% endhighlight %}

눈에 거슬리는 `#person` 부분이 보이지요? 얼랭에서 레코드를 사용하는 문법입니다.

엘릭서 스트럭트는 매우 간단하며, 위 코드와 유사한 기능을 다음과 같이 구현합니다.

{% highlight elixir %}
defmodule Person do
  defstruct fname: nil, lname: nil, phone: nil, address: nil
end

defmodule UsingStruct do
  def full_name(person) do
    person.fname <> " " <> person.lname
  end
end
{% endhighlight %}

## 5. 변수 리바인딩 허용

함수형 언어는 immutable 자료구조를 사용하기 때문에 자료에 변화가 있을 때마다 자료를 새로 만들게 됩니다. 따라서 자료의 상태가 변화할 때마다 그걸 새 변수에 바인드해야 합니다. 이러면 프로그램에 로직을 추가하려 할 때 변수 이름도 주의를 기울여서 변경해야 하기 때문에 매우 변화에 취약한 코드가 만들어집니다. 얼랭 코드를 봅시다.

{% highlight erlang %}
Users1 = user:get_all_users(),
Users2 = user:add_user(Users1, User),
Users3 = user:remove_user(Users2, User2)
{% endhighlight %}

이 코드에 중간 단계를 추가하려면 `User3`를 `User4`로 바꾸는 등의 작업을 해야 합니다. 이건 순수한 함수형 언어에 공통적으로 발생하는 문제입니다. 예를 들어 하스켈에서는 state monad를 사용해서 이런 로직을 다룹니다.

조제 발림은 매우 대담하게도 엘릭서에 변수 리바인딩을 허용했습니다. 엘릭서는 자료를 변형할 수 있다는 말이구나! 라고 이해할 수도 있지만 그렇지는 않습니다. 원래의 자료구조가 변형되는 것도 아니고, 엘릭서에는 순환 참조도 없기 때문에 안전합니다. 동일한 로직을 엘릭서로 작성한 코드입니다.

{% highlight elixir %}
users = User.get_all_users
users = User.add_user(users, user)
users = User.remove_user(users, user2)
{% endhighlight %}

이제 이름을 변경할 필요 없이 자유롭게 중간 단계를 추가할 수 있습니다.

## 6. 파이프 연산자 |> 추가

얼랭에는 파이프 연산자가 없기 때문에 함수 호출 여러개를 합성하려면 몇 단계로 쪼개서 중간중간에 변수에 할당하는 것이 좋습니다. 예를 들어 얼랭으로 짝수만을 골라내서 제곱을 한 뒤에 리스트의 순서를 뒤집어봅시다.

{% highlight erlang %}
lists:reverse(
  lists:map(
    fun(X) -> X * X end, lists:filter(fun(X) -> X rem 2 == 0 end, [1,2,3,4])
  )
).
{% endhighlight %}

이 코드는 매우 읽기 힘들기 때문에 여러 단계로 분리되어야 합니다. 엘릭서 파이프 연산자를 사용하면 훨씬 읽기 편한 코드를 작성할 수 있습니다.

{% highlight elixir %}
[1,2,3,4]
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
|> Enum.map(fn x -> x * x end)
|> Enum.reverse
{% endhighlight %}

## 7. 다형성 추가

얼랭에는 다형성을 간단하게 구현할 수 있는 방법이 없습니다. 반면 엘릭서에는 프로토콜이 있어서 그 프로토콜이 제공해야 할 함수의 목록을 명시할 수 있습니다.

예를 들어서 iterable 자료 구조를 새로 만들고, `Enum` 모듈을 통해서 이를 사용하고 싶다면 `Enumerable` 프로토콜에 명시된 인터페이스 함수를 모두 구현하기만 하면 됩니다. 그러면 `Enum` 모듈을 사용해서 커스텀 타입을 다룰 수 있습니다. 엑토와 피닉스가 이 방식을 사용해서 원래 라이브러리를 변경하지 않으면서도 기존에 지원되는 타입을 서드 파티 라이브러리가 확장할 수 있도록 하고 있습니다.

## 8. 리습 스타일 매크로 추가

얼랭에는 C 스타일 매크로가 있습니다. 즉 단순히 텍스트를 생성해주는 매크로이지요. 얼랭에는 다루기 매우 어려운 parse-transform 엔진도 있습니다. 이걸 사용하면 여러가지를 할 수 있기는 한데 매우 품이 많이 들 뿐 아니라, 만들어진 확장 프로그램도 변화에 매우 취약합니다. 저도 얼랭용으로 매우 간단한 확장 프로그램을 만든 적이 있는데 프로덕션에서는 절대 사용하지 않았습니다. 얼랭 버전이 바뀐 후에도 작동할 지 확신을 가질 수 없었기 때문입니다.

엘릭서는 완전히 다릅니다. 사실 엘릭서 언어의 핵심 문법은 매우 작고, 이 작은 문법과 매크로를 사용해서 나머지 부분을 만들어낸 것입니다. 다른 언어에서 특수한 문법인 것들, 예를 들면  `if/else`, `case`, `defmodule`, `def` 등을 엘릭서는 대부분 매크로로 구현합니다. 사실 매크로는 엘릭서에서 가장 중요한 기능 중 하나로, 덕분에 피닉스와 엑토의 DSL이 만들어질 수 있었습니다. 

## 9. 문법 변화

엘릭서 문법이 얼랭 문법보다 훨씬 낫다는 불평이 나올 것이라고 예상했나요? 사실 얼랭 문법은 매우 간결하며, 코드를 읽기 쉽고 좋게 작성할 수 있도록 해줍니다. 순수한 얼랭 문법으로 상태 기계를 표현한 코드는 환상적인 DSL처럼 보입니다. 예를 들어 두 언어로 각각 구현한 피보나치 수열을 봅시다.

{% highlight erlang %}
-module(fib).
-export([fib/1]).

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N - 2) + fib(N - 1).
{% endhighlight %}

{% highlight elixir %}
defmodule Fib do
  def fib(1), do: 1
  def fib(2), do: 1
  def fib(n), do: fib(n - 2) + fib(n - 1)
end
{% endhighlight %}

얼랭 코드는 일반적인 수학적 표기법과 매우 비슷하게 보이고, 엘릭서는 그보다 조금 더 장황합니다. 하지만 얼랭 코드는 세미콜론과 콤마를 사용해서 구문을 구분하기 때문에 변경하기 더 어려울 때가 있습니다. 또한 엘릭서 문법은 루비의 영향을 받았기 때문에 루비 개발자에게는 매우 매력적입니다. 

## 언제 어떤 언어를 사용해야 하나요?

엘릭서는 얼랭 바이트코드로 컴파일되고, 좋은 기능을 오버헤드 없이 제공하기 위해서 많은 노력을 기울이고 있습니다. 따라서 얼랭 코드를 작성한다고 실행 속도가 더 빨라지지는 않을 것입니다. 하지만 로우 레벨 기능이나 공용 라이브러리를 작성해서 얼랭VM 생태계 전체와 공유하고 싶다면 얼랭으로 작성하는 것이 이치에 맞을 것입니다.

반면 비지니스 로직이 꽤 있고 많은 사람들이 작업해야 되는 어플리케이션이면 엘릭서가 좋을 것입니다.
