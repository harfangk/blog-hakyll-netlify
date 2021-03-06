---
title: How to Bake Pi 리뷰
---

## 요약

[How to Bake Pi: an Edible Exploration of the Mathematics of Mathematics](https://www.amazon.com/How-Bake-Pi-Exploration-Mathematics/dp/0465097677) (파이를 굽는 방법: 수학의 수학에 대한 식용 안내서)는 제빵을 포함한 일상 생활 속의 예시를 사용해서 수학의 목적과 기초적인 개념을 이해하기 쉽게 설명해주는 책입니다.

추상수학에 대해서 배워본 적이 없지만 일반인도 이해할 수 있는 언어로 쓰인 범주론(category theory) 입문서를 원하는 분께 추천드립니다.

범주론을 배우고 싶거나 소프트웨어 개발에 이를 적용하는 방법을 배우고 싶은 분께는 추천하지 않습니다. 이 책은 범주론이 무엇을 다루는지에 대해 대략적으로 소개해주지만 실제로 범주론의 내용을 제대로 다루지는 않습니다. 범주론이라는 분야를 책으로 표현한다면, 이 책의 내용은 좀 길게 쓰인 서장정도에 해당할 것 같습니다.

(추가: 글 작성 이후에 알게 되었는데 이 책은 이미 한국어 번역본이 있습니다. [수학을 요리하다](http://www.kyobobook.co.kr/product/detailViewKor.laf?barcode=9791170220305&orderClick=357)라는 제목으로 출간되었습니다. 직접 읽어보진 않아서 번역의 질은 잘 모르겠습니다.)

<!--more-->

## 이 책만의 장점

제빵과 수학은 흔히 볼 수 있는 조합은 아닙니다. 이 책의 저자 Eugenia Chang은 제빵에 관한 비유를 최대한 활용해서 수학적 개념을 설명합니다. 예를 들어 추상화(abstraction)에 대해서 이야기할 때, 저자는 마요네즈와 홀랜다이즈 소스의 예시를 사용합니다. 두 소스를 만들기 위한 재료와 절차는 거의 동일하지만, 마요네즈에는 올리브 오일이 들어가고 올랜다이즈 소스에는 버터가 들어갑니다. 저자가 내린 추상화의 정의인 "작은 차이점을 제외하고는 실질적으로 동일한 것들"의 예시입니다.

제빵에 대한 비유는 책 전반에 걸쳐 등장하지만 그 외에도 일상 생활에서 가져온 비유가 많이 등장합니다. 추상에 대한 장에서 활용하는 예시로만 다양한 종류의 파이를 만드는 레시피, 숫자, 상심, 도로 표지판, 수학적 기호, 구글 지도 등등이 있습니다. 미묘한 느낌의 조합이긴 하지만 저자는 이 모든 것들이 일종의 추상화라는 것을 분명히 설명합니다.

물론 이런 비유가 좀 억지처럼 느껴질 때도 있고, 비유만으로는 그 본질적인 한계 때문에 설명하고자 하는 개념을 정확히 정의할 수가 없습니다. 하지만 저자의 목적은 수학을 보다 편하게 대할 수 있게 해주고, 독자가 수학을 개인적으로 이해, 혹은 저자가 표현하듯이 "깨달을 수 있게" 해주는 것입니다. 저자가 수학적 증명을 대충 보여주는 대신 여러가지 비유를 활용하려고 노력하는 것은 그 때문입니다. 덕분에 꽤나 즐겁게 읽을 수 있는 책이 완성되었으니 감사히 생각합니다.

## 책을 읽은 이유와 기대했던 점

하스켈을 배우면서 모노이드(monoid), 펑터(functor), 모나드(monad) 같은 기묘한 단어를 접하게 되었는데, 그 것들이 실제로 어떤 의미를 가지는지 당연히 궁금해졌습니다. 이게 추상수학의 한 분야인 범주론에서 빌려온 개념들이란 것을 알게 되었는데, 일단은 거기까지만 알아보기로 했습니다. 하지만 이에 대해서 더 배울 수 있는 기회는 계속 찾아보고 있었습니다. 그래서 범주론을 쉽게 소개해주는 책이 읽다고 들었을 때 바로 구매했습니다. 책을 읽기 전에 기대했던 것은 범주론에 대한 소개와 하스켈에서 접했던 용어들에 대한 설명이었는데, 책은 제 첫 번째 기대만 충족시켜주었습니다.

## 수학이란?

책의 1부의 제목은 `PART I: MATH`입니다. 직설적인 제목이죠. 저는 수학 공포증이 있는 것은 아니지만 그렇다고 그렇게 수학을 좋아하지도 않습니다.

고등학교와 대학교에서 배운 수학에서는 항상 방정식과 정리만을 다루었습니다. 교사나 책이 제시하는 새로운 방정식을 접하고, 무슨 방정식인지에 대한 설명, 예시 몇 가지, 그리고 연습문제를 잔뜩 푸는 식이었습니다. 이 이상한 방정식을 왜 배워야 하는지, 그리고 내가 작년에 배운 것들이랑 이게 어떻게 연결되는 지에 대해선 별다른 이야기를 들어본 적이 없습니다. 수학에 대해서는 대부분의 사람들이 비슷한 경험을 가지고 있으리라 봅니다.

이 책은 전혀 다른 방식으로 수학을 다룹니다. 특정한 방정식과 정리에 대해서 이야기하는 대신 그런 것들을 만들어내기 위한 수학적 원칙과 근본 개념에 대해서 이야기합니다. 저자가 정의하기로는 "수학은 논리 법칙을 따르는 모든 것을 논리 법칙을 사용해서 연구하는 학문"이며, 이 간결한 정의를 풀어내기 위해 수학의 핵심 개념들을 설명합니다.

추상화(abstraction)는 우리가 다루는 것들로부터 불필요한 세부사항을 제거하는 기법인데, 여기서 불필요한 세부사항이라는 것은 대상의 가장 중요한 특성을 파악하고 거기에 논리적 규칙을 적용하는데 방해되는 것들을 말합니다. 추상개념의 대표적인 예시로 숫자가 있습니다. 장바구니에 담긴 물품의 총 개수를 세고 싶을 때, 각 물품이 무엇인지는 추상화시켜서 제거한 뒤 하나의 물품이 존재한다는 사실에만 집중합니다. 이는 무엇인가를 센다는 목적을 위해서는 가장 적합한 수준의 추상화입니다.

이어서 저자는 원칙(principles)과 절차(processes)에 대해서 얘기하는데, 이는 앞서 말한 "논리적 법칙"에 해당합니다. 여기서는 수학에는 특정한 조건 하에서는 항상 참인 원칙들이 존재하고, 올바른 절차를 따르는 것은 정답을 맞추는 것만큼이나 중요하다는 점을 언급합니다.

이후에는 일반화(generalization)에 대해 설명하는데, 이 책의 1부에서 가장 중요한 내용이라고 생각합니다. 저자가 표현하기에 일반화는 어떤 개념에 걸린 조건을 조금씩 완화해서 더 많은 것들을 그 개념 안에 포함할 수 있도록 허용하는 과정이라고 합니다. 예를 들어 정사각형의 네 변의 길이와 네 모서리 각도는 모두 동일해야 합니다. 여기서 동일한 모서리 각도에 대한 조건을 살짝 완화해서, 서로 마주보는 모서리 각도끼리만 동일해도 된다고 하면 조금 더 일반화된 정사각형인 마름모가 탄생합니다. 마찬가지로 마름모에서 서로 마주보는 변의 길이만 동일하면 되도록 규칙을 완화하면 평행사변형이 되므로, 평행사변형도 조금 더 일반화된 마름모입니다.

이제 수학이 무엇인지 이해하기 위한 사고체계가 갖춰졌습니다. 수학은 실재하는 것들을 추상화한 개념을 다루는 학문으로, 각 추상개념은 특정한 원칙과 공리(axiom)를 따릅니다. 이러한 원칙을 완화하거나 강화함으로써 추상화의 수준을 조정하는 과정인 일반화를 적용하고, 각 추상개념 사이의 관계를 살펴봅니다.

수학을 저런 식으로 표현하는 것은 처음 보았습니다. 앞서 말했지만 제게 있어 수학은 괴롭고 기계적인 기법을 모아놓은 무엇인가일 뿐이었습니다. 반면 저자의 표현대로라면 오히려 수학은 어떤 게임의 규칙 자체를 가지고 놀 수 있는 흥미로운 메타 게임처럼 생각할 수 있습니다.

책의 2부까지 읽고 난 뒤에야 이는 범주론자의 관점에서 내린 수학의 정의라는 것을 깨닫게 되었습니다. 알고보니 저자는 수학의 기본 개념을 설명하는 과정에서 교묘하게 범주론의 개념도 설명하고 있던 셈입니다.

## 범주론이란?

2부의 제목은 `PART II: CATEGORY THEORY` 입니다. 간결하고 직설적인 제목을 참 좋아하는 것 같습니다.

범주론이 무엇인지에 대해서 저자는 다음과 같이 설명합니다.

> 범주론은 수학의 수학입니다. [...] 레고의 레고와 같이 일종의 메타 수학이죠. [...] *수학적인* 대상을 추상화하고, *수학*의 원칙과 절차를 연구하고 그를 공리화하고 일반화하려고 합니다.

제 생각에는 이런 식으로 쓸 수도 있을 것 같습니다. "수학적 방법론을 수학 자체에 재귀적으로 적용하는 것이 범주론이다."

2부에서는 범주론에서 중요하게 다루는 개념이 어떤 것들인지 설명함으로써 범주론이라는 분야를 보다 구체적으로 보여줍니다.

문맥(context)에 대한 장에서는 "범주론은 대상이 자체적으로 가진 절대적 특성보다는 어떤 문맥 하에서 대상을 다루는가에 더 중점을 둔다"고 합니다. 예를 들어 숫자 5는 자연수라는 문맥 하에서는 소수입니다. 하지만 실수라는 문맥 하에서는 1과 5 외에도 다른 숫자로 나누어질 수 있으므로 더 이상 소수가 아니게 됩니다. 따라서 숫자 5의 특성은 어떤 문맥에서 이를 살펴보는지에 따라 달라집니다.

또한 "[범주론은] 객체를 어떤 문맥에 놓고 살펴볼 지 결정하기 위한 방법으로 객체와 객체의 특성 자체를 살펴보기 보다는, 결정하기 위해서 객체가 다른 객체와 어떻게 관련되어 있는지를 강조합니다." 무슨 뜻인지는 그림으로 보는 편이 더 이해하기 편할 것입니다.

![relationship example](/images/relationship_example.png)

그림에 나온 관계인 `f`, `g`, `h` 등이야말로 범주론의 주요 연구 대상이라는 말입니다. 이러한 관계를 범주론에서는 사상(morphism) 또는 간단히 화살표(arrow)라고 부르는데, 함수를 표시하는 방법과 유사해 보인다고 생각할 수도 있습니다. 실제로 함수는 사상의 종류 중 하나이며 함수와 사상의 표기법도 어느 정도 유사합니다.

이런 개념들을 모두 설명한 뒤에 저자는 범주(category)의 정의를 제시합니다. 이 글에서 범주의 정의에 관해서 더 다루지는 않겠지만 어떤 것인지 보여주기 위해서 적도록 하겠습니다. 사실 책에서도 정의 자체에 대해서 깊이 있게 다루지 않는데, 제 생각에는 저자도 그냥 수학적으로 어떻게 정의하는지를 한 번 보여주고 싶었던 것 같습니다. 여기서는 하스켈에서 하듯이 `.`을 사용해서 합성(composition)을 표현하겠습니다.

어떠한 객체에 대한 범주에 포함된 사상은 다음 규칙을 따라야 한다.

1. 화살표 `f(a) = b`와 `g(b) = c`가 있을 때, 이를 합성하면 화살표 `g . f(a) = c`가 성립해야 한다.
2. 모든 객체에 대해 항등(identity) 화살표 `i(a) = a`가 존재해야 하며, 이는 모든 화살표 `f`에 대해서 `f . i = f` 와 `i . f = f`가 참이어야 한다는 것을 의미한다.
3. 세 화살표 `f, g, h`가 있을 때, 모든 합성 화살표에 대하여 `(h . g) . f = h . (g. f)`가 참이어야 한다.

이어서 저자는 범주론에서는 사상의 형태에 따라서 만들어지는 구조도 연구한다고 설명합니다. 서로 다른 종류의 객체임에도 불구하고, 객체끼리의 관계가 나타내는 구조를 그림으로 표현해보면 매우 유사한 경우가 있습니다. 책에서 제시한 예시 중 세 가지를 보여드리겠습니다. 모듈로 2에서의 덧셈, 1과 -1의 곱셈, 그리고 도형의 회전입니다.

![addition modulo 2](/images/addition_modulo_2.png) ![multiplication of 1 and -1](/images/multiplication_1_minus_1.png) ![rotation by degrees](/images/rotation_by_degrees.png)

책의 최후반부에서는 동일함(sameness)이나 보편적 특성(universal properties)에 대해서 다루는데, 문맥에 따라서 의미가 달라지는 이러한 개념들을 범주론에서 어떻게 연구하는지 간략히 보여줍니다.

책의 내용을 정리해보았는데, 별로 간략하지는 않군요. 안타깝게도 이 책은 여기서 끝납니다. 범주론에 대해서 본격적으로 설명하지는 않고 범주론이 무엇인지 소개하는 정도로 마무리됩니다.

## 기타 상념

수학이랑 간접적으로 연관된 주장도 책에 몇 가지 제시되는데 꽤 흥미롭습니다. 예를 들어 저자는 수학이 실제 삶에서 비논리적인 부분들을 추상화해서 제거하기 때문에 오히려 실제 삶보다 쉽다고 말하고, 사람들이 수학을 어렵다고 생각하는 것은 수학을 왜 배우는지 모르고 배우거나 수학이 삶에서 쉽게 만들어주는 것들에 별로 관심이 없기 때문이라고 합니다.

저는 이 말에 많이 공감이 되었습니다. 저도 수학을 배울 때 종종 "이걸 대체 왜 배워야 하는 것인가"라는 생각을 했었는데, 대부분은 비슷한 경험을 했으리라 생각합니다. 저는 허수가 무엇인지 배워야하는 이유를 아직도 들어본 적이 없습니다. 그리고 행렬을 배울 때도 이걸 어디에 써야할 지 감도 안 잡혔기 때문에 전혀 흥미가 생기지 않았습니다. 이를 고려하면 수학이 어려운 것은 가르치는 방법이 엉망이기 때문이라는 저자의 주장에도 일리가 있다고 생각합니다.

또한 추상수학이 철학과 매우 유사하다는 생각도 들었습니다. 저자는 논리의 한계나, 어떤 것을 아는 것과 이해하는 것과 믿는 것이 다르다는 점에 대해서 이야기하는데, 이건 철학에서 자주 다루는 주제입니다. 두 분야가 여러가지 측면에서 많이 비슷하다는 이야기를 전에 듣기는 했는데, 실제로 그렇게 느껴보기는 이번이 처음이었습니다.

그리고 소프트웨어 개발이라는 문맥에서의 범주론에 대해서는 결국 아무 것도 배우지 못 했지만, 적어도 이제 모나드나 펑터 같은 용어를 보다 편하게 대할 수 있게 되었습니다. 예전에는 완전 외계에서 온 개념처럼 느껴졌는데, 이제는 이런 개념도 특정 문맥에서는 일반적인 개념이며 어디서 그에 대해 더 배울 수 있을지도 알게 되었습니다.

## 추가적인 읽을거리

책을 읽는 동안 소프트웨어 개발이라는 측면에서의 범주론을 배울 수 있는 자료를 찾았습니다. [프로그래머를 위한 범주론](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)이라고 하는데, 읽을 책 목록에 넣어놓았습니다
