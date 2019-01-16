---
title: Emmet을 사용해서 erb, eex 에 <%= %> 태그 입력하기
---

전 Emmet을 정말 좋아합니다. HTML은 쓸 때마다 태그 닫는 것이 너무나도 짜증났는데 Emmet 덕분에 요즘은 HTML도 꽤나 즐겁게 쓰고 있습니다. HTML 문서를 작성할 일이 종종 있는데 혹시 아직 Emmet을 안 써보셨다면 꼭 써보세요. 장담하건데 신세계를 볼 수 있을 겁니다. 일반적으로 사용하는 에디터 대부분에서 플러그인 형식으로 지원하니 [여기](http://emmet.io/download/)서 확인해보세요.

<!--more-->

Emmet의 장점이라면 역시 HTML과 CSS를 제대로 하는데만 집중한다는 것입니다. 이거저거 다 하려다가 망하는 소프트웨어를 너무 많이 보아와서 Emmet이 한가지에만 집중하는 것이 정말 좋네요.

한 가지 문제가 있다면 제가 보통 쓰는 Embedded Ruby (.erb), Embedded Elixir (.eex) 에는 HTML 말고도 고유의 태그가 있다는 것입니다. Ruby나 Elixir 코드를 넣을 때 사용하는건데, 보통 `<%= %>` 나 `<% %>`가 대부분입니다. 딱 봐도 아시겠지만 타이핑하기 꽤나 귀찮습니다. 

보통 에디팅 모드에서는 [vim-ragtag](https://github.com/tpope/vim-ragtag) 를 써서 저 태그를 빠르게 입력하는데, Emmet으로 생성되는 결과물에는 그런 식으로 입력을 할 수가 없습니다. 예를 들어 다음 코드를 봅시다.

{% highlight erb %}<div id="box">  <%= for user <- @users do %>
  	<ul>
  	  <li class="item1"></li>
  	  <li class="item2"></li>
  	  <li class="item3"></li>
  	</ul>  <% end %>
</div>
{% endhighlight %}

Emmet을 사용하면 `div#box>ul>li.item$*3` 를 입력하고 확장변환 키를 입력합니다. 그러면 아래와 같은 코드가 생성됩니다.

{% highlight erb %}
<div id="box">
  <ul>
   <li class="item1"></li>
   <li class="item2"></li>
   <li class="item3"></li>
  </ul>
</div>
{% endhighlight %}

여기에 `<%= for user <- @users do %>` 와 `<% end %>` 를 직접 입력하고 그 사이에 들어가는 내용을 들여쓰기 해줍니다. 꽤나 귀찮아요.

다행히도 Emmet에서 커스텀 스니펫을 지원합니다. 문제는 [커스터마이제이션](http://docs.emmet.io/customization/) 관련 문서가 조금 부실하다는 겁니다. 사실 이 정도면 괜찮은 편인데 Emmet이 전반적으로 문서화가 너무 잘 되어 있어서 상대적으로 부족하게 느껴지는 것 같기도 합니다. 덕분에 조금 헤맸습니다.

Emmet은 JSON과 JavaScript 파일을 사용하는 커스터마이제이션을 지원하는데, 각 에디터마다 지원 방식이 조금씩 다르기 때문에 사용하는 에디터에 따라 문서를 참고하셔야 합니다. 

이게 제 `custom_snippets.json`의 내용입니다.

{% highlight json %}{
  "eelixir": {
    "extends": "html",
    "snippets": {
      "eex": "<%= | %>\n\t${child}<% end %>"
    }
  },
  "eruby": {
    "extends": "html",
    "snippets": {
      "erb": "<%= | %>\n\t${child}<% end %>"
    }
  }
}
{% endhighlight %}

삽질하면서 알게된 것들:

* 에디터가 인식하는 신택스 이름을 (eelixir, eruby 등) 정확히 입력해야 작동합니다. 
* Abbreviations와 snippets는 많이 다릅니다. 뭔가 추가하고 싶다면 아마 snippets 쪽에 추가하셔야 할 거에요. abbreviations는 HTML 기본 태그에만 사용되는 것 같습니다.
* `|` 는 `${cursor}`를 가리키는데, 태그가 확장변환된 이후 커서가 자리할 곳을 지정합니다.
* `${child}` 는 Emmet 신택스 트리의 하위 노드들이 표시될 장소를 지정합니다.
* `\n`와 `\t`는 그냥 사전 정의된 Emmet 변수로 각각 "newline" 과 "indentation" 를 나타냅니다.

이제 `div#box>eex>ul>li.item$*3` 를 입력하고 확장하면 다음과 같은 결과가 나옵니다.

{% highlight erb %}
<div id="box">  <%=  %>
  	<ul>
  	  <li class="item1"></li>
  	  <li class="item2"></li>
  	  <li class="item3"></li>
    </ul>    <% end %>
</div>
{% endhighlight %}

보시면 알겠지만 아직 `<% end %>` 부분에 들여쓰기 문제가 있습니다. 해결 중입니다. 그래도 이전에 비하면 훨씬 나아져서 꽤나 만족스럽습니다. 앞으로도 필요에 따라 계속 조정하면 되겠죠.
