---
title: 엘름 디버거에서 전체 텍스트 표시하기
---

엘름 디버거는 제가 써본 중 가장 우수한 디버거입니다. 엘름 언어 자체 특성 때문에 개발 과정에서 버그가 생길 일이 거의 없어서 사실 디버깅할 일이 없는게 함정이지만요. 엘름 디버거는 표시 가능한 문자열 길이에 제한을 두고, 지정된 길이를 넘어가는 문자열이나 자료 구조는 `...`으로 생략해버립니다. 합리적인 기본값이긴 한데 가끔 내가 정확히 어떤 메시지를 받았는지 알고 싶을 때는 짜증을 유발합니다. 리처드 펠드먼의 [RealWorld 샘플 앱](https://github.com/rtfeldman/elm-spa-example)에서 디버거가 긴 문자열을 생략하는 예시를 봅시다.

<!--more-->

![Default Debugger]({{ site.baseurl }}/assets/debugger_vanilla.png "Default Debugger")

말줄임표를 보는 것도 지겨워져서 전체 문자열을 표시할 방법을 연구하기 시작했고 어느 정도 성과를 거두었습니다. 이제 전체 메시지를 볼 수 있습니다.

![Debugger Without Truncation]({{ site.baseurl }}/assets/debugger_full_text.png "Debugger Without Truncation")

`elm-stuff/packages/elm-lang/virtual-dom/2.0.4/src/Native/Debugger.js` 파일에 있는 `messageToString` 함수를 아래 코드로 갈아치운 결과입니다.

```javascript
function messageToString(value)  {
	var result = '';
	for (var key in value)
	{
		result = result + ' ' + messageToStringHelper(value[key]);
	}
	return result;
}

function toFlatArray(value, result)
{
	if (value['ctor'] === '[]')
	{
		return result;
	}
	else
	{
		var new_result = [...result, value['_0']];
		return toFlatArray(value['_1'], new_result);
	}
}

function messageToStringHelper(value)
{
	switch (typeof value)
	{
		case 'boolean':
			return value ? 'True' : 'False';
		case 'number':
			return value + '';
		case 'string':
			return addSlashes(value, false);
	}
	if (value instanceof String)
	{
		return '\'' + addSlashes(value, true) + '\'';
	}
	if (typeof value !== 'object' || value === null)
	{
		return '…';
	}
	if (typeof value === 'object' && !('ctor' in value))
	{
		var result = [];
		for (var key in value)
		{
			result.push(key + ': ' + messageToStringHelper(value[key]));
		}
		return '{' + result.join(', ') + '}';
	}
	if (value.ctor.match(/^\_Task/))
	{
		return '…';
	}
	if (value.ctor.match(/^\_Tuple/))
	{
		var tupleResult = [];
		for (var key in value)
		{
			if (key !== 'ctor')
			{
				tupleResult.push(value[key]);
			}
		}
		return '(' + tupleResult.map(messageToStringHelper).join(', ') + ')';
	}
	if (['<decoder>', '_Process', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.ctor) >= 0)
	{
		return '…';
	}
	if (value.ctor === '::')
	{
		var arrayResult = toFlatArray(value, []).map(messageToStringHelper);
		return '[' + arrayResult.join(', ') + ']';
	}
	if (typeof value === 'object')
	{
		var result = '';
		for (var key in value)
		{
			result = result + ' ' +  messageToStringHelper(value[key]);
		}
		return result;
	}
}
```

엘름 디버거에서 `30ch`로 고정된 사이드바 너비도 꽤나 걸리적거립니다. `elm-stuff/packages/elm-lang/virtual-dom/2.0.4/src/VirtualDom/Debug.elm` 파일에 있는 코드를 수정하면 됩니다.

![Debugger With Wider Sidebar]({{ site.baseurl }}/assets/debugger_modified_sidebar.png "Debugger With Wider Sidebar")

```css
#values {
  display: block;
  float: left;
  height: 100%;
  /* width: calc(100% - 30ch); */
  width: 50%;
  margin: 0;
  overflow: auto;
  cursor: default;
}

.debugger-sidebar {
  display: block;
  float: left;
  /* width: 30ch; */ 
  width: 50%;
  height: 100%;
  color: white;
  background-color: rgb(61, 61, 61);
}
```

예쁜 코드는 아니지만 일단 제가 원하는 대로 동작은 하고 있습니다. 필요하시면 원하시는 대로 변경해서 사용하세요. 단, 어디까지나 개발 과정을 돕기 위한 몽키 패칭이니까 프로덕션용 빌드에서는 원래 함수로 되돌리는 것을 잊지 마세요.
