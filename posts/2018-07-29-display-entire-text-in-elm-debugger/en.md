---
title: How to Display Entire Text in Elm Debugger
---

Elm Debugger is one of the most amazing debugging tools I've ever used - too bad I rarely get to use it because Elm makes it so difficult to produce serious bugs. The Debugger places a limit on the length of displayable text in it, and truncates any text or data structure that might go over that limit. It's a sensible default, but sometimes it hampers my debugging effort especially when I'm trying to see the exact message I've received. Here's an example of how the it truncates messages in Richard Feldman's [RealWorld example app](https://github.com/rtfeldman/elm-spa-example):

<!--more-->

![Default Debugger](/images/debugger_vanilla.png "Default Debugger")

Those ellipses in the Debugger have annoyed me enough for me to come up with a way to get rid of them, and I was moderately successful. Here's how the messages look like now:

![Debugger Without Truncation](/images/debugger_full_text.png "Debugger Without Truncation")

I did it by overwriting the `messageToString` function in `elm-stuff/packages/elm-lang/virtual-dom/2.0.4/src/Native/Debugger.js`. Here's the replacement code:

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

Elm Debugger has another annoying default setting - the fixed width of `30ch` on its sidebar. You can override it by editing `elm-stuff/packages/elm-lang/virtual-dom/2.0.4/src/VirtualDom/Debug.elm`:

![Debugger With Wider Sidebar](/images/debugger_modified_sidebar.png "Debugger With Wider Sidebar")

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

It's not the prettiest code, but it does its job. Feel free to use or modify it as you see fit. Just don't forget that it's a monkey patching to aid the development process - you'd better get rid of it when building for production.
