---
title: Using Emmet to Type <%= %> in erb or eex 
---

Emmet is a lifesaver. Writing HTML used to be so painful and annoying that I had to groan at every single closing tag. Now I don't have to do that. If you write HTML documents but still do not use Emmet, you should definitely try it. It supports pretty much all major editors: check it out [here](http://emmet.io/download/).

<!--more-->

Emmet focuses on just doing HTML and CSS right, which is what makes it great. Too many softwares try to do too many things at once, and eventually collapse under the weight of their own complexities.

But I usually write Embedded Ruby (.erb) or Embedded Elixir (.eex), and they have their own tags to support embedding Ruby or Elixir into HTML. Those tags are mostly `<% %>` and `<%= %>`. If you've never used it, then yes, I can tell you that they are quite pesky to type.

I have [vim-ragtag](https://github.com/tpope/vim-ragtag) for writing those tags faster, but I can't automatically insert it into the structured output of Emmet. For example, let's say I wanted to type the following structure.

{% highlight erb %}<div id="box">  <%= for user <- @users do %>
  	<ul>
  	  <li class="item1"></li>
  	  <li class="item2"></li>
  	  <li class="item3"></li>
  	</ul>  <% end %>
</div>
{% endhighlight %}

With Emmet, I would type `div#box>ul>li.item$*3` and expand it. That will give me this.

{% highlight erb %}
<div id="box">
  <ul>
   <li class="item1"></li>
   <li class="item2"></li>
   <li class="item3"></li>
  </ul>
</div>
{% endhighlight %}

Then I will have to manually type in `<%= for user <- @users do %>` and `<% end %>`, and indent the contents within that block. That's quite annoying.

Fortunately, Emmet supports adding custom snippets. Although Emmet is quite well documented, documentation for its [customization](http://docs.emmet.io/customization/) feature was not quite up to that standard. It took me some time to figure it out.

Emmet's customization comes in the form of JSON and JavaScript files. Each editor has its own way of supporting it, so you will have to look into it yourself. 

This is what my `custom_snippets.json` looks like. 

{% highlight json %}
{
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

Some points that I learned:

* I need to enter the correct syntax name (i.e. eelixir, eruby) that my editor recognizes to make this work.
* Abbreviations and snippets are different. If you want to add something, it probably belongs in snippets. Abbreviations are mostly for HTML buliding blocks. 
* `|` is shorthand for `${cursor}`, which determines where the cursor is located after the tags are expanded.
* `${child}` represents children in the syntax tree. 
* `\n` and `\t` are preset variables that stand for "newline" and "indentation" respectively.

Now I can type `div#box>eex>ul>li.item$*3` and expand it, which gives me this.

{% highlight erb %}
<div id="box">  <%=  %>
  	<ul>
  	  <li class="item1"></li>
  	  <li class="item2"></li>
  	  <li class="item3"></li>
    </ul>    <% end %>
</div>
{% endhighlight %}

As you can see, there's an indentation issue with `<% end %>` which I'm trying to resolve. But this is still a great improvement over previous workflow, and I will keep tweaking it.
