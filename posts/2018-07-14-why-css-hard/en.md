---
title: Why I Find CSS So Hard
---

Once I heard someone half-jokingly say that he finds CSS harder than Haskell. At the time I just laughed at the joke, but after working with CSS for some time I now understand what he meant. CSS is complex. While it has simple syntax and structure, its output depends so much on the runtime context that it's hard to reliably predict the result.

## Three Problems

For me, three issues stood out:

1. Difficult to determine which CSS rule is finally applied
2. Difficult to know how CSS rules interact
3. Difficult to guess the runtime environment

<!--more-->

### Difficult to determine which CSS rule is finally applied

CSS allows duplicate declarations of CSS properties. Determining which declaration would be finally applied is actually quite easy - the algorithm, called [cascade](https://developer.mozilla.org/en-US/docs/Web/CSS/Cascade), is simple. The difficult part is building the exhaustive list of all duplicate CSS properties.

First, CSS properties can be declared anywhere in any file, commonly called the global namespace problem. In order to predict the output, we must go through all CSS files loaded with the document file. Skimming through dozens of lines of CSS is not hard; fastidiously rummaging through thousands of lines of CSS across dozens of files is tedious, if not impossible.

Second, CSS properties of a DOM element can be inherited from its ancestors, requiring us to inspect not only the rules for a particular DOM element, but also all the rules for all of its ancestors. For example, the font value of an element at the end of DOM tree could have been declared for any of its ancestors.

Third, there are too many ways to match a particular DOM element because CSS selectors can be flexibly combined. Take the following example HTML code:

```html
<div class="container">
  <p id="content" class="sample-text">
    <span>Sample Text</span>
  </p>
</div>
```

There are numerous ways to match the `span` tag in that snippet. Just to name a few: `span` type selector, nested type selectors of `div p span`, nested combined selectors `.container #content span`, partially nested combined selectors: `.sample-text span`, and so on. As a result, it's impossible to quickly and reliably identify which properties would apply to which element in the end. 

In theory, this means that all CSS rules of all CSS stylesheets should be inspected in order to build the exhaustive list of CSS rules that match a DOM element and determine the output. In practice, we can make educated guesses so it rarely comes close to that level of complexity. But when our guesses go wrong, the root problem rears its ugly head. As a result, debugging CSS problems often feels more like a hit-or-miss process than logical problem solving.

### Difficult to know how CSS rules interact

Most CSS properties interact with other properties to determine their outputs, adding another layer of complexity. Outputs for some CSS rules, such as `background-color`, `text-decoration`, and `cursor`, are not affected by other CSS properties, but they are more the exception than the rule.

Sometimes it's easy to guess which properties interact with one another. It's obvious that `font-size`, `font-weight`, and `font-family` properties all work together on typographic styling. But most of the time the interactions are difficult to guess from the names.

For example, does `border` property affect the values of `width` and `height`? Usually no, but sometimes yes, depending on the value of `box-sizing` property. Does increasing `font-size` affect them? Usually yes, but sometimes no, depending on properties like `overflow`. What about the size of children elements? Usually yes, unless those children elements have certain values for `position` property. The list goes on. 

### Difficult to guess the runtime environment

The vendor and version of the web browser, and sometimes even that of the operating system, also affect the output of CSS.

Each web browser implements CSS specifications in different ways. This issue is much less problematic in modern browsers, but Microsoft's browsers still have some notable differences from other browsers. Older browsers do not support newly introduced CSS properties, so we have to consider which browsers we want to support.

User's browser setting is another factor. Zooming in, changing font size, or using user-defined local CSS can all dramatically alter the output.

Moreover, operating system settings can also affect the output. MacOS and Windows use different font rendering system, making fonts look slightly bolder in MacOS, so same `font-weight` CSS property will not produce same outputs. Operating systems have their own text size options, which also affect the typography.

Lastly, user's device also affects the final output of CSS. Device's screen size determines the layout, and its color calibration can sometimes nullify carefully designed color schemes.

## How to Survive

### Difficult to determine which CSS rule is finally applied

I've mentioned global namespace, inheritance, and flexible selectors here, but global namespace is the root cause. Inheritance is a non-issue when there aren't many ancestor properties to inspect; flexible selectors can be handled through conventions.

Numerous solutions to the CSS global namespace issue have been proposed over the years. My favorite are the following two approaches. I suggest starting with BEM, and then introducing CSS Modules once the project gets complex enough to justify its cost.

#### BEM Naming Convention

[BEM](https://en.bem.info/) provides a naming convention for CSS ids and classes that are distinct enough to avoid collision in the global namespace of CSS. It strikes a good balance between simplicity and specificity, allowing developers to quickly pick it up and adapt it to the project's needs.

Since BEM is just a naming convention, it does not have any dependency like Javascript or SASS. It's a good starting place when you you want to keep the project approachable and avoid unnecessary complexity. Since BEM itself is simple, it's really easy to add other dependencies as the project evolves.

Still, it has the drawbacks of a manually enforced convention. Team members should have shared understanding of what it is, how to use it, and discipline to follow the convention. In the end, however, there will be inconsistencies in how it's followed.

#### CSS Modules

[CSS Modules](https://github.com/css-modules/postcss-modules) provides automatic local scoping of CSS by adding unique hash string to the CSS selectors used in each file, scoping their namespace to that particular file.

CSS Modules is much harder to set up. Using it as a [PostCSS](https://github.com/postcss/postcss) plugin loaded through [PostCSS Loader](https://github.com/postcss/postcss-loader) in [webpack](https://webpack.js.org/) using Javascript is the most common approach. That's a lot of dependencies for a single feature, especially when webpack itself is notoriously complex to set up.

Nevertheless, CSS Modules provides a reliable automatic solution to the global namespace problem and at some point the project might get complex enough to require it.

### Difficult to know how CSS rules interact

Unfortunately, the only way to learn which CSS properties interact with one another in what way is through experience. There doesn't seem to be a reliable heuristic for guessing the relationship based on the names. My advice is to be patient and approach it like when trying to learn the vocabulary of a foreign language, rather than figuring out fundamental axioms of a formal system.

### Difficult to guess the runtime environment

Unfortunately, this is a problem that cannot be solved through the code. We can't change the runtime environment of the users, nor can we write CSS that will render perfectly for all runtime environments. We have to come to peace with the reality - getting stressed over something we can't change is waste of our energies. Instead, it's wiser to define which runtime environments to target and support them. 

## Accepting CSS As It Is

At first CSS felt too unpredictable and unstructured so I hated it. But I'd learned that remaining open-minded is essential when learning something new, so I tried to assess and use CSS from its point of view, not mine. Once I decided to just accept CSS as it is, I started to appreciate it as a tool that did its job. It still does throw me off sometimes, but I even started to have some fun writing it. Granted, it will not be my favorite thing, but I've come to terms with it.
