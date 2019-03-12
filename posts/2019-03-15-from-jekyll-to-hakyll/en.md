---
title: From Jekyll to Hakyll":" An Epic Tale of Yak Shaving
---

## All Yak Shavings Start With A Missing Syntax Highlighting

It all began when I noticed that the Elm code snippets in my posts did not have syntax highlighting. It was easy to figure out what was going on. I had set up my blog using `GitHub Pages` gem, which included Jekyll and other supporting libraries. The syntax highlighting library `rouge`, however, was locked to version 2.2.1 that did not support Elm. Forcefully upgrading `rouge` might have resolved the issue, but then I thought, why not get rid of the `GitHub Pages` gem and upgrade all the dependencies? So began the yak shaving. 

<!--more-->

But then, why not fix that inconsistent CSS that had been bothering me? But then, why not try out a different type of static site generator? But then, why not experiment with a language that I have been learning recently? Fortunately Hakyll, a static site generator in Haskell, seemed to be quite stable and have most of the features I needed. So I decided to rewrite my blog in Hakyll. At this point I fully realized that I was in a long yak shaving trip, but I just shrugged and started the journey.

## Haskell: No Trial and Error For Understanding Program Behavior

Hakyll is the first Haskell library that I've used extensively, and it felt surprisingly difficult to pick up. When I first try out new libraries in other languages, I first skim through the documentation to get the general sense of it. Then I try running small bits of codes, observing the results to check whether my understanding of the libraries' behaviors are correct. I'm quite sure that this is a typical approach.

The problem was that I couldn't use this approach with Haskell. When I tried to run codes from Hakyll after reading its documentations, the compiler simply told me that the types did not match. Such feedback was of little use to me as it wasn't telling me anything about whether the codes I wrote were correct or not. To be fair, the compiler was explaining to me that I did something wrong, but it was explaining my mistakes in terms of Hakyll types, the very same things that I was trying to understand in the first place so it wasn't very helpful. So I had to rely on documentations and source codes to understand what those Hakyll types meant and how they worked, which felt similar to trying to understand new mathematical concepts by just reading the theorems and their proofs without reaching a pen.

Another issue was that I had to understand a major portion of the library before I could use it even a bit. Hakyll provides `hakyll :: Rules a -> IO ()` as the main interface with IO. So I need `Rules a` type. According to the documentation, it represents `the different rules used to run the compilers`. A `Rules a` is created using functions like `match :: Pattern -> Rules () -> Rules ()`, `route :: Routes -> Rules ()`, and `compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()`. Now I had to understand what `Pattern`, `Routes`, `Comipler a`, `Item a` types represented and how to use them, and this journey down the rabbit hole continued on. Such a journey is quite common in software, but Haskell compounded the difficulty by effectively forcing me to complete the whole journey in one go, as I couldn't use trial and error to understand the types bit by bit.

Fortunately, Hakyll's author provides plenty of examples through tutorials and example implementations, which were immensely helpful as I tried to understand what was going on in Hakyll. Without them, I wonder how much longer it would have taken me to complete this journey. If you plan to write Haskell libraries, please provide plenty of example codes. Haskell's strict compiler makes it inherently more difficult to experiment with the libraries, especially to beginners like me, so every little bit to mitigate that hurdle helps a lot.

## Internationalization Is Always My Bane

Just like in my previous blog, internationalization was the most painful feature to implement. Hakyll does not provide this functionality, so I had to implement it myself. I could google an existing implementation but I did not like it. In that implementation, The source markdown files had texts for both languages preceded by language codes, and texts in unused language were removed with Unix `sed` utility. The markdown files had texts like this:

```md
Fr: ## Bienvenue
En: ## Welcome
Fr: Bienvenue sur mon site.
En: Welcome on my website.
```

It felt wrong to have texts in multiple languages intermixed like that. Not only did it look weird, but also would have conflicted much with my writing process. So I decided for another structure where posts would be kept in separate files for each language under a folder. For example, the markdown files for this post are in `posts/2019-03-15-from-jekyll-to-hakyll/en.md` and `posts/2019-03-15-from-jekyll-to-hakyll/ko.md`. Having Hakyll find and transform these files into appropriate HTML files was straightforward.

The problem was that posts needed links to versions in other languages. In order to create the links, Hakyll had to know what other files existed in the same folder as the file that was getting built. This is an IO operation that must be represented inside the IO type in Haskell, so I listed all post directories and files within the `main :: IO ()` function, then used them to build posts.

Another problem was actually creating the links in the templates. I had to iterate over the available language versions, which would have been simple if I could pass to Hakyll template a custom tuple or record that contained link texts and link urls. But Hakyll template only accepted `Item a` type that represented some content and an identifier. I created a list of `Item String` that had "en" or "ko" as its identifiers and empty strings as their bodies, as I only needed those identifiers, then pass the list to template to build the appropriate links. It's not necessarily a misuse of `Item a` type, but it felt like a unnecessarily convoluted way to create links.

## Vertical Rhythm and Rem-based Layout

I had first learned of the concept of vertical rhythm while reading a [post](https://www.sylvaindurand.org/improving-typography-on-jekyll) on it. I loved how the texts looked in his posts, so I asked his permission and copied the author's CSS in the previous version of my blog. At the time I wasn't familiar with CSS so I had copied the entire CSS file. I got more familiar with CSS in the meantime, so decided to get a better understanding of the concept and reimplement it myself. This [post](https://zellwk.com/blog/responsive-vertical-rhythm/) was very informing, and I followed the advice in there to create a rudimentary vertical rhythm. The result was not as elegant as Sylvain's, but it was something I could understand and maintain well enough. Moreover, I also created a Rem-based layout inspired by another [blog](https://sonnym.github.io).

## Netlify Over Github Pages Any Time

I also decided to move my hosting from Github Pages to Netlify. The main reason was that Netlify supported redirection rules, allowing me to check the users' HTTP headers and send them to `/ko/index.html` or `/en/index.html` accordingly. Github Pages did not provide this feature, so I had to provide English content in `index.html` and Korean content in  a separate `index_ko.html` file. This inconsistency has been bothering me from the beginning. Netlify also provides most of Github Pages' strengths, such as automatically building and deploying sites upon new push to source code repositories, while supporting a lot more features so I highly recommend it over Github Pages.

## Plot Twist: It's Still Missing Elm Syntax Highlighting

After I finished rewriting the blog, I realized that I still did not have Elm syntax highlighter. To convert markdown files to HTML, Hakyll uses a Haskell library called Pandoc, which in turn uses a library called Skylighting for syntax highlighting functionality. And this Skylighting library was missing the syntax definition for Elm. So after all that adventure through Haskell types and new CSS techniques, I was back at where I've begun my yak shaving journey. I did feel a bit dismayed, but I decided to take one last step and wrote down the missing syntax definition, which is now waiting for review in both Skylighting and KDE's syntax highlighting library. In the meantime, I've configured my `stack.yml` to use my fork of Skylighting. That finally concluded the rewriting.

## But Was It Worth It?

The rewrite took way longer than I've expected. If someone asked me whether I would recommend Hakyll as a static site generator for beginners, I wouldn't. I think Haskell, because of its learning curves, is an overkill for a static site generator. Go or Javascript ones would have been easier and faster. On the other hand, this was a great opportunity for me to learn and write an actual Haskell program. So if you're already familiar with static site generators, and if practicing Haskell is your goal, then I would definitely recommend Hakyll as the library has straightforward abstractions and plenty of examples and tutorials online.
