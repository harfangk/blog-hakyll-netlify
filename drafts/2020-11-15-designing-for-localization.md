---
layout: post
ref: designing-for-localization
date: 2020-11-15 00:00:00 +0900
title: Designing for Localization
lang: en
---

Are you working on a product that will be localized one day? Then I have some recommendations for you. I've localized two small products as a frontend developer and translator, neither of which was designed with localization in mind. They were not easy to work with. While I'm writing this post to save the translators from facing the same problem, this also matters to other professions. If your design does not have a room for localization, the localization process will tear apart the user experience that you've carefully laid out. It's a lose-lose situation and no one wants that. So here are my recommendations.

# 1. Leave room for varying text length and height
![Length difference example 1](/images/bithumb_ko.png)
![Length difference example 2](/images/bithumb_en.png)
The images should be self-explanatory. It's taken from [Bithumb](https://bithumb.com), a Korean cryptocurrency exchange. The Korean text has 138 characters, while the English one has 351 characters. The ratio is 1 to 2.54. Based on my experience, English texts are 1.5 to 2 times longer than Korean texts. German texts, in turn, are 1.3 to 2 times longer than English texts. The variance does decrease as the source text gets longer, but the difference is meaningful only for long articles or books.

Text height can vary a lot. The characters used in Asian languages like Thai, Hindi, Korean, Japanese, and Chinese are taller than Latin alphabets. Moreover, they tend to have larger leading too. That 1.5 line height perfect for English can make a Korean website completely unusable.

    20 likes
    20 μου αρέσει
    좋아요 20 개

# 2. Do not put non-text elements within a sentence
![UI element example](/images/shirt_dropdown.png)
In the above example, a drop-down element is put in the middle of the sentence. Never do this. 

First, it's ugly as sin. So I doubt any sane designer would do something like this.

Second, it becomes a grammatical nightmare. If the options were shirt and pants, the German equivalent would be `Ich möchte ein neues Shirt kaufen.` and `Ich möchte eine neue Hose kaufen.` Notice that the article and adjective change along with the object. This is necessary since German has more rigorous rules for inflection than English does.

Third, it becomes a frontend nightmare. Notice that the German version has a different word order. If it was Korean, the drop-down option would be at the beginning of the sentence. So the frontend developer would have had to put the drop-down at different locations for each language.

Fourth, it's difficult for translation tools fs
![Link example 1](/images/stripe_en.png)
![Link example 2](/images/stripe_ch.png)


# 3. Do not put texts with different styles within the same sentence
![Mid-sentence style example 1](/images/quanda_en.png)
![Mid-sentence style example 2](/images/quanda_ko.png)
    Tooling issues
    Most translation tools do not support indicating style change mid-sentence.
    Coloring or italicizing certain word
# 4. Consider how date, currencies are written
    Formal Japanese date requires era
    American English is MMDDYYYY, European is DDMMYYYY
    European decimal delimiter is , not .
    Turkish puts % before number
    Korean writes 20원 to ₩20
    Bahrain uses three decimal places
# 5. Avoid embedding texts within images
# 6. Use context-neutral texts (no packrat, no metaphor, Mark special smart branding copy and provide purpose behind that text, no abbreviations)
# 7. Font variety may be limited
# 8. Right to left, top to bottom
