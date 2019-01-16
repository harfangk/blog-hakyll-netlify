---
title: What Is JSON Web Token (JWT)?
---

I needed an authentication system for my application, so I looked at the existing libraries for Elixir and found some promising ones, such as [ueberauth](https://github.com/ueberauth/ueberauth), [guardian](https://github.com/ueberauth/guardian), and [openmaize](https://github.com/riverrun/openmaize). 

I started to look into how they did things, then I came across a discussion about JWT in authentication. Now then what the hell *is* JWT? All I know at this point is that it's another acronym that I don't know. I swear that this field is at least as obsessed with acronyms as the military, and that's definitely not a compliment. 

<!--more-->

## JSON Web Token (JWT)

So google told me that it means JSON Web Token (JWT). In a nutshell, it's a JSON object encoded and signed to be used like a token. It's relatively new, laid out in [RFC-7519](https://tools.ietf.org/html/rfc7519) in May, 2015. 

The data to put in JWT looks like this:

{% highlight js %}
{ // headerJSON
  "alg": "HS256", // algorithm used for signature
  "typ": "JWT" // type of token
}
{ // payloadJSON
  "sub": "1234567890", // reserved keyword (subject)
  "exp": 300, // reserved keyword (expiration time)
  "name": "Leonardo DiCaprio", // custom keyword
  "canWinOscar": true // custom keyword
}
{% endhighlight %}

Here's the process to create a JWT:

{% highlight js %}
header = base64_encode(headerJSON)
payload = base64_encode(payloadJSON);
signature = base64_encode(HMAC_SHA256(header + "." + payload, secret))
jwt = header + "." + payload + "." + signature
{% endhighlight %}

Which will result in:
{% highlight js %}
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.
eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImV4cCI6MzAwfQ.
WlYm_cYdqaTioKJ4bvSqElSmymKLJQLTYIwSnhnomm4
{% endhighlight %}

Here are some helpful posts to understand what it is and how to use it: [verbose introduction](https://jwt.io/introduction/), [less verbose introduction](https://stormpath.com/blog/jwt-the-right-way), [do not use JWT for sessions](http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/), [do not use JWT for sessions - now with flowchart!](http://cryto.net/~joepie91/blog/2016/06/19/stop-using-jwt-for-sessions-part-2-why-your-solution-doesnt-work/), [discussion on the previous article](https://news.ycombinator.com/item?id=11895440). So what is it good for?

## Advantage

The way I understand it, JWT tries to encapsulate in it all the data that the server needs to authorize for predefined actions. It removes the burden of server-side validation other than checking the signature against tempering of data. It can also invalidate itself through self-contained expiration timer.

So when the server gets a JWT, the server checks if the signature is correct, decodes it, checks that it's not expired, then authorizes the requested action solely based on the data inside JWT. The server does not have to access database or session to validate it. 

In terms of the above example, the server does not check whether "Leonardo DiCaprio" can actually win Oscar. JWT says true to he "canWinOscar", so the server carries out that action without further questioning. Well, as long as the signature is valid and the current time is within 300 seconds of signing.

## Disadvantage
JWT is designed to contain all the necessary data for authorization. This removes responsibility from the server, but at the same time it removes control from the server.

Most importantly, it makes server-side invalidation of individual JWT rather difficult. It *can* be done, but there are caveats. In general, you have two options: 

1. Invalidate the secret key used for signing, but this results in invalidation of all JWTs signed with that key.  
2. Keep track of problematic and unexpired JWTs in a revocation list. This can easily get too complicated and defeat the purpose of using JWT.

## Conclusion
It seems that JWT is best used as a self-contained, temporary, and self-destructing token for one-time authorization for a particular action. It's a different tool from session or traditional token, and should not be used in their places.

So I won't be using it for my authentication. Instead, I decided to roll out my own. 

This ended up as another chase into a rabbit hole, but I did gain firmer understanding of how sessions and tokens work so I guess it was worth it. 
