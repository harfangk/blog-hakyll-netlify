---
title: JSON Web Token (JWT)란 무엇인가?
---

진행 중인 프로젝트에 로그인 시스템이 필요해서 Elixir에 있는 라이브러리를 살펴봤더니 [ueberauth](https://github.com/ueberauth/ueberauth), [guardian](https://github.com/ueberauth/guardian), [openmaize](https://github.com/riverrun/openmaize) 등 괜찮아 보이는 라이브러리가 금방 나왔습니다. 

어떻게 동작하는지 살펴보기 시작했는데, 각 라이브러리에 대해 논의하는 포럼 글에서 JWT에 관한 이야기가 나오더군요. JWT는 또 뭐냐... 알지 못하는 약어가 새로 튀어나왔습니다. 진짜 이 분야는 군대만큼이나 약어에 환장한 것 같습니다. 어쨌든 나왔으니 알아봐야죠.

<!--more-->

## JSON Web Token (JWT)

구글링해보니 JWT는 JSON Web Token의 약어입니다. 간단히 말하면 토큰처럼 사용할 수 있도록 JSON을 인코딩하고 서명한 것입니다. 상당히 새로운 포맷으로 2015년 5월에야 [RFC-7519](https://tools.ietf.org/html/rfc7519)가 등록되었습니다.

대충 내용물은 이렇습니다.

```js
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
```

저 내용물은 이렇게 JWT로 전환합니다.

```js
header = base64_encode(headerJSON)
payload = base64_encode(payloadJSON);
signature = base64_encode(HMAC_SHA256(header + "." + payload, secret))
jwt = header + "." + payload + "." + signature
```

그러면 다음과 같은 결과물이 나옵니다.

```js
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.
eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImV4cCI6MzAwfQ.
WlYm_cYdqaTioKJ4bvSqElSmymKLJQLTYIwSnhnomm4
```

JWT가 무엇이고, 어떻게 사용해야할 지에 대해서는 다음 글이 도움이 되었습니다. [장문의 소개글](https://jwt.io/introduction/), [조금 더 짧은 소개글](https://stormpath.com/blog/jwt-the-right-way), [세션용으로 JWT 좀 쓰지마](http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/), [세션용으로 JWT 좀 쓰지마 - 플로차트 추가 버전!](http://cryto.net/~joepie91/blog/2016/06/19/stop-using-jwt-for-sessions-part-2-why-your-solution-doesnt-work/), [이전 글에 대한 토론 스레드](https://news.ycombinator.com/item?id=11895440). 그래서 JWT는 뭐에 좋은 걸까요?

## 장점

제가 이해하는 바로는 JWT는 서버가 요청자에게 특정 기능을 허가해줄 지 판단하기 위해 필요로 하는 모든 데이터를 JWT 안에 담아두려 합니다. 덕분에 데이터에 누가 손댔는지 여부를 확인하기 위해 서명을 확인하는 것 외에는 서버에서 인증 작업을 하지 않아도 됩니다. 또 JWT 내부에 만료 시간을 넣어서 스스로 만료되도록 할 수도 있습니다.

즉, 서버가 JWT를 받으면 서명이 맞는지 확인하고, 디코드하고, 만료되지 않은 것을 확인하고, 요청받은 기능을 허가해줄 지 JWT에 담긴 데이터를 보고 판단합니다. 검증을 위해 데이터베이스나 세션에 접근하지 않아도 되는거죠.

위의 예시를 통해 보자면, 서버는 "Leonardo DiCaprio"가 실제로 오스카를 수상할 수 있을지 여부를 별도로 확인하지 않습니다. JWT에 "canWinOscar"가 true로 되어 있으니 그냥 바로 요청을 수행합니다. 물론 서명이 맞고, 서명 이후 아직 300초가 지나지 않았다는 전제 하에서 말이죠. 

## 단점

기본적으로 JWT에는 인가에 필요한 모든 자료가 내재되어 있도록 디자인되었습니다. 서버의 책임을 덜어주지만 동시에 서버가 가진 통제력도 빼았아가게 됩니다.

가장 문제가 되는 것은 서버에서 개개의 JWT를 무효화시키는 것이 어렵다는 점입니다. 물론 하려면 방법이 있긴 하지만 단점이 너무나 뚜렷합니다. 크게는 다음의 두 가지 방법이 있습니다.

1. 서명에 사용된 비밀 키를 무효화합니다. 문제는 이러면 해당 키를 사용해 서명된 모든 JWT가 무효화됩니다.
2. 문제가 있고 아직 만료되지 않은 JWT의 목록을 만들고 유지합니다. 단, 이는 조금만 잘못하면 너무 복잡해져서 JWT를 사용하는 의미가 없게 되어버립니다. 

## 결과

JWT는 서버로부터 독립적으로 기능하고, 짧은 시간만 작동하며, 스스로 만료된다는 특징을 가진 일회성 토큰으로 사용되는 것이 가장 적합한 것 같습니다. 세션이나 기존의 토큰을 대체할 수는 없고, JWT만의 용도가 있습니다.

결국 제가 만들려는 인증 시스템에는 못 쓸 것 같고, 그냥 간단하게 제가 스스로 만들기로 했습니다. 

또 삽질로 끝나게 됐지만 세션이랑 토큰의 작동 원리를 더 확실히 알게 됐으니 뭐 수지는 맞는 것 같습니다.
