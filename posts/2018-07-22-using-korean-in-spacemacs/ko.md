---
title: Spacemacs에서 한국어 사용하기
---

## Spacemacs에서 한국어 사용하기

Vim에서 하스켈 개발 환경을 설정하다가 짜증이 나서 대안을 찾다가 Spacemacs를 설치해서 사용해보고 있는데, 별도의 설정 없이 설치하자마자 바로 쓸 수 있을 정도로 기본 설정이 훌륭해서 현재까지는 만족스럽게 쓰고 있습니다. 다만 한국어를 설정하는 방법이 옛날 글만 있거나 이맥스 초보가 이해하기 어렵게 작성되어 있어서 좀 헤멨습니다. 문서를 찾아보면서 어느 정도 원하는 대로 설정하는데 성공해서 설정을 공유합니다. 한국어 글꼴은 `D2Coding`을 사용하고 있는데 원하시는 글꼴을 설치해서 사용하시면 되겠습니다.

환경은 Ubuntu 16.04.4, Emacs 26.1입니다.

<!--more-->

```elisp
;; ~/.spacemacs

(defun dotspacemacs/user-config ()
  ...
  (set-language-environment "Korean")
  (prefer-coding-system 'utf-8)
  (setq default-korean-keyboard "3f")
  (global-set-key (kbd "<kana>") 'toggle-input-method)
  (global-unset-key (kbd "S-SPC"))
  (set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))
  ;; 정정: 아래와 같이 유니코드 범위를 명시할 필요 없이 위의 줄만으로도 한글 유니코드는 커버됩니다.
  ;; (set-fontset-font "fontset-default" '(#x1100 . #x11ff) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '#x20a9 '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#x302e . #x302f) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#x3130 . #x318f) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#x3200 . #x321e) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#x3260 . #x327f) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#xa960 . #xa97f) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#xac00 . #xd7a3) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#xd7b0 . #xd7ff) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '(#xffa1 . #xffdc) '("D2Coding" . "iso10646"))
  ;; (set-fontset-font "fontset-default" '#xffe6 '("D2Coding" . "iso10646"))
  ...
  )
```

각 줄의 의미는 아래와 같습니다.

* `(set-language-environment "Korean")`: 한국어 언어 설정을 불러옵니다. `C-h L Korean <RET>`를 실행하시면 어떤 기능을 하는지 확인할 수 있습니다.
* `(prefer-coding-system 'utf-8)`: 한국어 언어 설정에서 지원하는 기본 인코딩 시스템은 `ISO-2022-KR`, `EUC-KR`과 `CP949`입니다. 대신 `utf-8`로 설정했습니다.
* `(setq default-korean-keyboard "3f")`: 기본은 두벌식인데 저는 세벌식 최종을 사용하므로 이렇게 설정했습니다. 두벌식을 사용할 경우 필요 없는 설정입니다. 사용 가능한 입력기 목록은 `C-h I hangul`에서 확인할 수 있습니다.
* `(global-set-key (kbd "<kana>") 'toggle-input-method)`: 입력기 전환 기능을 한/영 키에 설정합니다.
* `(global-unset-key (kbd "S-SPC"))`: 기본적으로 설정되어 있는 입력기 전환 키는 `Shift+Space`인데 자꾸 원하지 않을때 전환하는 일이 생겨서 키를 해제했습니다.
* `(set-fontset-font "fontset-default" '(#x1100 . #x11ff) '("D2Coding" . "iso10646"))`: `U+1100-U+11ff` 범위 내의 문자에는 `D2Coding` 글꼴을 사용하도록 설정했습니다. 해당 범위는 `iso-10646` 유니코드 표준 상에서 [Hangul Jamo](http://www.unicode.org/charts/PDF/U1100.pdf)에 해당하는 영역입니다.
* `(set-fontset-font "fontset-default" '#x20a9 '("D2Coding" . "iso10646"))`: [Currency Symbols](http://www.unicode.org/charts/PDF/U20A0.pdf)에서 반각 원화 기호의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#x302e . #x302f) '("D2Coding" . "iso10646"))`: [CJK Symbols and Punctuation](http://unicode.org/charts/PDF/U3000.pdf)에서 중세 한국어 성조 표기 기호의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#x3130 . #x318f) '("D2Coding" . "iso10646"))`: [Hangul Compatibility Jamo](http://www.unicode.org/charts/PDF/U3130.pdf)에 해당하는 범위의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#x3200 . #x321e) '("D2Coding" . "iso10646"))`: [Enclosed CJK Letters and Months](http://www.unicode.org/charts/PDF/U3200.pdf)에서 괄호로 감싸인 기호의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#x3260 . #x327f) '("D2Coding" . "iso10646"))`: [Enclosed CJK Letters and Months](http://www.unicode.org/charts/PDF/U3200.pdf)에서 원으로 감싸인 기호 및 KS기호의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#xa960 . #xa97f) '("D2Coding" . "iso10646"))`: [Hangul Jamo Extended-A](http://www.unicode.org/charts/PDF/UA960.pdf)에 해당하는 문자 범위의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#xac00 . #xd7a3) '("D2Coding" . "iso10646"))`: [Hangul Syllables](http://unicode.org/charts/PDF/UAC00.pdf)에 해당하는 문자 범위의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#xd7b0 . #xd7ff) '("D2Coding" . "iso10646"))`: [Hangul Jamo Extended-B](http://www.unicode.org/charts/PDF/UD7B0.pdf)에 해당하는 문자 범위의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '(#xffa1 . #xffdc) '("D2Coding" . "iso10646"))`: [Halfwidth and Fullwidth Forms](http://www.unicode.org/charts/PDF/UFF00.pdf)에서 한글 반각 문자의 글꼴을 지정합니다.
* `(set-fontset-font "fontset-default" '#xffe6 '("D2Coding" . "iso10646"))`: [Halfwidth and Fullwidth Forms](http://www.unicode.org/charts/PDF/UFF00.pdf)에서 전각 원화 기호의 글꼴을 지정합니다.
