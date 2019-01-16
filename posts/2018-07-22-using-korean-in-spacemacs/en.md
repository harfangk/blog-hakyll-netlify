---
title: Using Korean in Spacemacs
---

## Using Korean in Spacemacs

I've started trying out Emacs because I lost patience while trying to set up Haskell development environment in Vim. Some random person on the Internet suggested Spacemacs as an alternative, so I decided to give it a try. So far I'm liking it - it works out of box without much configuration. Unfortunately the documentation on setting up Korean language environment was a bit outdated and/or fragmented for an Emacs newbie like me to easily understand. After some researching I've come up with a working configuration so I'll share it here. I'm using `D2Coding` font, but feel free to use whatever you like.

I'm using Ubuntu 16.04.4 and Emacs 26.1.

<!--more-->

```elisp
;; ~/.spacemacs

(defun dotspacemacs/user-config ()
  (set-language-environment "Korean")
  (prefer-coding-system 'utf-8)
  (setq default-korean-keyboard "3f")
  (global-set-key (kbd "<kana>") 'toggle-input-method)
  (global-unset-key (kbd "S-SPC"))
  (set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))
  ;; EDIT: There's no need to specify unicode ranges as below. The above line should be good enough.
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
  )
```

This is what the setting means line-by-line:

* `(set-language-environment "Korean")`: Load Korean language environment. Use command `C-h L Korean <RET>` to see what it does.
* `(prefer-coding-system 'utf-8)`: Korean language environment has `ISO-2022-KR`, `EUC-KR`, and `CP949` coding systems. I use `utf-8` instead.
* `(setq default-korean-keyboard "3f")`: Default is 2-beolsik, but I use 3-beolsik-final. You won't need this line if you use 2-beolsik. To see the list of available input methods, try `C-h I hangul`.
* `(global-set-key (kbd "<kana>") 'toggle-input-method)`: Assign input toggle function to Korean/English key.
* `(global-unset-key (kbd "S-SPC"))`: By default, `Shift+Space` is the input toggle key. I unbind it because I keep unintentionally switching the input. 
* `(set-fontset-font "fontset-default" '(#x1100 . #x11ff) '("D2Coding" . "iso10646"))`: Use `D2Coding` font for characters between `U+1100-U+11ff`, called [Hangul Jamo](http://www.unicode.org/charts/PDF/U1100.pdf) in `iso-10646` Unicode standard.
* `(set-fontset-font "fontset-default" '#x20a9 '("D2Coding" . "iso10646"))`: Font setting for halfwidth Korean Won symbol in [Currency Symbols](http://www.unicode.org/charts/PDF/U20A0.pdf).
* `(set-fontset-font "fontset-default" '(#x302e . #x302f) '("D2Coding" . "iso10646"))`: Font setting for characters that represent medieval Korean tones in [CJK Symbols and Punctuation](http://unicode.org/charts/PDF/U3000.pdf).
* `(set-fontset-font "fontset-default" '(#x3130 . #x318f) '("D2Coding" . "iso10646"))`: Font setting for [Hangul Compatibility Jamo](http://www.unicode.org/charts/PDF/U3130.pdf).
* `(set-fontset-font "fontset-default" '(#x3200 . #x321e) '("D2Coding" . "iso10646"))`: Font setting for Korean characters enclosed in parenthesis in [Enclosed CJK Letters and Months](http://www.unicode.org/charts/PDF/U3200.pdf).
* `(set-fontset-font "fontset-default" '(#x3260 . #x327f) '("D2Coding" . "iso10646"))`: Font setting for Korean characters enclosed in circle in [Enclosed CJK Letters and Months](http://www.unicode.org/charts/PDF/U3200.pdf).
* `(set-fontset-font "fontset-default" '(#xa960 . #xa97f) '("D2Coding" . "iso10646"))`: Font setting for [Hangul Jamo Extended-A](http://www.unicode.org/charts/PDF/UA960.pdf).
* `(set-fontset-font "fontset-default" '(#xac00 . #xd7af) '("D2Coding" . "iso10646"))`: Font setting for [Hangul_Syllables](http://unicode.org/charts/PDF/UAC00.pdf).
* `(set-fontset-font "fontset-default" '(#xd7b0 . #xd7ff) '("D2Coding" . "iso10646"))`: Font setting for [Hangul_Jamo_Extended-B](http://www.unicode.org/charts/PDF/UD7B0.pdf).
* `(set-fontset-font "fontset-default" '(#xffa1 . #xffdc) '("D2Coding" . "iso10646"))`: Font setting for halfwidth Korean characters in [Halfwidth and Fullwidth Forms](http://www.unicode.org/charts/PDF/UFF00.pdf).
* `(set-fontset-font "fontset-default" '#xffe6 '("D2Coding" . "iso10646"))`: Font setting for fullwidth Korean Won symbol in [Halfwidth and Fullwidth Forms](http://www.unicode.org/charts/PDF/UFF00.pdf).
