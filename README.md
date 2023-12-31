﻿# NCss! (Nim ⋈ CSS)
An experiment to write CSS in Nim.


## Challenges
In syntactic side I faced several challenges:

1. Nim considers `#` as comment
2. you cannot write (`.`) at the first of experssion
3. you cannot write (`:`) at the middle of a selector
4. Infix priority issues in selectors
5. percent `%` operator is only valid as perfix, not postfix
6. Cannot group idents without pars
7. the DotExpr is not parsed as intended in CSS

```nim
#id # (1)
.class # (2)
button:hover # (3)
article>span+img # (4)
12% # (5)
span, main: ... # (6)
1.px 2.px # (7) is parsed as px(1, px(2))
```

## Solutions

### 1..4
For challenges 1..4 I came up with using almost-identical [Unicode](https://www.amp-what.com/) of those characters. hopefully I found what I was looking for.

- `—` -> `-`
- `﹟` -> `#`
- `․` -> `.`
- `꞉` -> `:`
- `＜` -> `<`
- `＋` -> `+`

### 5
just use prefix version:
```nim
%12
```

### 6
for (6) I had to group them inside something, so I put them inside a par.

```nim
(span, main):
  ...
```

### 7
for (7) I had to use [Custom Numeric Literals](https://nim-lang.org/docs/manual.html#numeric-literals-custom-numeric-literals).


```nim
margin: 1'px 0 0 4'px
```

## Purpose
I develop front-end applications using [Karax](https://github.com/karaxnim/karax), through the process I've faced several problems:

1. defining vars, functions, ... in [Sass](https://sass-lang.com/)  is wierd
2. I cannot use constants I declared in Sass in my Nim code

So as an experiment, I tried to write my CSS in Nim. there was no libraries, so I had to come up with something new.

## Foot note
~~It seems crazy but I don't see much future for this work...~~

I think having an extension for your IDE to convert css syntax to corresponding valid Nim syntax is very good idea.

## Test it for yourself
```bash
nimble install # write it in project folder to install dependencies
nim r src/ncss # run the minimal working code and get CSS repr
```
