[![Build Status](https://travis-ci.com/twlz0ne/dom-select.el.svg?branch=main)](https://travis-ci.com/twlz0ne/dom-select.el)

# dom-select.el

CSS-style selector based on dom.el.

## Requirement

Emacs 25.1+

## Installation

``` elisp
(quelpa '(dom-select
          :fetcher github
          :repo "twlz0ne/dom-select.el"
          :files ("dom-select.el")))
```

## Usage

``` elisp
(dom-select dom "selector string")
```

Supported selectors and combinators:

| Syntax                | Example            | Example description                                                              |
|:----------------------|:-------------------|:---------------------------------------------------------------------------------|
| `.class`              | `.intro`           | Select all elements with `class="intro"`                                         |
| `.class1.class2`      | `.name1.name2`     | Select all elements with both `name1` and `name2` set within its class attribute |
| `#id`                 | `#firstname`       | Select all elements with `id="firstname"`                                        |
| `tag`                 | `p`                | Select all `<p>` elements                                                        |
| `[attr]`              | `[target]`         | Selects all elements with a `target` attribute                                   |
| `[attr=fullstring]`   | `[target=_blank]`  | Select all elements with `target="_blank"`                                       |
| `[attr\|=prefix]`     | `[lang\|=en]`      | Select all elements with a lang attribute value equals "en" or starts with "en-" |
| `[attr~=word]`        | `[title~=flower]`  | Select all elements with title attribute containing the word "flower"            |
| `[attr^=prefix]`      | `a[href^=https]`   | Select every `<a>` element whose href attribute value begins with "https"        |
| `[attr*=substring]`   | `a[href*=example]` | Select every `<a>` element whose href attribute value contains "example"         |
| `[attr$=prefix]`      | `a[href$=.pdf]`    | Select every `<a>` element whose href attribute value ends with "https"          |
| `ancestor descendant` | `div p`            | Select all `<p>` elements inside `<div>` elements                                |
| `parent > child`      | `div > p`          | Select all `<p>` elements where the parent is a `<div>` element                  |
| `prev + adjacent`     | `div + p`          | Select all `<p>` elements that are placed immegiately after `<div>` elements     |
| `prev ~ siblings`     | `p ~ ul`           | Select every `<ul>` element that are placed by a `<p>` element                   |
| `parent < child`      | `div < p`          | Select all `<div>` elements which contains at least one `<p>` element            |

## Alternative projects

- [elquery](https://github.com/AdamNiederer/elquery/) The HTML library for elisp
- [doom.el](http://www.github.com/toroidal-code/doom.el/) DOM implementation and manipulation library
