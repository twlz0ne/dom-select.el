;;; dom-select.el --- CSS-style selector based on dom.el -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/11/20
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/dom-select.el
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; CSS-style selector based on dom.el.
;;
;; ## Requirement
;;
;; Emacs 25.1+
;;
;; ## Installation
;;
;; ``` elisp
;; (quelpa '(dom-select
;;           :fetcher github
;;           :repo "twlz0ne/dom-select.el"
;;           :files ("dom-select.el")))
;; ```
;;
;; ## Usage
;;
;; ``` elisp
;; (dom-select dom "selector string")
;; ```
;;
;; Supported selectors and combinators:
;;
;; | Syntax                | Example            | Example description                                                              |
;; |:----------------------|:-------------------|:---------------------------------------------------------------------------------|
;; | `.class`              | `.intro`           | Select all elements with `class="intro"`                                         |
;; | `.class1.class2`      | `.name1.name2`     | Select all elements with both `name1` and `name2` set within its class attribute |
;; | `#id`                 | `#firstname`       | Select all elements with `id="firstname"`                                        |
;; | `tag`                 | `p`                | Select all `<p>` elements                                                        |
;; | `[attr=fullstring]`   | `[target=_blank]`  | Select all elements with `target="_blank"`                                       |
;; | `[attr|=prefix]`      | `[lang|=en]`       | Select all elements with a lang attribute value equals "en" or starts with "en-" |
;; | `[attr~=word]`        | `[title~=flower]`  | Select all elements with title attribute containing the word "flower"            |
;; | `[attr^=prefix]`      | `a[href^=https]`   | Select every `<a>` element whose href attribute value begins with "https"        |
;; | `[attr*=substring]`   | `a[href*=example]` | Select every `<a>` element whose href attribute value contains "example"         |
;; | `[attr$=prefix]`      | `a[href$=.pdf]`    | Select every `<a>` element whose href attribute value ends with "https"          |
;; | `ancestor descendant` | `div p`            | Select all `<p>` elements inside `<div>` elements                                |
;; | `parent > child`      | `div > p`          | Select all `<p>` elements where the parent is a `<div>` element                  |
;; | `prev + adjacent`     | `div + p`          | Select all `<p>` elements that are placed immegiately after `<div>` elements     |
;; | `prev ~ siblings`     | `p ~ ul`           | Select every `<ul>` element that are placed by a `<p>` element                   |
;; | `parent < child`      | `div < p`          | Select all `<div>` elements which contains at least one `<p>` element            |

;;; Change Log:

;;  0.1.0  2020/11/20  Initial version.
;;                     Supported selectors:
;;                     - Type Selector
;;                     - Class Selector
;;                     - ID Selector
;;                     - Attribute Selectors: =, |=, ~=, ^=, *=, $=
;;                     Supported combinators: SPC, >, +, ~, <

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'dom)

(defconst dom-select-axis-separators "[\s>+~\<\>]")
(defconst dom-select-tag-regexp "^[A-Za-z0-9\-_]+")
(defconst dom-select-id-regexp "#\\([A-Za-z0-9\-_]+\\)")
(defconst dom-select-class-regexp "\\.\\([A-Za-z0-9\-_]+\\)")
(defconst dom-select-attr-regexp "\\[\\([A-Za-z0-9\-_]+\\)\\([\\^\|~\\*$]?=\\)\\(.+?\\)\\]")


;;; Utils & Polyfills

(defsubst dom-select--s-contains-word-p (word string)
  "Return non-nil if whole WORD contained in STRING."
  (string-match-p (concat "\\_<" word "\\_>") string))

(defsubst dom-select--s-equal-or-startwith-p (word string)
  "Return non-nil if STRING equals WORD.

or starts with WORD followed by a hyphen (-)."
  (string-match-p (concat "^" word "\\(?:\\-\\|$\\)") string))

(defalias 'dom-select--search
  (if (fboundp 'dom-search) 'dom-search
    ;; Copied from the original function `dom-search' added in 27.1
    (lambda (dom predicate)
      (let ((matches
             (cl-loop for child in (dom-children dom)
                      for matches = (and (not (stringp child))
                                         (dom-select--search child predicate))
                      when matches
                      append matches)))
        (if (funcall predicate dom)
            (cons dom matches)
          matches))))
  "Return elements in DOM where PREDICATE is non-nil.
PREDICATE is called with the node as its only parameter.")


;;; Parse selectors

(defsubst dom-select--extract-tag (string)
  "Extract tag from STRING.

Example:

  (dom-select--extract-tag \"div[foo=1][bar=2]\")
  ;; => div"
  (save-match-data
    (when (string-match dom-select-tag-regexp string)
      (intern (match-string 0 string)))))

(defsubst dom-select--extract-id (string)
  "Extract id from STRING.

Example:

  (dom-select--extract-id \"div#foo\")
  ;; => \"foo\""
  (save-match-data
    (when (string-match dom-select-id-regexp string)
      (match-string 1 string))))

(defsubst dom-select--extract-classes (string)
  "Extract classes from STRING.

Example:

  (dom-select--extract-classes \"div.foo.bar\")
  ;; => (\"bar\" \"foo\")"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match dom-select-class-regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defsubst dom-select--extract-attrs (string)
  "Extract attrs from STRING.

Example:

  (dom-select--extract-attrs \"div[foo=1][bar=2]\")
  ;; => ((bar \"=\" \"2\") (foo \"=\" \"1\"))"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match dom-select-attr-regexp string pos)
        (push
         (list (intern (match-string 1 string))
               (match-string 2 string)
               (match-string 3 string))
         matches)
        (setq pos (match-end 0)))
      matches)))

(defsubst dom-select--convert-axis (string)
  "Return axis code by STRING."
  (pcase-exhaustive string
    ("<" 'parent) ;; equivelant to parent:has(>child)
    (">" 'child)
    (" " 'descendant)
    ("+" 'next-adjacent)
    ("~" 'next-siblings)))

(defsubst dom-select--parse-selector (string)
  "Convert selector STRING to list.

Example:

  (dom-select--parse-selector \"div#foo.bar[attr1=aaa][attr2=bbb]\")
  ;; =>
  ;; ((tag div)
  ;;  (id \"foo\")
  ;;  (class \"bar\")
  ;;  (attr1 \"=\" \"aaa\")
  ;;  (attr2 \"=\" \"bbb\"))"
  (let ((tag (dom-select--extract-tag string))
        (id (dom-select--extract-id string))
        (attrs  (dom-select--extract-attrs string))
        (classes (mapcar
                  (lambda (class)
                    (list 'class class))
                  (dom-select--extract-classes string))))
    (remove nil (append `(,(when tag (list 'tag tag))
                          ,(when id (list 'id id))
                          ,@(when classes classes))
                        (reverse attrs)))))

(defsubst dom-select--split-selector (string)
  "Split selector STRING.

Return a string list in the form of (current axis rest), for example:

  (dom-select--split-selector \"foo>bar>qux\")
  ;; => (\"foo\" \">\" \"bar>qux\")"
  (let ((pos 0)
        (reg (concat "\\(?:\s*\\)?\\("
                     dom-select-axis-separators
                     "\\)\\(?:\s*\\)?")))
    (catch 'break
      (save-match-data
        (while (string-match reg string pos)
          (setq pos (match-end 1))
          (let* ((axis (match-string 1 string))
                 (curr (substring string 0 (match-beginning 1)))
                 (rest (substring string pos))
                 (unclosed-attr-p (save-match-data
                                    (string-match-p "^[^\\[]*\\(\\]\\)" rest))))
            (unless unclosed-attr-p
              (throw 'break (list (string-trim curr) axis (string-trim rest)))))))
      (list string))))


;;; Select elements

(defsubst dom-select--node-match-p (node pred)
  "Return t if NODE satisfying PRED."
  (pcase-exhaustive pred
    (`(tag ,tag) (eq tag (car node)))
    (`(id ,id) (string= id (dom-attr node 'id)))
    (`(class ,class) (dom-select--s-contains-word-p class (dom-attr node 'class)))
    (`(,attr ,op ,pattern)
     (funcall (pcase-exhaustive op
                ("="  #'string=)
                ("*=" #'string-match-p)
                ("|=" #'dom-select--s-equal-or-startwith-p)
                ("~=" #'dom-select--s-contains-word-p)
                ("^=" #'string-prefix-p)
                ("$=" #'string-suffix-p))
              pattern
              (dom-attr node attr)))))

(defun dom-select--current (node preds)
  "Return current NODE if the PREDS return non-nil."
  (catch 'break
    (mapc (lambda (pred)
            (when (not (and (consp node)
                            (dom-select--node-match-p node pred)))
              (throw 'break nil)))
          preds)
    node))

(defun dom-select--children (parent preds)
  "Return children from PARENT node.
PREDS is in the form of ((pred match partial) ...), for example:

  `((tag li)
    (class \"foo\"))"
  (remove nil
          (mapcar
           (lambda (node)
             (catch 'break
               (mapc (lambda (pred)
                       (when (not (and (consp node)
                                       (dom-select--node-match-p node pred)))
                         (throw 'break nil)))
                     preds)
               node))
           (when (consp parent)
             (if (consp (car parent))
                 ;; flatten first level
                 (apply #'append (mapcar #'dom-children parent))
               (dom-children parent))))))

(defun dom-select--descendant (parent preds)
  "Return descendant from PARENT node.
PREDS is in the form of ((pred match partial) ...), for example:

  `((tag li)
    (class \"foo\"))"
  (dom-select--search parent
              (lambda (node)
                (catch 'break
                  (mapc (lambda (pred)
                          (unless (dom-select--node-match-p node pred)
                            (throw 'break nil)))
                        preds)
                  node))))

(defun dom-select--next-siblings (nodes preds ancestor-dom)
  "Return next siblings of NODES.
PREDS is in the form of ((pred match partial) ...).
ANCESTOR-DOM is a DOM tree where to find parent node of NODES."
  (apply #'append
         (mapcar
          (lambda (node)
            (let* ((siblings (dom-children (dom-parent ancestor-dom node)))
                   (index (1+ (cl-position-if
                               (lambda (it)
                                 (equal it node))
                               siblings))))
              (seq-filter (lambda (it)
                            (and (consp it)
                                 (dom-select--current it preds)))
                          (seq-drop siblings index))))
          nodes)))

(defun dom-select--next-adjacent (nodes preds ancestor-dom)
  "Return next adjacent of NODES.

PREDS is in the form of ((pred match partial) ...).
ANCESTOR-DOM is a DOM tree where to find parent node of NODES."
  (mapcar (lambda (node)
            (let* ((siblings (dom-children (dom-parent ancestor-dom node)))
                   (index (1+ (cl-position-if
                               (lambda (it)
                                 (equal it node))
                               siblings))))
              (catch 'break
                (mapc (lambda (it)
                        (when (and (consp it) (dom-select--current it preds))
                          (throw 'break it)))
                      (seq-drop siblings index))
                nil)))
          nodes))

(defun dom-select--parent (nodes preds)
  "Return parent nodes of NODES.
PREDS is in the form of ((pred match partial) ...)."
  (seq-filter (lambda (node)
                (dom-select--children node preds))
              nodes))



;;;###autoload
(defun dom-select (dom selectors &optional init-axis ancestor-dom)
  "Return all nodes machted by SELECTORS.

INIT-AXIS specific where to start to search.
ANCESTOR-DOM is a DOM tree for adjacent/siblings combinator.

Example:

  (dom-select dom \"ul.class[attr^=prefix] > li ~ li\")"
  (let ((init-axis (or init-axis 'descendant))
        rest-selectors
        next-axis
        preds
        dom-result)
    (pcase (dom-select--split-selector selectors)
      (`(,curr ,axis ,rest)
       (setq preds (dom-select--parse-selector curr))
       (setq next-axis (dom-select--convert-axis axis))
       (setq rest-selectors rest))
      (_ (setq preds (dom-select--parse-selector selectors))))
    (setq dom-result
          (pcase-exhaustive init-axis
            (`current (dom-select--current dom preds))
            (`child (dom-select--children dom preds))
            (`descendant (dom-select--descendant dom preds))
            (`next-adjacent (dom-select--next-adjacent dom preds ancestor-dom))
            (`next-siblings (dom-select--next-siblings dom preds ancestor-dom))
            (`parent (dom-select--parent dom preds))))
    (if (and rest-selectors dom-result)
        (dom-select dom-result rest-selectors next-axis dom)
      dom-result)))

(provide 'dom-select)

;;; dom-select.el ends here
