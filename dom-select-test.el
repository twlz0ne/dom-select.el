;;; dom-select-test.el --- Test dom-select -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'dom-select)

(when noninteractive
  (transient-mark-mode))

(defun --should-eq (actual expected)
  (should
   (equal
    (with-temp-buffer
      (dom-pp actual t)
      (buffer-string))
    (with-temp-buffer
      (dom-pp expected t)
      (buffer-string)))))



(ert-deftest dom-select-test-s-equal-or-startwith-p ()
  (should (dom-select--s-equal-or-startwith-p "foo" "foo"))
  (should (dom-select--s-equal-or-startwith-p "foo" "foo-bar bar"))
  (should (not (dom-select--s-equal-or-startwith-p "foo" "bar foo-bar")))
  (should (not (dom-select--s-equal-or-startwith-p "foo" "foobar")))
  (should (not (dom-select--s-equal-or-startwith-p "foo" "bar foo"))))



(ert-deftest dom-select-test-split-selector ()
  (should (equal (dom-select--split-selector "foo > bar") '("foo" ">" "bar")))
  (should (equal (dom-select--split-selector "foo ~ bar") '("foo" "~" "bar")))
  (should (equal (dom-select--split-selector "foo + bar") '("foo" "+" "bar")))
  (should (equal (dom-select--split-selector "foo < bar") '("foo" "<" "bar")))
  (should (equal (dom-select--split-selector "foo[attr=aaa bbb] bar") '("foo[attr=aaa bbb]" " " "bar")))
  )

(ert-deftest dom-select-test-extract-attrs ()
  (should (equal (dom-select--extract-attrs "tag[foo=bar]")  '((foo string= "bar"))))
  (should (equal (dom-select--extract-attrs "tag[foo^=bar]") '((foo string-prefix-p "bar"))))
  (should (equal (dom-select--extract-attrs "tag[foo~=bar]") '((foo dom-select--s-contains-word-p "bar"))))
  (should (equal (dom-select--extract-attrs "tag[foo|=bar]") '((foo dom-select--s-equal-or-startwith-p "bar"))))
  (should (equal (dom-select--extract-attrs "tag[foo*=bar]") '((foo string-match-p "bar"))))
  (should (equal (dom-select--extract-attrs "tag[foo$=bar]") '((foo string-suffix-p "bar"))))
  (should (equal (dom-select--extract-attrs "tag[attr1=value1][attr2=value2]") '((attr2 string= "value2") (attr1 string= "value1")))))

(ert-deftest dom-select-test-parse-class ()
  (should (equal (dom-select--parse-selector ".foo")
                 '((class string-match-p "\\_<foo\\_>"))))
  (should (equal (dom-select--parse-selector "div.foo.bar#foobar")
                 '((tag div)
                   (id foobar)
                   (class string-match-p "\\_<bar\\_>")
                   (class string-match-p "\\_<foo\\_>")))))



(ert-deftest dom-select-test-select-hierarchy ()
  (with-temp-buffer
    (insert "\
<html>
  <body>
    <ul id=\"list1\">
      <li class=\"foo\"></li>
      <li class=\"bar\"></li>
      <li class=\"qux\"></li>
    </ul>
    <ul id=\"list2\">
      <li class=\"foo\"></li>
      <li class=\"bar\"></li>
      <li class=\"qux\"></li>
    </ul>
    <ul id=\"list3\">
      <li class=\"quux\"><span></span></li>
    </ul>
  </body>
</html>")
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (--should-eq (dom-select dom "ul#list1~ul")
                   '((ul ((id . "list2"))
                         (li ((class . "foo")))
                         (li ((class . "bar")))
                         (li ((class . "qux"))))))

      (--should-eq (dom-select dom "li.foo~li")
                   '((li ((class . "bar"))) (li ((class . "qux")))
                     (li ((class . "bar"))) (li ((class . "qux")))))

      (--should-eq (dom-select dom "ul#list1>li.foo~li")
                   '((li ((class . "bar")))
                     (li ((class . "qux")))))

      (--should-eq (dom-select dom "li.foo+li")
                   '((li ((class . "bar"))) (li ((class . "bar")))))

      (--should-eq (dom-select dom "li < span")
                   '((li ((class . "quux")) (span nil)))))))

(ert-deftest dom-select-test-select-with-attr ()
  (with-temp-buffer
    (insert "\
<html>
  <body>
    <ul id=\"list1\">
      <li class=\"foo\"></li>
      <li class=\"bar foobar\"></li>
      <li class=\"qux\"></li>
    </ul>
    <ul id=\"list2\">
      <li class=\"foo\"></li>
      <li class=\"bar foobar\"></li>
      <li class=\"qux\"></li>
    </ul>
    <ul id=\"list3\">
      <li class=\"foobar\"></li>
      <li class=\"foo-bar bar\"></li>
      <li class=\"bar foo-bar\"></li>
    </ul>
  </body>
</html>")
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (--should-eq (dom-select dom "li.foo")
                   '((li ((class . "foo")))
                     (li ((class . "foo")))))
      (--should-eq (dom-select dom "li.bar.foobar")
                   '((li ((class . "bar foobar")))
                     (li ((class . "bar foobar")))))
      (--should-eq (dom-select dom "li[class=foo]")
                   '((li ((class . "foo")))
                     (li ((class . "foo")))))
      (--should-eq (dom-select dom "li[class^=foo]")
                   '((li ((class . "foo")))
                     (li ((class . "foo")))))
      (--should-eq (dom-select dom "li[class$=qux]")
                   '((li ((class . "qux")))
                     (li ((class . "qux")))))
      (--should-eq (dom-select dom "li[class~=foo]")
                   '((li ((class . "foo")))
                     (li ((class . "foo")))))
      (--should-eq (dom-select dom "li[class*=foo]")
                   '((li ((class . "foo"))) (li ((class . "bar foo")))
                     (li ((class . "foo"))) (li ((class . "bar foo")))))
      (--should-eq (dom-select dom "li[class|=foo]")
                   '((li ((class . "foo")))
                     (li ((class . "foo")))
                     (li ((class . "foo-bar bar"))))))))

(provide 'dom-select-test)

;;; dom-select-test.el ends here
