;;; xquery-mode.el --- A simple mode for editing xquery programs
;; Time-stamp: <2005-03-26 18:05:39 sacharya>

;;; Copyright (C) 2005 Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>

;; This file is not part of GNU Emacs.

;; xquery-mode.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.:

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; 

(require 'nxml-mode)

;;; Code:
(define-generic-mode 'xquery-mode
  '(("(:" . ":)") ("<!--" . "-->"))
  '("xquery" "version" "encoding" "at" "module" "namespace" "child" "descendant" "parent" "attribute" "self" "descendant-or-self" "ancestor" "following-sibling" "preceding-sibling" "following" "preceding" "ancestor-or-self" "declare" "function" "option" "ordering" "ordered" "unordered" "default" "order" "external" "or" "and" "div" "idiv" "mod" "in"  "construction" "satisfies" "return" "then" "else" "boundary-space" "base-uri" "preserve" "strip" "copy-namespaces" "no-preserve" "inherit" "no-inherit" "to" "where" "collation" "intersect" "union" "except" "as" "case" "instance" "of" "castable" "item" "element" "schema-element" "schema-attribute" "processing-instruction" "comment" "text" "empty" "import" "schema" "is" "eq" "ne" "gt" "ge" "lt" "le" "some" "every" "for" "let" "cast" "treat" "validate" "document-node" "document" "node" "if" "typeswitch" "by" "stable" "ascending" "descending" "greatest" "least" "variable") ;keywords
  '(("\\(\\$\\w+\\)" 1 font-lock-variable-name-face) ;; \\(\\s_\\|\\w\\)
    ("\\(\\w*:?\\w+\\)\\s *(" 1 font-lock-function-name-face)
    ("\\(<\\)\\(/?\\)\\(\\w*\\)\\(:?\\)\\(\\w+\\).*?\\(/?\\)\\(>\\)" 
     (1 'nxml-tag-delimiter-face) 
     (2 'nxml-tag-slash-face)
     (3 'nxml-element-prefix-face) 
     (4 'nxml-element-colon-face)
     (5 'nxml-element-local-name-face)
     (6 'nxml-tag-slash-face)
     (7 'nxml-tag-delimiter-face) 
     )
    ("\\(\\w*\\)\\(:?\\)\\(\\w+\\)=\\([\"']\\)\\(.*?\\)\\([\"']\\)" 
     (1 'nxml-attribute-prefix-face) 
     (2 'nxml-attribute-colon-face)
     (3 'nxml-attribute-local-name-face) 
     (4 'nxml-attribute-value-delimiter-face)
     (5 'nxml-attribute-value-face)
     (6 'nxml-attribute-value-delimiter-face))
    ("\\(/\\)\\(\\w*\\)\\(:?\\)\\(\\w+\\)" 
     (1 font-lock-constant-face)
     (2 font-lock-constant-face) 
     (3 font-lock-constant-face)
     (4 font-lock-constant-face)
     )
    ("as\\s +\\(\\w*:?\\w+\\)" 
     (1 font-lock-type-face)
     )
    ) ;font-lock-list
  '(".xq\\'") ;auto-mode-list
  '(xquery-set-indent-function xquery-set-up-syntax-table)         ;function list
  "A Major mode for editing xquery."
  )



(defun xquery-set-indent-function ()
  "Set the indent function for xquery mode."
  (setq nxml-prolog-end (point-min))
  (setq nxml-scan-end (copy-marker (point-min) nil))
  (set (make-local-variable 'indent-line-function) 'xquery-indent-line)
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'xquery-forward-sexp)
  (local-set-key "/" 'nxml-electric-slash)
  )

(defun xquery-forward-sexp (&optional arg)
  "Xquery forward s-expresssion.
This function is not very smart, it tries to use
`nxml-forward-balanced-item' if it sees '>' or '<' characters in
the direction you are going, and uses the regular `forward-sexp'
otherwise. "
  (if (> arg 0)
      (progn                                 
        (if (looking-at "[ \t]*<")
            (nxml-forward-balanced-item arg)
          (let ((forward-sexp-function nil)) (forward-sexp arg))))
    (if (looking-back ">[ \t]*")
  ")")))

