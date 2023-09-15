;;; vue-ts-mode.el --- Vue major mode based on tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jacob
;;
;; Author: Jacob Schmocker <>
;; Maintainer: Jacob Schmocker <>
;; Created: September 01, 2023
;; Version: 0.0.1
;; Keywords: languages
;; Homepage: https://github.com/theschmocker/vue-ts-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(eval-and-compile
  (require 'cl-lib))

(require 'treesit)
(require 'typescript-ts-mode)
(require 'js)
(require 'css-mode)

(defvar editorconfig-indentation-alist)

(when (and (featurep 'editorconfig)
           (not (assq 'vue-ts-mode editorconfig-indentation-alist)))
  (add-to-list 'editorconfig-indentation-alist (list 'vue-ts-mode 'vue-ts-mode-indent-offset)))

(defgroup vue-ts nil
  "Group for `vue-ts-mode'."
  :group 'languages)

(defcustom vue-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `vue-ts-mode'.

Will be overridden by `tab-width' When `indent-tabs-mode' is non-nil."
  :group 'vue-ts
  :type '(integer))

(defcustom vue-ts-mode-auto-close-tags t
  "Automatically insert a closing tag name after typing \"</\"."
  :group 'vue-ts
  :type '(boolean))

(defun vue-ts-mode--comment-end-matcher (node parent bol)
  "Match the closing \"-->\" on its own line in a comment node.

See `treesit-simple-indent-rules' for info about the NODE, PARENT, and BOL
arguments to the matcher function."
  (print (list node parent bol))
  (and (vue-ts-mode--treesit-node-type-p "comment" parent)
       (save-excursion
         (goto-char bol)
         (looking-at "-->" t))))

(defvar vue-ts-mode-indent-rules
  `((vue
     (vue-ts-mode--comment-end-matcher parent-bol 0)
     ((parent-is "component") column-0 0)
     ((parent-is "script_element") column-0 0)
     ((parent-is "style_element") column-0 0)
     ((node-is "element") parent-bol vue-ts-mode-indent-offset)
     ((node-is "start_tag") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "/>") parent-bol 0)
     ((parent-is "start_tag") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "element") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "text") grand-parent vue-ts-mode-indent-offset) ;; text includes newline after previous element sibling
     ((node-is "attribute") parent-bol vue-ts-mode-indent-offset)
     ((node-is "directive_attribute") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "quoted_attribute_value") parent-bol 0)
     ((parent-is "attribute_value") parent-bol 0)
     ((node-is "interpolation") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "comment") parent-bol vue-ts-mode-indent-offset)))
  "Base tree-sitter indent rules for Vue templates.")

(defun vue-ts-mode--replace-indent-rules-offset (indent-rules &optional from-symbol to-symbol)
  "Replace FROM-SYMBOL with TO-SYMBOL in INDENT-RULES list.

Intended for use in overriding mode-specific indent offset variables in indent
rules for nested languages."
  (cl-labels ((replace-in-tree (node)
                (cl-typecase node
                  (list (mapcar #'replace-in-tree node))
                  (symbol (if (eq node from-symbol)
                              to-symbol
                            node))
                  (t node))))
    (replace-in-tree indent-rules)))

(defvar vue-ts-mode-javascript-indent-rules
  (vue-ts-mode--replace-indent-rules-offset
   js--treesit-indent-rules
   'js-indent-level
   'vue-ts-mode-indent-offset)
  "Tree-sitter indent rules for embedded JavaScript.")

(defvar vue-ts-mode-typescript-indent-rules
  (vue-ts-mode--replace-indent-rules-offset
   (typescript-ts-mode--indent-rules 'typescript)
   'typescript-ts-mode-indent-offset
   'vue-ts-mode-indent-offset)
  "Tree-sitter indent rules for embedded TypeScript.")

(defvar vue-ts-mode-css-indent-rules
  (vue-ts-mode--replace-indent-rules-offset
   css--treesit-indent-rules
   'css-indent-offset
   'vue-ts-mode-indent-offset)
  "Tree-sitter indent rules for embedded CSS.")

(defface vue-ts-mode-html-tag-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for html tags.")

(defface vue-ts-mode-builtin-tag-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for Vue builtin tags, like template, component, transition, etc.")

(defface vue-ts-mode-sfc-tag-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for Vue builtin tags, like template, script, and style.")

(defface vue-ts-mode-attribute-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for html attributes.")

(defface vue-ts-mode-attribute-value-face
  '((t . (:inherit font-lock-string-face)))
  "Face for html attribute values.")

(defface vue-ts-mode-builtin-directive-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for built-in directive names.")

(defface vue-ts-mode-dynamic-directive-argument-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for dynamic directive arguments, e.g. :[arg].")

(defface vue-ts-mode-shorthand-prefix-face
  '((t . (:inherit font-lock-bracket-face)))
  "Face for characters in shorthand directives.
Ex: the # in #slot-name or @ in @click")

(defvar vue-ts-mode-builtin-directives
  '("v-text" "v-html" "v-show" "v-if" "v-else" "v-else-if" "v-for" "v-on"
    "v-bind" "v-model" "v-slot" "v-pre" "v-once" "v-memo" "v-cloak")
  "Built-in directive attribute names.")

(defvar vue-ts-mode-sfc-tags
  '("template" "script" "style")
  "Top-level SFC tag names.")

(defvar vue-ts-mode-builtin-tags
  '("template" "component" "transition")
  "Tag names of built-in Vue elements/components.")

(defun vue-ts-mode--prefix-sub-language-feature (language font-lock-settings)
  "Add \"vue-\" + LANGUAGE prefix to feature names in FONT-LOCK-SETTINGS.

FONT-LOCK-SETTINGS is a list of rules suitable for `treesit-font-lock-settings'.
Helps with re-use of font lock rules for nested languages from their respective
major modes."
  (let ((prefix (concat "vue-" (symbol-name language) "-")))
    (cl-loop for rule in font-lock-settings
             for rule-copy = (cl-copy-list rule)
             do (setf (nth 2 rule-copy) (intern (concat prefix (symbol-name (nth 2 rule)))))
             collect rule-copy)))

(defvar vue-ts-mode-font-lock-settings
  (list
   :language 'vue
   :feature 'vue-template
   `(((tag_name) @vue-ts-mode-html-tag-face)

     (quoted_attribute_value "\"" @vue-ts-mode-attribute-value-face)

     ((attribute
       (attribute_name) @vue-ts-mode-attribute-face
       ("=") :? @vue-ts-mode-attribute-face
       (quoted_attribute_value
        (attribute_value) @vue-ts-mode-attribute-value-face) :?))
     (directive_name) @vue-ts-mode-attribute-face
     (directive_argument) @vue-ts-mode-attribute-face
     (directive_dynamic_argument
      "[" @font-lock-bracket-face
      (directive_dynamic_argument_value) @vue-ts-mode-dynamic-directive-argument-face
      "]" @font-lock-bracket-face)
     (directive_attribute
      "=" @font-lock-keyword-face)

     ["<" ">" "</" "/>"] @font-lock-bracket-face

     (interpolation
      ["{{" "}}"] @font-lock-bracket-face )

     (comment) @font-lock-comment-face)

   :language 'vue
   :feature 'vue-template
   :override t
   `(((tag_name) @vue-ts-mode-builtin-tag-face
      (:match ,(regexp-opt vue-ts-mode-builtin-tags) @vue-ts-mode-builtin-tag-face))

     ((directive_name) @vue-ts-mode-shorthand-prefix-face
      (:match "^\\(#\\|@\\|:\\)" @vue-ts-mode-shorthand-prefix-face)))

   :language 'vue
   :feature 'vue-template
   :override t
   `(((component
       :anchor
       (_
        (_
         (tag_name) @vue-ts-mode-sfc-tag-face
         (:match ,(regexp-opt vue-ts-mode-sfc-tags) @vue-ts-mode-sfc-tag-face))) :*))

     ((directive_attribute
       (directive_name) @vue-ts-mode-builtin-directive-face
       (:match ,(regexp-opt vue-ts-mode-builtin-directives)
               @vue-ts-mode-builtin-directive-face)))))
  "Base `treesit-font-lock-settings' for Vue templates in `vue-ts-mode'.")

(defvar vue-ts-mode-javascript-font-lock-settings
  (vue-ts-mode--prefix-sub-language-feature
   'javascript
   js--treesit-font-lock-settings)
  "JavaScript `treesit-font-lock-settings' for `vue-ts-mode'.

Adapted from `js-ts-mode'.")

(defvar vue-ts-mode-typescript-font-lock-settings
  (vue-ts-mode--prefix-sub-language-feature
   'typescript
   (typescript-ts-mode--font-lock-settings 'typescript))
  "TypeScript `treesit-font-lock-settings' for `vue-ts-mode'.

Adapted from `typescript-ts-mode'")

(defvar vue-ts-mode-css-font-lock-settings
  (vue-ts-mode--prefix-sub-language-feature
   'css
   css--treesit-settings)
  "CSS `treesit-font-lock-settings' for `vue-ts-mode'.

Adapted from `css-ts-mode'")

(defun vue-ts-mode--sfc-element-imenu-index (root simple-imenu-settings defun-name-function)
  "Create an imenu index for nested language tags.

This is a modified version of `treesit-simple-imenu'.

ROOT is the root node for the nested language.
SIMPLE-IMENU-SETTINGS is a list suitable for `treesit-simple-imenu'.
DEFUN-NAME-FUNCTION is a function suitable for `treesit-defun-name-function'
that works for the specific nested language."
  (mapcan (lambda (setting)
            (pcase-let ((`(,category ,regexp ,pred ,name-fn)
                         setting))
              (when-let* ((tree (treesit-induce-sparse-tree
                                 root regexp))
                          (index (let ((treesit-defun-name-function defun-name-function))
                                   (treesit--simple-imenu-1
                                    tree pred name-fn))))
                (if category
                    (list (cons category index))
                  index))))
          simple-imenu-settings))

(defun vue-ts-mode--element-top-level-p (element-node)
  "Return t if ELEMENT-NODE is a child of the top-level component node."
  (vue-ts-mode--treesit-node-type-p "component" (treesit-node-parent element-node)))

(defun vue-ts-mode--get-sfc-element-start-tags (root)
  "Return a list of the component's top level element treesit-nodes.

ROOT is the root component node."
  (let ((sfc-elements (treesit-query-capture
                       root
                       `((start_tag
                          (tag_name) @tag-name
                          (:match ,(regexp-opt vue-ts-mode-sfc-tags) @tag-name)) @start-tag)
                       nil
                       nil)))
    (cl-loop for (cap . node) in sfc-elements
             if (and (eq cap 'start-tag)
                     (vue-ts-mode--element-top-level-p (treesit-node-parent node)))
             collect node)))

(defun vue-ts-mode--imenu-tag-entries ()
  "Return a list of imenu entries for HTML tags."
  (cl-loop for el in (vue-ts-mode--get-all-elements)
           if (not (vue-ts-mode--element-top-level-p el))
           collect (let ((tag (vue-ts-mode--treesit-find-child el (regexp-opt '("start_tag" "self_closing_tag")))))
                     (cons (treesit-node-text tag)
                           (treesit-node-start el)))))

(defun vue-ts-mode-imenu-index ()
  "Create an imenu index for `vue-ts-mode'."
  (let* ((root (treesit-buffer-root-node 'vue))
         (sfc-elements (vue-ts-mode--get-sfc-element-start-tags root))
         (typescript-items (vue-ts-mode--sfc-element-imenu-index
                            (treesit-buffer-root-node 'typescript)
                            `(("Function" "\\`function_declaration\\'" nil nil)
                              ("Variable" "\\`lexical_declaration\\'"
                               js--treesit-valid-imenu-entry nil)
                              ("Class" ,(rx bos (or "class_declaration"
                                                    "method_definition")
                                            eos)
                               nil nil))
                            #'js--treesit-defun-name)))
    (cl-remove-if-not
     #'cdr
     `(("SFC" . ,(mapcar (lambda (node)
                           (cons (treesit-node-text node)
                                 (treesit-node-start node)))
                         sfc-elements))
       ("TypeScript" . ,typescript-items)
       ("Tags" . ,(vue-ts-mode--imenu-tag-entries))))))

(defun vue-ts-mode--script-no-lang-p (raw-text-node)
  "Return t if RAW-TEXT-NODE's parent tag has no lang attribute."
  (null
   (treesit-query-capture
    (treesit-node-parent raw-text-node)
    '((attribute
       (attribute_name) @attr
       (:equal "lang" @attr))))))

(defun vue-ts-mode--point-in-range-p (point range)
  "Return t if POINT is within RANGE.

RANGE should be a cons cell of numbers: (start . end)."
  (when range
    (<= (car range) point (cdr range))))

(defun vue-ts-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((parser-range-alist
          (mapcar (lambda (p)
                    (cons (treesit-parser-language p) (treesit-parser-included-ranges p))) (treesit-parser-list)))
         (match (cl-find-if (lambda (pair)
                              (let ((ranges (cdr pair)))
                                (cl-some (lambda (range)
                                           (and range
                                                (vue-ts-mode--point-in-range-p point range)))
                                         ranges)))
                            parser-range-alist)))
    (if match
        (car match)
      'vue)))

(defvar vue-ts-mode--javascript-range-rules
  '(:embed javascript
    :host vue
    (((script_element (raw_text) @capture) @_script
      (:pred vue-ts-mode--js-script-element-p @_script)))))

(defvar vue-ts-mode--typescript-range-rules
  '(:embed typescript
    :host vue
    (((script_element (raw_text) @capture) @_script
      (:pred vue-ts-mode--ts-script-element-p @_script)))))

(defvar vue-ts-mode--css-range-rules
  '(:embed css
    :host vue
    (((style_element (raw_text) @css) @_style-element
      ;; TODO: uncomment this when adding other CSS lang support
      ;; (:pred vue-ts-mode--css-style-element-p @_style-element)
      ))))

(defvar vue-ts-mode-font-lock-feature-list
  '((vue-template
     vue-typescript-comment
     vue-typescript-declaration
     vue-javascript-comment
     vue-javascript-definition
                  vue-css-selector
     vue-css-comment
     vue-css-query
     vue-css-keyword)

    (vue-typescript-keyword
     vue-typescript-string
     vue-typescript-escape-sequence
     vue-javascript-keyword
     vue-javascript-string
     vue-css-property
     vue-css-constant
     vue-css-string)

    (vue-typescript-constant
     vue-typescript-expression
     vue-typescript-identifier
     vue-typescript-number
     vue-typescript-pattern
     vue-typescript-property
     vue-javascript-assignment
     vue-javascript-constant
     vue-javascript-escape-sequence
     ;; vue-javascript-jsx
     vue-javascript-number
     vue-javascript-pattern
     vue-javascript-string-interpolation
     vue-css-error
     vue-css-variable
     vue-css-function
     vue-css-operator
     vue-css-bracked)

    (vue-typescript-function
     vue-typescript-bracket
     vue-typescript-delimiter
     vue-javascript-bracket
     vue-javascript-delimiter
     vue-javascript-function
     vue-javascript-operator
     vue-javascript-property))
  "Font lock features for Vue templates and embedded languages.")

(define-derived-mode vue-ts-mode prog-mode "Vue"
  "Tree-sitter mode for Vue."
  (when (treesit-ready-p 'vue)
    (treesit-parser-create 'vue))

  (setq-local electric-indent-chars
              (append "{}():;,<>/=" electric-indent-chars))

  (setq-local treesit-font-lock-feature-list vue-ts-mode-font-lock-feature-list)
  (setq-local treesit-font-lock-settings
              (append
               (apply #'treesit-font-lock-rules vue-ts-mode-font-lock-settings)
               (when (treesit-ready-p 'javascript)
                 vue-ts-mode-javascript-font-lock-settings)
               (when (treesit-ready-p 'typescript)
                 vue-ts-mode-typescript-font-lock-settings)
               (when (treesit-ready-p 'css)
                 vue-ts-mode-css-font-lock-settings)))

  (setq-local treesit-simple-indent-rules
              (append
               vue-ts-mode-indent-rules
               (when (treesit-ready-p 'javascript)
                 vue-ts-mode-javascript-indent-rules)
               (when (treesit-ready-p 'typescript)
                 vue-ts-mode-typescript-indent-rules)
               (when (treesit-ready-p 'css)
                 vue-ts-mode-css-indent-rules)))

  (setq treesit-range-settings
        (apply #'treesit-range-rules
               (append
                (when (treesit-ready-p 'javascript)
                  vue-ts-mode--javascript-range-rules)
                (when (treesit-ready-p 'typescript)
                  vue-ts-mode--typescript-range-rules)
                (when (treesit-ready-p 'css)
                  vue-ts-mode--css-range-rules))))

  ;; TODO: for experimental multi-parsers for interpolations and directive bindings
  ;; #'vue-ts-mode--setup-interpolation-parsers
  ;; :embed 'javascript
  ;; :host 'vue
  ;; '((interpolation
  ;;    (raw_text) @capture))

  (setq-local imenu-create-index-function #'vue-ts-mode-imenu-index)

  (setq-local treesit-language-at-point-function #'vue-ts-mode--treesit-language-at-point)

  (when indent-tabs-mode
    (setq-local vue-ts-mode-indent-offset tab-width))

  (modify-syntax-entry ?>  "." vue-ts-mode-syntax-table)

  (add-hook 'post-command-hook #'vue-ts-mode--auto-close-tag-post-command-h nil t)
  (add-hook 'post-command-hook #'vue-ts-mode--language-at-point-post-command-h nil t)

  (add-hook 'vue-ts-mode-language-at-point-functions #'vue-ts-mode--language-at-point-comment-vars-function)

  (treesit-major-mode-setup)

  ;; HACK: language at point is always detected as vue the first time otherwise.
  ;; TODO: seeing the same issue at bol after reindent as well
  (run-with-timer 0.0 nil #'vue-ts-mode--language-at-point-post-command-h))

(defun vue-ts-mode--tag-attr-p (tag-node attr &optional value)
  "Return t if TAG-NODE has attribute ATTR.

If VALUE is non-nil, also check that the attribute has that value."
  (let* ((attrs (treesit-filter-child tag-node (vue-ts-mode--treesit-node-type-p "attribute"))))
    (and attrs
         (cl-some (lambda (attr-node)
                    (and (equal attr (treesit-node-text (vue-ts-mode--treesit-find-child attr-node "attribute_name")))
                         (or (not value)
                             (equal value (treesit-node-text (vue-ts-mode--treesit-find-child (vue-ts-mode--treesit-find-child attr-node "quoted_attribute_value")
                                                                                              "attribute_value"))))))
                  attrs))))

(defun vue-ts-mode--js-script-element-p (script-element)
  "Return t if SCRIPT-ELEMENT is a JavaScript language block."
  (let ((start-tag (vue-ts-mode--treesit-find-child script-element "start_tag")))
    (or (not (vue-ts-mode--tag-attr-p start-tag "lang"))
        (vue-ts-mode--tag-attr-p start-tag "lang" "js"))))

(defun vue-ts-mode--ts-script-element-p (script-element)
  "Return t if SCRIPT-ELEMENT is a TypeScript language block."
  (let ((start-tag (vue-ts-mode--treesit-find-child script-element "start_tag")))
    (vue-ts-mode--tag-attr-p start-tag "lang" "ts")))

(defun vue-ts-mode--css-style-element-p (style-element)
  "Return t if STYLE-ELEMENT is a CSS language block."
  (let ((start-tag (vue-ts-mode--treesit-find-child style-element "start_tag")))
    (or (not (vue-ts-mode--tag-attr-p start-tag "lang"))
        (vue-ts-mode--tag-attr-p start-tag "lang" "css"))))

(defun vue-ts-mode--scss-style-element-p (style-element)
  "Return t if STYLE-ELEMENT is an SCSS language block."
  (let ((start-tag (vue-ts-mode--treesit-find-child style-element "start_tag")))
    (vue-ts-mode--tag-attr-p start-tag "lang" "scss")))

(defvar-local vue-ts-mode--last-lang-at-point nil
  "Last detected language at point.

Supports `vue-ts-mode--language-at-point-post-command-h'.")

(defvar vue-ts-mode-language-at-point-functions nil
  "Functions run when the language at point changes.

The function should take a single argument, which is a language name symbol.")

(defun vue-ts-mode--language-at-point-post-command-h ()
  "Call `vue-ts-mode-language-at-point-functions' when lang at point changes."
  (let ((lang (vue-ts-mode--treesit-language-at-point (point))))
    (when (not (eq lang vue-ts-mode--last-lang-at-point))
      (setq vue-ts-mode--last-lang-at-point lang)
      (run-hook-with-args 'vue-ts-mode-language-at-point-functions lang))))

(defun vue-ts-mode--language-at-point-comment-vars-function (lang)
  "Set various comment vars based on LANG.

LANG is a symbol returned by `vue-ts-mode--treesit-language-at-point'."
  (print lang)
  (cl-case lang
    (vue
     (setq-local comment-start "<!-- ")
     (setq-local comment-end " -->")
     (setq-local comment-start-skip "<!--[ \t]*")
     (setq-local comment-end-skip "[ \t]*--[ \t\n]*>"))
    ((javascript typescript scss)
     (setq-local comment-start "// ")
     (setq-local comment-end "")
     (setq-local comment-start-skip (rx (or (seq "/" (+ "/"))
                                            (seq "/" (+ "*")))
                                        (* (syntax whitespace))))
     (setq-local comment-end-skip
                 (rx (* (syntax whitespace))
                     (group (or (syntax comment-end)
                                (seq (+ "*") "/"))))))
    (css
     (setq-local comment-start "/*")
     (setq-local comment-start-skip "/\\*+[ \t]*")
     (setq-local comment-end "*/")
     (setq-local comment-end-skip "[ \t]*\\*+/"))))

(defvar-local vue-ts-mode--interpolation-parsers nil)

;; This is a start. Part of the problem is that when `treesit-font-lock-fontify-region'
;; fontifies for a rule, it only grabs the root node of the rule's language.
;; I need it to do fontification for ALL javascript parsers. I might be able to
;; use add-function to advise it buffer-locally to get all js nodes

;; (defun vue-ts-mode--setup-interpolation-parsers (_beg _end)
;;   (mapc #'treesit-parser-delete vue-ts-mode--interpolation-parsers)
;;   (setq vue-ts-mode--interpolation-parsers nil)
;;   (let ((ranges (treesit-query-range
;;                  (treesit-buffer-root-node 'vue)
;;                  '((interpolation
;;                     (raw_text) @capture)
;;                    ((directive_attribute
;;                      (quoted_attribute_value
;;                       (attribute_value) @attr_js)))))))
;;     (dolist (range ranges)
;;       (let ((parser (treesit-parser-create 'javascript nil t)))
;;         (treesit-parser-set-included-ranges parser (list range))
;;         (push parser vue-ts-mode--interpolation-parsers)))))

;; https://github.com/Sorixelle/astro-ts-mode/blob/207e5da093aa8141b9dd2f5e98afd8952832b4b0/astro-ts-mode.el#L218
(defun vue-ts-mode--advice-for-treesit-buffer-root-node (&optional lang)
  "Return the current ranges for the LANG parser in the current buffer.

If LANG is omitted, return ranges for the first language in the parser list.

If `major-mode' is currently `vue-ts-mode', or if LANG is vue, this function
instead always returns t."
  (if (or (eq lang 'vue) (not (eq major-mode 'vue-ts-mode)))
      t
    (treesit-parser-included-ranges
     (treesit-parser-create
      (or lang (treesit-parser-language (car (treesit-parser-list))))))))

(advice-add
 #'treesit-buffer-root-node
 :before-while
 #'vue-ts-mode--advice-for-treesit-buffer-root-node)

(defun vue-ts-mode--advice-for-treesit--merge-ranges (_ new-ranges _ _)
  "Return truthy if `major-mode' is `vue-ts-mode', and if NEW-RANGES is non-nil."
  (and (derived-mode-p 'vue-ts-mode) new-ranges))

;; (advice-add
;;  #'treesit--merge-ranges
;;  :before-while
;;  #'vue-ts-mode--advice-for-treesit--merge-ranges)

(cl-defun vue-ts-mode--treesit-node-type-p (node-type &optional (node nil node-provided))
  "Return t if NODE's type is matches the regexp NODE-TYPE.

This is a curried function. If NODE is not provided, return a function which
takes a treesit-node as an argument and perform the same NODE-TYPE comparison"
  (if node-provided
      (string-match-p node-type (treesit-node-type node))
    (lambda (node)
      (string-match-p node-type (treesit-node-type node)))))

(defun vue-ts-mode--treesit-find-child (node pred)
  "Return the first child of NODE which satisfies PRED.

PRED may be a regexp used to match a child node's type or a function which
receives each child node as its argument."
  (car (treesit-filter-child node (if (stringp pred)
                                      (vue-ts-mode--treesit-node-type-p pred)
                                    pred))))

(defun vue-ts-mode--treesit-sibling-until (node pred &optional backward)
  "Return the first sibling of NODE which satisfies PRED.

PRED may be a regexp used to match a sibling node's type or a function which
receives each sibling node as an argument.

If BACKWARD is non-nil, search siblings before NODE."
  (when (stringp pred)
    (setq pred (vue-ts-mode--treesit-node-type-p pred)))
  (let (matched-sibling
        (sibling-by (if backward
                        #'treesit-node-prev-sibling
                      #'treesit-node-next-sibling)))
    (while-let ((sibling (and node (funcall sibling-by node t))))
      (if (funcall pred sibling)
          (progn
            (setq matched-sibling sibling)
            (setq node nil))
        (setq node sibling)))
    matched-sibling))

(defun vue-ts-mode--treesit-previous-sibling-start-tag (node)
  "Return the nearest sibling of NODE which has type \"start_tag\"."
  (vue-ts-mode--treesit-sibling-until node "start_tag" t))

(defun vue-ts-mode--close-tag ()
  "Close the nearest start tag when \"</\" is before point."
  (when (looking-back "</" (- (point) 2))
    (when-let* ((current-node (treesit-node-at (point)))
                (start-tag (vue-ts-mode--treesit-previous-sibling-start-tag current-node))
                (start-tag-name (vue-ts-mode--treesit-find-child start-tag "tag_name"))
                (start-tag-end-pos (treesit-node-end start-tag)))
      (let ((before-close-tag (- (point) 2)))
        (insert (treesit-node-text start-tag-name t))
        (insert ?\>)
        (when (eql start-tag-end-pos before-close-tag)
          (goto-char start-tag-end-pos))
        (indent-according-to-mode)))))

(defun vue-ts-mode--auto-close-tag-post-command-h ()
  "Run `vue-ts-mode--close-tag' after \"</\" inserted in `post-command-hook'."
  (when (and vue-ts-mode-auto-close-tags
             (memq this-command
                   (list 'self-insert-command
                         (key-binding [remap self-insert-command]))))
    (vue-ts-mode--close-tag)))

;;; Navigation

(defconst vue-ts-mode--element-node-type-regexp
  (regexp-opt (list "template_element" "element" "script_element" "style_element")))

(defun vue-ts-mode--element-at-pos (&optional pos)
  "Return the treesit-node of the nearest element around POS.

The returned node will have type \"element\", \"template_element\",
\"script_element\", or \"style_element\"."
  (let ((pos (or pos (point))))
    (treesit-parent-until
     (treesit-node-at pos 'vue)
     (lambda (n)
       (string-match-p vue-ts-mode--element-node-type-regexp
                       (treesit-node-type n))))))

(defun vue-ts-mode--treesit-node-contains-pos-p (node pos)
  "Return t if POS is between NODE's start and end."
  (<= (treesit-node-start node)
      pos
      (treesit-node-end node)))

(defun vue-ts-mode--element-goto-tag (pos target)
  "Move point to the start of TARGET tag of the element at POS.

Target is a symbol whose value is one of start, end, or match."
  (forward-to-indentation 0)

  (if (< pos (point))
      (setq pos (point))
    (goto-char pos))

  (when-let ((element (vue-ts-mode--element-at-pos pos)))
    (if-let ((self-closing-tag (vue-ts-mode--treesit-find-child element "self_closing_tag")))
        (goto-char (treesit-node-start self-closing-tag))
      (when-let ((start-tag (vue-ts-mode--treesit-find-child element "start_tag"))
                 (end-tag (vue-ts-mode--treesit-find-child element "end_tag")))
        (cl-ecase target
          (end (goto-char (treesit-node-start end-tag)))
          (start (goto-char (treesit-node-start start-tag)))
          (match
           (cond ((vue-ts-mode--element-match-goto-end-p start-tag end-tag pos)
                  (goto-char (treesit-node-start end-tag)))
                 ((vue-ts-mode--element-match-goto-start-p start-tag end-tag pos)
                  (goto-char (treesit-node-start start-tag)))
                 (t (goto-char (treesit-node-start end-tag))))))))))

(defun vue-ts-mode-element-match (pos)
  "Move point to the related start/end of the tag at POS."
  (interactive "d")
  (vue-ts-mode--element-goto-tag pos 'match))

(defun vue-ts-mode--pos-directly-between-tags-p (start-tag end-tag pos)
  "Return t if POS is directly between START-TAG and END-TAG.

For example: <tag>|</tag>, but not <tag> |</tag>."
  (and (eql pos (treesit-node-end start-tag))
       (eql pos (treesit-node-start end-tag))))

(defun vue-ts-mode--element-match-goto-end-p (start-tag end-tag pos)
  "Return t if point should move to the end tag of the element at POS.

START-TAG and END-TAG belong to the element in question."
  (and (not (vue-ts-mode--pos-directly-between-tags-p start-tag end-tag pos))
       (or (vue-ts-mode--treesit-node-contains-pos-p start-tag pos)
           (and (not (vue-ts-mode--treesit-node-contains-pos-p end-tag pos))
                (let ((current-line (line-number-at-pos pos))
                      (start-tag-start-line (line-number-at-pos (treesit-node-start start-tag)))
                      (start-tag-end-line (line-number-at-pos (treesit-node-end start-tag))))
                  (or (eql current-line start-tag-start-line)
                      (eql current-line start-tag-end-line)))))))

(defun vue-ts-mode--element-match-goto-start-p (start-tag end-tag pos)
  "Return t if point should move to the start tag of the element at POS.

START-TAG and END-TAG belong to the element in question."
  (or (vue-ts-mode--treesit-node-contains-pos-p end-tag pos)
      (vue-ts-mode--pos-directly-between-tags-p start-tag end-tag pos)
      (let ((current-line (line-number-at-pos pos))
            (end-tag-start-line (line-number-at-pos (treesit-node-start end-tag)))
            (end-tag-end-line (line-number-at-pos (treesit-node-end end-tag))))
        (and (not (vue-ts-mode--treesit-node-contains-pos-p start-tag pos))
             (or (eql current-line end-tag-start-line)
                 (eql current-line end-tag-end-line))))))

(defun vue-ts-mode--get-all-elements ()
  "Return a list of all element nodes in the buffer."
  (flatten-tree (treesit-induce-sparse-tree
                 (treesit-buffer-root-node 'vue)
                 (vue-ts-mode--treesit-node-type-p
                  vue-ts-mode--element-node-type-regexp))))

(defun vue-ts-mode-element-next (pos)
  "Move point to the beginning of the nearest HTML element after POS."
  (interactive "d")
  (when-let* ((elements (vue-ts-mode--get-all-elements))
              (next-element (cl-find-if (lambda (n)
                                          (and n (< pos (treesit-node-start n))))
                                        elements)))
    (goto-char (treesit-node-start next-element))))

(defun vue-ts-mode-element-previous (pos)
  "Move point to the beginning of the nearest HTML element at or before POS."
  (interactive "d")
  (when-let* ((elements (vue-ts-mode--get-all-elements)))
    (cl-loop for el in elements
             for next in (cdr elements)
             if (and (> pos (treesit-node-start el))
                     (<= pos (treesit-node-start next)))
             return (goto-char (treesit-node-start el)))))

;;; Tranformations

(defun vue-ts-mode--swap-nodes (node-a node-b)
  "Swap NODE-A and NODE-B in the buffer."
  (pcase-let ((`(,node-a-start . ,node-a-end)
               (vue-ts-mode--treesit-create-node-markers node-a))
              (`(,node-b-start . ,node-b-end)
               (vue-ts-mode--treesit-create-node-markers node-b)))
    (let ((node-a-contents (treesit-node-text node-a))
          (node-b-contents (treesit-node-text node-b)))
      (atomic-change-group
        (delete-region node-a-start node-a-end)
        (delete-region node-b-start node-b-end)
        (goto-char node-a-start)
        (insert node-b-contents)
        (goto-char node-b-start)
        (insert node-a-contents)
        (goto-char node-b-start))
      (set-marker node-a-start nil)
      (set-marker node-a-end nil)
      (set-marker node-b-start nil)
      (set-marker node-b-end nil))))

(defun vue-ts-mode--treesit-create-node-markers (node)
  "Return a cons cell containing markers for NODE's start and end positions."
  (cons (copy-marker (treesit-node-start node) nil)
        (copy-marker (treesit-node-end node) t)))

(defun vue-ts-mode-element-transpose (pos &optional backward)
  "Swap the element at POS and its sibling.

If a prefix argument is supplied for BACKWARD, swap the element with its
previous sibling."
  (interactive (list (point) current-prefix-arg))
  (forward-to-indentation 0)

  (if (< pos (point))
      (setq pos (point))
    (goto-char pos))

  (when-let* ((element (vue-ts-mode--element-at-pos pos))
              (sibling (vue-ts-mode--treesit-sibling-until
                        element
                        vue-ts-mode--element-node-type-regexp
                        backward)))
    (vue-ts-mode--swap-nodes element sibling)))

(provide 'vue-ts-mode)
;;; vue-ts-mode.el ends here
