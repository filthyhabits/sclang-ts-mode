;;; sclang-ts-mode.el --- Enhanced SuperCollider mode with tree-sitter support -*- lexical-binding: t; -*-

;; Enhanced SuperCollider mode using tree-sitter for syntax highlighting and indentation.
;; This mode can work standalone or enhance the existing sclang-mode when available.

(require 'treesit)
(require 'sclang-mode nil t) ; Optional dependency, do not error if missing.

;; A few declare-function lines avoid byte‑compiler nags (calling C functions)
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")

;;; Grammar Installation

(defcustom sclang-ts-grammar-source
  '(supercollider "https://github.com/madskjeldgaard/tree-sitter-supercollider")
  "Grammar source for SuperCollider tree-sitter."
  :type '(list symbol string)
  :group 'sclang
  :version "29.1")

(defun sclang-ts-install-grammar ()
  "Install the SuperCollider tree-sitter grammar."
  (interactive)
  (let ((treesit-language-source-alist (list sclang-ts-grammar-source)))
    (treesit-install-language-grammar 'supercollider)))

(defun sclang-ts-ensure-grammar ()
  "Ensure SuperCollider tree-sitter grammar is available, installing if necessary."
  (unless (treesit-ready-p 'supercollider)
    (message "SuperCollider tree-sitter grammar not found, installing...")
    (condition-case err
        (progn
          (sclang-ts-install-grammar)
          (message "SuperCollider tree-sitter grammar installed successfully")
          t)
      (error 
       (message "Failed to install SuperCollider tree-sitter grammar: %s" 
                (error-message-string err))
       (message "You can try installing manually with M-x sclang-ts-install-grammar")
       nil))))

;;; Font Lock Rules

(defvar sclang-ts-font-lock-rules
  '(:language supercollider
    :feature comment
    :override t
    ((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)
    
    :language supercollider
    :feature string
    :override t
    ((string) @font-lock-string-face)
    
    :language supercollider
    :feature number
    :override t
    ((number) @font-lock-number-face
     (float) @font-lock-number-face
     (integer) @font-lock-number-face)
    
    :language supercollider
    :feature class
    :override t
    ((class) @font-lock-type-face
     (receiver (class) @font-lock-type-face)
     (function_call (receiver (class) @font-lock-type-face)))
    
    :language supercollider
    :feature function
    :override t
    ((method_name) @font-lock-function-name-face)
    
    :language supercollider
    :feature variable
    :override t
    (
     ;; locals: var sig;
     (variable (local_var (identifier) @font-lock-variable-name-face))
     ;; env vars: ~foo
     (variable (environment_var) @font-lock-variable-name-face)
     )

    :language supercollider
    :feature param
    :override t
    (
     ;; parameters declared with pipes: | out = 0 |
     (parameter_list (argument (identifier) @font-lock-variable-use-face))
     (parameter_list (argument name: (identifier) @font-lock-variable-use-face))

     ;; parameters declared with 'arg' syntax: arg out = 0;
     ;; (some grammars use a dedicated node, but yours shows plain (argument ...),
     ;; so these two lines also catch that case anywhere.)
     (argument (identifier) @font-lock-variable-use-face)
     (argument name: (identifier) @font-lock-variable-use-face)
     )

    :language supercollider
    :feature paramkeys
    :override t
    ((associative_item (identifier) @font-lock-keyword-face))

    :language supercollider
    :feature keyword
    :override t
    (
     ;; Control structure keywords: if, case, while, for, etc. NEW
     (control_structure (_ name: _ @font-lock-keyword-face))
     ;; named-argument keys: doneAction:, out:, mix:, room:, etc.
     (named_argument (identifier) @font-lock-keyword-face))
    
    :language supercollider
    :feature symbol
    :override t
    ((symbol) @font-lock-builtin-face)
    
    :language supercollider
    :feature punctuation
    :override t
    (("(" ")" "[" "]" "{" "}") @font-lock-bracket-face
     (";" "," ".") @font-lock-punctuation-face)

    :language supercollider
    :feature operator
    :override t
    (((binary_expression operator: ("+") @font-lock-operator-face))
     ((binary_expression operator: ("/") @font-lock-operator-face))
     ((binary_expression operator: ("*") @font-lock-operator-face))
     ((binary_expression operator: ("-") @font-lock-operator-face))
     ((binary_expression operator: ("==") @font-lock-operator-face))
     ((binary_expression operator: ("!=") @font-lock-operator-face))
     ((binary_expression operator: ("<") @font-lock-operator-face))
     ((binary_expression operator: ("<=") @font-lock-operator-face))
     ((binary_expression operator: (">") @font-lock-operator-face))
     ((binary_expression operator: (">=") @font-lock-operator-face))
     ((binary_expression operator: ("|") @font-lock-operator-face))
     ((binary_expression operator: ("||") @font-lock-operator-face))
     ((binary_expression operator: ("&&") @font-lock-operator-face))
     ((binary_expression operator: ("%") @font-lock-operator-face)))
     ;; ((binary_expression operator: ("%%") @font-lock-operator-face)))
    
    :language supercollider
    :feature escape
    :override t
    (((string (escape_sequence) @font-lock-escape-face))
     ((literal (string (escape_sequence) @font-lock-escape-face))))
     ;; escape sequences inside strings: \" \n \t \\ …
     ;; (optional) if your grammar sometimes wraps strings as (literal (string …))

    :language supercollider
    :feature declkeyword
    :override t
    ([ "arg" "classvar" "const" "var" ] @font-lock-keyword-face)
    ;; Match declaration keywords
    
    :language supercollider
    :feature sc-literals
    :override t
    (
     ;; true / false
     ((literal (bool) @font-lock-constant-face))
     ;; nil / inf (as builtin_var)
     ((variable (builtin_var) @font-lock-constant-face)
      (:match "\\`\\(?:nil\\|inf\\)\\'" @font-lock-constant-face))
     ;; pi (as number)
     ((literal (number) @font-lock-constant-face)
      (:match "\\`pi\\'" @font-lock-constant-face))
     ;; $a, $Z, $\n, etc.
     ((literal (char) @font-lock-constant-face)))

   
    :language supercollider
    ;; TEMP: visualize grammar gaps
    :feature error
    :override t
    ((ERROR) @font-lock-warning-face))
  "Font-lock rules for SuperCollider tree-sitter mode.")

;;; Indentation Rules

(defvar sclang-ts-indent-level 4
  "Indentation level for SuperCollider tree-sitter mode.")

(defvar sclang-ts-mode-map
  (make-sparse-keymap)
  "Keymap for `sclang-ts-mode'.")

(defun sclang-ts-electric-dot (n)
  "Insert a dot and re-indent line if it's at beginning of a chained method call."
  (interactive "p")
  (self-insert-command n)
  (when (save-excursion
          (backward-char n)
          (skip-chars-backward " \t")
          (bolp))
    (indent-according-to-mode)))

;; Override dot binding in sclang-ts-mode
(define-key sclang-ts-mode-map "." #'sclang-ts-electric-dot)

(defun sclang-ts-electric-return ()
  "Split ()/{}/[] and indent respecting existing tree-sitter rules."
  (interactive)
  (let ((ch (char-before))
        (close (char-after)))
    (cond
     ;; Case 1: Between matching delimiters with nothing between them: (|)
     ((and ch close
           (memq ch '(?\( ?\{ ?\[))
           (memq close '(?\) ?\} ?\])))
      (progn
        ;; Create the three-line structure
        (newline)
        (save-excursion
          (newline)
          (indent-according-to-mode))
        ;; Use temporary content to get proper indentation
        (insert "x")
        (indent-according-to-mode)
        (delete-char -1)))
     
     ;; Case 2: After any opening delimiter: (|, {|, [|
     ((and ch (memq ch '(?\( ?\{ ?\[)))
      (progn
        (newline)
        ;; Calculate indentation - only indent if it's not a top-level grouping paren
        (when (sclang-ts--should-indent-after-delimiter ch)
          (let ((base-indent (save-excursion
                               (forward-line -1)
                               (current-indentation))))
            (indent-to (+ base-indent sclang-ts-indent-level))))))
     
     ;; Case 3: Everything else
     (t (progn
          (newline)
          ;; Use temporary content to help tree-sitter understand the context
          (insert "x")
          (indent-according-to-mode)
          (delete-char -1))))))

(defun sclang-ts--should-indent-after-delimiter (delimiter)
  "Determine if we should indent after DELIMITER based on context."
  (cond
   ;; Always indent after braces and brackets
   ((memq delimiter '(?\{ ?\[)) t)
   
   ;; For parentheses: only don't indent if at absolute beginning of line (column 0)
   ((eq delimiter ?\()
    (save-excursion
      (backward-char 2) ; Go to the opening paren (skip the newline we just created)
      (let ((paren-col (current-column)))
        ;; Don't indent only if paren is at column 0
        (not (= paren-col 0)))))
   
   (t nil)))

(define-key sclang-ts-mode-map (kbd "RET") #'sclang-ts-electric-return)

;; Helper: is this the outermost grouping paren block?
(defun sclang-ts--top-level-code-block-p (node parent &rest _)
  "Return non-nil when PARENT is a code_block directly under source_file."
  (let ((gp (and parent (treesit-node-parent parent))))
    (and (equal (treesit-node-type parent) "code_block")
         gp
         (equal (treesit-node-type gp) "source_file"))))

(defvar sclang-ts-indent-rules
  `((supercollider
     ;; Closing delimiters align with the opener line
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)

     ;; Children of case statements (function_blocks inside case) - FIRST PRIORITY
     ((parent-is "case") parent-bol ,sclang-ts-indent-level)

     ;; Children of switch statements (all arguments inside switch) - FIRST PRIORITY  
     ((parent-is "switch") parent-bol ,sclang-ts-indent-level)
     
     ;; ALL children inside control structures (if/while/for etc.)
     ((parent-is "control_structure") parent-bol ,sclang-ts-indent-level)
     
     ;; No extra indent for outer grouping parens: ( ... )
     ((and (parent-is "code_block") sclang-ts--top-level-code-block-p)
      parent-bol 0)
     
     ;; Inside (...) grouping blocks
     ((parent-is "code_block") parent-bol ,sclang-ts-indent-level)
     
     ;; Inside function/lambda bodies { ... } - general case
     ((parent-is "function_block") parent-bol ,sclang-ts-indent-level)
     
     ;; Variable definitions and sequences
     ((parent-is "variable_definition") parent-bol ,sclang-ts-indent-level)
     ((parent-is "variable_definition_sequence") parent-bol ,sclang-ts-indent-level)
     
     ;; Parameter lists in function definitions
     ((parent-is "parameter_list") parent-bol ,sclang-ts-indent-level)
     ;;((parent-is "argument") first-sibling 0)  ; align arguments in parameter lists
     
     ;; All argument_calls in a parameter_call_list should align with the first one
     ((node-is "argument_calls") first-sibling 0)
     
     ;; Inside call parens: SinOsc.ar( ... )
     ((node-is "parameter_call_list") parent-bol ,sclang-ts-indent-level)
     
     ;; Extra safety: if the list is malformed and you're sitting directly
     ;; under the call node, still indent one level.
     ;;((parent-is "method_call")   parent-bol ,sclang-ts-indent-level)
     ((node-is "method_call")   parent-bol ,sclang-ts-indent-level)
     ;; ((parent-is "function_call") parent-bol ,sclang-ts-indent-level)
     
     ;; Collections:
     ;; - bracket arrays: [ ... ]  → collection
     ;; - paren ducts:   ( key: val, ... ) → collection
     ((parent-is "collection") parent-bol ,sclang-ts-indent-level)
     
     ;; Indexing (rarely multi-line, but safe to support):
     ((parent-is "indexed_collection") parent-bol ,sclang-ts-indent-level)
     
     ;; Binary expressions (for multi-line expressions)
     ((parent-is "binary_expression") first-sibling 0)
     
     ;; Definitions (keep if you edit .sc class files too)
     ((parent-is "function_definition") parent-bol ,sclang-ts-indent-level)
     ((parent-is "method_definition")   parent-bol ,sclang-ts-indent-level)
     ((parent-is "class_definition")    parent-bol ,sclang-ts-indent-level)
     
     ;; Fallback
     (no-node parent-bol 0)))
  "Indentation rules for SuperCollider tree-sitter mode.")
  
;; ;;; Imenu Support

;; (defun sclang-ts-imenu-node-p (node)
;;   "Check if NODE should be included in imenu."
;;   (member (treesit-node-type node)
;;           '("method_definition" "function_definition" "class_definition")))

;; (defun sclang-ts-imenu-name-function (node)
;;   "Return the name of NODE for imenu."
;;   (let ((name-node (treesit-node-child-by-field-name node "name")))
;;     (when name-node
;;       (treesit-node-text name-node t))))

;; ;;; Navigation Functions

;; (defun sclang-ts-defun-name (node)
;;   "Return the defun name of NODE."
;;   (treesit-node-text
;;    (pcase (treesit-node-type node)
;;      ("method_definition"
;;       (treesit-node-child-by-field-name node "name"))
;;      ("function_definition" 
;;       (treesit-node-child-by-field-name node "name"))
;;      ("class_definition"
;;       (treesit-node-child-by-field-name node "name"))
;;      (_ node))
;;    t))

;;; Tree-sitter Setup Function

(defun sclang-ts-setup-treesitter ()
  "Set up tree-sitter features for SuperCollider mode."
  (if (or (treesit-ready-p 'supercollider)
          (sclang-ts-ensure-grammar))
      (progn
        (message "Setting up SuperCollider tree-sitter...")
        
        ;; Tree-sitter setup
        (treesit-parser-create 'supercollider)
        
        ;; Font-lock
        (setq-local treesit-font-lock-settings
                    (apply #'treesit-font-lock-rules sclang-ts-font-lock-rules))
        (setq-local treesit-font-lock-feature-list
		    '((comment)                           ;; Level 1: Comments and definitions
		      (string number class function)      ;; Level 2: Keywords, strings, data types, functions
		      (variable param keyword paramkeys symbol sc-literals declkeyword)    ;; Level 3: Full fontification 
		      (punctuation operator escape error)  ;; Level 4: Everything else
		      ))
	(setq-local treesit-font-lock-level 4)
        
        ;; Indentation
        (setq-local treesit-simple-indent-rules sclang-ts-indent-rules)
        
        ;; ;; Navigation
        ;; (setq-local treesit-defun-type-regexp
        ;;             (rx (or "method_definition" "function_definition" "class_definition")))
        ;; (setq-local treesit-defun-name-function #'sclang-ts-defun-name)
        
        ;; ;; Imenu
        ;; (setq-local treesit-simple-imenu-settings
        ;;             `(("Function" "\\`function_definition\\'" nil nil)
        ;;               ("Method" "\\`method_definition\\'" nil nil)
        ;;               ("Class" "\\`class_definition\\'" nil nil)))
        
        ;; Enable tree-sitter features
        (treesit-major-mode-setup)
        
        ;; Force font-lock refresh
        (font-lock-flush)
        (font-lock-ensure)
        
        (message "SuperCollider tree-sitter setup complete"))
    (progn
      (message "SuperCollider tree-sitter grammar not available")
      (message "Run M-x sclang-ts-install-grammar to install it"))))

;;; Mode Definition

;; Define the mode, inheriting from sclang-mode if available, otherwise prog-mode
(if (featurep 'sclang-mode)
    (progn
      ;;;###autoload
      (defun sclang-ts-test-specific-method ()
	"Test highlighting for the specific method structure we found."
	(interactive)
	(when (treesit-ready-p 'supercollider)
	  ;; Test the exact pattern from the tree structure
	  (setq-local treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language supercollider
                       :feature test
                       :override t
                       ;; Test just method_name nodes
                       ((method_name) @font-lock-warning-face)))
	  (setq-local treesit-font-lock-feature-list '((test)))
	  (setq-local treesit-font-lock-level 4)
	  (font-lock-flush)
	  (font-lock-ensure)
	  (message "Testing direct method_name pattern - 'kr' should be yellow/orange")))

;;;###autoload
      (defun sclang-ts-debug-method-nodes ()
	"Debug method nodes by showing all nodes in the tree."
	(interactive)
	(when (treesit-ready-p 'supercollider)
	  (let ((root (treesit-buffer-root-node)))
	    (when root
              (with-current-buffer (get-buffer-create "*sclang-ts-node-debug*")
		(erase-buffer)
		(insert "=== All Tree-sitter Nodes ===\n\n")
		(sclang-ts--walk-tree root 0)
		(display-buffer (current-buffer)))))))

      (defun sclang-ts--walk-tree (node depth)
	"Walk the tree-sitter tree and print all nodes."
	(let ((indent (make-string (* depth 2) ? ))
              (type (treesit-node-type node))
              (text (treesit-node-text node t)))
	  (insert (format "%s%s" indent type))
	  (when (< (length text) 50)
	    (insert (format ": %S" text)))
	  (insert "\n"))
	(dolist (child (treesit-node-children node))
	  (sclang-ts--walk-tree child (1+ depth))))

;;;###autoload
      (defun sclang-ts-test-all-method-patterns ()
	"Test multiple method highlighting patterns."
	(interactive)
	(when (treesit-ready-p 'supercollider)
	  ;; Try multiple patterns to see which one works
	  (setq-local treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language supercollider
                       :feature test
                       :override t
                       ;; Pattern 1: Direct method_name
                       ((method_name) @font-lock-warning-face)
                       ;; Pattern 2: Method name in method call
                       ((method_call (method_name) @font-lock-error-face))
                       ;; Pattern 3: With field name
                       ((method_call name: (method_name) @font-lock-success-face))
                       ;; Pattern 4: Any node containing "method"
                       ((method_call) @font-lock-info-face)))
	  (setq-local treesit-font-lock-feature-list '((test)))
	  (setq-local treesit-font-lock-level 4)
	  (font-lock-flush)
	  (font-lock-ensure)
	  (message "Applied multiple test patterns - check colors")))

;;;###autoload
      (defun sclang-ts-inspect-node-at-point ()
	"Show the tree-sitter node type at point for debugging."
	(interactive)
	(when (treesit-ready-p 'supercollider)
	  (let ((node (treesit-node-at (point))))
	    (if node
		(message "Node at point: %s (parent: %s)" 
			 (treesit-node-type node)
			 (treesit-node-type (treesit-node-parent node)))
              (message "No tree-sitter node at point")))))

;;;###autoload
      (defun sclang-ts-refresh-highlighting ()
	"Refresh tree-sitter syntax highlighting in current buffer."
	(interactive)
	(when (and (treesit-ready-p 'supercollider)
		   (treesit-parser-list))
	  (setq-local treesit-font-lock-level 4)
	  (font-lock-flush)
	  (font-lock-ensure)
	  (message "SuperCollider syntax highlighting refreshed")))

;;;###autoload
      (defun sclang-ts-debug-info ()
	"Show debug information for SuperCollider tree-sitter setup."
	(interactive)
	(let ((original-buffer (current-buffer))
              (grammar-available (treesit-ready-p 'supercollider))
              (parsers (when (boundp 'treesit-parser-list)
			 (treesit-parser-list)))
              (font-lock-mode (bound-and-true-p font-lock-mode))
              (treesit-font-lock (bound-and-true-p treesit-font-lock-mode))
              (feature-list (when (boundp 'treesit-font-lock-feature-list)
                              treesit-font-lock-feature-list))
              (font-lock-level (when (boundp 'treesit-font-lock-level)
				 treesit-font-lock-level)))
	  (with-current-buffer (get-buffer-create "*sclang-ts-debug*")
	    (erase-buffer)
	    (insert "=== SuperCollider Tree-sitter Debug Info ===\n\n")
	    (insert (format "Original buffer: %s\n" (buffer-name original-buffer)))
	    (insert (format "Major mode: %s\n" (with-current-buffer original-buffer major-mode)))
	    (insert (format "Grammar available: %s\n" grammar-available))
	    (insert (format "Font-lock mode: %s\n" font-lock-mode))
	    (insert (format "Tree-sitter font-lock mode: %s\n" treesit-font-lock))
	    (insert (format "Active parsers: %s\n" parsers))
	    (insert (format "Emacs version: %s\n" emacs-version))
	    (insert (format "Tree-sitter available: %s\n" (treesit-available-p)))
	    (when grammar-available
              (insert "\n=== Font-lock features ===\n")
              (insert (format "Feature list: %s\n" feature-list))
              (insert (format "Font-lock level: %s\n" font-lock-level)))
	    (display-buffer (current-buffer)))))

;;;###autoload
      (define-derived-mode sclang-ts-mode sclang-mode "SuperCollider[TS]"
        "Enhanced SuperCollider mode with tree-sitter support.

This mode extends sclang-mode with tree-sitter features for improved
syntax highlighting, indentation, and code navigation."
        :group 'sclang
	(message "setup from define-derived-mode")
        (sclang-ts-setup-treesitter)))
  (progn
    ;;;###autoload
    (define-derived-mode sclang-ts-mode prog-mode "SuperCollider[TS]"
      "SuperCollider mode with tree-sitter support.

This mode provides SuperCollider editing support with tree-sitter features
for improved syntax highlighting, indentation, and code navigation."
      :group 'sclang
      ;; Set up basic SuperCollider syntax if sclang-mode is not available
      (setq-local comment-start "// ")
      (setq-local comment-end "")
      (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
      (setq-local comment-end-skip "[ \t]*\\*+/")
      (message "from if")
      (sclang-ts-setup-treesitter))))

;;; Additional Functions for Existing sclang-mode Users

;;;###autoload
(defun sclang-ts-mode-enable ()
  "Enable tree-sitter features in current sclang-mode buffer."
  (interactive)
  (if (eq major-mode 'sclang-mode)
      (if (or (treesit-ready-p 'supercollider)
              (sclang-ts-ensure-grammar))
          (progn
	    (message "from sclang-ts-mode-enable")
            (sclang-ts-setup-treesitter)
            (message "Tree-sitter features enabled"))
        (message "SuperCollider tree-sitter grammar could not be installed"))
    (message "Not in sclang-mode buffer")))

;;;###autoload
(defun sclang-ts-mode-enable-maybe ()
  "Enable tree-sitter features if in sclang-mode and grammar is available."
  (when (and (eq major-mode 'sclang-mode)
             (or (treesit-ready-p 'supercollider)
                 (sclang-ts-ensure-grammar)))
    (message "from sclang-ts-mode-enable-maybe")
    (sclang-ts-setup-treesitter)))

;;; Auto-mode and Setup

;;;###autoload
(defun sclang-ts-setup ()
  "Set up SuperCollider tree-sitter integration.
This function adds file associations and provides commands for
enabling tree-sitter support."
  (interactive)
  
  ;; Ensure grammar is available before setting up auto-mode
  (when (or (treesit-ready-p 'supercollider)
            (sclang-ts-ensure-grammar))
    (add-to-list 'auto-mode-alist '("\\.sc\\'" . sclang-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.scd\\'" . sclang-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.scsyndef\\'" . sclang-ts-mode))
    (message "SuperCollider tree-sitter mode associations added")))

;;; Interactive Commands

;;;###autoload
(defun sclang-ts-check-grammar ()
  "Check if SuperCollider tree-sitter grammar is available."
  (interactive)
  (if (treesit-ready-p 'supercollider)
      (message "SuperCollider tree-sitter grammar is available and ready")
    (message "SuperCollider tree-sitter grammar is not available. Run M-x sclang-ts-install-grammar")))

;;;###autoload
(defun sclang-ts-toggle-mode ()
  "Toggle between sclang-mode and sclang-ts-mode."
  (interactive)
  (cond
   ((eq major-mode 'sclang-ts-mode)
    (when (fboundp 'sclang-mode)
      (sclang-mode)
      (message "Switched to sclang-mode")))
   ((eq major-mode 'sclang-mode)
    (if (or (treesit-ready-p 'supercollider)
            (sclang-ts-ensure-grammar))
        (progn
          (sclang-ts-mode)
          (message "Switched to sclang-ts-mode"))
      (message "SuperCollider tree-sitter grammar not available")))
   (t
    (message "Not in a SuperCollider buffer"))))

(provide 'sclang-ts-mode)

;;; sclang-ts-mode.el ends here
