# sclang-ts-mode

Experimental enhanced Emacs SuperCollider mode using tree-sitter for syntax highlighting and indentation.

Created with insufficiant knowledge of any of the involved fields: SuperCollider syntax and indentation, Tree-sitter, Emacs, Elisp etc.

Based on the great SuperCollider [grammar](https://github.com/madskjeldgaard/tree-sitter-supercollider) work by Mads Kjekdgaard and contributers.

Hopefully someone can use this as a starting point for a more solid version.

## Usage
```
(add-to-list 'load-path "path/to/sclang-ts-mode.el")
(require 'sclang-ts-mode)
(sclang-ts-setup)
```
