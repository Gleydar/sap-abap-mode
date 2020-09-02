# sap-abap-mode

This is a fork of [qianmarv/sap-abap-mode](https://github.com/qianmarv/sap-abap-mode).
The development of the original project has been discontinued.

`sap-abap-mode` currently supports syntax highlighting and indentation[^1] of ABAP development files.

## Installation

Add this repository to your `load-path`:
```cl
(add-to-list 'load-path "path/to/sap-abap-mode")
(require 'abap-mode)
(add-to-list 'auto-mode-alist '("\\.abap\\'" . abap-mode))
;; ADT files as well
(add-to-list 'auto-mode-alist '("\\.\\(asprog\\|asinc\\|aclass\\)\\'" . abap-mode))
```

[^1]: the indentation rules only cover basic statements
