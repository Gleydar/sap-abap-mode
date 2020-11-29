# sap-abap-mode

This is a fork of [qianmarv/sap-abap-mode](https://github.com/qianmarv/sap-abap-mode).
The development of the original project has been discontinued.

`sap-abap-mode` currently supports syntax highlighting and indentation<sup>[1](#footnote1)</sup> of
ABAP development files and CDS views.

## Installation

Add this repository to your `load-path`:
```cl
(add-to-list 'load-path "path/to/sap-abap-mode")
(require 'abap-mode)
(add-to-list 'auto-mode-alist '("\\.abap\\'" . abap-mode))
;; ABAP CDS Mode
(require 'abap-cds-mode)
(add-to-list 'auto-mode-alist '("\\.cds\\'" . abap-cds-mode))
;; ADT files as well
(add-to-list 'auto-mode-alist '("\\.\\(asprog\\|asinc\\|aclass\\)\\'" . abap-mode))
(add-to-list 'auto-mode-alist '("\\.asddls\\'" . abap-cds-mode))
```

<a name="footnote1">1</a>: the indentation rules only cover basic statements
