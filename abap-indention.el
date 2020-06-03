;;; abap-indention.el --- Indentation functions    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Marian Piatkowski
;; Copyright (C) 2018  Marvin Qian

;; Author: Marian Piatkowski <marianpiatkowski@web.de>
;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: ABAP indentation, Emacs

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

;;; Commentary:

;;

;;; Code:

(setq abap--keywords-open '("IF" "ELSE" "LOOP" "DO" "FORM" "CASE" "CLASS" "TRY" "METHOD" "BEGIN OF"))

(setq abap--keywords-close '("ENDIF" "ENDCLASS" "ENDMETHOD" "ENDTRY" "END" "ENDLOOP" "ENDFORM" "ENDCASE" "ENDDO" "END OF"))


(defun abap-delete-leading-space()
  " Delete leading SPACE / TAB"
  (let ((end (progn
               (back-to-indentation)
               (point)))
        (beg (progn
               (move-beginning-of-line nil)
               (point))))
    (delete-region beg end)
    )
  )

(defun abap-is-empty-line()
  "Check space line"
  ;; (beginning-of-line)
  (save-excursion
    ;; (back-to-indentation)
    ;; (looking-at "$")))
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun abap-is-comment-line()
  (save-excursion
    (back-to-indentation)
    (if (looking-at "\"")
        t
      (beginning-of-line)
      (looking-at "*"))
    ))

(defun abap-is-first-line()
  (= 1 (point)))

(defun abap-goto-prev-statement-line()
  "goto previous non empty line"
  (previous-line)
  (if (and (not (abap-is-first-line))
           (or (abap-is-empty-line)
               (abap-is-comment-line)))
      (abap-goto-prev-statement-line)
    ))

(defun abap-get-prev-line-width ()
  "Get width of previous non empty line"
  (save-excursion
    (abap-goto-prev-statement-line)
    (current-column)))

(defun abap-calc-indent ()
  "Get width of previous non empty line"
  (save-excursion
    (back-to-indentation)
    ;; (beginning-of-line)
    ;; Close
    (let ((offset (if (looking-at (regexp-opt abap--keywords-close 'words))
                      (* -1 tab-width)
                    0)))
      (abap-goto-prev-statement-line)
      (if (looking-at (regexp-opt abap--keywords-open 'words))
          (+ (current-column) tab-width offset)
        (+ (current-column) offset)
        )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;; - indentation of statements over multiple lines may have to be done manually
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abap-indent-line ()
  "Indent ABAP Line"
  (interactive)
  (beginning-of-line)
  ; first line is always not indented
  (if (bobp)
    (indent-line-to 0)
  ; else
  (let ((not-indented t) cur-indent)
    (back-to-indentation)
    ; look for closing keywords or visibility attributes
    (if (or (looking-at (regexp-opt abap--keywords-close 'words))
            (looking-at (regexp-opt '("PUBLIC SECTION" "PROTECTED SECTION" "PRIVATE SECTION") 'words)))
      (progn
        (save-excursion
          (forward-line -1)
          (while (abap-is-empty-line)
            (forward-line -1))
          (back-to-indentation)
          (if (looking-at (regexp-opt abap--keywords-open 'words))
            (setq cur-indent (current-indentation))
            (setq cur-indent (- (current-indentation) tab-width)))) ; end save-excursion
        (if (< cur-indent 0) ; we can't indent past the left margin
            (setq cur-indent 0))) ; end progn
    ; else
    (save-excursion
      (while not-indented ; iterate backwards until we find an indentation hint
        (forward-line -1)
        (back-to-indentation)
        ; look whether previous line starts with an opening keyword
        (if (looking-at (regexp-opt abap--keywords-open 'words))
          (progn
            (setq cur-indent (+ (current-indentation) tab-width))
            (setq not-indented nil))
          ; otherwise look whether line is non-empty and does not contain any visibility attributes
          (if (and (not (abap-is-empty-line))
                   (not (looking-at (regexp-opt '("PUBLIC SECTION" "PROTECTED SECTION" "PRIVATE SECTION") 'words))))
            (progn
              (setq cur-indent (current-indentation))
              (setq not-indented nil))
            ; break if we are at the first line
            (if (bobp)
              (setq not-indented nil))))
        )))
    (if cur-indent
      (indent-line-to cur-indent)
    (indent-line-to 0)))) ; if we didn't see an indentation hint

  ;; (unless (abap-is-comment-line)
  ;;   (let ((width tab-width)
  ;;         (indent (abap-calc-indent)))
  ;;     ;; (save-excursion
  ;;     (abap-delete-leading-space)
  ;;     (indent-to indent))))
  )


(provide 'abap-indention)
;;; abap-indention.el ends here
