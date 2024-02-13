;;; fix-engraved.el --- fix engraved bug -*- lexical-binding:t -*-

;; Author: Gerard Vermeulen <gerard.vermeulen@posteo.net>
;; Version: 0.0.1

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

;; This file is the tangled output of `fix-engraved.org' tailored for
;; use with `fix-engraved.el' which users can evaluate.  Otherwise,
;; users can go to the "Export to LaTeX" section of `fix-engraved.org'
;; and execute the `fe-do-it' source block after installation of
;; `fix-engraved.el'.

;;; Code:

(require 'ox-latex)

(defun fe-org-babel-file-as-block-body (filename)
  "Copy FILENAME contents to an indented Org Babel source block body."
  (when (file-readable-p filename)
    (let ((delete-trailing-whitespace t)
	  (n (if org-src-preserve-indentation
	         0 org-edit-src-content-indentation)))
      (with-temp-buffer
        (insert-file-contents filename)
        (string-rectangle (point-min) (point-max)
		          (make-string n ?\s))
        (delete-trailing-whitespace (point-min) (point-max))
        (buffer-substring (point-min) (point-max))))))

(defun fe-toggle-toc ()
  "Toggle `org-latex-toc-command' between toplevel and file local values."
  (interactive)
  (if (eq org-latex-toc-command
          (default-toplevel-value 'org-latex-toc-command))
      (progn
        (save-buffer)
        (revert-buffer nil 'noconfirm)
        (message "Now `org-latex-toc-command' has its file local value"))
    (setq-local org-latex-toc-command
                (default-toplevel-value 'org-latex-toc-command))
    (message "Now `org-latex-toc-command' has its toplevel value")))

(defun fe-org-latex-engraved-source-block-filter (data _backend _info)
  "Convert unbreakable \"Code\" environments to breakable in non-floats.

Set `org-latex-engraved-preamble' to define an IfBooleanTF Code environment."
  (unless (string-match "^\\\\DeclareTColorBox\\[\\]{Code}{s}"
                        org-latex-engraved-preamble)
    (user-error
     "`org-latex-engraved-preamble' defines no `IfBooleanTF' environment"))
  (when (eq org-latex-src-block-backend 'engraved)
    ;; Transform only blocks matching at position 0.  Therefore, do
    ;; not transform blocks that are listing environments.
    (when (and (string-match "\\`\\\\begin{Code}\n" data))
      (setq data (replace-match "\\begin{Code}*\n" t 'literal data)))))

(defun fe-advice (&rest _args)
  "Install the `fix-engraved' bug work-around."
  (when (eq org-latex-src-block-backend 'engraved)
    ;; I prefer bigger fonts and boxes.  This is not essential.
    (setq-local org-latex-engraved-preamble
                "\\usepackage{fvextra}
[FVEXTRA-SETUP]
% Make code and line numbers normalsize. Make line numbers grey.
\\renewcommand\\theFancyVerbLine{
  \\normalsize\\color{black!40!white}\\arabic{FancyVerbLine}}
% Do not rely on an eventual call to `engrave-faces-latex-gen-preamble'.
\\usepackage{xcolor}
\\providecolor{EfD}{HTML}{f7f7f7}
\\providecolor{EFD}{HTML}{28292e}
% Define a breakable Code environment to prettily wrap the fontified code.
\\usepackage[breakable,xparse]{tcolorbox}
\\DeclareTColorBox[]{Code}{s}{
  colback=EfD, colframe=EFD, colupper=EFD,
  fontupper=\\normalsize\\setlength{\\fboxsep}{0pt},
  boxsep=1.0pt, arc=1.0pt, outer arc=1.0pt, boxrule=0.5pt,
  IfBooleanTF={#1}{breakable}{unbreakable},
  left=2.0pt, right=2.0pt, top=1.0pt, bottom=1.0pt
}
[LISTINGS-SETUP]")
    (setq-local org-export-filter-src-block-functions
                '(fe-org-latex-engraved-source-block-filter))))

;;;###autoload
(defun fe-enable ()
  "Enable an `engraved' bug work-around."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (user-error "Don't call `fe-enable' outside `org-mode' buffers")
    (advice-add 'org-export-dispatch :before 'fe-advice)
    (message "Enabled an `engraved' bug work-around")))

;;;###autoload
(defun fe-disable ()
  "Disable an `engraved' bug work-around."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (user-error "Don't call `fe-disable' outside `org-mode' buffers")
    (setq-local org-latex-engraved-preamble
                (default-toplevel-value 'org-latex-engraved-preamble))
    (setq-local org-export-filter-src-block-functions nil)
    (advice-remove 'org-export-dispatch 'fe-advice)
    (message "Disabled an `engraved' bug work-around")))

(when (derived-mode-p 'org-mode)
  (fe-enable))

(provide 'fix-engraved)

;;; fix-engraved.el ends here
