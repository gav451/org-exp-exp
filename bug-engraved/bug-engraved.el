;;; bug-engraved.el --- demonstrate engraved bug -*- lexical-binding:t -*-

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

;; This file is the tangled output of `bug-engraved.org' tailored for
;; use with `bug-engraved.el'.  Users should go to the "Export to
;; LaTeX" section of `bug-engraved.org' and execute the `be-do-it'
;; source block after evaluation or installation of `bug-engraved.el'.

;;; Code:

(require 'ox-latex)

(defun be-org-latex-engraved-source-block-filter (data _backend _info)
  "Replace \"Code\" with \"Breakable\" in non-floating DATA environments.

Set `org-latex-engraved-preamble' to define a Breakable (non-floating)
environment and an unbreakable Code (floating) environment."
  (unless (string-match "^\\\\DeclareTColorBox\\[\\]{Breakable}"
                        org-latex-engraved-preamble)
    (user-error
     "`org-latex-engraved-preamble' defines no `Breakable' environment"))
  (when (eq org-latex-src-block-backend 'engraved)
    ;; Transform only blocks matching at position 0.  Therefore, do
    ;; not transform blocks that are listing environments.
    (when (string-match "\\`\\\\begin{Code}\n" data)
      (setq data (replace-match "\\begin{Breakable}\n" t 'literal data))
      (if (string-match "^\\\\end{Code}\n" data)
          (setq data (replace-match "\\end{Breakable}\n" t 'literal data))
        (error "Match `^\\\\end{Code}' failure")))))

(defun be-setup (how)
  (if (eq how 'plain-engraved)
      (progn (setq-local org-latex-engraved-preamble
                         (default-toplevel-value 'org-latex-engraved-preamble))
             (setq-local org-export-filter-src-block-functions nil))
    (setq-local org-latex-engraved-preamble
                (format "\\usepackage{fvextra}
[FVEXTRA-SETUP]
%% Make code and line numbers normalsize. Make line numbers grey.
\\renewcommand\\theFancyVerbLine{
  \\normalsize\\color{black!40!white}\\arabic{FancyVerbLine}}
%% Do not rely on an eventual call to `engrave-faces-latex-gen-preamble'.
\\usepackage{xcolor}
\\providecolor{EfD}{HTML}{f7f7f7}
\\providecolor{EFD}{HTML}{28292e}
%% Define a breakable Code environment to prettily wrap the fontified code.
\\usepackage[breakable,xparse]{tcolorbox}
\\DeclareTColorBox[]{Breakable}{o}{
  colback=EfD, colframe=EFD, colupper=EFD,
  fontupper=\\normalsize\\setlength{\\fboxsep}{0pt},
  IfNoValueTF={#1}{
    boxsep=2pt, arc=2.5pt, outer arc=2.5pt, boxrule=1.0pt
  }{
    boxsep=2.5pt, arc=0pt, outer arc=0pt, boxrule=0pt, leftrule=1.5pt
  },
  left=2pt, right=2pt, top=1pt, bottom=0.5pt, breakable
}
%% Define an unbreakable Code environment to fontify code inside floats.
\\DeclareTColorBox[]{Code}{o}{
  colback=EfD, colframe=EFD, colupper=EFD,
  fontupper=\\normalsize\\setlength{\\fboxsep}{0pt},
  IfNoValueTF={#1}{
    boxsep=2pt, arc=2.5pt, outer arc=2.5pt, boxrule=1.0pt
  }{
    boxsep=2.5pt, arc=0pt, outer arc=0pt, boxrule=0pt, leftrule=1pt
  },
  left=2pt, right=2pt, top=1pt, bottom=1pt, %s
}
[LISTINGS-SETUP]" (or (and (eq how 'fixed-engraved) "unbreakable")
                      (and (eq how 'boxed-engraved) "breakable")
                      (user-error "(be-setup `%S') yells BOOM!" how))))
    (setq-local org-export-filter-src-block-functions
                '(be-org-latex-engraved-source-block-filter)))
  (setq-local org-latex-toc-command
              "\\tableofcontents\\label{toc}\n\\listoflistings\n\\newpage\n")
  (setq-local org-latex-src-block-backend 'engraved))

;;;###autoload
(defun be-select-how-and-export-to-latex ()
  "Export Org buffer to LaTeX file after prompting how to export.

The `engraved' source block export backend may lead to defects in
compiled `pdf' for floating listings where vertical line spacing may not
be constant.  Option `plain-engraved' selects Org upstream, option
`fixed-engraved' selects a bug work-around, and option `boxed-engraved'
selects a bug amplification which may lead to terrible `pdf' output."
  (interactive)
  (let ((how
         (intern-soft
          (completing-read
           "How: " '(plain-engraved fixed-engraved boxed-engraved) nil t)))
        (seconds 1))
    (be-setup how)
    (run-with-timer seconds nil #'org-latex-export-to-latex)
    how))

(provide 'bug-engraved)

;;; bug-engraved.el ends here
