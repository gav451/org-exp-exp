;;; any-backend.el --- demonstrate any block backend -*- lexical-binding:t -*-

;; Author: Gerard Vermeulen <gerard.vermeulen@posteo.net>
;; Version: 0.0.1

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is the tangled output of `any-backend.org' tailored for
;; use with `any-backend.el'.  Users should go to the "Export to
;; LaTeX" section of `any-backend.org' and execute the `ab-do-it'
;; source block after evaluation or installation of `any-backend.el'.

;;; Code:

(require 'ox-latex)

(defun ab-org-babel-file-as-block-body (filename)
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

(defun ab-org-latex-engraved-source-block-filter (data _backend _info)
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

(defun ab-org-latex-setup (how main)
  "Set `org-latex-src-block-backend' and `org-latex-toc-command'.
HOW selects the sub-backend and MAIN selects which Org features to use."
  (cond
   ((memq how '(minted plain-engraved fixed-engraved boxed-engraved))
    (setq-local
     org-latex-toc-command
     "\\tableofcontents\\label{toc}\n\\listoflistings\n\\newpage\n"))
   ((eq how 'listings)
    (setq-local
     org-latex-toc-command
     "\\tableofcontents\\label{toc}\n\\lstlistoflistings\n\\newpage\n"))
   ((eq how 'verbatim)
    (setq-local
     org-latex-toc-command
     "\\tableofcontents\\label{toc}\n\\listoffigures\\newpage\n"))
   (t (user-error "Correct argument `%S' for `ab-setup'" how)))
  (if (memq how '(plain-engraved fixed-engraved boxed-engraved))
      (setq-local org-latex-src-block-backend 'engraved)
    (setq-local org-latex-src-block-backend how))
  (if main (setq-local org-latex-packages-alist nil)
    (setq-local org-latex-packages-alist
                (or (and (eq how 'minted) '(("" "minted")))
                    (and (eq how 'listings) '(("" "listings")
                                              ("" "color")))))))

(defun ab-setup (how main)
  "Set `org-latex-src-block-backend', `org-latex-toc-command',
`org-latex-engraved-preamble', and `org-export-filter-src-block-functions'.
HOW selects the sub-backend and MAIN selects which Org features to use."
  (ab-org-latex-setup how main)
  (if (memq how '(verbatim listings minted plain-engraved))
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
                      (user-error "(ab-setup `%S') yells BOOM!" how))))
    (setq-local org-export-filter-src-block-functions
                '(ab-org-latex-engraved-source-block-filter)))
  org-latex-src-block-backend)

;;;###autoload
(defun ab-select-how-and-export-to-latex (main)
  "Export Org buffer to LaTeX file after prompting how to export.
In case MAIN is non-nil use org-9.7pre features, or in case MAIN is nil
use org-9.16 features, to export to LaTeX.

The `engraved' source block export backend may lead to defects in
compiled `pdf' for floating listings where vertical line spacing may not
be constant.  Option `plain-engraved' selects Org upstream, option
`fixed-engraved' selects a bug work-around, and option `boxed-engraved'
selects a bug amplification which may lead to terrible `pdf' output.
The other choices are `verbatim', `listings', and `minted' to select the
other backends."
  (interactive)
  (let ((how
         (intern-soft
          (completing-read
           "How: " '(plain-engraved fixed-engraved boxed-engraved
                                    verbatim listings minted)
           nil t)))
        (seconds 1))
    (ab-setup how main)
    (run-with-timer seconds nil #'org-latex-export-to-latex)
    (if main
        (format "#+latex_header: <<%s>>" how)
      (format "%% %s" how))))

(provide 'any-backend)

;;; any-backend.el ends here
