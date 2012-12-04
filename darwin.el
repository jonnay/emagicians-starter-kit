
(when (fboundp 'ns-list-colors)
  (setq x-colors (ns-list-colors)))

(setq org-latex-to-pdf-process 
      '("/usr/texbin/pdflatex -interaction nonstopmode -output-directory %o %f" 
        "/usr/texbin/pdflatex -interaction nonstopmode -output-directory %o %f" 
        "/usr/texbin/pdflatex -interaction nonstopmode -output-directory %o %f"))
