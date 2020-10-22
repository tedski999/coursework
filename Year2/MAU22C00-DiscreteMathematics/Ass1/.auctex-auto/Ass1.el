(TeX-add-style-hook
 "Ass1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("amsmath" "fleqn")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "fancyhdr"
    "amsmath"
    "amssymb"
    "hyperref")
   (TeX-add-symbols
    "qedsymbol")
   (LaTeX-add-labels
    "eq:1.a"
    "eq:1.b"
    "eq:1.c"
    "eq:1.d"
    "eq:1.1"
    "eq:1.2"
    "eq:1.3"
    "eq:1.4"
    "eq:1.5"
    "eq:3.a"
    "eq:3.b"
    "eq:4.1"
    "eq:4.2"))
 :latex)

