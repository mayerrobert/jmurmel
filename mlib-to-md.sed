# File:
# mlib-to-md.sed
#
# Description: transforms mlib.lisp to Markdown
#
# Usage:
# sed -nf mlib-to-md.sed mlib.lisp > mlib.md

# Comment lines starting with three or more ; will become regular text
s/^;;;;* \([^=].*\)$/\1/gp

# Comment lines that start with ';;; =' will become headings
s/^;;;;* ===\([^=]*\).*$/#\1/gp
s/^;;; ==\([^=]*\).*$/##\1/gp
s/^;;* =\(.*\)$/###\1/gp

# Pass through empty comment lines
s/^;;*$//gp
