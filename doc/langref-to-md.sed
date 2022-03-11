# File:
# langref-to-md.sed
#
# Description: transforms murmel-langref.lisp to Markdown
#
# Usage:
# sed -nf langref-to-md.sed murmel-langref.lisp > murmel-langref.md

# If a line doesn't start with a ; indent it 4 spaces
s/^\([^;].*\)$/    \1/gp

# Comment lines starting with one or more ; will become regular text
s/^;;* \([^=*].*\)$/\1/gp

# Comment lines that start with ';;; =' will become headings
s/^;;; ===\([^=]*\).*$/#\1/gp
s/^;;; ==\([^=]*\).*$/##\1/gp
s/^;;* =\(.*\)$/###\1/gp

# Pass through empty lines as well as empty comment lines
s/^;*$//gp
