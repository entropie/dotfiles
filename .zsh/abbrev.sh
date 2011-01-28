# ~/.zsh/abbreviation.sh - by Michael 'manveru' Fellinger

# zsh% ls -l /dev Im<space>
# # gets replaced by
# zsh% ls -l /dev | more

# Don't expand with CTRL-x[space]

setopt extendedglob

typeset -A abbreviations
abbreviations=(
  "Ia"    "| awk"
  "Iag"   "| agrep"
  "Ieg"   "| egrep"
  "Ig"    "| grep"
  "Igr"   "| groff -s -p -t -e -Tlatin1 -mandoc"
  "Ih"    "| head"
  "Ik"    "| keep"
  "Im"    "| most"
  "Ip"    "| $PAGER"
  "Is"    "| sort"
  "It"    "| tail"
  "Iv"    "| ${VISUAL:-${EDITOR}}"
  "Iw"    "| wc"
  "Ix"    "| xargs"
)

magic-abbrev-expand() {
  local MATCH
  LBUFFER=${LBUFFER%%(#m)[_a-zA-Z0-9]#}
  LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
  zle self-insert
}

no-magic-abbrev-expand() {
  LBUFFER+=' '
}

zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand
bindkey " " magic-abbrev-expand
bindkey "^x " no-magic-abbrev-expand
