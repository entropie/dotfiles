
autoload -U compinit && compinit
autoload -U colors && colors
autoload -U promptinit && promptinit
autoload -U age

# zsh          Zsh overview (this section)
# zshmisc      Anything not fitting into the other sections
# zshexpn      Zsh command and parameter expansion
# zshparam     Zsh parameters
# zshoptions   Zsh options
# zshbuiltins  Zsh built-in functions
# zshzle       Zsh command line editing
# zshcompwid   Zsh completion widgets
# zshcompsys   Zsh completion system
# zshcompctl   Zsh completion control
# zshmodules   Zsh loadable modules
# zshzftpsys   Zsh built-in FTP client
# zshall       Meta-man page containing all of the above

if [[ -r ~/.zsh/local ]]; then
  . ~/.zsh/local
fi

if [[ -r ~/.zsh/options ]]; then
  . ~/.zsh/options
else
  echo "zshrc: could not load ~/.zsh/options"
fi

if [[ -r ~/.zsh/zgit ]]; then
  . ~/.zsh/zgit
else
  echo "zshrc: could not load ~/.zsh/prompt"
fi

if [[ -r ~/.zsh/prompt ]]; then
  . ~/.zsh/prompt
else
  echo "zshrc: could not load ~/.zsh/prompt"
fi


if [[ -r ~/.zsh/zle ]]; then
  . ~/.zsh/zle
else
  echo "zshrc: could not load ~/.zsh/zle"
fi

if [[ -r ~/.zsh/environment ]]; then
  . ~/.zsh/environment
else
  echo "zshrc: could not load ~/.zsh/environment"
fi

if [[ -r ~/.zsh/aliases ]]; then
  . ~/.zsh/aliases
else
  echo "zshrc: could not load ~/.zsh/aliases"
fi

if [[ -r ~/.zsh/compsys ]]; then
  . ~/.zsh/compsys
else
  echo "zshrc: could not load ~/.zsh/compsys"
fi

if [[ -r ~/.zsh/misc ]]; then
  . ~/.zsh/misc
else
  echo "zshrc: could not load ~/.zsh/misc"
fi

if [[ -r ~/.zsh/abbrev.sh ]]; then
  . ~/.zsh/abbrev.sh
else
  echo "zshrc: could not load ~/.zsh/abbrev.sh"
fi



if [[ -x /Users/mit/bin/hypno.pl ]]; then
  /Users/mit/bin/hypno.pl
fi


if [[ -x /usr/bin/fortune ]]; then
  fortune -a
fi

