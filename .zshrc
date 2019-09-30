
#
source /etc/profile

#
alias -g xcopy='xclip -selection clipboard'
alias -g xpaste='xclip -selection clipboard -o'

[ -r /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -r /usr/usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

HISTFILE=~/.zhistory
SAVEHIST=5000
setopt  APPEND_HISTORY
setopt  HIST_IGNORE_ALL_DUPS
setopt  HIST_IGNORE_SPACE
setopt NO_BEEP
setopt AUTO_CD
setopt CORRECT_ALL
setopt SH_WORD_SPLIT
setopt histexpiredupsfirst histfindnodups
setopt histignoredups histnostore histverify histignorespace extended_history  share_history
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash
setopt  IGNORE_EOF

typeset -U path cdpath fpath manpath

autoload colors && colors
autoload -U zcalc

export PROMPT='%n@%m> '
export RPROMPT='[%~]'

zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfilesetopt menucomplete

zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*:expand:*' tag-order all-expansions
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~''*?.old' '*?.pro'
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' menu yes select

autoload -U compinit && compinit

[[ -n ${key[Home]}    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n ${key[End]}     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n ${key[Insert]}  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n ${key[Delete]}  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n ${key[Up]}      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n ${key[Down]}    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n ${key[Left]}    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n ${key[Right]}   ]]  && bindkey  "${key[Right]}"   forward-char
[[ -n ${key[Backspace]}   ]]  && bindkey  "${key[Backspace]}"   backward-delete-charbindkey "^[OB" down-line-or-search
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history

unlimit
limit stack 8192
limit core 0
limit -s

umask 022

setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }

extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2) tar xvjf $1   ;;
      *.tar.gz)  tar xvzf $1   ;;
      *.tar.xz)  tar xvfJ $1   ;;
      *.bz2)     bunzip2 $1    ;;
      *.rar)     unrar x $1    ;;
      *.gz)      gunzip $1     ;;
      *.tar)     tar xvf $1    ;;
      *.tbz2)    tar xvjf $1   ;;
      *.tgz)     tar xvzf $1   ;;
      *.zip)     unzip $1      ;;
      *.Z)       uncompress $1 ;;
      *.7z)      7z x $1       ;;
      *)         echo "'$1' cannot be extracted via >extract<" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# aliases
alias -g g='git'
alias -g H="| head"
alias -g T="| tail"
alias -g G="| grep"
alias -g L="| less"
alias -g M="| most"
alias -g B="&|"
alias -g HL="--help"
alias -g LL="2>&1 | less"
alias -g CA="2>&1 | cat -A"
alias -g NE="2> /dev/null"
alias -g NUL="> /dev/null 2>&1"

alias -g KU="| iconv -c -f koi8r -t utf8"
alias -g CU="| iconv -c -f cp1251 -t utf8"

alias -g UK="| iconv -c -f utf8 -t koi8r"
alias -g UC="| iconv -c -f utf8 -t cp1251"

alias spacman="sudo pacman"
alias pstop="sudo pppoe-stop"
alias pstart="sudo pppoe-start"
alias halt="sudo halt"
alias reboot="sudo reboot"
alias gparted="sudo gparted"


