
# zmodload zsh/zprof

## Create a hash table for globally stashing variables without polluting main
## scope with a bunch of identifiers.
typeset -A __TABLE

__TABLE[ITALIC_ON]=$'\e[3m'
__TABLE[ITALIC_OFF]=$'\e[23m'

#
# Completion
#

fpath=($HOME/.zsh/completions $fpath)

autoload -U compinit
compinit -u

## Make completion:

# Colorize completions using default `ls` colors.
zstyle ':completion:*' list-colors ''

# Allow completion of ..<Tab> to ../ and beyond.
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(..) ]] && reply=(..)'

# $CDPATH is overpowered (can allow us to jump to 100s of directories) so tends
# to dominate completion; exclude path-directories from the tag-order so that
# they will only be used as a fallback if no completions are found.
zstyle ':completion:*:complete:(cd|pushd):*' tag-order 'local-directories named-directories'

# Categorize completion suggestions with headings:
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format %F{default}%B%{$__TABLE[ITALIC_ON]%}--- %d ---%{$__TABLE[ITALIC_OFF]%}%b%f

# Enable keyboard navigation of completions in menu
# (not just tab/shift-tab but cursor keys as well):
zstyle ':completion:*' menu select


#
# Prompt
#
alias base16_default-dark="$HOME/.zsh/base16-shell/scripts/base16-default-dark.sh"
alias base16_solarized-light="$HOME/.zsh/base16-shell/scripts/base16-solarized-light.sh"

# base16_default-dark
base16_solarized-light

# http://zsh.sourceforge.net/Doc/Release/User-Contributions.html
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' disable-patterns "${(b)HOME}/code/portal(|-ee)(|/*)"
zstyle ':vcs_info:*' stagedstr "%F{green}●%f" # default 'S'
zstyle ':vcs_info:*' unstagedstr "%F{red}●%f" # default 'U'
zstyle ':vcs_info:*' use-simple true
zstyle ':vcs_info:git+set-message:*' hooks git-untracked
zstyle ':vcs_info:git*:*' formats '[%b%m%c%u] ' # default ' (%s)-[%b]%c%u-'
zstyle ':vcs_info:git*:*' actionformats '[%b|%a%m%c%u] ' # default ' (%s)-[%b|%a]%c%u-'
zstyle ':vcs_info:hg*:*' formats '[%m%b] '
zstyle ':vcs_info:hg*:*' actionformats '[%b|%a%m] '
zstyle ':vcs_info:hg*:*' branchformat '%b'
zstyle ':vcs_info:hg*:*' get-bookmarks true
zstyle ':vcs_info:hg*:*' get-revision true
zstyle ':vcs_info:hg*:*' get-mq false
zstyle ':vcs_info:hg*+gen-hg-bookmark-string:*' hooks hg-bookmarks
zstyle ':vcs_info:hg*+set-message:*' hooks hg-message

function +vi-hg-bookmarks() {
  emulate -L zsh
  if [[ -n "${hook_com[hg-active-bookmark]}" ]]; then
    hook_com[hg-bookmark-string]="${(Mj:,:)@}"
    ret=1
  fi
}

function +vi-hg-message() {
  emulate -L zsh

  # Suppress hg branch display if we can display a bookmark instead.
  if [[ -n "${hook_com[misc]}" ]]; then
    hook_com[branch]=''
  fi
  return 0
}

function +vi-git-untracked() {
  emulate -L zsh
  if [[ -n $(git ls-files --exclude-standard --others 2> /dev/null) ]]; then
    hook_com[unstaged]+="%F{blue}●%f"
  fi
}

RPROMPT_BASE="\${vcs_info_msg_0_}%F{blue}%(4~|.../%3~|%~)%f"
# RPROMPT_BASE="\${vcs_info_msg_0_}%(4~|.../%3~|%~)"
setopt PROMPT_SUBST

# Anonymous function to avoid leaking variables.
function () {
  local SUFFIX=$(printf '%%F{red}λ%%f')
  # export PS1="%B${SUFFIX}%b%F{yellow}%B%(1j.*.)%(?..!)%b%f "
  # export PS1="%F{green}${SSH_TTY:+%n@%m}%f${SSH_TTY:+:}%B%F{#2222DD}%1~%F{yellow}%(1j.*.)%(?..!)%f${SUFFIX}%b "
  export PS1="%F{green}${SSH_TTY:+%n@%m}%f${SSH_TTY:+:}%F{blue}%1~%F{yellow}%(1j.*.)%(?..!)%f${SUFFIX} "
  # export PS1="%F{green}${SSH_TTY:+%n@%m}%f${SSH_TTY:+:}%F{blue}%1~%F{yellow}%(1j.*.)%(?..!)%f${SUFFIX} "
  if [[ -n "$TMUXING" ]]; then
    # Outside tmux, ZLE_RPROMPT_INDENT ends up eating the space after PS1, and
    # prompt still gets corrupted even if we add an extra space to compensate.
    export ZLE_RPROMPT_INDENT=0
  fi
}

export RPROMPT=$RPROMPT_BASE

#
# Options
#

setopt AUTO_CD                 # [default] .. is shortcut for cd .. (etc)
setopt AUTO_PARAM_SLASH        # tab completing directory appends a slash
setopt AUTO_PUSHD              # [default] cd automatically pushes old dir onto dir stack
setopt AUTO_RESUME             # allow simple commands to resume backgrounded jobs
setopt CLOBBER                 # allow clobbering with >, no need to use >!
setopt IGNORE_EOF              # [default] prevent accidental C-d from exiting shell
setopt INTERACTIVE_COMMENTS    # [default] allow comments, even in interactive shells
setopt LIST_PACKED             # make completion lists more densely packed
setopt MENU_COMPLETE           # auto-insert first possible ambiguous completion
setopt NO_NOMATCH              # [default] unmatched patterns are left unchanged
setopt PRINT_EXIT_VALUE        # [default] for non-zero exit status
setopt PUSHD_IGNORE_DUPS       # don't push multiple copies of same dir onto stack
setopt PUSHD_SILENT            # [default] don't print dir stack after pushing/popping

# Bindings

bindkey -e # vi bindings, set -e to emacs bindings
bindkey 'jk' vi-cmd-mode

# Use "cbt" capability ("back_tab", as per `man terminfo`), if we have it:
if tput cbt &> /dev/null; then
  bindkey "$(tput cbt)" reverse-menu-complete # make Shift-tab go to previous completion
fi

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "\e[A" history-beginning-search-backward-end  # cursor up
bindkey "\e[B" history-beginning-search-forward-end   # cursor down

autoload -U select-word-style
select-word-style bash # only alphanumeric chars are considered WORDCHARS

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^x' edit-command-line

bindkey ' ' magic-space # do history expansion on space

# Replace standard history-incremental-search-{backward,forward} bindings.
# These are the same but permit patterns (eg. a*b) to be used.
bindkey "^r" history-incremental-pattern-search-backward
bindkey "^s" history-incremental-pattern-search-forward

# Make CTRL-Z background things and unbackground them.
function fg-bg() {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
  else
    zle push-input
  fi
}
zle -N fg-bg
bindkey '^Z' fg-bg

#
# Hooks
#

autoload -U add-zsh-hook

typeset -F SECONDS
function -record-start-time() {
  emulate -L zsh
  ZSH_START_TIME=${ZSH_START_TIME:-$SECONDS}
}
add-zsh-hook preexec -record-start-time

function -report-start-time() {
  emulate -L zsh
  if [ $ZSH_START_TIME ]; then
    local DELTA=$(($SECONDS - $ZSH_START_TIME))
    local DAYS=$((~~($DELTA / 86400)))
    local HOURS=$((~~(($DELTA - $DAYS * 86400) / 3600)))
    local MINUTES=$((~~(($DELTA - $DAYS * 86400 - $HOURS * 3600) / 60)))
    local SECS=$(($DELTA - $DAYS * 86400 - $HOURS * 3600 - $MINUTES * 60))
    local ELAPSED=''
    test "$DAYS" != '0' && ELAPSED="${DAYS}d"
    test "$HOURS" != '0' && ELAPSED="${ELAPSED}${HOURS}h"
    test "$MINUTES" != '0' && ELAPSED="${ELAPSED}${MINUTES}m"
    if [ "$ELAPSED" = '' ]; then
      SECS="$(print -f "%.2f" $SECS)s"
    elif [ "$DAYS" != '0' ]; then
      SECS=''
    else
      SECS="$((~~$SECS))s"
    fi
    ELAPSED="${ELAPSED}${SECS}"
    if [[ -n "$TMUX" ]]; then
        export RPROMPT="%F{cyan}%{$__TABLE[ITALIC_ON]%}${ELAPSED}%{$__TABLE[ITALIC_OFF]%}%f $RPROMPT_BASE %F{yellow}[T]%f"
    else
        export RPROMPT="%F{cyan}%{$__TABLE[ITALIC_ON]%}${ELAPSED}%{$__TABLE[ITALIC_OFF]%}%f $RPROMPT_BASE "
    fi
    unset ZSH_START_TIME
  else
    if [[ -n "$TMUX" ]]; then
        export RPROMPT="$RPROMPT_BASE %F{yellow}[T]%f"
    else
        export RPROMPT="$RPROMPT_BASE"
    fi
  fi
}
add-zsh-hook precmd -report-start-time

function -auto-ls-after-cd() {
  emulate -L zsh
  # Only in response to a user-initiated `cd`, not indirectly (eg. via another
  # function).
  if [ "$ZSH_EVAL_CONTEXT" = "toplevel:shfunc" ]; then
    ls --color=always
  fi
}
add-zsh-hook chpwd -auto-ls-after-cd

# Remember each command we run.
function -record-command() {
  __TABLE[LAST_COMMAND]="$2"
}
add-zsh-hook preexec -record-command

# Update vcs_info (slow) after any command that probably changed it.
function -maybe-show-vcs-info() {
  local LAST="$__TABLE[LAST_COMMAND]"

  # In case user just hit enter, overwrite LAST_COMMAND, because preexec
  # won't run and it will otherwise linger.
  __TABLE[LAST_COMMAND]="<unset>"

  # Check first word; via:
  # http://tim.vanwerkhoven.org/post/2012/10/28/ZSH/Bash-string-manipulation
  case "$LAST[(w)1]" in
    cd|cp|git|rm|touch|mv|:)
      vcs_info
      ;;
    *)
      ;;
  esac
}

add-zsh-hook precmd -maybe-show-vcs-info

# adds `cdr` command for navigating to recent directories
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# enable menu-style completion for cdr
zstyle ':completion:*:*:cdr:*:*' menu selection

# fall through to cd if cdr is passed a non-recent dir as an argument
zstyle ':chpwd:*' recent-dirs-default true

# Uncomment this to get syntax highlighting:
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# autosuggestions (has medium overhead)
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# History Configuration
HISTSIZE=5000               # How many lines of history to keep in memory
HISTFILE=~/.zsh_history     # Where to save history to disk
SAVEHIST=5000               # Number of history entries to save to disk
HISTDUP=erase               # Erase duplicates in the history file
setopt    appendhistory     # Append history to the history file (no overwriting)
setopt    sharehistory      # Share history across terminals
setopt    incappendhistory  # Immediately append to the history file, not just when a term is killed


stty -ixon

alias :e=vim
alias :q=exit
alias :sp='test -n "$TMUX" && tmux split-window'
alias :vs='test -n "$TMUX" && tmux split-window -h'

alias vim="nvim"
# alias vi="nvim -u ~/.minimal_vimrc" # without vimrc
alias ls="ls --color=always"
alias ll="ls -l"
alias fd="fdfind"

alias vrc="vim ~/.config/nvim/init.lua"
alias zrc="vim ~/.zshrc"
alias gco="git checkout"

alias noop=":"

PATH=$PATH:/bin/
PATH=$PATH:$HOME/bin/
PATH=$PATH:$HOME/.zsh/bin/
PATH=$PATH:$HOME/.vim/pack/bundle/opt/vcs-jump/bin/
PATH=$PATH:/usr/local/bin/
PATH=$PATH:/usr/local/sbin/
PATH=$PATH:$HOME/.local/bin/
PATH=$PATH:$HOME/.cargo/bin/
PATH=$PATH:/usr/sbin/
PATH=$PATH:$HOME/.cabal/bin/
PATH=$PATH:/usr/local/
PATH=$PATH:$HOME/.local/bin/
PATH=$PATH:$HOME/.cabal/bin/
PATH=$PATH:$HOME/.ghcup/bin/
PATH=$PATH:$HOME/Tools/nvim-linux64/bin/
PATH=$PATH:$HOME/.elan/bin/
PATH=$PATH:/opt/homebrew/bin/
PATH=$PATH:/Library/TeX/texbin/
export PATH

# echo -e -n "\e[4 q"
# blinking bar cursor
# echo -e -n "\x1b[\x35 q"

fzf_history_search() {
  setopt extendedglob
  candidates=(${(f)"$(fc -li -1 0 | fzf +s +m -x -e -q "$BUFFER")"})
  local ret=$?
  if [ -n "$candidates" ]; then
    BUFFER="${candidates[@]/(#m)*/${${(As: :)MATCH}[4,-1]}}"
    BUFFER="${BUFFER[@]/(#b)(?)\\n/$match[1]
}"
    zle vi-fetch-history -n $BUFFER
  fi
  zle reset-prompt
  return $ret
}

autoload fzf_history_search
zle -N fzf_history_search

bindkey '^r' fzf_history_search

fzf_project() {
    project=$(echo "imitator\ncduce\ntiny-sat\nprism\nlean-smt\neditor\nlam\nhighlight-lean" | fzf-tmux)
    case $project in
        imitator)
            cd ~/Projects/CNRS/LIPN/imitator/
        ;;
        cduce)
            cd ~/Projects/CNRS/LMF/cduce/
        ;;
        prism)
            cd ~/Projects/prism/
        ;;
        lean-smt)
            cd ~/Projects/lean-smt/
        ;;
        editor)
            cd ~/Projects/editor/
        ;;
        lam)
            cd ~/Projects/lam/
        ;;
        highlight-lean)
            cd ~/Vault/Vault/.obsidian/plugins/obsidian-sample-plugin/
        ;;
        tiny-sat)
            cd ~/Projects/ocaml/TinySAT/
        ;;
    esac
    zle reset-prompt
}

autoload fzf_project
zle -N fzf_project

bindkey '^p' fzf_project

fzf_switch_session() {
    session_names=()

    sessions=$(tmux ls)
    tmp=$IFS
    IFS=$'\n'
    for session in $sessions; do
        session_name=$(echo -n $session | cut -d: -f1)
        session_names+=($session_name)
    done
    IFS=$tmp

    chosen_session=$(echo $session_names | fzf-tmux)

    tmux switch -t $chosen_session
}

autoload fzf_switch_session
zle -N fzf_switch_session

bindkey '^t' fzf_switch_session

fzf_find_dir() {
    dirs=$(find . -type d)
    chosen_dir=$(echo $dirs | fzf-tmux)
    if [ -n "$chosen_dir" ]; then
        cd $chosen_dir
        zle reset-prompt
    fi
}

autoload fzf_find_dir
zle -N fzf_find_dir

bindkey '^o' fzf_find_dir

function get_lean () {
    cvc5 $1 --lang=smt --dag-thresh=0 --dump-proofs --proof-granularity=theory-rewrite --proof-format=lean --enum-inst
}

SUDO_EDITOR=/home/tomazgomes/Tools/nvim-linux64/bin//nvim
export SUDO_EDITOR
export EDITOR="nvim"
export BROWSER="firefox"

# zprof > /tmp/foo

[ -f "/Users/tmascarenhas/.ghcup/env" ] && source "/Users/tmascarenhas/.ghcup/env" # ghcup-env
[[ ! -r /Users/tmascarenhas/.opam/opam-init/init.zsh ]] || source /Users/tmascarenhas/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
