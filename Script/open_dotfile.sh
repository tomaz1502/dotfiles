#!/bin/bash

choice=`echo "zsh vim emacs doom tmux" | tr ' ' '\n' | fzf`

case $choice in
     zsh)
         nvim ~/.zshrc
     ;;
     vim)
         nvim ~/Desktop/dotfiles/Vim/.vimrc
     ;;
     emacs)
         nvim ~/.emacs.d/init.el
     ;;
     doom)
         nvim ~/.doom.d/config.el
     ;;
     tmux)
         nvim ~/.tmux.conf
     ;;
esac
