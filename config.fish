alias :e=vim
alias :q=exit
alias :sp='test -n "$TMUX" && tmux split-window'
alias :vs='test -n "$TMUX" && tmux split-window -h'

abbr vim "nvim"
abbr t "tmux"
abbr v "nvim"

abbr gco "git checkout"

abbr sai "sudo apt install"
abbr sar "sudo apt remove"

abbr fsc "nvim ~/.config/fish/config.fish"
abbr spc "nvim ~/.spectrwm.conf"
abbr vrc "nvim ~/Tom/dotfiles/.vimrc"
abbr zrc "nvim ~/.zshrc"


function git_is_repo -d "Check if directory is a repository"
  test -d .git; or command git rev-parse --git-dir >/dev/null 2> /dev/null
end

function git_is_dirty -d "Check if there are changes to tracked files"
  git_is_repo; and not command git diff --no-ext-diff --quiet --exit-code
end

function git_is_staged -d "Check if repo has staged changes"
  git_is_repo; and begin
    not command git diff --cached --no-ext-diff --quiet --exit-code
  end
end

function git_is_touched -d "Check if repo has any changes"
  git_is_repo; and begin
    # The first checks for staged changes, the second for unstaged ones.
    # We put them in this order because checking staged changes is *fast*.  
    not command git diff-index --cached --quiet HEAD -- >/dev/null 2>&1
    or not command git diff --no-ext-diff --quiet --exit-code >/dev/null 2>&1
  end
end

function git_branch_name -d "Get current branch name"
  git_is_repo; and begin
    command git symbolic-ref --short HEAD 2> /dev/null;
      or command git show-ref --head -s --abbrev | head -n1 2> /dev/null
  end
end

function fish_greeting
end

function fish_title
    echo Fish
end

function my_round                                        
    set x $argv[1]
    set hundred_x (math "$x*100")
    set r_hundred_x (math "floor($hundred_x)")
    math "$r_hundred_x/100"
end

function fish_prompt
    set -l last_command_status $status

    set -l symbol '$'

    set -l normal_color (set_color normal)
    set -l symbol_color (set_color red -o)
    set -l error_color (set_color yellow -o)

    if test $last_command_status -eq 0
        echo -n -s $symbol_color $symbol " " $normal_color
    else
        echo -n -s $symbol_color $symbol $error_color "! " $normal_color
    end
end


function fish_right_prompt
    set -l cwd
    set -l cwd_color (set_color blue)
    set -l symbol_color (set_color blue)

    set -l normal_color (set_color normal)
    set -l branch_color (set_color yellow)
    set -l meta_color (set_color red)
    set -l symbol_color (set_color blue -o)
    set -l error_color (set_color red -o)

    if git_is_repo
        echo -n -s $branch_color \{(git_branch_name)
        # set -l git_meta ""
        echo $meta_color
        if test (command git ls-files --others --exclude-standard | wc -w 2> /dev/null) -gt 0
            # set git_meta "$git_meta?"
            echo -n -s \?
        end
        if test (command git rev-list --walk-reflogs --count refs/stash 2> /dev/null)
            # set git_meta "$git_meta\$"
            echo -n -s \$
        end
        if git_is_touched
            git_is_dirty && echo -n -s (set_color red) ●
            git_is_staged && echo -n -s (set_color green) ●
            # git_is_dirty && set git_meta "$git_meta●"
            # git_is_staged && set git_meta "$git_meta●"
        end
        set -l commit_count (command git rev-list --count --left-right (git remote)/(git_branch_name)"...HEAD" 2> /dev/null)
        if test $commit_count
            set -l behind (echo $commit_count | cut -f 1)
            set -l ahead (echo $commit_count | cut -f 2)
            if test $behind -gt 0
                # set git_meta "$git_meta🠋"
                echo -n -s 🠋
            end
            if test $ahead -gt 0
                # set git_meta "$git_meta🠉"
                echo -n -s 🠉
            end
        end
        # if test $git_meta
        #     echo -n -s $meta_color $git_meta
        # end 
        echo -n -s $branch_color \} $normal_color " "
    end

    set cwd (prompt_pwd)
    echo -n -s $cwd_color "$cwd"
    # set_color --dim

    set -l S (math $CMD_DURATION/1000)
    set -l M (math $S/60)

    echo -n -s " | "
    if test $M -gt 1
        echo -n -s (my_round $M) m
    else if test $S -gt 1
        echo -n -s (my_round $S) s
    else
        echo -n -s (my_round $CMD_DURATION) ms
    end
    set_color normal
end


