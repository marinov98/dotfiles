## THIS IS MEANT FOR MAC 


## Git integration
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
} 
 
PS1="\[$(tput bold)\]\n";
PS1+="\[$(tput setaf 39)\]$(whoami) ";        # blue  user
PS1+="\[$(tput setaf 148)\]at: "
PS1+="\[$(tput setaf 196)\]\W";   # red directories
PS1+="\[$(tput setaf 162)\]\$(parse_git_branch) >> "; #github integration
PS1+="\[$(tput sgr0)\]";
export PS1;


#   -----------------------------
#   2. MAKE TERMINAL BETTER
#   -----------------------------

alias ls='ls -GFh'
alias ls='ls -G'
alias ll='ls -lG'
alias cp='cp -iv'                           # Preferred 'cp' implementation
alias mv='mv -iv'                           # Preferred 'mv' implementation
alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
alias ll='ls -FGlAhp'                       # Preferred 'ls' implementation
alias less='less -FSRXc'                    # Preferred 'less' implementation

alias cd..='cd ../'                         # Go back 1 directory level (for fast typers)
alias ..='cd ../'                           # Go back 1 directory level
alias ...='cd ../../'                       # Go back 2 directory levels
alias .3='cd ../../../'                     # Go back 3 directory levels
alias .4='cd ../../../../'                  # Go back 4 directory levels
alias .5='cd ../../../../../'               # Go back 5 directory levels
alias .6='cd ../../../../../../'            # Go back 6 directory levels

alias v='vim'                               # v:            Opens any file in vim editor
alias rr='ranger'                           # ra:           Opens ranger
alias t='tmux'                              # t:            Opens tmux

alias get='sudo apt install'                # get:          UBUNTU: installs a specified package
alias rem='sudo apt remove'                 # rem:          UBUNTU: removes specified package        
alias purge='sudo apt purge'                # purge:        UBUNTU: purges specified package
alias p='sudo pacman'                       # p:            ARCH installed a specified package

alias f='open -a Finder ./'                 # f:            Opens current directory in MacOS Finder
alias ~="cd ~"                              # ~:            Go Home
alias c='code .'                            # c:            Open VS Code
alias which='type -all'                     # which:        Find executables
alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias show_options='shopt'                  # Show_options: display bash options settings
alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up
alias cic='set completion-ignore-case On'   # cic:          Make tab-completion case-insensitive

mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash

#### History Size
export HISTSIZE=10000
export HISTFILESIZE=120000

# Powerline
# function _update_ps1() {
#     PS1=$(powerline-shell $?)
# }

# if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
#     PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
# fi

neofetch
