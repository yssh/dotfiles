# definition of environmental variables
PATH=/usr/local/bin:$PATH
MANPATH=/usr/local/share/man:$MANPATH
LANG=ja_JP.UTF-8
HISTCONTROL=ignoreboth

export PATH MANPATH LANG HISTCONTROL

# prompt
PS1="[\u@\h \W]\\$ "

# definition of aliases
alias ls="ls -FG"
alias lsa="ls -FGa"
alias lst="ls -lG"
alias lsta="ls -lGa"
alias mv="mv -i"
alias cp="cp -i"
alias rm="rm -i"
alias rmm="rm -i *~; rm -i .*~"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias remem='du -sx / &> /dev/null & sleep 25 && kill $!'
alias be='bundle exec'
