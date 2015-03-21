# rbenv
if which rbenv > /dev/null; then
  eval "$(rbenv init -)"
fi

# pyenv
if which pyenv > /dev/null; then
  eval "$(pyenv init -)";
fi

# bash-completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Load .bashrc
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi
