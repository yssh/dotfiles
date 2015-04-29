#
# .zshrc | Coding: uft-8
#

# --------------------------------------
# General Settings
# --------------------------------------
# Variables
case ${OSTYPE} in
    darwin*)
        export PATH=/usr/local/bin:$PATH
        export MANPATH=/usr/local/share/man:$MANPATH
        ;;
    linux*)
        export PATH=$HOME/.linuxbrew/bin:$PATH
        export MANPATH=$HOME/.linuxbrew/share/man:$MANPATH
        export INFOPATH=$HOME/.linuxbrew/share/info:$INFOPATH
        export LD_LIBRARY_PATH=$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH
        ;;
esac

export LANG=ja_JP.UTF-8
export PAGER=lv

bindkey -e                                               # キーバインドをemacsモードに

setopt no_beep                                           # ビープ音無効化
setopt auto_cd                                           # ディレクトリ名だけでcd
setopt auto_pushd                                        # 自動でpush
setopt pushd_ignore_dups                                 # ディレクトリスタックに重複するものは古い方を削除
setopt print_eight_bit                                   # 補完候補リストの日本語を適正表示
# setopt correct                                           # コマンドのスペル訂正

### Complement ###
case ${OSTYPE} in
    darwin*)
        fpath=(/usr/local/share/zsh-completions $fpath)
        ;;
    linux*)
        fpath=($HOME/.linuxbrew/share/zsh-completions $fpath)
        ;;
esac
autoload -U compinit; compinit                           # 補完機能有効化
setopt list_packed                                       # 補完候補を詰めて表示
setopt list_types                                        # 補完候補一覧でファイルの種別を識別マーク表示 (ls -F の記号)
setopt magic_equal_subst                                 # コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt always_last_prompt                                # カーソル位置は保持したままファイル名一覧を順次その場で表示

bindkey "^[[Z" reverse-menu-complete                     # Shift-Tabで補完候補を逆順

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'      # 補完時に大文字小文字を区別しない
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}    # 補完候補の色付け
zstyle ':completion:*:default' menu select=2             # 補完候補のカーソル選択有効化
zstyle ':completion:*:processes' menu yes select=2
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'                       # Ctrl-Wでカーソル前の1単語を削除したとき、/までで削除を止める

### Glob ###
setopt extended_glob                                     # グロブ機能を拡張
# setopt globdots                                          # .からはじまるファイルをマッチ
unsetopt caseglob                                        # ファイルグロブで大文字小文字を区別しない

### History ###
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_all_dups                              # 重複するコマンド行は古い方を削除
setopt hist_reduce_blanks                                # 余分なスペースを削除してヒストリに保存

bindkey '^p' history-beginning-search-backward
bindkey '^n' history-beginning-search-forward


# --------------------------------------
# Look & Feel Settings
# --------------------------------------
# VCS
autoload -Uz vcs_info

# Terminal Title
case "${TERM}" in
kterm*|xterm*)
    precmd() {
        echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
        vcs_info
    }
    ;;
esac

# Prompt
setopt prompt_subst
PROMPT='[%n@%m %C]$ '
RPROMPT='${vcs_info_msg_0_}'


# --------------------------------------
# Other Settings
# --------------------------------------
# Aliases
case ${OSTYPE} in
    darwin*)
        alias ls="ls -FG"
        alias lsa="ls -FGa"
        alias lst="ls -lG"
        alias lsta="ls -lGa"
        alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
        alias vim="/usr/local/bin/vim"
        ;;
    linux*)
        alias ls="ls -F --color"
        alias lsa="ls -Fa --color"
        alias lst="ls -l --color"
        alias lsta="ls -la --color"
        ;;
esac

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias mkdir="mkdir -p"

# Global Aliases
alias -g G='| grep'
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g S='| sort'
alias -g W='| wc'
alias -g X='| xargs'

# rbenv
if which rbenv > /dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

# pyenv & virtualenvwrapper
if which pyenv > /dev/null 2>&1; then
    eval "$(pyenv init -)"

    if pip > /dev/null 2>&1; then
        pyenv virtualenvwrapper
    fi
fi

# nodenv
if which nodenv > /dev/null 2>&1; then
    eval "$(nodenv init -)"
fi

# direnv
if which direnv > /dev/null 2>&1; then
    eval "$(direnv hook zsh)"
fi
