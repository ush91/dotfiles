# $ZDOTDIR/.zshrc

## /etc/zshenv ##
# if [[ -d "$HOME"/.zsh.d ]]
# then
#     ZDOTDIR="$HOME"/.zsh.d/
# fi

##日本語
export LANG=ja_JP.UTF-8 #UTF-8
setopt print_eight_bit  #ファイル名

##エイリアス
alias ls="ls -F"
alias la="ls -a"

##補完機能
autoload -Uz compinit
compinit                     #有効化
setopt correct						#もしかして:
setopt auto_list                                    #一覧
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' #大文字区別
bindkey "[Z" reverse-menu-complete                #Shift-Tabで補完逆行

##ディレクトリ
function chpwd() {ls} #cdしたらls
# setopt auto_cd                     #ディレクトリ名だけでcd
setopt auto_pushd                  #cdでpushd

##ヒストリ
HISTFILE=$ZDOTDIR/zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups   #直前のダブりは省く
setopt hist_ignore_space  #先頭スペースで保存しない
setopt hist_reduce_blanks #余分なスペースを省く
setopt share_history      #履歴共有

##zsh-notify
source ~/.zsh.d/zsh-notify/notify.plugin.zsh

##プロンプト
PROMPT="
%F{white}%n [%m]
%(!.#.$) %f"
#%F{white}%n%f:%F{white}%m%f%(!.#.$) "
RPROMPT="%F{red}[%~]%f"

# 右プロンプトを消す
setopt transient_rprompt

##タイトル
case "${TERM}" in
    xterm* | kterm* )
precmd () {
  echo -ne "\e]1;${PWD:t}\a"
  echo -ne "\e]2;${PWD}\a"

}

#precmd() {
#    echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
#};

preexec () {
#echo -ne "\ek${(s: :)${1}}[1]\e\\"
#echo -ne "\e]0;${(s: :)1[1]}\a"

  echo -ne "\e]1;${1%% *}\a"
}

;;
esac
