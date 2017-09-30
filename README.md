# dotfiles
## Personal configuration for xmonad, dzen, zsh and others.

## Dependencies

* xmonad

* xfce4

* xfce4-power-manager

* dzen2

* conky

* the_silver_searcher

* urxvt

* slock

* xsel

* xclip

* curl

* xstarter: cmake, glib

* tmux && tmux-themepack

* zsh && oh-my-zsh

* viewnior

* scrot

* https://gist.githubusercontent.com/wandernauta/6800547/raw/2c2ad0f3849b1b1cd1116b80718d986f1c1e7966/sp

* wicd wicd-curses

git clone https://github.com/lchsk/emacs.d ~/.emacs.d

cp -r ~/dotfiles/.fonts ~/

cp .Xresources .zshrc .lchsk_zsh .xinitrc .tmux.conf ~/

Change user name in .Xresources  /.zshrc slock.service

xrdb -merge ~/.Xresources

chsh -s `which zsh`
