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

* viewnior && gwenview

* scrot

* https://gist.githubusercontent.com/wandernauta/6800547/raw/2c2ad0f3849b1b1cd1116b80718d986f1c1e7966/sp

* sudo pacman -S wpa_supplicant wireless_tools networkmanager network-manager-applet gnome-keyring stalonetray feh

* enable/start wpa_supplicant.service NetworkManager.service

--
* wicd wicd-curses

git clone https://github.com/lchsk/emacs.d ~/.emacs.d

cp -r ~/dotfiles/.fonts ~/

cp .Xresources .zshrc .lchsk_zsh .xinitrc .tmux.conf ~/

Change user name in .Xresources

/.zshrc

Modify username and path:
/etc/systemd/system/slock.service
/etc/systemd/system/sleep.target.wants/slock.service
slock.service

xrdb -merge ~/.Xresources

chsh -s `which zsh`
