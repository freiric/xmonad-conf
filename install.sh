sudo apt install xmonad haskell-stack
# toolbar
sudo apt install trayer
# power mnanager icon tool
sudo apt install xfce4-power-manager
# volume icon tool
sudo apt install volumeicon-alsa
# image viewer for dekstop background
sudo apt install feh
sudo apt install conky
sudo apt install libiw-dev
cd .xmonad
# From inside ~/.xmonad.
git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
# git clone "https://github.com/jaor/xmobar" xmobar-git
stack init
stack install
