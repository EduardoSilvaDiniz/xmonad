#!/usr/bin/env bash
# [... default stuff that your distro may throw in here ...] #

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x5f5f5f --height 18 &

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr

# Set a nice background
feh --bg-fill --no-fehbg ~/Imagens/Wallpapers/just-a-girls.jpg
# Fire up screensaver
xscreensaver -no-splash &

# Power Management
xfce4-power-manager &

#Composer
xcompmgr &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi
