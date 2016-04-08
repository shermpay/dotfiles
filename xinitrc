#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap
xinitlog=$HOME/.xinit-log

# merge in defaults and keymaps

log() {
    echo "[$(date)] $1" >> "$xinitlog"
}

if [ ! -f "$xinitlog" ] ; then
    touch "$xinitlog"
fi
log "loading xinit"
 
if [ -f $sysresources ]; then
    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

log "loaded sys resources/modmap"

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi
 
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

log "loaded user resources/modmap"


if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?* ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# set the background
feh --no-fehbg --bg-fill ~/Pictures/wallpapers/current.jpg
xset r rate 150 50 -b # key repeat [rate] [delay] 
log "set keyboard repeat"

# start some nice programs
log "start xfdesktop"
xfce4-power-manager
log "start xfce4-power-manager"
supavolumed &
log "start supavolumed"
xscreensaver -no-splash &
log "start xscreensaver"
nm-applet &
log "start nm-applet"

if command -v redshift-gtk ; then
    redshift-gtk &
    log "start redshift-gtk"
fi


# twm &
# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x66+0+0 -name login
exec qtile
log "should not get here"
