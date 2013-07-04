#!/usr/bin/zsh

if [ ! "$(pidof mpd)" ];
    then mpd;
fi

if [ ! "$(pidof mpdscribble)" ];
    then mpdscribble;
fi

if [ ! "$(pidof ibus-daemon)" ];
    then ibus-daemon --xim &
fi

if [ ! "$(pidof synergyc)" ];
    then echo; #synergyc shou.gnusokyo &
fi

if [[ $1 == "gnome-shell" ]]; then
    if [ ! "$(pidof xbindkeys)" ];
        then xbindkeys;
    fi
    exec gnome-session
elif [[ $1 == "openbox" ]]; then
    tint2 &
    exec openbox
else
    xsetroot -cursor_name left_ptr &
    setxkbmap us altgr-intl &
    xmodmap ~/.xmodmaprc &
    nitrogen --restore &
    #compton -cCGb -o 0.25 -r 12 &
    nm-applet &
    NetworkManager &
    exec ck-launch-session dbus-launch zsh -c "xmonad"
fi
