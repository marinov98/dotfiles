# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022



# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

#feh --randomize --bg-fill ~/Pictures/redgalaxy.jpg

################################
##### KEYBOARD MODIFICATIONS
################################

# The commands below make caps lock act as both esc and ctrl
# they also make the old ctrl caps lock I swap instead of 
# completly eliminating caps lock because sometimes it would
# turn on and there would be no way to turn it back on
# If you ARE RUNNING i3wn these lines should not need to be uncommented 
# if you are encountering a problem where caps lock turns on and there is no way to turn it back on then uncomment the first line
# if you are running a GNOME desktop environment and NOT i3wn then only the second line needs to be uncommented since 
# you can get caps to act like control using gnome-tweak-tool
# Any other desktop environment without i3 would require both lines to be uncommented

# setxkbmap -option "ctrl:swapcaps"

# xcape -e '#66=Escape'




