# -*- sh -*-

export LOCALPREFIX=${HOME}/Programs
export PATH="$HOME/.cabal/bin:$PATH:$LOCALPREFIX/bin:$HOME/Programs/android-sdk-linux/tools:$HOME/Programs/android-sdk-linux/platform-tools:$HOME/perl5/bin:$HOME/build/smlnj/bin:$HOME/go/bin:$HOME/.local/bin"
export PATH_="$PATH"

export LOCATE_PATH=$HOME/.locatedb
export GOPATH="$HOME/go"

export EDITOR=emacs
export VISUAL=$EDITOR

export WINEARCH=win32

export QT_GRAPHICSSYSTEM=raster

export BROWSER=firefox

export LIBRARY_PATH=$LOCALPREFIX/lib
export CPATH=$LOCALPREFIX/include
export LD_LIBRARY_PATH=$LOCALPREFIX/lib

# Perl stuff
export PERL_LOCAL_LIB_ROOT="/home/robert/perl5"
export PERL_MB_OPT="--install_base /home/robert/perl5"
export PERL_MM_OPT="INSTALL_BASE=/home/robert/perl5"
export PERL5LIB="/home/robert/perl5/lib/perl5/x86_64-linux-thread-multi:/home/robert/perl5/lib/perl5"

export O3D_OVERRIDE_RENDER_MODE=2D

[[ -e "~/.zprofile.d" ]] && source $HOME/.profile.d/*

export _JAVA_AWT_WM_NONREPARENTING=1

export PATSHOME=$LOCALPREFIX/lib/ats2-postiats-0.1.11

export MOZ_USE_XINPUT2=1

# Emacs Daemon
[[ -e "/tmp/emacs$UID/server" ]] || emacs --daemon

