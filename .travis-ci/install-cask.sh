#!/bin/bash -x

# Install cask for Travis CI
# or if already installed, then check for updates

WORKDIR=${HOME}/local
CASKDIR=$WORKDIR/cask
SCRIPTDIR=`dirname $(readlink -f $0)`

. $SCRIPTDIR/retry.sh

cask_upgrade_cask_or_reset() {
    cask upgrade-cask || { rm -rf $HOME/.emacs.d/.cask && false; }
}

cask_install_or_reset() {
    cask install || { rm -rf .cask && false; }
}

# Bootstrap the cask tool and its dependencies
if [ -d $CASKDIR ]
then
    travis_retry cask_upgrade_cask_or_reset
else
    git clone https://github.com/cask/cask.git $CASKDIR
    travis_retry cask_upgrade_cask_or_reset
fi
