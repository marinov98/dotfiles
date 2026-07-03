#!/bin/bash

source scripts/pkg_install.sh
pkg_install_all

source scripts/omni_configurator.sh
copy_dotfiles_to_home
