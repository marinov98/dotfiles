NVIM_CONFIG_PREFIX=nvim/lua/marinvim

nvim_sync_plugins() {
  cp -r config/$NVIM_CONFIG_PREFIX/plugins/* ~/.config/$NVIM_CONFIG_PREFIX/plugins/
}

nvim_sync_settings() {
  cp -r config/$NVIM_CONFIG_PREFIX/preferences/* ~/.config/$NVIM_CONFIG_PREFIX/preferences/
}
