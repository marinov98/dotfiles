initialize_zsh() {
  mkdir -p ~/.zsh/plugins/

  # Download the scripts 
  curl -o ~/.zsh/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
  curl -o ~/.zsh/_git https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
}

# TODO: maybe add variables and flags to combine into one function
sync_zsh() {
  cp shell/zsh/.zshrc ~/.zshrc
}

sync_zsh_alt() {
  cp ~/.zshrc shell/zsh/.zshrc
}
