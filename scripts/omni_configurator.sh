copy_dotfiles_to_home() {
  echo "Resolving shell configs..."
  echo "Certain shell configurations will be copied based on the system"
  if [[ $(uname -s) == Linux ]] || [[ $(uname -s) == Darwin ]]
  then
    echo "machine found to be linux or Mac"
    echo "copying config..."
    cp -r config/* ~/.config/
    echo "creating '.ignore' from fzyIgnore..."
    mv ~/.config/fzyIgnore ~/.ignore
    echo "Finished setting up config"

    echo "detecting shell and copying shell config..."
    case "$SHELL" in
      */zsh)
        mv ~/.config/shell/zshrc ~/.zshrc
        echo "zsh config copied"
        ;;
      */bash)
        mv ~/.config/shell/bashrc ~/.bashrc
        mv ~/.config/shell/bash_profile ~/.bash_profile
        echo "bash config copied"
        ;;
      *)
        echo "Unsupported shell: $SHELL"
        ;;
    esac
    echo "shell done"
  else
    echo "Machine found to not be linux or Mac will not copy config!"
  fi

  echo "copying git configs..."
  cp git/.gitconfig ~/

  # FORMATTERS
  echo "Copying formatters and tmux..."
  cp code-formatters/.clang-format ~/
  cp code-formatters/.prettierrc ~/
  cp tmux/.tmux.conf ~/
  echo "formatters & tmux done"

  setup_zsh_git_completion
}

setup_zsh_git_completion() {
  mkdir -p ~/.zsh/plugins/
  curl -o ~/.zsh/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
  curl -o ~/.zsh/_git https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
}