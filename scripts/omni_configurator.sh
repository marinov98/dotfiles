copy_dotfiles_to_home() {
  echo "Resolving shell configs..."
  echo "Certain shell configurations will be copied based on the system"
  if [[ $(uname -s) == Linux ]] || [[ $(uname -s) == Darwin ]]
  then
    echo "machine found to be linux or Mac"
    mkdir -p ~/.config
    echo "copying config..."
    cp -r xdg_config/* ~/.config/
    echo "creating '.ignore' from fzyIgnore..."
    mv ~/.config/fzyIgnore ~/.ignore
    echo "Finished setting up config"

    echo "detecting shell and copying shell config..."
    case "$SHELL" in
      */zsh)
        mv ~/.config/shell/zshrc ~/.zshrc
        mv ~/.config/shell/zprofile ~/.zprofile

        mkdir -p ~/.zsh/plugins/
        curl -o ~/.zsh/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
        curl -o ~/.zsh/_git https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
        echo "zsh config copied"
        ;;
      */bash)
        mv ~/.config/shell/bashrc ~/.bashrc
        mv ~/.config/shell/bash_profile ~/.bash_profile
        curl -o ~/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
        echo "bash config copied"
        ;;
      *)
        echo "Unsupported shell: $SHELL"
        ;;
    esac
    rm -rf ~/.config/shell/bashrc
    rm -rf ~/.config/shell/bash_profile
    rm -rf ~/.config/shell/zshrc
    rm -rf ~/.config/shell/zprofile
    echo "shell done"
  else
    echo "Machine found to not be linux or Mac will not copy config!"
  fi

  # FORMATTERS
  echo "Copying formatters..."
  cp code-formatters/.clang-format ~/
  cp code-formatters/.prettierrc ~/
}

