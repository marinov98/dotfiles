copy_dotfiles_to_home() {
  echo "Resolving shell configs..."
  echo "Certain shell configurations will be copied based on the system"
  if [[ $(uname -s) == Linux ]] || [[ $(uname -s) == Darwin ]]
  then
      echo "machine found to be linux or Mac"
      echo "copying shell .."
      \cp shell/zsh/.zshrc ~/
      echo "shell done"
      echo "copying config..."
      \cp -r config/* ~/.config/
      echo "finished copying config"
  else
      echo "Machine found to not be linux or Mac will not copy config!"
  fi

  echo "copying git configs..."
  # cp git/git-completion.bash ~/
  cp git/.gitconfig ~/

  # FORMATTERS
  echo "Copying formatters and tmux..."
  cp code-formatters/.clang-format ~/
  cp code-formatters/.prettierrc ~/
  cp tmux/.tmux.conf ~/
  echo "formatters & tmux done"
}

