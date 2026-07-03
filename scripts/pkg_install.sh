#!/bin/bash
#
# Generic package installer. Detects the OS/package manager once,
# then walks scripts/packages.conf installing one package at a time
# (so a single bad/missing name doesn't abort the whole batch --
# apt, pacman, and brew all fail closed on unknown package names).

set -uo pipefail

PKG_MANAGER=""
INSTALL_CMD=""

# --- bootstrap ---------------------------------------------------------

bootstrap_brew() {
  if ! command -v brew &>/dev/null; then
    echo "Homebrew not found, installing..."
    xcode-select --install 2>/dev/null || true
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  fi
}

# --- detection -----------------------------------------------------------

detect_package_manager() {
  if [[ $(uname -s) == "Darwin" ]]; then
    PKG_MANAGER="brew"
    bootstrap_brew
  elif [[ $(uname -s) == "Linux" && -f /etc/os-release ]]; then
    source /etc/os-release
    case "$ID" in
      ubuntu|debian) PKG_MANAGER="apt" ;;
      arch)          PKG_MANAGER="pacman" ;;
      fedora)        PKG_MANAGER="dnf" ;;
      *)
        echo "Unsupported Linux distribution: $ID" >&2
        exit 1
        ;;
    esac
  else
    echo "Unsupported OS: $(uname -s)" >&2
    exit 1
  fi

  case "$PKG_MANAGER" in
    brew)
      INSTALL_CMD="brew install"
      ;;
    apt)
      INSTALL_CMD="sudo apt install -y"
      sudo apt update
      ;;
    pacman)
      INSTALL_CMD="sudo pacman -S --noconfirm"
      sudo pacman -Sy
      ;;
    dnf)
      INSTALL_CMD="sudo dnf install -y"
      ;;
  esac

  echo "Detected package manager: $PKG_MANAGER"
}

# --- name resolution -------------------------------------------------------

# Resolves one packages.conf line to the right name for $PKG_MANAGER.
resolve_name() {
  local line="$1"

  if [[ "$line" != *:* ]]; then
    echo "$line"
    return
  fi

  local name brew pacman dnf apt
  IFS=':' read -r name brew pacman dnf apt <<< "$line"

  case "$PKG_MANAGER" in
    brew)   echo "${brew:-$name}" ;;
    pacman) echo "${pacman:-$name}" ;;
    dnf)    echo "${dnf:-$name}" ;;
    apt)    echo "${apt:-$name}" ;;
  esac
}

# --- install loop -----------------------------------------------------

install_packages() {
  local conf_file="$1"
  local failed=()
  local pkg

  while IFS= read -r line; do
    [[ -z "$line" || "$line" == \#* ]] && continue

    pkg=$(resolve_name "$line")
    echo "Installing $pkg..."
    if ! $INSTALL_CMD "$pkg"; then
      echo "failed to install: $pkg (from '$line')" >&2
      failed+=("$pkg")
    fi
  done < "$conf_file"

  if [[ ${#failed[@]} -gt 0 ]]; then
    echo ""
    echo "The following packages failed to install -- you'll need to handle these manually:"
    printf '  - %s\n' "${failed[@]}"
  fi
}

pkg_install_all() {
  local conf_file
  conf_file="$(dirname "${BASH_SOURCE[0]}")/packages.conf"

  detect_package_manager
  install_packages "$conf_file"
}
