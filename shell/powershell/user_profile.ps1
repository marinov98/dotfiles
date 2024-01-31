# Modules
Import-Module posh-git

# PSReadonline
Set-PSReadLineOption -Colors @{Operator = "DarkCyan"; Parameter = "Red"; Command = "Yellow"; String = "Green"}
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None
Set-PSReadLineOption -PredictionSource History

# Alias
Set-Alias grep findstr
Set-Alias ll ls

# Utilities
function which ($command) {
  Get-Command -Name $command -ErrorAction SilentlyContinue |
    Select-Object -ExpandProperty Path -ErrorAction SilentlyContinue
}
