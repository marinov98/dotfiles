function Write-BranchName () {
    try {
        $branch = git rev-parse --abbrev-ref HEAD
        
        if ($branch -eq "HEAD") {
            $branch = git rev-parse --short HEAD
            Write-Host " ($branch)" -ForegroundColor "red"
        }
        else {
            Write-Host " ($branch)" -ForegroundColor "magenta"
        }
    } catch {
        Write-Host " (no branches yes)" -ForegroundColor "yellow"
    }
}

function prompt {
    $base = ""
    $path = "$($executionContext.SessionState.Path.CurrentLocation)"
    $userPrompt = "$(' >' * ($nestedPromptLevel + 1)) "
    
    Write-Host "`n$base" -NoNewline
    
    if (Test-Path .git) {
        Write-Host $path -NoNewline -ForegroundColor "green"
        Write-BranchName
    }
    else {
        Write-Host $path -ForegroundColor "green" -NoNewLine
    }
    
    return $userPrompt
}

Set-PSReadLineOption -Colors @{Operator = "DarkCyan"; Parameter = "Red"; Command = "Yellow"; String = "Green"}
