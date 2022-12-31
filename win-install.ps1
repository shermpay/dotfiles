# List of packages to be installed.
$Packages = 'firefox', 'vlc', '7zip', 'vscode', `
  'everything', 'sharex', 'scansnapmanager', 'steam', 'discord', `
  'Revo.Uninstaller', 'battle.net'
# Packages that won't be installed and the user will be reminded of.
$Extras = "Windows Store apps, Oculus, AMD/Radeon Drivers"

# Get the ID and security principal of the current user account
$myWindowsID = [System.Security.Principal.WindowsIdentity]::GetCurrent()
$myWindowsPrincipal = new-object System.Security.Principal.WindowsPrincipal($myWindowsID)
 
# Get the security principal for the Administrator role
$adminRole = [System.Security.Principal.WindowsBuiltInRole]::Administrator
 
# Check to see if we are currently running "as Administrator"
if (!$myWindowsPrincipal.IsInRole($adminRole)) {
  # We are not running "as Administrator" - so relaunch as administrator
   
  # Create a new process object that starts PowerShell
  $newProcess = new-object System.Diagnostics.ProcessStartInfo "PowerShell";
   
  # Specify the current script path and name as a parameter
  $newProcess.Arguments = $myInvocation.MyCommand.Definition;
   
  # Indicate that the process should be elevated
  $newProcess.Verb = "runas";
   
  # Start the new process
  [System.Diagnostics.Process]::Start($newProcess);
   
  # Exit from the current, unelevated, process
  exit
}
 
# We're now in an elevated shell so we can begin Choco work.
function InstallChoco {
  Set-ExecutionPolicy Bypass -Scope Process -Force; 
  [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; 
  Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
}

# Install the packages without checking checksums.
function InstallPackages {
  ForEach ($PackageName in $Packages) {
    choco install $PackageName --ignorechecksum -y
  }
  Write-Host "You still need to install $Extras!"
  Exit
}

# Check if Choco is installed...if not then install it.
If (Test-Path -Path "$env:ProgramData\Chocolatey") {
  InstallPackages
}
Else {
  InstallChoco
  InstallPackages
}
