# The environment of the Windows job of the CI (see `.gitlab-ci.yml`),
# dot-sourced by the job (PowerShell 7): the runner (tag `windows`) is a
# shell executor with Git for Windows, but without OCaml.
#
# The first job installs opam (the Windows binary), an opam root with its
# own Cygwin (which provides the mingw-w64 C toolchain and binutils) and a
# switch with the dependencies of camljit in `C:\ci` (`$env:CI_WINDOWS_DIR`,
# shared with the other projects that use the runner: one switch per project
# and version of OCaml); the next jobs reuse them, and only install the
# dependencies that changed. The jobs that use the switch are serialized by
# `resource_group` in `.gitlab-ci.yml`.

$ErrorActionPreference = 'Stop'
# The failures of the external commands stop the script too.
$PSNativeCommandUseErrorActionPreference = $true

$ocamlVersion = if ($env:OCAML_VERSION) { $env:OCAML_VERSION } else { '5.3' }
$opamVersion = '2.6.1'
$ciDir = if ($env:CI_WINDOWS_DIR) { $env:CI_WINDOWS_DIR } else { 'C:\ci' }
$switch = "camljit-$ocamlVersion"

$env:OPAMROOT = "$ciDir\opam"
$env:OPAMYES = '1'
$env:OPAMCONFIRMLEVEL = 'unsafe-yes'
# The mingw-w64 C compiler and binutils (x86_64-w64-mingw32-gcc, -as, ...)
# are in the Cygwin of opam, which `opam env` does not add to PATH.
$env:PATH = "$ciDir\bin;$env:OPAMROOT\.cygwin\root\bin;$env:PATH"

New-Item -ItemType Directory -Force "$ciDir\bin" | Out-Null
if (-not (Test-Path "$ciDir\bin\opam.exe")) {
  Invoke-WebRequest -OutFile "$ciDir\bin\opam.exe" `
    "https://github.com/ocaml/opam/releases/download/$opamVersion/opam-$opamVersion-x86_64-windows.exe"
}

if (-not (Test-Path "$env:OPAMROOT\config")) {
  opam init --bare --disable-sandboxing --no-setup --cygwin-internal-install
}
if (-not ((opam switch list --short) -contains $switch)) {
  opam switch create $switch "ocaml-base-compiler.$ocamlVersion.0"
}
(& opam env --switch=$switch --set-switch) -split '\r?\n' |
  ForEach-Object { Invoke-Expression $_ }

opam install --deps-only --with-test .
