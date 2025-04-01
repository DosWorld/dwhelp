  Hello! Here is a new help-system, which try to replace Norton Guide and
acts as unix-man.

  To meet this goals, dwhelp purpose:

* Simplified hyper-text. Minimal required for documentation. (from my point
of view, modern hypertext became looks like a HQ-magazine with live banners,
but not looks like a document)
* Smart search for .HLP-file.
* Possibility attach .HLP to your .EXE. Sort of "self-described EXE".
User can lost .HLP, but all required .EXE always live in PATH.

So, this 3 points allow acts as unix-man without additional settings,
argeements between software vendors etc. You can configure your system
in FreeDOS-way, or in a minimalistic old-times way - does not matter.

Another primary feature: possibility select part of help and copy to clipboard.

Where dwhelp search .HLP-files?
*. current directory
*. directory with dwhelp.exe
*. into directory %HELPPATH%
*. into %PATH%

What dwhelp search?
*. file name, entered by you (dos_api => dos_api.hlp)
*. file name with %LANG% (dos_api_CS.hlp)
*. .EXE-file, which can contains help (dos_api.exe)

For more info run DWHELP.EXE DWHELP

== License
MIT (see License.txt)
