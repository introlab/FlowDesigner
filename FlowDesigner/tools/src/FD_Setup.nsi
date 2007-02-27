; example1.nsi
;
; This script is perhaps one of the simplest NSIs you can make. All of the
; optional settings are left to their default settings. The installer simply 
; prompts the user asking them where to install, and drops a copy of example1.nsi
; there. 

;--------------------------------

; The name of the installer
Name "FlowDesigner"

; The file to write
OutFile "FD_Setup.exe"

; The default installation directory
InstallDir C:\FlowDesigner

;--------------------------------

; Pages

Page directory
Page instfiles

;--------------------------------

; The stuff to install
Section "binaries" ;No components page, name is not important

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR\bin
  
  ; Put file there
  File bin\batchflow.exe
  File bin\liblibflow.dll
  File bin\liblibflow.dll.a
  File bin\qtflow.exe
  File bin\info2def.pl
  
SectionEnd ; end the section

Section "toolbox"
	SetOutPath $INSTDIR\toolbox\bin
		
	;Put file there
	File libnnet.def
	File libflow.def
	File libfuzzy.def
	File toolbox\bin\liblibfuzzy.dll
	File toolbox\bin\liblibfuzzy.dll.a
	File toolbox\bin\liblibnnet.dll
	File toolbox\bin\liblibnnet.dll.a
	
SectionEnd ;end the section

Section "QT"
	SetOutPath $INSTDIR\bin
		
	;Put file there
	File C:\Qt\4.2.2\bin\*.dll
	
SectionEnd ;end the section

Section "libxml2"
	SetOutPath $INSTDIR\bin
		
	;Put file there
	File C:\Dev-Cpp\bin\*.dll

	
SectionEnd ;end the section
