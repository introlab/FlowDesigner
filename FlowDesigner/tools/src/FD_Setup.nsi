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
  File bin\*
  
SectionEnd ; end the section


Section "headers" ;No components page, name is not important

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR\include\flowdesigner
  
  ; Put file there
  File include\flowdesigner\*.h

SectionEnd ; end the section

Section "fuzzy"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\fuzzy
		
	;Put file there
	File lib\flowdesigner\toolbox\fuzzy\*
		
SectionEnd ;end the section

Section "libflow"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\libflow
		
	;Put file there
	File lib\flowdesigner\toolbox\libflow\*
		
SectionEnd ;end the section

Section "nnet"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\nnet
		
	;Put file there
	File lib\flowdesigner\toolbox\nnet\*
		
SectionEnd ;end the section

Section "vq"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\vq
		
	;Put file there
	File lib\flowdesigner\toolbox\vq\*
		
SectionEnd ;end the section

Section "hmm"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\hmm
		
	;Put file there
	File lib\flowdesigner\toolbox\hmm\*
		
SectionEnd ;end the section

Section "audio"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\audio
		
	;Put file there
	File lib\flowdesigner\toolbox\audio\*
		
SectionEnd ;end the section

Section "effects"
	SetOutPath $INSTDIR\lib\flowdesigner\toolbox\effects
		
	;Put file there
	File lib\flowdesigner\toolbox\effects\*
		
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
