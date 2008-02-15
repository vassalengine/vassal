!include "MUI2.nsh"

; Note: VERSION and TMPDIR are defined from the command line in the Makefile
;!define VERSION "3.1.0-svn3025"
;!define TMPDIR "/home/uckelman/projects/VASSAL/uckelman-working/tmp"
!define SRCDIR "${TMPDIR}/VASSAL-${VERSION}"
!define UROOT "Software\Microsoft\Windows\CurrentVersion\Uninstall\VASSAL-${VERSION}"

Name "VASSAL-${VERSION}"
OutFile "${TMPDIR}/VASSAL-${VERSION}-windows.exe"

InstallDir "$PROGRAMFILES\VASSAL\VASSAL-${VERSION}"
InstallDirRegKey HKLM "SOFTWARE\vassalengine.org\VASSAL-${VERSION}" ""
DirText "Select the directory in which to install VASSAL-${VERSION}:"

!define MUI_ABORTWARNING
!define MUI_UNABORTWARNING

!define MUI_COMPONENTSPAGE_NODESC
;!define MUI_TEXT_COMPONENTS_TITLE "Choose Components"
;!define MUI_TEXT_COMPONENTS_SUBTITLE "Choose which features of VASSAL you want to install."

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES
;!define MUI_FINISHPAGE_RUN "$INSTDIR\VASSAL.exe"
;!define MUI_FINISHPAGE_TEXT "Start VASSAL?"
;!insertmacro MUI_PAGE_FINISH


;Var StartMenuFolder
;!insertmacro MUI_PAGE_STARTMENU "Application" $StartMenuFolder
;!define MUI_STARTMENUPAGE_TEXT_CHECKBOX "Do not create Start menu items."
;!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
;!insertmacro MUI_PAGE_FINISH

Section "Base Files" BaseFiles
  SectionIn RO

  ; set the files to bundle
  SetOutPath "$INSTDIR"
  File "${SRCDIR}/*.exe"
  File "${SRCDIR}/*.bat"
    
  SetOutPath "$INSTDIR\lib"
  File /r "${SRCDIR}/lib/*"

  ; write keys to the registry
  WriteRegStr HKLM "SOFTWARE\vassalengine.org\VASSAL-${VERSION}" "" "$INSTDIR"

  WriteRegStr HKLM ${UROOT} "DisplayName" "VASSAL (${VERSION})"
  WriteRegStr HKLM ${UROOT} "DisplayVersion" "${VERSION}"
  WriteRegStr HKLM ${UROOT} "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM ${UROOT} "UninstallString" '"$INSTDIR\uninst.exe"'
  WriteRegStr HKLM ${UROOT} "Publisher" "vassalengine.org"
  WriteRegStr HKLM ${UROOT} "URLInfoAbout" "http://www.vassalengine.org"
  WriteRegStr HKLM ${UROOT} "URLUpdateInfo" "http://www.vassalengine.org"
  WriteRegDWORD HKLM ${UROOT} "NoModify" 0x00000001
  WriteRegDWORD HKLM ${UROOT} "NoRepair" 0x00000001

  ; create the uninstaller
  WriteUninstaller "$INSTDIR\uninst.exe"
SectionEnd 

Section "Start Menu shortcuts" StartShortcuts
  ; create the Start menu items
  CreateDirectory "$SMPROGRAMS\VASSAL\VASSAL-${VERSION}"
  CreateShortCut "$SMPROGRAMS\VASSAL\VASSAL-${VERSION}\VASSAL.lnk" "$INSTDIR\VASSAL.exe"
SectionEnd

Section "Desktop shortcuts" DesktopShortcuts
  ; create desktop shortcuts
  CreateShortCut "$DESKTOP\VASSAL-${VERSION}.lnk" "$INSTDIR\VASSAL.exe"
SectionEnd

UninstallText "This will uninstall VASSAL-${VERSION} from your system."

Section Uninstall
  ; delete the uninstaller
  Delete "$INSTDIR\uninst.exe"

  ; delete the desktop shortucts
  Delete "$DESKTOP\VASSAL-${VERSION}.lnk"

  ; delete the Start menu items
  RMDir /r "$SMPROGRAMS\VASSAL\VASSAL-${VERSION}"

  ; delete registry keys
  DeleteRegKey HKLM "SOFTWARE\vassalengine.org\VASSAL-${VERSION}"
  DeleteRegKey HKLM ${UROOT}

  ; delete the install director
  RMDir /r "$INSTDIR"

  ; delete VASSAL if empty
  RMDir "$PROGRAMFILES\VASSAL" 
  RMDir "$SMPROGRAMS\VASSAL"
SectionEnd 
