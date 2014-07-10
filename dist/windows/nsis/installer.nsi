#
#  $Id$
#
#  Copyright (c) 2008-2012 by Joel Uckelman
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Library General Public
#  License (LGPL) as published by the Free Software Foundation.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  Library General Public License for more details.
#
#  You should have received a copy of the GNU Library General Public
#  License along with this library; if not, copies are available
#  at http://www.opensource.org.
#

#
# General Configuration
#

; Note: VERSION and TMPDIR are defined from the command line in
; the Makefile. These are here as a reminder only.
;!define VERSION "3.1.0-svn3025"
;!define TMPDIR "/home/uckelman/projects/VASSAL/uckelman-working/tmp"

!define SRCDIR "${TMPDIR}/VASSAL-${VERSION}"
!define UNINST "Software\Microsoft\Windows\CurrentVersion\Uninstall"
!define VNAME "VASSAL (${VERSION})"
!define UROOT "${UNINST}\${VNAME}"
!define AROOT "Software\Classes"
!define JAVA_MINIMUM "1.6.0_45"
# FIXME: would be better if we read the download URL from our own site
!define JRE_32_URL "http://javadl.sun.com/webapps/download/AutoDL?BundleId=76208"
!define JRE_64_URL "http://javadl.sun.com/webapps/download/AutoDL?BundleId=76209"

Name "VASSAL"
OutFile "${TMPDIR}/VASSAL-${VERSION}-windows.exe"

; InstDir is set in .onInit based on the architecture
InstallDir ""

RequestExecutionLevel admin

# compression
SetCompress auto
SetCompressor /SOLID lzma
SetDatablockOptimize on

# includes for various functions
!include "FileFunc.nsh"
!include "nsDialogs.nsh"
!include "WinMessages.nsh"
!include "WinVer.nsh"
!include "WordFunc.nsh"
!include "x64.nsh"

!addincludedir "dist/windows/nsis"
!include "GetJavaVersion.nsh"

#
# Modern UI 2 setup
#
!include "MUI2.nsh"
!define MUI_ABORTWARNING

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\orange.bmp"
!define MUI_HEADERIMAGE_UNBITMAP "${NSISDIR}\Contrib\Graphics\Header\orange-uninstall.bmp"

!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\orange-install.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"

!define MUI_WELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\orange.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\orange-uninstall.bmp"

#
# Install Pages
#
; Welcome page
!define MUI_WELCOMEPAGE_TITLE_3LINES
!insertmacro MUI_PAGE_WELCOME

; Setup Type page
Page custom preSetupType leaveSetupType

; Uninstall Old Versions page
Page custom preUninstallOld leaveUninstallOld

; Java Check page
Page custom preJavaCheck leaveJavaCheck

; Select Install Directory page
!define MUI_PAGE_CUSTOMFUNCTION_PRE preDirectory
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveDirectory
# FIXME: do something to make sure that given directory is ok
!define MUI_DIRECTORYPAGE_VERIFYONLEAVE
!insertmacro MUI_PAGE_DIRECTORY

; Shortcuts page
Page custom preShortcuts leaveShortcuts

; Start Menu page
Var StartMenuFolder
!define MUI_PAGE_CUSTOMFUNCTION_PRE preStartMenu
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE leaveStartMenu
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "VASSAL"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT HKLM
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${UROOT}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "StartMenuFolder"
!insertmacro MUI_PAGE_STARTMENU StartMenu $StartMenuFolder

; Confirm Install page
Page custom preConfirm leaveConfirm

; Install Files page
!insertmacro MUI_PAGE_INSTFILES

; Finish page
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_FINISHPAGE_TITLE_3LINES
!define MUI_FINISHPAGE_RUN
!define MUI_FINISHPAGE_RUN_FUNCTION launchApp
;!define MUI_FINISHPAGE_RUN_TEXT $(LAUNCH_TEXT)
;!define MUI_PAGE_CUSTOMFUNCTION_PRE preFinish
!insertmacro MUI_PAGE_FINISH

#
# Uninstall Pages
#

; Welcome page
!insertmacro MUI_UNPAGE_WELCOME

; Confirm page
!insertmacro MUI_UNPAGE_CONFIRM

; Remove Files page
!insertmacro MUI_UNPAGE_INSTFILES

; Finish page
!define MUI_UNFINISHPAGE_NOAUTOCLOSE
!insertmacro MUI_UNPAGE_FINISH

; must be set after the pages, or header graphics fail to show up!
!insertmacro MUI_LANGUAGE "English"

#
# Macros
#


; sets registry to architecture-appropriate one
!macro SetNativeRegView
  ${If} ${RunningX64}
    SetRegView 64
  ${Else}
    SetRegView 32
  ${EndIf}
!macroend

!define SetNativeRegView "!insertmacro SetNativeRegView"


; skips a page in a Standard install
!macro SkipIfNotCustom
  ${If} $CustomSetup == 0
    Abort
  ${EndIf}
!macroend

!define SkipIfNotCustom "!insertmacro SkipIfNotCustom"


!macro ForceSingleton _MUTEX
  #
  # Only one instance of the installer may run at a time.
  # Based on http://nsis.sourceforge.net/Allow_only_one_installer_instance.
  #

  ; set up a mutex
  BringToFront
  System::Call "kernel32::CreateMutexA(i 0, i 0, t '${_MUTEX}') i .r0 ?e"
  Pop $0

  ; if the mutex already existed, find the installer running before us
  ${If} $0 != 0
    StrLen $0 "${_MUTEX}"
    IntOp $0 $0 + 1

    ; loop until we find the other installer
    ${Do}
      FindWindow $1 '#32770' '' 0 $1
      ${If} $1 == 0
        Abort
      ${EndIf}

      System::Call "user32::GetWindowText(i r1, t .r2, i r0) i."
      ${If} $2 == "${_MUTEX}"
        ; bring it to the front and die
        System::Call "user32::ShowWindow(i r1,i 9) i."
        System::Call "user32::SetForegroundWindow(i r1) i."
        Abort
      ${EndIf}
    ${Loop}
  ${EndIf}
!macroend

!define ForceSingleton "!insertmacro ForceSingleton"


; detect running instances of VASSAL
!macro WaitForVASSALToClose
  #
  # Detect running instances of VASSAL.
  # Based on http://nsis.sourceforge.net/Get_a_list_of_running_processes.
  #

  ; no PSAPI on Windows 9x and ME, so don't try there
  ${IfNot} ${IsNT}
    Goto cannot_check
  ${EndIf}

check_processes:
  ; allocate a buffer
  System::Alloc 1024
  Pop $R9

  ; get the process array
  System::Call "Psapi::EnumProcesses(i R9, i 1024, *i .R1)i .R8"
  ${If} $R8 == 0
    System::Free $R9
    Goto cannot_check
  ${EndIf}

  IntOp $R2 $R1 / 4 ; Divide by sizeof(DWORD) to get number of processes
  StrCpy $R4 0      ; R4 is our counter variable

  ${Do}
    System::Call "*$R9(i .R5)" ; Get next PID
    ${If} $R5 == 0      ; skip PID 0
      Goto next_iteration
    ${ElseIf} $R5 < 0   ; done if PID < 0
      ${Break}
    ${EndIf}

    System::Call "Kernel32::OpenProcess(i 1040, i 0, i R5)i .R8"
    ${If} $R8 == 0
      Goto next_iteration
    ${EndIf}

    System::Alloc 1024
    Pop $R6

    System::Call "Psapi::EnumProcessModules(i R8, i R6, i 1024, *i .R1)i .R7"
    ${If} $R7 == 0
      System::Free $R6
      GoTo next_iteration
    ${EndIf}

    System::Alloc 256
    Pop $R7

    System::Call "*$R6(i .r6)" ; Get next module
    System::Free $R6
    System::Call "Psapi::GetModuleBaseName(i R8, i r6, t .R7, i 256)i .r6"

    ${If} $6 == 0
      System::Free $R7
      System::Free $R9
      GoTo cannot_check
    ${EndIf}

    ${If} $R7 == "VASSAL.exe"
      System::Free $R7
      System::Free $R9

      MessageBox MB_OKCANCEL|MB_ICONEXCLAMATION "An instance of VASSAL is currently running.$\n$\nPlease close it and press OK to continue, or Cancel to quit." IDCANCEL bail_out
      Sleep 500
      Goto check_processes
    ${EndIf}

    System::Free $R7

next_iteration:
    IntOp $R4 $R4 + 1 ; Add 1 to our counter
    IntOp $R9 $R9 + 4 ; Add sizeof(int) to our buffer address
  ${LoopWhile} $R4 < $R2

  System::Free $R9
  Return

cannot_check:
  MessageBox MB_OKCANCEL|MB_ICONEXCLAMATION "Unable to determine whether an instance of VASSAL is running.$\n$\nPlease close all instances of VASSAL before proceeding." IDCANCEL bail_out
  Return

bail_out:
  Abort
!macroend

!define WaitForVASSALToClose "!insertmacro WaitForVASSALToClose"



!macro FindVASSALVersions
  StrCpy $R0 0
  ${Do}
    EnumRegKey $0 HKLM "${UNINST}" $R0
    StrCpy $R1 "$0" 8
    ${If} $R1 == "VASSAL ("
      ${WordFind} "$RemoveOtherVersions" "$\n" "E/$R0" $1
      IfErrors 0 +2
      StrCpy $RemoveOtherVersions "$RemoveOtherVersions$0$\n"
      ClearErrors
    ${EndIf}
    IntOp $R0 $R0 + 1
  ${LoopUntil} $0 == ""
!macroend

!define FindVASSALVersions "!insertmacro FindVASSALVersions"


#
# Setup Option Variables
#
Var CustomSetup
Var AddDesktopSC
Var AddStartMenuSC
Var AddQuickLaunchSC
Var InstallJRE
Var RemoveOtherVersions

#
# Functions
#
Function un.onInit
  ${SetNativeRegView}
  ${ForceSingleton} "VASSAL-${VERSION}-uninstaller"
  ${WaitForVASSALToClose}
FunctionEnd


Function .onInit
  ${SetNativeRegView}

  ${If} $InstDir == "" ; /D= was not used on the command line
    ${If} ${RunningX64}
      StrCpy $InstDir "$PROGRAMFILES64\VASSAL-${VERSION}"
    ${Else}
      StrCpy $InstDir "$PROGRAMFILES32\VASSAL-${VERSION}"
    ${EndIf}
  ${EndIf}

  ${ForceSingleton} "VASSAL-installer"
  ${WaitForVASSALToClose}
FunctionEnd


Function preSetupType
  ; save registers
  Push $0

  !insertmacro MUI_HEADER_TEXT "Setup Type" "Choose setup options"

  nsDialogs::Create /NOUNLOAD 1018
  Pop $0

  ${NSD_CreateLabel} 0 0 100% 12u "Choose the type of setup you prefer, then click Next."
	Pop $0
  ${NSD_CreateRadioButton} 15u 23u 100% 12u "&Standard"
	Pop $0
  SendMessage $0 ${BM_SETCHECK} ${BST_CHECKED} 1   ; select Standard
  ${NSD_CreateLabel} 30u 37u 100% 12u "VASSAL will be installed with the most common options."
	Pop $0
  ${NSD_CreateRadioButton} 15u 54u 100% 12u "&Custom"
	Pop $CustomSetup
  ${NSD_CreateLabel} 30u 68u 100% 24u "You may choose individual options to be installed. Recommended for experienced$\nusers."
	Pop $0

  nsDialogs::Show

  ; restore registers
  Pop $0
FunctionEnd


Function leaveSetupType
  ; read the install type from the Custom radio button
  ${NSD_GetState} $CustomSetup $CustomSetup
FunctionEnd


Var KeepListBox
Var RemoveListBox
Var KeepButton
Var RemoveButton

Function preUninstallOld
  StrCpy $RemoveOtherVersions ""

  ; find all versions of VASSAL, checking the 32- and 64-bit hives
  SetRegView 32
  ${FindVASSALVersions}
  SetRegView 64
  ${FindVASSALVersions}
  ${SetNativeRegView}

  ; remove all versions in Standard setup, skip this page
  ${SkipIfNotCustom}

  ${If} $RemoveOtherVersions == ""
    ; no versions installed, skip this page
  ${OrIf} $RemoveOtherVersions == "${VNAME}$\n"
    ; only this version installed, remove it and skip this page
    Abort
  ${EndIf}

  !insertmacro MUI_HEADER_TEXT "Remove Old Versions" "Uninstalling previous versions of VASSAL"

  nsDialogs::Create /NOUNLOAD 1018
  Pop $0

  ${NSD_CreateLabel} 0 0 100% 24u "The installer has found these other versions of VASSAL installed on your computer. Please select the versions of VASSAL you would like to remove now."
  Pop $0

  ${NSD_CreateLabel} 0 32u 120u 12u "To Keep:"
  Pop $0

  ${NSD_CreateListBox} 0 44u 120u 90u ""
  Pop $KeepListBox

  ${NSD_CreateButton} 125u 74u 50u 14u "Remove >"
  Pop $RemoveButton
  ${NSD_OnClick} $RemoveButton removeClicked

  ${NSD_CreateButton} 125u 90u 50u 14u "< Keep"
  Pop $KeepButton
  ${NSD_OnClick} $KeepButton keepClicked

  ${NSD_CreateLabel} 180u 32u 120u 12u "To Remove:"
  Pop $0

  ${NSD_CreateListBox} 180u 44u 120u 90u ""
  Pop $RemoveListBox

  ; populate the keep list
  StrCpy $R1 "$RemoveOtherVersions"
  StrCpy $RemoveOtherVersions ""
  StrCpy $R0 0
  ${Do}
    IntOp $R0 $R0 + 1
    ${WordFind} "$R1" "$\n" "E+$R0" $1
    IfErrors 0 +3
    ClearErrors
    ${Break}

    ${If} $1 == "${VNAME}"
      ; automatically uninstall existing copies of this version
      StrCpy $RemoveOtherVersions "${VNAME}$\n"
    ${Else}
      ; add entries for versions which are not this one
      SendMessage $KeepListBox ${LB_ADDSTRING} 0 "STR:$1"
      Pop $0
    ${EndIf}
  ${Loop}

  ; ready the buttons
  SendMessage $KeepListBox ${LB_SETCURSEL} 0 0
  Call adjustButtons

  ${NSD_OnChange} $KeepListBox adjustButtons
  ${NSD_OnChange} $RemoveListBox adjustButtons

  nsDialogs::Show
FunctionEnd


Function moveSelection
  ; move selected item from box $R1 to box $R0
  Pop $R0
  Pop $R1
  SendMessage $R1 ${LB_GETCURSEL} 0 0 $0
  ${If} $0 == LB_ERR
    Return
  ${EndIf}
  System::Call "user32::SendMessage(i $R1,i ${LB_GETTEXT},i r0, t .r1)i .r2"
  SendMessage $R1 ${LB_DELETESTRING} $0 0
  SendMessage $R0 ${LB_ADDSTRING} 0 "STR:$1"
FunctionEnd


Function adjustButtons
  ; disable a button if its source listbox has no selection
  SendMessage $KeepListBox ${LB_GETCURSEL} 0 0 $0
  ${If} $0 == -1
    StrCpy $0 0
  ${Else}
    StrCpy $0 1
  ${EndIf}
  EnableWindow $RemoveButton $0

  SendMessage $RemoveListBox ${LB_GETCURSEL} 0 0 $0
  ${If} $0 == -1
    StrCpy $0 0
  ${Else}
    StrCpy $0 1
  ${EndIf}
  EnableWindow $KeepButton $0
FunctionEnd


Function removeClicked
  ; move a selected item from Keep to Remove
  Push $KeepListBox
  Push $RemoveListBox
  Call moveSelection
  Call adjustButtons
FunctionEnd


Function keepClicked
  ; move a selected item from Remove to Keep
  Push $RemoveListBox
  Push $KeepListBox
  Call moveSelection
  Call adjustButtons
FunctionEnd


Function leaveUninstallOld
  ; collect the old versions to be removed from the remove list box
  SendMessage $RemoveListBox ${LB_GETCOUNT} 0 0 $1
  ${For} $0 0 $1
    System::Call "user32::SendMessage(i $RemoveListBox,i ${LB_GETTEXT},i r0, t .r2)i .r4"
    StrCpy $RemoveOtherVersions "$RemoveOtherVersions$2$\n"
  ${Next}
FunctionEnd


Function preJavaCheck
  ; save registers
  Push $0
  Push $1
  Push $R1
  Push $R2

  StrCpy $InstallJRE 0  ; set default

  ${GetJavaVersion} $1
  ${VersionConvert} "$1" "_" $R1
  ${VersionConvert} "${JAVA_MINIMUM}" "_" $R2
  ${VersionCompare} "$R1" "$R2" $2

  ${If} $2 < 2   ; JAVA_VERSION >= JAVA_MINIMUM
    Abort        ; then skip this page, installed Java is ok
  ${Endif}

  !insertmacro MUI_HEADER_TEXT "Installing Java" "Download and install a JRE for VASSAL"

  nsDialogs::Create /NOUNLOAD 1018
  Pop $0

  ${If} $1 == ""
    StrCpy $0 "The installer has not found a Java Runtime Environment (JRE) installed on your computer."
  ${Else}
    StrCpy $0 "The installer has found version $1 of the Java Runtime Environment (JRE) installed on your computer."
  ${EndIf}

  StrCpy $1 "We recommend running VASSAL with a JRE no older than version ${JAVA_MINIMUM}.$\n$\n$\n"

  ${If} $CustomSetup == 1
    ${NSD_CreateLabel} 0 0 100% 24u "$0 $1If you have a JRE which the installer has not detected, or if you wish to install a JRE yourself, unselect this option."
    Pop $0

    ${NSD_CreateCheckBox} 15u 32u 100% 12u "Install a Java Runtime Environment"
    Pop $InstallJRE
    SendMessage $InstallJRE ${BM_SETCHECK} ${BST_CHECKED} 1
  ${Else}
    ${NSD_CreateLabel} 0 0 100% 100% "$0 $1The installer will download and install a suitable JRE for you. Please select the defaults when the JRE installer appears."
    Pop $0

    StrCpy $InstallJRE 1
  ${EndIf}

  nsDialogs::Show

  ; restore registers
  Pop $R2
  Pop $R1
  Pop $1
  Pop $0
FunctionEnd


Function leaveJavaCheck
  ${If} $CustomSetup == 1
    ; read whether to install a JRE from the check box
    ${NSD_GetState} $InstallJRE $InstallJRE
  ${EndIf}
FunctionEnd


Function preDirectory
  ${SkipIfNotCustom}
FunctionEnd


Function leaveDirectory
FunctionEnd


Function preShortcuts
  ; save registers
  Push $0

  ; set shortcuts defaults
  StrCpy $AddDesktopSC 1
  StrCpy $AddStartMenuSC 1
  StrCpy $AddQuickLaunchSC 1

  ; present user with choices in a custom install
  ${SkipIfNotCustom}
  !insertmacro MUI_HEADER_TEXT "Set Up Shortcuts" "Create Program Icons"

  nsDialogs::Create /NOUNLOAD 1018
  Pop $0

  ${NSD_CreateLabel} 0 0 100% 12u "Create icons for VASSAL:"
  Pop $0
  ${NSD_CreateCheckBox} 15u 20u 100% 12u "On my &Desktop"
  Pop $AddDesktopSC
  SendMessage $AddDesktopSC ${BM_SETCHECK} ${BST_CHECKED} 1
  ${NSD_CreateCheckBox} 15u 40u 100% 12u "In my &Start Menu Programs folder"
  Pop $AddStartMenuSC
  SendMessage $AddStartMenuSC ${BM_SETCHECK} ${BST_CHECKED} 1
  ${NSD_CreateCheckBox} 15u 60u 100% 12u "In my &Quick Launch bar"
  Pop $AddQuickLaunchSC
  SendMessage $AddQuickLaunchSC ${BM_SETCHECK} ${BST_CHECKED} 1

  nsDialogs::Show

  ; restore registers
  Pop $0
FunctionEnd


Function leaveShortcuts
  ; read which shortcuts to create from the check boxes
  ${NSD_GetState} $AddDesktopSC $AddDesktopSC
  ${NSD_GetState} $AddStartMenuSC $AddStartMenuSC
  ${NSD_GetState} $AddQuickLaunchSC $AddQuickLaunchSC
FunctionEnd


Function preStartMenu
  ${SkipIfNotCustom}
  ; also skip if the user unselected this option
  ${If} $AddStartMenuSC == 0
    Abort
  ${EndIf}
FunctionEnd


Function leaveStartMenu
FunctionEnd


Function preConfirm
  ; save registers
  Push $0

  !insertmacro MUI_HEADER_TEXT "Ready to Install" "Please confirm that you are ready to install"

  nsDialogs::Create /NOUNLOAD 1018
  Pop $0

  ${NSD_CreateLabel} 0 0 100% 100% "The installer is ready to install VASSAL on your computer.$\n$\n$\nClick $\"Install$\" to start the installation."
  Pop $0

  nsDialogs::Show

  ; restore registers
  Pop $0
FunctionEnd


Function leaveConfirm
FunctionEnd


Function launchApp
  ; Launch via explorer.exe becuase it is already running as the user,
  ; not as admin, which will launch us as user also.
  Exec '"$WINDIR\explorer.exe" "$INSTDIR\VASSAL.exe"'
FunctionEnd


#
# Install Section
#
Section "-Application" Application
  SectionIn RO

  ; remove old versions of VASSAL, if requested
  ${If} $RemoveOtherVersions != ""
    ; split version strings on '\n'
    ; there must be at least one '\n', or WordFind finds no words
    StrCpy $0 0   ; word indices are 1-based
    ${Do}
      IntOp $0 $0 + 1
      ${WordFind} "$RemoveOtherVersions" "$\n" "E+$0" $1
      IfErrors 0 +2
      ${Break}

      DetailPrint "Uninstall: $1"

      ; look for 64-bit install
      ${If} ${RunningX64}
        SetRegView 64

        ; get old install and uninstaller paths
        ReadRegStr $2 HKLM "${UNINST}\$1" "InstallLocation"
        ReadRegStr $3 HKLM "${UNINST}\$1" "UninstallString"
        IfErrors 0 found
        ClearErrors
      ${EndIf}

      ; look for 32-bit install
      SetRegView 32
      ReadRegStr $2 HKLM "${UNINST}\$1" "InstallLocation"
      ReadRegStr $3 HKLM "${UNINST}\$1" "UninstallString"
      IfErrors cleanup found

    found:
      IfFileExists "$3" 0 cleanup

      ; copy the uninstaller to $TEMP
      CopyFiles "$3" "$TEMP"
      ${GetFileName} $3 $3

      ; run the uninstaller silently
      ExecWait '"$TEMP\$3" /S _?=$2'
      IfErrors 0 +2
      DetailPrint "Failed: $1"
      ClearErrors

      Delete "$TEMP\$3"   ; remove the uninstaller copy

    cleanup:
      ClearErrors

      ; clean up leftover reg keys
      ${If} ${RunningX64}
        SetRegView 64
        DeleteRegKey HKLM "${UNINST}\$1"
      ${EndIf}

      SetRegView 32
      DeleteRegKey HKLM "${UNINST}\$1"
    ${Loop}
  ${EndIf}

  ; install a JRE, if necessary
  ${If} $InstallJRE == 1
    ; choose a 64-bit JRE for 64-bit systems
    ${If} ${RunningX64}
      StrCpy $R1 ${JRE_64_URL}
    ${Else}
      StrCpy $R1 ${JRE_32_URL}
    ${EndIf}

    DetailPrint "Downloading a JRE from $R1"
    StrCpy $0 "$TEMP\jre_installer.exe"
    NSISdl::download /TIMEOUT=30000 $R1 $0
    Pop $R0 ; Get the return value
    StrCmp $R0 "success" +3
    MessageBox MB_OK "Java download failed: $R0"
    Quit

    ${If} $CustomSetup == 1
      ; provide a full JRE installer
      ExecWait $0
    ${Else}
      ; provide a JRE installer requiring no user interaction
      ; options reference: http://java.sun.com/javase/6/docs/technotes/guides/deployment/deployment-guide/silent.html
      ExecWait "$0 /qr ADDLOCAL=ALL"
    ${EndIf}

    Delete $0
    DetailPrint "Installed a JRE"
  ${EndIf}

  ${SetNativeRegView}

  ; set the files to bundle
  !include "${TMPDIR}/install_files.inc"

  ; write registry keys for uninstaller
  WriteRegStr HKLM "${UROOT}" "DisplayName" "VASSAL (${VERSION})"
  WriteRegStr HKLM "${UROOT}" "DisplayVersion" "${VERSION}"
  WriteRegStr HKLM "${UROOT}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "${UROOT}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${UROOT}" "Publisher" "vassalengine.org"
  WriteRegStr HKLM "${UROOT}" "URLInfoAbout" "http://www.vassalengine.org"
  WriteRegStr HKLM "${UROOT}" "URLUpdateInfo" "http://www.vassalengine.org"
  WriteRegDWORD HKLM "${UROOT}" "NoModify" 0x00000001
  WriteRegDWORD HKLM "${UROOT}" "NoRepair" 0x00000001

  ; create the uninstaller
  WriteUninstaller "$INSTDIR\uninst.exe"

  ; create the shortcuts
  ; don't use version number in shortcut names for Standard install
  ${If} $CustomSetup == 1
    StrCpy $0 "VASSAL ${VERSION}"
  ${Else}
    StrCpy $0 "VASSAL"
  ${EndIf}

  ; CreateShortCut uses $OUTDIR as the working directory for shortcuts
  SetOutPath "$INSTDIR"

  ; create the desktop shortcut
  ${If} $AddDesktopSC == 1
    CreateShortCut "$DESKTOP\$0.lnk" "$INSTDIR\VASSAL.exe"
    WriteRegStr HKLM "${UROOT}" "DesktopShortcut" "$DESKTOP\$0.lnk"
  ${EndIf}

  !insertmacro MUI_STARTMENU_WRITE_BEGIN StartMenu
    ; create the Start Menu shortcut
    ${If} $AddStartMenuSC == 1
      CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
      CreateShortCut "$SMPROGRAMS\$StartMenuFolder\$0.lnk" "$INSTDIR\VASSAL.exe"
      WriteRegStr HKLM "${UROOT}" "StartMenuShortcut" "$SMPROGRAMS\$StartMenuFolder\$0.lnk"
    ${EndIf}
  !insertmacro MUI_STARTMENU_WRITE_END

  ; create the quick launch shortcut
  ${If} $AddQuickLaunchSC == 1
    CreateShortCut "$QUICKLAUNCH\$0.lnk" "$INSTDIR\VASSAL.exe"
    WriteRegStr HKLM "${UROOT}" "QuickLaunchShortcut" "$QUICKLAUNCH\$0.lnk"
  ${EndIf}

  ; create file associations
  WriteRegStr HKLM "${AROOT}\.vmod" "" "VASSALModule"
;  WriteRegStr HKLM ".vmod" "Content Type" "application/vnd.vassal.module"
  WriteRegStr HKLM "${AROOT}\VASSALModule" "" "VASSAL Module"
  WriteRegStr HKLM "${AROOT}\VASSALModule\DefualtIcon" "" ""
  WriteRegStr HKLM "${AROOT}\VASSALModule\shell\open\command" "" '$INSTDIR\VASSAL.exe --load "%1"'
  WriteRegStr HKLM "${AROOT}\VASSALModule\shell\edit\command" "" '$INSTDIR\VASSAL.exe --edit "%1"'

  WriteRegStr HKLM "${AROOT}\.vlog" "" "VASSALGameLog"
;  WriteRegStr HKLM ".vmod" "Content Type" "application/vnd.vassal.log"
  WriteRegStr HKLM "${AROOT}\VASSALGameLog" "" "VASSAL Game Log"
  WriteRegStr HKLM "${AROOT}\VASSALGameLog\DefualtIcon" "" ""
  WriteRegStr HKLM "${AROOT}\VASSALGameLog\shell\open\command" "" '$INSTDIR\VASSAL.exe --load "%1"'

  WriteRegStr HKLM "${AROOT}\.vsav" "" "VASSALSavedGame"
;  WriteRegStr HKLM ".vsav" "Content Type" "application/vnd.vassal.save"
  WriteRegStr HKLM "${AROOT}\VASSALSavedGame" "" "VASSAL Saved Game"
  WriteRegStr HKLM "${AROOT}\VASSALSavedGame\DefualtIcon" "" ""
  WriteRegStr HKLM "${AROOT}\VASSALSavedGame\shell\open\command" "" '$INSTDIR\VASSAL.exe --load "%1"'

  ; notify Windows that file associations have changed
  ${RefreshShellIcons}
SectionEnd

#
# Uninstall Section
#
Section Uninstall
  ${SetNativeRegView}

  ; delete the uninstaller
  Delete "$INSTDIR\uninst.exe"

  ; delete the desktop shortuct
  ReadRegStr $0 HKLM "${UROOT}" "DesktopShortcut"
  ${If} $0 != ""
    Delete "$0"
  ${EndIf}

  ; delete the quick launch shortcut
  ReadRegStr $0 HKLM "${UROOT}" "QuickLaunchShortcut"
  ${If} $0 != ""
    Delete "$0"
  ${EndIf}

  ; delete the Start Menu items
  ReadRegStr $0 HKLM "${UROOT}" "StartMenuShortcut"
  ${If} $0 != ""
    Delete "$0"
  ${EndIf}

  ; delete the Start Menu folder
  !insertmacro MUI_STARTMENU_GETFOLDER StartMenu $StartMenuFolder
  RMDir "$SMPROGRAMS\$StartMenuFolder"

  ; delete registry keys
  DeleteRegKey HKLM "${UROOT}"

  ${If} ${RunningX64}
    ; kill the 32-bit registry tree if empty
    DeleteRegKey /ifempty HKLM "Software\Wow6432Node\vassalengine.org\VASSAL"
    DeleteRegKey /ifempty HKLM "Software\Wow6432Node\vassalengine.org"
  ${EndIf}

  ; kill the registry tree if empty
  DeleteRegKey /ifempty HKLM "Software\vassalengine.org\VASSAL"
  DeleteRegKey /ifempty HKLM "Software\vassalengine.org"

  ; remove file associations if they are ours
  ReadRegStr $0 HKLM "${AROOT}\VASSALModule\shell\open\command" ""
  ${If} $0 != ""
    ; the file associations are ours if they start with our $INSTDIR
    StrLen $1 "$INSTDIR\"
    StrCpy $0 $0 $1 0
    ${If} $0 == "$INSTDIR\"
      DeleteRegKey HKLM "${AROOT}\.vmod"
      DeleteRegKey HKLM "${AROOT}\VASSALModule"

      DeleteRegKey HKLM "${AROOT}\.vlog"
      DeleteRegKey HKLM "${AROOT}\VASSALGameLog"

      DeleteRegKey HKLM "${AROOT}\.vsav"
      DeleteRegKey HKLM "${AROOT}\VASSALSavedGame"

      ; notify Windows that file associations have changed
      ${RefreshShellIcons}
    ${EndIf}
  ${EndIf}

  ; delete the installed files and directories
  !include "${TMPDIR}/uninstall_files.inc"

  ; delete VASSAL from start menu if empty
  RMDir "$SMPROGRAMS\VASSAL"
SectionEnd
