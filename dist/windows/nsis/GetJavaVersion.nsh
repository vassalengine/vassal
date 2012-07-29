# 
#  $Id$
# 
#  Copyright (c) 2009-2010 by Joel Uckelman
#
#  GetJREPath adapted from
#  http://nsis.sourceforge.net/Java_Launcher_with_automatic_JRE_installation
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
# GetJavaVersion
#
# Find installed Java and return version as x.y.z_u string.
#
# Usage example:
#
#   ${GetJavaVersion} $0
#   ${VersionConvert} "$0" "_" $0
#   ${VersionCompare} "1.6.0.16" "$0" $1
#   ${If} $1 < 2
#     DetailPrint ">= 1.6.0_08"
#   ${Else}
#     DetailPrint "< 1.6.0_08" 
#   ${Endif}
#

!ifndef GETJAVAVERSION_INCLUDED
!define GETJAVAVERSION_INCLUDED

!include Util.nsh
!include WordFunc.nsh
!include x64.nsh

!verbose push
!verbose 3
!ifndef _GETJAVAVERSION_VERBOSE
  !define _GETJAVAVERSION_VERBOSE 3
!endif
!verbose ${_GETJAVAVERSION_VERBOSE}
!define GETJAVAVERSION_VERBOSE `!insertmacro GETJAVAVERSION_VERBOSE`
!verbose pop

!macro GETJAVAVERSION_VERBOSE _VERBOSE
  !verbose push
  !verbose 3
  !undef _GETJAVAVERSION_VERBOSE
  !define _GETJAVAVERSION_VERBOSE ${_VERBOSE}
  !verbose pop
!macroend

!macro GetJavaVersionCall _RESULT
  !verbose push
  !verbose ${_GETJAVAVERSION_VERBOSE}
  ${CallArtificialFunction2} GetJavaVersion_
  Pop ${_RESULT}
  !verbose pop
!macroend

!define GetJavaVersion "!insertmacro GetJavaVersionCall"
!define un.GetJavaVersion "!insertmacro GetJavaVersionCall"

!macro GetJavaVersion
!macroend

!macro un.GetJavaVersion
!macroend

!macro GetJavaVersion_
  !verbose push
  !verbose ${_GETJAVAVERSION_VERBOSE}

  ; save registers
  Push $0
  Push $1
  Push $R0

  ${GetJREPath} $0
  IfErrors NotFound 

  ; Ask java what version it is
  nsExec::ExecToStack '"$0" -fullversion'
  Pop $0  ; exit code
  Pop $1  ; output
 
  ; Output from java looks like 'java full version "\d+.\d+.\d+_\d+-b\d+"'
  ; We want everything up to the build number
  ${WordFind2X} $1 'java full version "' '-' "E+1" $R0
  IfErrors NotFound Found

NotFound:
  StrCpy $R0 ""

Found:
  Push $R0
  Exch 3

  ; restore registers
  Pop $R0
  Pop $1
  Pop $0

  !verbose pop
!macroend

#
# GetJREPath
#
# Find the path to java.exe.
#
 
!macro GetJREPathCall _RESULT
  !verbose push
  !verbose ${_GETJAVAVERSION_VERBOSE}
  ${CallArtificialFunction} GetJREPath_
  Pop ${_RESULT}
  !verbose pop
!macroend

!define GetJREPath "!insertmacro GetJREPathCall"
!define un.GetJREPath "!insertmacro GetJREPathCall"

!macro GetJREPath
!macroend

!macro un.GetJREPath
!macroend

; the paths of JRE and JDK keys in the registry
!define JRE_ROOT "SOFTWARE\JavaSoft\Java Runtime Environment"
!define JDK_ROOT "SOFTWARE\JavaSoft\Java Development Kit"

!define JAVA_EXE "java.exe"

!macro CheckRegistryJRE
  ClearErrors
  ReadRegStr $R1 HKLM "${JRE_ROOT}" "CurrentVersion"
  ReadRegStr $R0 HKLM "${JRE_ROOT}\$R1" "JavaHome"
  IfErrors +3
  StrCpy $R0 "$R0\bin\${JAVA_EXE}"
  IfFileExists $R0 JREFound
!macroend

!macro CheckRegistryJDK
  ClearErrors
  ReadRegStr $R1 HKLM "${JDK_ROOT}" "CurrentVersion"
  ReadRegStr $R0 HKLM "${JDK_ROOT}\$R1" "JavaHome"
  IfErrors +3 
  StrCpy $R0 "$R0\bin\${JAVA_EXE}"
  IfFileExists $R0 JREFound
!macroend

!macro GetJREPath_
  !verbose push
  !verbose ${_GETJAVAVERSION_VERBOSE}

  ; save registers
  Push $R0
  Push $R1 

  ; check JavaHome
  ClearErrors
  ReadEnvStr $R0 "JAVA_HOME"
  IfErrors CheckRegistry
  StrCpy $R0 "$R0\bin\${JAVA_EXE}"
  IfFileExists $R0 JREFound

CheckRegistry:
  ${If} ${RunningX64}
    SetRegView 64
    !insertmacro CheckRegistryJRE
    !insertmacro CheckRegistryJDK
  ${EndIf}
  SetRegView 32
  !insertmacro CheckRegistryJRE
  !insertmacro CheckRegistryJDK

JRENotFound:
  StrCpy $R0 ""    
  SetErrors

JREFound:
  Push $R0
  Exch 2

  ; restore registers
  Pop $R1
  Pop $R0

  !verbose pop
!macroend

!endif ; GETJAVAVER_INCLUDED
