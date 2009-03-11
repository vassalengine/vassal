# 
#  $Id$
# 
#  Copyright (c) 2009 by Joel Uckelman
#  Adapted from http://nsis.sourceforge.net/Get_full_Java_version by pyropunk
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

# GetJavaVersionParts
#
# Find installed Java and return major, minor, micro and build/update version
#
# For some reason v1.2.1_004 did not give a build version, but it's the only
# one of its kind.
#
# There are 3 ways to get the build version:
#   1) from the UpdateVersion key
#   2) or from the MicroVersion key
#   3) or from the JavaHome key
# 
#
# Usage example:
#
#   ${GetJavaVersionParts} $0 $1 $2 $3
#   ; $0 major version
#   ; $1 minor version
#   ; $2 micro version
#   ; $3 build/update version
#   StrCmp $0 "" JavaNotInstalled
#   StrCmp $3 "" nobuild
#   DetailPrint "$0.$1.$2_$3"
#   Goto fin
# nobuild:
#   DetailPrint "$0.$1.$2"
#   Goto fin
# JavaNotInstalled:
#   DetailPrint "Java Not Installed"
# fin:
#
#
# GetJavaVersion
#
# Find installed Java and return version as x.y.z_b string.
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

; the paths of JRE and JDK keys in the registry
!define JRE_ROOT "SOFTWARE\JavaSoft\Java Runtime Environment"
!define JDK_ROOT "SOFTWARE\JavaSoft\Java Development Kit"

!macro GetJavaVersionPartsCall _RESULT1 _RESULT2 _RESULT3 _RESULT4
  !verbose push
  !verbose ${_GETJAVAVERSION_VERBOSE}
  ${CallArtificialFunction} GetJavaVersionParts_
  Pop ${_RESULT1}
  Pop ${_RESULT2}
  Pop ${_RESULT3}
  Pop ${_RESULT4}
  !verbose pop
!macroend

!define GetJavaVersionParts "!insertmacro GetJavaVersionPartsCall"
!define un.GetJavaVersionParts "!insertmacro GetJavaVersionPartsCall"

!macro GetJavaVersionParts
!macroend

!macro un.GetJavaVersionParts
!macroend


!macro GetJavaVersionParts_
  push $R0
  push $R1
  push $2
  push $0
  push $3
  push $4
 
  ReadRegStr $2 HKLM "${JRE_ROOT}" "CurrentVersion"
  StrCmp $2 "" DetectTry2
  ReadRegStr $3 HKLM "${JRE_ROOT}\$2" "MicroVersion"
  StrCmp $3 "" DetectTry2
  ReadRegStr $4 HKLM "${JRE_ROOT}\$2" "UpdateVersion"
  StrCmp $4 "" 0 GotFromUpdate
  ReadRegStr $4 HKLM "${JRE_ROOT}\$2" "JavaHome"
  Goto GotJRE
DetectTry2:
  ReadRegStr $2 HKLM "${JDK_ROOT}" "CurrentVersion"
  StrCmp $2 "" NoFound
  ReadRegStr $3 HKLM "${JDK_ROOT}\$2" "MicroVersion"
  StrCmp $3 "" NoFound
  ReadRegStr $4 HKLM "${JDK_ROOT}\$2" "UpdateVersion"
  StrCmp $4 "" 0 GotFromUpdate
  ReadRegStr $4 HKLM "${JDK_ROOT}\$2" "JavaHome"
GotJRE:
  ; calc build version
  strlen $0 $3
  intcmp $0 1 0 0 GetFromMicro
  ; get it from the path
GetFromPath:
  strlen $R0 $4
  intop $R0 $R0 - 1
  StrCpy $0 ""
loopP:
  StrCpy $R1 $4 1 $R0
  StrCmp $R1 "" DotFoundP
  StrCmp $R1 "_" UScoreFound
  StrCmp $R1 "." DotFoundP
  StrCpy $0 "$R1$0"
  Goto GoLoopingP
DotFoundP:
  push ""
  Exch 6
  goto CalcMicro
UScoreFound:
  push $0
  Exch 6
  goto CalcMicro
GoLoopingP:
  intcmp $R0 0 DotFoundP DotFoundP
  IntOp $R0 $R0 - 1
  Goto loopP
GetFromMicro:
  strcpy $4 $3
  goto GetFromPath
GotFromUpdate:
  push $4
  Exch 6
 
CalcMicro:
  Push $3 ; micro
  Exch 6
  ; break version into major and minor
  StrCpy $R0 0
  StrCpy $0 ""
loop:
  StrCpy $R1 $2 1 $R0
  StrCmp $R1 "" done
  StrCmp $R1 "." DotFound
  StrCpy $0 "$0$R1"
  Goto GoLooping
DotFound:
  Push $0 ; major
  Exch 5
  StrCpy $0 ""
GoLooping:
  IntOp $R0 $R0 + 1
  Goto loop
 
done:
  Push $0 ; minor
  Exch 7
  ; restore register values
  pop $0
  pop $2
  pop $R1
  pop $R0
  pop $3
  pop $4
  return
NoFound:
  pop $4
  pop $3
  pop $0
  pop $2
  pop $R1
  pop $R0
  push ""
  push ""
  push ""
  push ""
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
  Push $2
  Push $3
  Push $R0

  ${GetJavaVersionParts} $0 $1 $2 $3
  StrCmp $0 "" notinstalled
  StrCmp $3 "" nobuild
  StrCpy $R0 "$0.$1.$2_$3"
  Goto fin
nobuild:
  StrCpy $R0 "$0.$1.$2"
  Goto fin
notinstalled:
  StrCpy $R0 ""
fin:
  Push $R0
  Exch 5
  
  ; restore registers
  Pop $0
  Pop $R0
  Pop $3
  Pop $2
  Pop $1

  !verbose pop
!macroend

!endif ; GETJAVAVER_INCLUDED
