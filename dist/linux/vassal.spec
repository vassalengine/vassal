#Do not repack the jars as there are permission issues in the
#jar packaging

%define __jar_repack %{nil}

Name:		vassal
Summary:	A game engine for building and playing board games and card games
Version:	3.2.13
Release:	2%dist
Source0:	http://downloads.sourceforge.net/vassalengine/VASSAL-%{version}-src.zip
Source1:	https://boblfoot.fedorapeople.org/vassal/vassal.1.gz
URL:		https://vassalengine.org/
BuildRequires:	java-devel
BuildArch:	noarch
Requires:	java
# This contains several java routines licensed under various FSF licenses
# 
# LGPLv2+ Licensed Packages
# Main Package & Package BrowserLauncher2 & Package jLayer & Package logback &
# Package swing-layout & Package swing-worker & Package swingx
#
# ASL2.0 Licensed Packages
# Package batik & Package commons-codec & Package commons-io 
# Package commons-lang & Package guava & Package smack
#
# SPL Licensed Packages
# Package BeanShell & Package sun-icons & Package wizard 
#
# BSD Licensed Packages
# Package janino
#
# GPLv2 Licensed Packages
# Package java-getopt
#
# CCO Licensed Packages
# Package tango
#
License:	LGPLv2+ and ASL 2.0 and SPL and BSD and GPLv2 and CC0

%description
Vassal is a game engine for building and playing online adaptations of board
games and card games. Play live on the Internet or by email. Vassal runs on
all platforms, and is free, open-source software.

%prep
%setup -q -n VASSAL-%{version}-src
cp -p %SOURCE1 .

%build
make jar

%install
#make some install dirs
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_exec_prefix}/lib/%{name}
mkdir -p %{buildroot}%{_mandir}/man8

#install package
cp -avr lib %{buildroot}%{_exec_prefix}/lib/%{name}
cp -avr doc %{buildroot}%{_exec_prefix}/lib/%{name}
cp -av vassal.1.gz %{buildroot}%{_mandir}/man8
cp -av dist/VASSAL.sh %{buildroot}%{_exec_prefix}/lib/%{name}/vassal.sh
ln -s %{_exec_prefix}/lib/%{name}/vassal.sh %{buildroot}%{_bindir}/vassal

#Menu entry
install -d -m755 %{buildroot}%{_datadir}/applications
cat > %{buildroot}%{_datadir}/applications/%{name}.desktop <<EOF
[Desktop Entry]
Name=Vassal Game Engine
Comment=%summary
Exec=%{name}
Icon=boards_section
Terminal=false
Type=Application
Categories=Game;BoardGame;
EOF

%files
%{_bindir}/*
%{_exec_prefix}/lib/%{name}
%{_datadir}/applications/%{name}.desktop
%{_mandir}/man8/vassal.1.gz

%changelog
* Fri Aug 22 2014 Bob Lightfoot <boblfoot@fedoraproject.org> 3.2.13-2
- Miscellaneous cleanup changes for better operability
- switch to the less restrictive java & java-devel in requries
- remove the launcher script and class path declare from majelia rpm
- this favors vassals native launcher script
- remove the el7.centos from changelog even though rpmlint warns

* Mon Aug 18 2014 Bob Lightfoot <boblfoot@fedoraproject.org> 3.2.13-1
- initial build of 3.2.13
