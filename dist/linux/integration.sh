#!/bin/bash

VASSAL_DIR=`realpath $(dirname $0)`

# --- Colours ----------------------------------------------
# See also
# https://stackoverflow.com/questions/5947742/
# Reset
NC='\E[m'       # Text Reset

# Regular Colors
B='\E[0;30m'       # Black
R='\E[0;31m'       # Red
G='\E[0;32m'       # Green
Y='\E[0;33m'       # Yellow
B='\E[0;34m'       # Blue
P='\E[0;35m'       # Purple
C='\E[0;36m'       # Cyan
W='\E[0;37m'       # White

# Bold
BF='\E[1;30m'      # Black
BR='\E[1;31m'      # Red
BG='\E[1;32m'      # Green
BY='\E[1;33m'      # Yellow
BB='\E[1;34m'      # Blue
BP='\E[1;35m'      # Purple
BC='\E[1;36m'      # Cyan
BW='\E[1;37m'      # White

# Underline
UN='\E[4;30m'      # Black
UR='\E[4;31m'      # Red
UG='\E[4;32m'      # Green
UY='\E[4;33m'      # Yellow
UB='\E[4;34m'      # Blue
UP='\E[4;35m'      # Purple
UC='\E[4;36m'      # Cyan
UW='\E[4;37m'      # White

# --- For logging purposes -------------------------------------------
msg() {
    if test $verb -lt 1 ; then return ; fi

    echo -e $@
}

# --- Help message ---------------------------------------------------
usage()
{
    cat <<-EOF
	Usage: $0 [OPTIONS]
	
	Options:
	  -h,--help		     This help
	  -u,--user		     Install for current user only (default)
	  -s,--system		     Install system wide
	  -v,--vassal-path DIR	     VASSAL installation (${VASSAL_DIR})
	  -d,--desktop 		     Make desktop short-cut (default)
	  -D,--no-desktop	     Do not make desktop short-cut
	  -V,--verbose		     Be verbose
	  -r,--remove                Remove VASSAL desktop integration
	  
	System-wide (-s,--system) requrires super-user privileges.  Run
	this script with f.ex. 'sudo'.

	Default is to install for current user only.

	By default, a desktop short-cut is installed.
	EOF
}

# --- Delete a file --------------------------------------------------
del_file() {
    file=$1
    msg "${BY}Removing ${UC}$file${NC}"
    if test ! -f $file ; then
        echo "$file does not exist" > /dev/stderr
        return 0
    fi
    rm -f $file 
}

# --- Settings -------------------------------------------------------
user=1
verb=0
app=application-x-vassal
app_name=org.vassalengine.vassal
rem=0
desktop_dir=$HOME/Desktop
wrap=1

# --- Handle command line --------------------------------------------
while test $# -gt 0 ; do
    case x$1 in
        x-h|x--help) usage ; exit 0 ;;
        x-u|x--user) user=1 ;;
        x-s|x--system) user=0 ;;
        x-V|x--verbose) verb=1 ;;
        x-v|x--vassal-path) VASSAL_DIR=$2 ; shift ;;
        x-d|x--desktop) desktop_dir=$HOME/Desktop ;;
        x-D|x--no-desktop) desktop_dir= ;;
        x-r|x--remove|x--uninstall) rem=1 ;; 
        *) echo "$0: Unknown option $1" > /dev/stderr ; exit 1 ;;
    esac
    shift
done

# --- Check VASSAL directory -----------------------------------------
if test ! -f $VASSAL_DIR/VASSAL.sh && test $rem -eq 0; then
    echo "$0: '$VASSAL_DIR' is not a valid VASSAL installation" > /dev/stderr
    exit 1
fi

# --- Set prefix and check privileges --------------------------------
if test $user -gt 0 ; then
    prefix=$HOME/.local
else
    prefix=/usr
    if test $UID -ne 0 ; then
        echo "${BC}You need super-user permissions to install in ${UR}${prefix}${NC}" \
             > /dev/stderr
        exit 0
    fi
fi

# --- Set target directories -----------------------------------------
mimetype_dir=$prefix/share/mime/packages
app_dir=$prefix/share/applications
icon_dir=$prefix/share/icons/hicolor/scalable
wrap_file=$prefix/bin/vassal
man_dir=$prefix/share/man/man6

# --- Show current settings ------------------------------------------
msg "${BB}VASSAL installation:\t${UC}$VASSAL_DIR${NC}"
msg "${BB}Mime-types:\t\t${UC}$mimetype_dir${NC}"
msg "${BB}Applications:\t\t${UC}$app_dir${NC}"
msg "${BB}Icons:\t\t\t${UC}$icon_dir${NC}"

# --- Create desktop launcher(s) -------------------------------------
mk_app() {
    if test $rem -gt 0 ; then
        del_file ${app_dir}/${app_name}.desktop

        if test x$desktop_dir != x ; then
            del_file ${desktop_dir}/${app_name}.desktop
        fi
        
        del_file $wrap_file
        
        return 0
    fi

    launch=${VASSAL_DIR}/VASSAL.sh
    
    msg "${BY}Making wrapper script in ${UG}${wrap_file}${NC}"
    mkdir -p `dirname ${wrap_file}`
    cat <<-EOF > ${wrap_file}
    	#!/bin/sh
	#
	# Set JVM option by for example
	#
	# export _JAVA_OPTIONS="-Dswing.systemlaf=javax.swing.plaf.metal.MetalLookAndFeel"
	#
	
	${launch} "\$@"
	EOF

    chmod a+x ${wrap_file}

    launch=$wrap_file
        
    msg "${BY}Making desktop launcher in ${UG}${app_dir}/${app_name}.desktop${NC}"

    mkdir -p ${app_dir}
    sed "s,Exec=vassal,Exec=${launch},g" \
        < ${VASSAL_DIR}/${app_name}.desktop \
        > ${app_dir}/${app_name}.desktop${NC}
    

    if test "x${desktop_dir}" != "x" ; then
        msg "${BY}Making user Desktop short-cut ${UG}${desktop_dir}/${NC}"
        cp ${app_dir}/${app_name}.desktop ${desktop_dir}/
    fi
}

# --- Create mime-type database entry --------------------------------
mk_mime() {
    if test $rem -gt 0 ; then
        del_file ${mimetype_dir}/${app_name}.xml
        return 0
    fi
    
    msg "${BY}Making mime-type entry in ${UG}${mimetype_dir}/${app}.xml${NC}"

    mkdir -p ${mimetype_dir}
    cp ${VASSAL_DIR}/${app_name}.mime.xml ${mimetype_dir}/${app_name}.xml
}

# --- Make icon ------------------------------------------------------
mk_icon() {
    if test $rem -gt 0 ; then
        del_file ${icon_dir}/mimetypes/${app_name}.svg
        del_file ${icon_dir}/apps/${app_name}.svg
        return 0
    fi
    
    msg "${BY}Installing icon ${UG}${icon_dir}/${app}.svg${NC}"

    mkdir -p ${icon_dir}/mimetypes 
    mkdir -p ${icon_dir}/apps 
    cp $VASSAL_DIR/VASSAL.svg ${icon_dir}/mimetypes/${app_name}.svg
    cp $VASSAL_DIR/VASSAL.svg ${icon_dir}/apps/${app_name}.svg
}

# --- Make man -------------------------------------------------------
mk_man() {
    if test $rem -gt 0 ; then
        del_file ${man_dir}/vassal.6
        return 0
    fi
    
    msg "${BY}Installing man(1) page ${UG}${man_dir}/vassal.6${NC}"

    mkdir -p ${man_dir}
    cp $VASSAL_DIR/vassal.6 ${man_dir}/vassal.6
}

# --- Update databases -----------------------------------------------
update_entries() {
    mime_dir=`dirname ${mimetype_dir}` 
    msg "${BY}Updating desktop database ${UG}${app_dir}${NC}"
    update-desktop-database ${app_dir}

    msg "${BY}Updating mime database ${UG}${mime_dir}${NC}"
    update-mime-database ${mime_dir}
}


# --- Run everything -------------------------------------------------
mk_icon
mk_app
mk_mime
mk_man
update_entries

#
# EOF
#
