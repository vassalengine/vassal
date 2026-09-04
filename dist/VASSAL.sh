#!/bin/bash
#
# Consolidated launch wrapper script.  This script has a number of
# features that can help users and developers:
#
# - A file to load can be specified relative to the current directory, e.g.,
#
#        $ VASSAL.sh -l Foo.vmod

# - One can specify an alternative java executable by setting the
#   environment variable JAVA, e.g.,
#
#        $ JAVA=/usr/local/bin/java VASSAL.sh
#
# - One can execute Vassal in a debugger, for example
#
#        $ VASSAL.sh --debug --source ~/vassal/vassal-app/src/main/java \\
#                  --load Foo.vmod
#
# - One can specify an alternative debugger by setting the environment
#   variable JDB, for example
#
#        $ JDB=/usr/local/bin/jdb VASSAL.sh ...
#
# - One can set JVM properties on the commandline by passing the
#   option -D (possibly multiple times), for example
#
#        $ VASSAL.sh -Dswing.defaultlaf=com.sun.java.swing.plaf.motif.MotifLookAndFeel
#
# - One can skip the module manager and directly open either the
#   player or editor by passing the option --direct, for example
#
#        $ VASSAL.sh --direct --load Foo.vmod
#
#   This is useful when debugging or when one want to pass Java
#   property values to the player or editor (the module manager
#   doesn't do that).  Note, when a debugging options is passed, this
#   is also passed.
#
# - One can start a local Vassal server by passing the option --server
#
#        $ VASSAL.sh --server 
#
# --- The various entry points and the JAR ---------------------------
ver_entry=VASSAL.launch.JavaVersionChecker
mod_entry=VASSAL.launch.ModuleManager
ply_entry=VASSAL.launch.Player
edt_entry=VASSAL.launch.Editor
trl_entry=VASSAL.i18n.TranslateVassalWindow
srv_entry=VASSAL.chat.node.Server
srv_url=
srv_port=

# --- Find the path to the installation ------------------------------
path=$(realpath "$0")
path=$(dirname "$path")

# --- Special handling of MacOS --------------------------------------
os=$(uname)
case $os in
    Darwin)
        path=$(realpath "${path}/../..")
	java="${JAVA}"
	jdb="${JDB}"
	if test "x$java" = "x/usr/bin/java" ; then
	    # Found MacOS java wrapper in /usr/bin.
	    # Check that it actually points to valid installation
	    if ! /usr/libexec/java_home -F > /dev/null 2>&1 ; then
		# No Java runtime installed, reset to shipped
		java=
	    fi
	fi
	if test ! -x "$java" ; then 
            java="${path}/Contents/MacOS/jre/bin/java"
	fi
        pre="arch -$(arch)"
        jar="${path}/Contents/Resources/Java/Vengine.jar"
        defs=("-Duser.home=$HOME"
              "-Duser.dir=$path"
              "-Xdock:name=VASSAL"
              "-Xdock:icon=${path}/Contents/Resources/VASSAL.icns")
        ;;
    *)
	java=${JAVA:-$(command -v java 2>/dev/null)}
	jdb=${JDB:-$(command -v jdb 2>/dev/null)}
        pre=
        jar="$path/lib/Vengine.jar"
        defs=("-Duser.home=$HOME" "-Duser.dir=$path")
        ;;    
esac

# --- Check for Java -------------------------------------------------
if test ! -x "$java" ; then
    cat <<-EOF
	$0: Java not installed.  Consult your OS documentation for how to install it.
	EOF
    exit 1
fi

# --- Check for Java version -----------------------------------------
if ! "$java" -cp "$jar" $ver_entry 2>/dev/null ; then
    cat <<-EOF
	$0: Java installation too old to run VASSAL.  Consult your OS documentation for how to upgrade it.
	EOF
    exit 1
fi

# --- Some modus operandi --------------------------------------------
# Do we do direct execution?, i.e., by-pass the module manager. Are we
# debugging?  Possible source path for when debugging, e.g.,
# ~/vassal/vassal-app/src/main/java if the VASSAL sources are cloned
# to ~/vassal
cmd=${java}
drt=0
dbg=0
src=
hlp=0
entry=$mod_entry

# --- Parse command line ---------------------------------------------
while test $# -gt 0 ; do
    case x$1 in
        x-h|x--help)
            hlp=1
            args+=("--help")
            ;;
        x-g|x--debug|x--jdb)
            cmd="$jdb"
            dbg=1
            drt=1
            defs+=("-Duser.home=$HOME" "-Duser.dir=$path")
            ;;
        x-s|x--source)
            src="$src:$2"
            shift
            ;;
        x-D*)
            # Defines must come before JAR and entry point 
            defs+=("$1")
            ;;
        x--direct)
            # If passed, then we by-pass the module manager and
            # execute the relevant entry point directly.
            drt=1
            ;;
        x--translate)
            entry=$trl_entry
            ;;
        x-l|x--load)
            args+=("--load")
            if test $drt -gt 0 ; then
                entry=$ply_entry
            fi
            ;;
        x-e|x--edit)
            args+=("--edit")
            if test $drt -gt 0 ; then
                entry=$edt_entry
            fi
            ;;
        x-n|x--new)
            args+=("--new")
            if test $drt -gt 0 ; then
                entry=$edt_entry
            fi
            ;;
        x--new-extension|x--edit-extension)
            args+=("$1")
            if test $drt -gt 0 ; then
                entry=$edt_entry
            fi
            ;;
        x--server)
            entry=$srv_entry
            ;;
        x-URL|x--url|x-u)
            srv_url="$2"
            ;;
        *)
            # If argument is a file ... 
            if test -f "$1" ; then
                # ... then store full path name since VASSAL changes
                # the current directory before opening target files.
                f=$(realpath "$1")
                args+=("$f")
            else
                args+=("$1")
            fi
            ;;
    esac
    shift
done

# --- Check server ---------------------------------------------------
if test "x$entry" == "x$srv_entry" && test "x$srv_url" == "x" ; then
    args+=("-URL" "none")
fi

# --- Run java with defines, entry point, and other arguments --------
${pre} "${cmd}" "${defs[@]}" -classpath "${jar}" "$entry" "${args[@]}"

# --- Extra help -----------------------------------------------------
if test $hlp -gt 0 ; then
    cat <<-EOF
	In addition to the above options defined by VASSAL proper,
	this wrapper script allows for a number of additional options.

	Options:
	  -g, --debug		  Run the VASSAL process in a debugger
	  -s, --source directory  Set VASSAL source directory for debugger
	  -Dvariable=value	  Set Java variable to value
	  --direct		  By-pass the module manager and run
	                          player, editor, ..., directly
	                          (useful when debugging)
	  --server                Start local Vassal server
	  --translate             Run translation tool

	The Vassal source directory should point to

	    /some/directory/vassal/vassal-app/src/main/java

	where /some/directory/vassal is where you have the Vassal source tree.
	EOF
fi
#
# EOF
#

