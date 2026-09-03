#!/bin/bash
#
# --- Find the path to the installation ------------------------------
path="$(dirname "$0")/../.."

# Set default definitions and arguments 
defs=("-Duser.home=$HOME"
      "-Duser.dir=$path"
      "-Xdock:name=VASSAL"
      "-Xdock:icon=Contents/Resources/VASSAL.icns")
args=()

# --- Architecture ---------------------------------------------------
if sysctl machdep.cpu.brand_string | grep -q Intel ; then
    arch=x86_64
else
    arch=arm64
fi

# --- Find Java and debugger -----------------------------------------
java=Contents/MacOS/jre/bin/java
cmd="$java"

# --- The various entry points and the JAR ---------------------------
ver_entry=VASSAL.launch.JavaVersionChecker
mod_entry=VASSAL.launch.ModuleManager
ply_entry=VASSAL.launch.Player
edt_entry=VASSAL.launch.Editor
trl_entry=VASSAL.i18n.TranslateVassalWindow
srv_entry=VASSAL.chat.node.Server
entry=$mod_entry
jar="Contents/Resources/Java/Vengine.jar"
srv_url=null

# --- Some modus operandi --------------------------------------------
# Do we do direct execution?, i.e., by-pass the module manager. Are we
# debugging?  Possible source path for when debugging, e.g.,
# ~/vassal/vassal-app/src/main/java if the VASSAL sources are cloned
# to ~/vassal
drt=0
src=
hlp=0

# --- Parse command line ---------------------------------------------
while test $# -gt 0 ; do
    case x$1 in
        x-h|x--help)
            hlp=1
            args+=("--help")
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
        x--new-extension)
            args+=("$1")
            if test $drt -gt 0 ; then
                entry=$edt_entry
            fi
            ;;
        x--edit-extension)
            args+=("$1")
            if test $drt -gt 0 ; then
                entry=$edt_entry
            fi
                        ;;
        x--server)
            entry=$srv_entry
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

# --- Go to application directory ------------------------------------
cd $path

# --- Check server ---------------------------------------------------
if test "x$entry" == "x$srv_entry" ; then
    args+=("-URL" "$srv_url")
fi

# --- Run java with defines, entry point, and other arguments --------
arch -${arch} "${cmd}" "${defs[@]}" -classpath "${jar}" "$entry" "${args[@]}"

# --- Extra help -----------------------------------------------------
if test $hlp -gt 0 ; then
    cat <<-EOF
	In addition to the above options defined by VASSAL proper,
	this wrapper script allows for a number of additional options.

	Options:
	  -Dvariable=value	  Set Java variable to value
	  --direct		  By-pass the module manager and run
	                          player, editor, ..., directly.
	  --server                Start local Vassal server
	EOF
fi
#
# EOF
#

