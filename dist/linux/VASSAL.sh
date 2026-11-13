#!/bin/bash
#
# Set JVM option by for example
#
# export _JAVA_OPTIONS="-Dswing.systemlaf=javax.swing.plaf.metal.MetalLookAndFeel"
#
# --- Find the path to the installation ------------------------------
path=$(realpath "$0")
path=$(dirname "$path")

# Set default definitions and arguments 
defs=("-Duser.home=$HOME" "-Duser.dir=$path")
args=()

# --- Find Java and debugger -----------------------------------------
java=${JAVA:-$(command -v java 2>/dev/null)}
jdb=${JAVA:-$(command -v jdb 2>/dev/null)}
cmd="$java"

# --- The various entry points and the JAR ---------------------------
ver_entry=VASSAL.launch.JavaVersionChecker
mod_entry=VASSAL.launch.ModuleManager
ply_entry=VASSAL.launch.Player
edt_entry=VASSAL.launch.Editor
trl_entry=VASSAL.i18n.TranslateVassalWindow
srv_entry=VASSAL.chat.node.Server
entry=$mod_entry
jar="$path/lib/Vengine.jar"
srv_url=null

# --- Some modus operandi --------------------------------------------
# Do we do direct execution?, i.e., by-pass the module manager. Are we
# debugging?  Possible source path for when debugging, e.g.,
# ~/vassal/vassal-app/src/main/java if the VASSAL sources are cloned
# to ~/vassal
drt=0
dbg=0
src=
hlp=0

# --- Ensure a locale which can read non-ASCII filenames -------------
# The JVM decodes filenames with the charset named by sun.jnu.encoding,
# which it derives from the C library locale at startup and which cannot
# be changed afterwards with -D. If that locale is unusable -- unset, C /
# POSIX, or naming locale data which is not installed -- the charset is
# ASCII and every path holding a non-ASCII character throws
# InvalidPathException, which is enough to break simply listing a home
# directory (vassalengine/vassal#15314).
#
# Two subtleties:
#  * LC_ALL is what has to be set. A LANG naming missing locale data makes
#    setlocale(LC_ALL, "") fail as a whole, which defeats a per-category
#    LC_CTYPE even though `locale charmap` then reports UTF-8.
#  * `locale charmap` alone is therefore not a sufficient test; the locale
#    must also load without error, which is what the stderr check covers.
#
# A locale which is merely not UTF-8 is left alone: a legacy ISO-8859-1
# system round-trips its own filenames perfectly well, and overriding it
# would be the regression rather than the fix. Only ASCII is repaired.
#
# Set VASSAL_NO_LOCALE_FIX=1 to skip this.
locale_handles_non_ascii() {
    # the locale must load as a whole ...
    test -z "$(locale 2>&1 >/dev/null)" || return 1
    # ... and must not be ASCII, which cannot represent non-ASCII filenames
    case "$(locale charmap 2>/dev/null)" in
        ANSI_X3.4-1968|ANSI_X3.4-1986|US-ASCII|ASCII) return 1 ;;
    esac
    return 0
}

if test -z "$VASSAL_NO_LOCALE_FIX" && ! locale_handles_non_ascii ; then
    for loc in C.UTF-8 C.utf8 en_US.UTF-8 en_US.utf8 ; do
        if LC_ALL=$loc locale_handles_non_ascii ; then
            export LC_ALL=$loc
            break
        fi
    done
fi

# --- Check for Java -------------------------------------------------
if test ! -x "$java" ; then
    cat <<-EOF
	$0: Java not installed.  Try to install it with f.ex.

	   sudo apt install default-jre

	EOF
    exit 1
fi

# --- Check for Java version -----------------------------------------
if ! "$java" -cp "$jar" $ver_entry 2>/dev/null ; then
    cat <<-EOF
	$0: Java installation too old to run VASSAL.  Please upgrade it
        with for example

           sudo apt upgrade default-jre

	EOF
    exit 1
fi

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

# --- Set source path ------------------------------------------------
if test "x$cmd" == "x$jdb" ; then
    defs+=("-sourcepath" "$src")
fi

# --- Check server ---------------------------------------------------
if test "x$entry" == "x$srv_entry" ; then
    args+=("-URL" "$srv_url")
fi


# --- Run java with defines, entry point, and other arguments --------
"${cmd}" "${defs[@]}" -classpath "${jar}" "$entry" "${args[@]}"

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
	EOF
fi
# /opt/vassal/current/VASSAL.sh "$@"
#
# EOF
#

