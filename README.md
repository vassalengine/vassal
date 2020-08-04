The VASSAL Team is happy to announce the release of VASSAL 3.3.2.

********************************************************************************
IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT
********************************************************************************

* Things may be broken which worked in 3.2.17. If you find a bug introduced
  since 3.2.17, please report it.

* If you save a module in VASSAL 3.3, you will NOT be able to open it
  afterwards with VASSAL 3.2. We recommend keeping a backup copy of
  any pre-3.3 modules you plan to modify in 3.3 until you've verified that
  everything works to your satisfaction.

* Some modules containing custom drawing code may render incorrectly on HiDPI
  displays. If you are the maintainer of such a module, let us know and we
  can advise you about what needs updating for 3.3 in your code. (We are aware
  already that VASL has this problem and are working with its maintainers to
  update it.)

********************************************************************************
IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT IMPORTANT
********************************************************************************


*********************
Download
*********************

For Linux:

  https://github.com/vassalengine/vassal/releases/download/3.3.2/VASSAL-3.3.2-linux.tar.bz2

For Mac OS X:

  https://github.com/vassalengine/vassal/releases/download/3.3.2/VASSAL-3.3.2-macosx.dmg

For Windows (64-bit):

  https://github.com/vassalengine/vassal/releases/download/3.3.2/VASSAL-3.3.2-windows-64.exe

For Windows (32-bit):

  https://github.com/vassalengine/vassal/releases/download/3.3.2/VASSAL-3.3.2-windows-32.exe

For other operating systems:

  https://github.com/vassalengine/vassal/releases/download/3.3.2/VASSAL-3.3.2-other.zip


*************************
Changes since 3.3.1
*************************

* 13182: Piece images in Inventory windows are drawn offset on HiDPI screens
* 13181: Piece images in Inventory windows have truncated height on Macs
* 13176: Bundled Java fails to start on Mac OS X < 10.13
* 13175: Add bundle for 32-bit Windows
* 13152: Version number and year set incorrectly in Mac DMG's Info.plist
* 13143: HTTPS URLs fail to load (handshake_failure) due to missing
    jdk.crypto.ec module
* 13140: Versions with a build number are incorrectly sorted before versions
    without a build number
* 13136: Update notifier still has old SourceForge URL for downloads
* 13126: Right mouse button and Control-Click broken on Macs
* 12861: Improved behavior of Zone editor

For changes prior to this release, see the change log.


*********************
Minimum Requirements
*********************

VASSAL 3.3 requires Java 11 or later.

The Windows and Mac packages have an appropriate version of Java bundled with
them, so there is no need to install Java separately on those operating
systems.


*********************
Help us test
*********************

Should you find any problems/oddities/bugs, please tell us so that we can
fix them for a future release of VASSAL.

Report bugs here in the Technical Support & Bugs category at the VASSAL
forums:

http://www.vassalengine.org/forum/viewforum.php?f=3

or by email to bugs@vassalengine.org.


Cheers,

The VASSAL Team
