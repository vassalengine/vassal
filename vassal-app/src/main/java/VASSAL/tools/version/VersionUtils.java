package VASSAL.tools.version;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import VASSAL.Info;
import VASSAL.tools.io.IOUtils;


public class VersionUtils {
  protected VersionUtils() {}

  private static final String baseURL = "http://www.vassalengine.org/util/";

  private static final String currentRelease = "current-release";
  private static final String currentBeta = "current-beta";
  private static final String bugCheck = "check-version-bug.php?version=";


  private static VassalVersion release = null;
  private static VassalVersion beta = null;

  public static VassalVersion getRelease() throws IOException {
    if (release == null) release = getVersion(baseURL + currentRelease);
    return release;
  }

  public static VassalVersion getBeta() throws IOException {
    if (beta == null) beta = getVersion(baseURL + currentBeta);
    return beta;
  }

  private static VassalVersion getVersion(String url) throws IOException {
    try (InputStream in = new URL(url).openStream()) {
      final VassalVersion version =
        new VassalVersion(IOUtils.toString(in, StandardCharsets.UTF_8).trim());
      return version;
    }
  }

  // TODO delete commented code or reactivate it
/*
  public static boolean isReportable(String version)
      throws IOException, NumberFormatException {

    try (InputStream in = new URL(baseURL + bugCheck + version).openStream()) {
      final int result = Integer.parseInt(IOUtils.toString(in));

      switch (result) {
      case 0: return false;
      case 1: return true;
      default:
        throw new NumberFormatException("bad return value: " + result);
      }
    }
  }
*/

  public static boolean isCurrent(VassalVersion version) throws IOException {
    // a version is current if it would update to itself
    return version.equals(update(version));
  }

  public static VassalVersion update(VassalVersion version) throws IOException {
    VassalVersion current = VersionUtils.getRelease();
    switch (sgn(version.compareTo(current))) {
    case -1: // version is older than the current release
      return current;
    case  0: // version is the current release
      return version;
    case  1:
      // version is newer than the current release
      current = VersionUtils.getBeta();
      switch (sgn(version.compareTo(current))) {
      case -1:  // version is older than the current beta
        return current;
      case  0:  // version is the current beta
      case  1:  // version is newer than the current beta
        return version;
      }
    }

    throw new IllegalStateException();
  }

  private static int sgn(int i) {
    return Integer.compare(i, 0);
  }

  public static void main(String[] args) throws IOException {
    final VassalVersion v = new VassalVersion(Info.getVersion());
    System.out.println(v + " is current? " + isCurrent(v));
  }
}
