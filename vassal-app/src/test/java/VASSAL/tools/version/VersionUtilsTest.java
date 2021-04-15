package VASSAL.tools.version;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class VersionUtilsTest {
  @Test
  public void testNextMinorVersion() {
    assertEquals("3.5", VersionUtils.nextMinorVersion("3.4.0"));
    assertEquals("3.10", VersionUtils.nextMinorVersion("3.9.1"));
  }

  @Test
  public void testTruncateToMinorVersion() {
    assertEquals("3.4", VersionUtils.truncateToMinorVersion("3.4.0"));
    assertEquals("3.9", VersionUtils.truncateToMinorVersion("3.9"));
    assertEquals("3.0", VersionUtils.truncateToMinorVersion("3.0.27"));
    assertEquals("3.5", VersionUtils.truncateToMinorVersion("3.5.0-SNAPSHOT-deadbeef"));
  }

  private static class DummyVersionInfo implements VersionInfo {
    @Override
    public String getRelease() throws IOException {
      return "3.5.5";
    }

    @Override
    public String getBeta() throws IOException {
      return "3.6.0-beta1";
    }
  }

  @Test
  public void testCompareReportable() throws IOException {
    VersionUtils.setVersionInfo(new DummyVersionInfo());
    assertEquals(-1, VersionUtils.compareReportable("3.5.4"));
    assertEquals(0, VersionUtils.compareReportable("3.5.5"));
    assertEquals(-1, VersionUtils.compareReportable("3.5.6-SNAPSHOT"));
    assertEquals(0, VersionUtils.compareReportable("3.6.0-beta1"));
    assertEquals(1, VersionUtils.compareReportable("3.6.0-SNAPSHOT"));
  }
}
