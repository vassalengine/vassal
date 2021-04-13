package VASSAL.tools.version;

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
}
