package VASSAL.tools.version;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;

import static junit.framework.Assert.assertEquals;

@RunWith(BlockJUnit4ClassRunner.class)
public class GitPropertiesTest {

  @Test
  public void shouldReadVersionFromProperties() {
    // prepare
    GitProperties properties = new GitProperties();

    // run
    final String version = properties.getVersion();

    // assert
    assertEquals("3.3.1-test-86-g0123456789ab", version);
  }

  @Test
  public void shouldFallbackToDefaultWhenNoPropertiesFile() {
    // prepare
    GitProperties properties = new GitProperties("notexist");

    // run
    final String version = properties.getVersion();

    // assert
    assertEquals("3.3.0-0-g_development", version);
  }

}
