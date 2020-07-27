package VASSAL.tools.version;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

@RunWith(BlockJUnit4ClassRunner.class)
public class GitPropertiesTest {

  @Test
  public void shouldReadVersionFromProperties() {
    // prepare
    GitProperties properties = new GitProperties();

    // run
    final String version = properties.getVersion();

    // assert
    assertThat(version, is(equalTo("3.4.5-beta2")));
  }

  @Test
  public void shouldFallbackToDefaultWhenNoPropertiesFile() {
    // prepare
    GitProperties properties = new GitProperties("notexist");

    // run
    final String version = properties.getVersion();

    // assert
    assertThat(version, is(equalTo("3.x development version")));
  }

}
