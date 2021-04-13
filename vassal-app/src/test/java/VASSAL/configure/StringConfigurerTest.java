package VASSAL.configure;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

public class StringConfigurerTest {

  @Test
  public void Test() {
    final String key = "key";
    final String name = "name";
    final String value = "value";

    final StringConfigurer config = new StringConfigurer(key, name, value);
    config.getControls();

    assertThat(config.getKey(), is(equalTo(key)));
    assertThat(config.getName(), is(equalTo(name)));
    assertThat(config.getValueString(), is(equalTo(value)));
  }
}