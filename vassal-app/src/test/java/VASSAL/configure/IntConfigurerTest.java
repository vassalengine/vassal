package VASSAL.configure;

import java.awt.Point;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

public class IntConfigurerTest {

  @Test
  public void Test() {
    final String key = "key"; // NON-NLS
    final String name = "name"; // NON-NLS
    final Integer value = 42;
    final Integer dflt = 24;

    IntConfigurer config = new IntConfigurer(key, name, value);

    // check gui builds
    config.getControls();

    //Check basic functionality
    assertThat(config.getKey(), is(equalTo(key)));
    assertThat(config.getName(), is(equalTo(name)));
    assertThat(config.getValue(), is(equalTo(value)));
    assertThat(config.getIntValue(dflt), is(equalTo(value)));
    assertThat(config.getValueString(), is(equalTo(String.valueOf(value))));

    // Trying to set value to a non-number string should quietly fail and not change value
    config.setValue("xyzzy"); // NON-NLS
    assertThat(config.getIntValue(dflt), is(equalTo(value)));

    // Setting value to an int String should change value
    config.setValue("52");
    assertThat(config.getIntValue(dflt), is(equalTo(52)));

    // Setting value to rubbish will allow default value to be returned. [Why not fail quietly when trying to set???]
    config.setValue(new Point(0, 0));
    assertThat(config.getIntValue(dflt), is(equalTo(dflt)));

  }
}