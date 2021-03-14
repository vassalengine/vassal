package VASSAL.configure;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.NamedKeyManager;
import VASSAL.tools.NamedKeyStroke;
import javax.swing.KeyStroke;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class NamedHotKeyConfigurerTest {

  @Test
  public void Test() {

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Mock DataArchive to return a list of image names
      final DataArchive da = mock(DataArchive.class);
      when(da.getImageNames()).thenReturn(new String[0]);

      // Mock GameModule to return a DataArchive
      final GameModule gm = mock(GameModule.class);
      when(gm.getDataArchive()).thenReturn(da);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      NamedKeyManager nkm = new NamedKeyManager();
      final String key = "key"; // NON-NLS
      final String name = "name"; // NON-NLS
      final NamedKeyStroke namedStroke = NamedKeyStroke.of("xyzzy");
      final NamedKeyStroke keyStroke = NamedKeyStroke.of(KeyStroke.getKeyStroke("A"));

      NamedHotKeyConfigurer config = new NamedHotKeyConfigurer(key, name, namedStroke);

      // check gui builds
      config.getControls();

      //Check basic functionality
      assertThat(config.getKey(), is(equalTo(key)));
      assertThat(config.getName(), is(equalTo(name)));
      assertThat(config.getValue(), is(equalTo(namedStroke)));
      assertThat(config.getValueNamedKeyStroke(), is(equalTo(namedStroke)));

      // Check encoding/decoding
      String encoded = NamedHotKeyConfigurer.encode(namedStroke);
      NamedKeyStroke decoded = NamedHotKeyConfigurer.decode(encoded);
      assertThat(decoded, is(equalTo(namedStroke)));

      // Check setting encoded value
      config.setValue(encoded);
      assertThat(config.getValue(), is(equalTo(namedStroke)));

      config = new NamedHotKeyConfigurer(key, name, keyStroke);

      // check gui builds
      config.getControls();

      // Repeat with an old-style KeyStroke

      //Check basic functionality
      assertThat(config.getKey(), is(equalTo(key)));
      assertThat(config.getName(), is(equalTo(name)));
      assertThat(config.getValue(), is(equalTo(keyStroke)));
      assertThat(config.getValueNamedKeyStroke(), is(equalTo(keyStroke)));
      assertThat(config.getValueString(), is(equalTo("65,0")));

      // Check encoding/decoding
      encoded = NamedHotKeyConfigurer.encode(keyStroke);
      decoded = NamedHotKeyConfigurer.decode(encoded);
      assertThat(decoded, is(equalTo(keyStroke)));

      // Check setting encoded value
      config.setValue(encoded);
      assertThat(config.getValue(), is(equalTo(keyStroke)));
    }
  }
}
