package VASSAL.configure;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import VASSAL.counters.BasicPiece;
import VASSAL.counters.DecoratorTest;
import VASSAL.tools.icon.IconFactory;
import java.awt.image.BufferedImage;
import javax.swing.ImageIcon;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class BeanShellExpressionConfigurerTest extends DecoratorTest {

  @Test
  public void constructors() {
    BufferedImage dummyImage = new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR);
    // Create a static mock for IconFactory and return the Calculator icon when asked. Allows Editors with Beanshell configurers to initialise.
    try (MockedStatic<IconFactory> staticIf = Mockito.mockStatic(IconFactory.class)) {
      staticIf.when(() -> IconFactory.getIcon("calculator", 12)).thenReturn(new ImageIcon(dummyImage));
      final String key = "key";
      final String name = "name";
      final String value = "x * 2";
      final BasicPiece piece = createBasicPiece();
      final BeanShellExpressionConfigurer.Option option = BeanShellExpressionConfigurer.Option.PME;


      BeanShellExpressionConfigurer c;

      c = new BeanShellExpressionConfigurer(value, piece);
      assertThat(c.getValueString(), is(equalTo(value)));

      assertThat(c.cleanName("abc"), is(equalTo("abc"))); // NON-NLS
      assertThat(c.cleanName("a bc"), is(equalTo("GetProperty(\"a bc\")"))); // NON-NLS

      c = new BeanShellExpressionConfigurer(key, name, value, piece);
      assertThat(c.getKey(), is(equalTo(key)));
      assertThat(c.getName(), is(equalTo(name)));
      assertThat(c.getValueString(), is(equalTo(value)));

      c = new BeanShellExpressionConfigurer(key, name, value, piece, option);
      assertThat(c.getKey(), is(equalTo(key)));
      assertThat(c.getName(), is(equalTo(name)));
      assertThat(c.getOption(), is(equalTo(option)));
      assertThat(c.getValueString(), is(equalTo(value)));
    }
  }


}