package VASSAL.i18n;

import static org.junit.Assert.assertNotNull;

import java.util.Locale;

import org.junit.Test;


public class ResourcesTest {
  @Test
  public void canSetLocale() {
    Locale locale = Resources.getLocale();
    assertNotNull(locale);
  }
}
