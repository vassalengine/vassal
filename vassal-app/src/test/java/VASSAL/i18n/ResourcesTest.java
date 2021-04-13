package VASSAL.i18n;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Locale;

import org.junit.jupiter.api.Test;


public class ResourcesTest {
  @Test
  public void canSetLocale() {
    Locale locale = Resources.getLocale();
    assertNotNull(locale);
  }
}
