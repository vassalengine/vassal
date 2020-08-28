package VASSAL;

import org.junit.Test;

import static org.junit.Assert.*;

public class InfoTest {
  @Test
  public void testIsModuleTooNewYes() {
    assertTrue(Info.isModuleTooNew("3.5.0"));
  }

  @Test
  public void testIsModuleTooNewNo() {
    assertFalse(Info.isModuleTooNew("3.4.0"));
  }

  @Test
  public void testHasOldFormatYes() {
    assertTrue(Info.hasOldFormat("3.3.0"));
  }

  @Test
  public void testHasOldFormatNo() {
    assertFalse(Info.hasOldFormat("3.4.0"));
  }
}
