package VASSAL.counters;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;

import org.hamcrest.Matchers;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class PropertiesPieceFilterTest {

  // A property with a null value
  public static final String PROP1_KEY = "p1";
  public static final String PROP1_VAL = null;

  // A property with a String value
  public static final String PROP2_KEY = "p2";
  public static final String PROP2_VAL = "p2val";

  // Properties to test numeric comparisons
  public static final String PROP_8_KEY = "p_8";
  public static final String PROP_8_VAL = "8";
  public static final String PROP_12_KEY = "p_12";
  public static final String PROP_12_VAL = "12";

  // Properties to test Alpha comparisons
  public static final String PROP_ABC_KEY = "p_abc";
  public static final String PROP_ABC_VAL = "abc";
  public static final String PROP_ABCD_KEY = "p_abcd";
  public static final String PROP_ABCD_VAL = "abcd";
  public static final String PROP_AB_KEY = "p_ab";
  public static final String PROP_AB_VAL = "ab";
  public static final String PROP_AC_KEY = "p_ac";
  public static final String PROP_AC_VAL = "ac";
  /*
   * Exercise the various Legacy property match expression comparisons
   */
  @Test
  public void test() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Ensure any null properties are recognized by the GameModule and null returned.
      final GameModule gm = mock(GameModule.class);
      when(gm.getProperty(Matchers.anything())).thenReturn(null);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final BasicPiece bp1 = new BasicPiece();
      bp1.setProperty(PROP1_KEY, PROP1_VAL);
      bp1.setProperty(PROP2_KEY, PROP2_VAL);
      bp1.setProperty(PROP_8_KEY, PROP_8_VAL);
      bp1.setProperty(PROP_12_KEY, PROP_12_VAL);
      bp1.setProperty(PROP_AB_KEY, PROP_AB_VAL);
      bp1.setProperty(PROP_ABC_KEY, PROP_ABC_VAL);
      bp1.setProperty(PROP_ABCD_KEY, PROP_ABCD_VAL);
      bp1.setProperty(PROP_AC_KEY, PROP_AC_VAL);

      PieceFilter filter;

      // Test !=
      filter = PropertiesPieceFilter.parse("p1!=null");
      assertThat("Compare null property != null", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p1!=junk");
      assertThat("Compare null property != string", filter.accept(bp1), is(true));

      filter = PropertiesPieceFilter.parse("p2!=");
      assertThat("Compare property != null", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p2!=junk");
      assertThat("Compare property != wrong value", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p2!=p2val");
      assertThat("Compare property != correct value", filter.accept(bp1), is(false));

      filter = PropertiesPieceFilter.parse("p_8!=12");
      assertThat("Compare 8 != 12", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_8!=8");
      assertThat("Compare 8 != 8", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_12!=8");
      assertThat("Compare 12 != 8", filter.accept(bp1), is(true));

      // Test <=
      filter = PropertiesPieceFilter.parse("p_8<=12");
      assertThat("Compare 8 <= 12", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_8<=8");
      assertThat("Compare 8 <= 8", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_12<=8");
      assertThat("Compare 12 <= 8", filter.accept(bp1), is(false));

      filter = PropertiesPieceFilter.parse("p_ab<=ac");
      assertThat("Compare ab <= ac", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab<=ab");
      assertThat("Compare ab <= ab", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ac<=ab");
      assertThat("Compare ac <= ab", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab<=abc");
      assertThat("Compare ab <= abc", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_abc=<ab");
      assertThat("Compare abc <= ab", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab<=");
      assertThat("Compare ab <= null", filter.accept(bp1), is(false));

      // Test >=
      filter = PropertiesPieceFilter.parse("p_8>=12");
      assertThat("Compare 8 >= 12", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_8>=8");
      assertThat("Compare 8 >= 8", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_12>=8");
      assertThat("Compare 12 >= 8", filter.accept(bp1), is(true));

      filter = PropertiesPieceFilter.parse("p_ab>=ac");
      assertThat("Compare ab >= ac", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab>=ab");
      assertThat("Compare ab >= ab", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ac>=ab");
      assertThat("Compare ac >= ab", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab>=abc");
      assertThat("Compare ab >= abc", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_abc>=ab");
      assertThat("Compare abc >= ab", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab>=");
      assertThat("Compare ab >= null", filter.accept(bp1), is(true));

      // Test <
      filter = PropertiesPieceFilter.parse("p_8<12");
      assertThat("Compare 8 < 12", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_8<8");
      assertThat("Compare 8 < 8", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_12<8");
      assertThat("Compare 12 < 8", filter.accept(bp1), is(false));

      filter = PropertiesPieceFilter.parse("p_ab<ac");
      assertThat("Compare ab < ac", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab<ab");
      assertThat("Compare ab < ab", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ac<ab");
      assertThat("Compare ac < ab", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab<abc");
      assertThat("Compare ab < abc", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_abc<ab");
      assertThat("Compare abc < ab", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab<");
      assertThat("Compare ab < null", filter.accept(bp1), is(false));


      // Test >
      filter = PropertiesPieceFilter.parse("p_8>12");
      assertThat("Compare 8 > 12", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_8>8");
      assertThat("Compare 8 > 8", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_12>8");
      assertThat("Compare 12 > 8", filter.accept(bp1), is(true));

      filter = PropertiesPieceFilter.parse("p_ab>ac");
      assertThat("Compare ab > ac", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab>ab");
      assertThat("Compare ab > ab", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ac>ab");
      assertThat("Compare ac > ab", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab>abc");
      assertThat("Compare ab > abc", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_abc>ab");
      assertThat("Compare abc > ab", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab>");
      assertThat("Compare ab > null", filter.accept(bp1), is(true));

      // Test =
      filter = PropertiesPieceFilter.parse("p1=null");
      assertThat("Compare null property = null", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p1=junk");
      assertThat("Compare null property = string", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p1=true");
      assertThat("Compare null property = true", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p1=false");
      assertThat("Compare null property = false", filter.accept(bp1), is(false));

      filter = PropertiesPieceFilter.parse("p2=");
      assertThat("Compare property = null", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p2=junk");
      assertThat("Compare property = wrong value", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p2=p2val");
      assertThat("Compare property = correct value", filter.accept(bp1), is(true));

      filter = PropertiesPieceFilter.parse("p_8=12");
      assertThat("Compare 8 = 12", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_8=8");
      assertThat("Compare 8 = 8", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_12=8");
      assertThat("Compare 12 = 8", filter.accept(bp1), is(false));

      // Test =~
      filter = PropertiesPieceFilter.parse("p_abc=~a|x|abc|d");
      assertThat("Compare abc matches a|x|abc|d", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ac=~a|x|abc|d");
      assertThat("Compare ac matches a|x|abc|d", filter.accept(bp1), is(false));

      // Test !~
      filter = PropertiesPieceFilter.parse("p_abc!~a|x|abc|d");
      assertThat("Compare abc not matches a|x|abc|d", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ac!~a|x|abc|d");
      assertThat("Compare ac not matches a|x|abc|d", filter.accept(bp1), is(true));

      // Test &&
      filter = PropertiesPieceFilter.parse("p_ab=ab && p_abc>ac");
      assertThat("Check && Test 1", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_ab=ab && p_abc<ac");
      assertThat("Check && Test 2", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_abc=ab && p_abc<ac");
      assertThat("Check && Test 3", filter.accept(bp1), is(false));
      filter = PropertiesPieceFilter.parse("p_abc=ab && p_abc>ac");
      assertThat("Check && Test 4", filter.accept(bp1), is(false));

      // Test ||
      filter = PropertiesPieceFilter.parse("p_ab=ab || p_abc>ac");
      assertThat("Check || Test 1", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_ab=ab || p_abc<ac");
      assertThat("Check || Test 2", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_abc=ab || p_abc<ac");
      assertThat("Check || Test 3", filter.accept(bp1), is(true));
      filter = PropertiesPieceFilter.parse("p_abc=ab || p_abc>ac");
      assertThat("Check || Test 4", filter.accept(bp1), is(false));
    }
  }

  /*
   * Test specifically for a regression of bug 13425.
   * Legacy property match expression  null != "false" evaluating as true.
   * That may be valid in a Java sense, but not on a Vassal sense.
   */
  @Test
  public void bug_13425() {
    // Regression test for Issue 13425
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      when(gm.getProperty("mos")).thenReturn(null);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final BasicPiece bp = new BasicPiece();
      final PieceFilter filter = PropertiesPieceFilter.parse("mos!=false");
      assertThat("Bug 13425 regression test", filter.accept(bp), is(true));
    }
  }
}