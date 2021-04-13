package VASSAL.counters;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class PropertiesPieceFilterTest {


  /**
   * More complex tests that cannot be parameterized
   * See {@link PropertiesPieceFilterParameterizedTest}
   */

  @Test
  public void test() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Ensure any null properties are recognized by the GameModule and null returned.
      final GameModule gm = mock(GameModule.class);
      when(gm.getProperty(Matchers.anything())).thenReturn(null);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final BasicPiece bp1 = new BasicPiece();
      bp1.setProperty("p_ab", "ab");
      bp1.setProperty("p_abc", "abc");

      PieceFilter filter;

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

}