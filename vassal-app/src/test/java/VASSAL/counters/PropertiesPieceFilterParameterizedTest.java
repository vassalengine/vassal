package VASSAL.counters;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import VASSAL.build.GameModule;

@RunWith(Parameterized.class)
public class PropertiesPieceFilterParameterizedTest {

  private final String[] input;
  private final boolean expectedResult;

  public PropertiesPieceFilterParameterizedTest(String[] input, boolean expectedResult) {
    this.input = input;
    this.expectedResult = expectedResult;
  }

  /**
   * The first element is an array of test inputs, the second is the expected result
   */
  @Parameterized.Parameters
  public static Collection<Object[]> testData() {
    return Arrays.asList(new Object[][]{
      // next case is a regression for Bug 13425:
      { new String[] { null, "mos!=false" }, true },
      { new String[] { null, "mos!=true" }, true },
      { new String[] { null, "mos=false" }, false },
      { new String[] { null, "mos=true" }, false },

      { new String[] { "true", "mos=true" }, true },
      { new String[] { "true", "mos=false" }, false },
      { new String[] { "false", "mos=true" }, false },
      { new String[] { "false", "mos=false" }, true },

      { new String[] { "true", "mos!=true" }, false },
      { new String[] { "true", "mos!=false" }, true },
      { new String[] { "false", "mos!=true" }, true },
      { new String[] { "false", "mos!=false" }, false },
    });
  }

  @Test
  public void testComparisonFilters() {
    String propertyValue = input[0];
    String expression = input[1];

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      when(gm.getProperty("mos")).thenReturn(propertyValue);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final BasicPiece bp = new BasicPiece();
      final PieceFilter filter = PropertiesPieceFilter.parse(expression);
      assertThat(filter.accept(bp), is(expectedResult));
    }
  }

}
