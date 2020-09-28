package VASSAL.counters;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;

import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import VASSAL.build.GameModule;

/**
 * Test basic Legacy Property Match functionality.
 * All operators are explicitly codes in VASSAL, so need to check them thoroughly.
 * NOTE: Only need to test that =~ and !~ work, no need to test Regexp functionality
 *       as this is provided by Java libraries.
 */
@RunWith(Parameterized.class)
public class PropertiesPieceFilterParameterizedTest {

  private static final String TEST_PROPERTY = "p";
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
      // ----------------------------------
      // = Operator
      // = Integer
      { new String[] { "8", "=", "12" }, false },
      { new String[] { "12", "=", "8" }, false },
      { new String[] { "8", "=", "8" }, true },
      { new String[] { null, "=", "8" }, false },
      { new String[] { "8", "=", "" }, false },
      { new String[] { "abc", "=", "8" }, false },
      { new String[] { "8", "=", "abc" }, false },
      { new String[] { "true", "=", "8" }, false },
      { new String[] { "8", "=", "true" }, false },

      // = Float
      { new String[] { ".8", "=", ".12" }, false },
      { new String[] { ".12", "=", ".8" }, false },
      { new String[] { ".8", "=", ".8" }, true },
      { new String[] { null, "=", ".8" }, false },
      { new String[] { ".8", "=", "" }, false },
      { new String[] { "abc", "=", ".8" }, false },
      { new String[] { ".8", "=", "abc" }, false },
      { new String[] { "true", "=", ".8" }, false },
      { new String[] { ".8", "=", "true" }, false },

      // = Boolean
      { new String[] { "true", "=", "true" }, true },
      { new String[] { "true", "=", "false" }, false },
      { new String[] { "false", "=", "true" }, false },
      { new String[] { "false", "=", "false" }, true },

      { new String[] { null, "=", "false" }, false },
      { new String[] { null, "=", "true" }, false },
      { new String[] { "true", "=", "" }, false },
      { new String[] { "false", "=", "" }, false },

      // = Strings
      { new String[] { "ab", "=", "a" }, false },
      { new String[] { "ab", "=", "ab" }, true },
      { new String[] { null, "=", "ab" }, false },
      { new String[] { "ab", "=", "" }, false },
      { new String[] { "ab", "=", "true" }, false },

      // ----------------------------------
      // != Operator
      // = Integer
      { new String[] { "8", "!=", "12" }, true },
      { new String[] { "12", "!=", "8" }, true },
      { new String[] { "8", "!=", "8" }, false },
      { new String[] { null, "!=", "8" }, true },
      { new String[] { "8", "!=", "" }, true },
      { new String[] { "abc", "!=", "8" }, true },
      { new String[] { "8", "!=", "abc" }, true },
      { new String[] { "true", "!=", "8" }, true },
      { new String[] { "8", "!=", "true" }, true },

      // = Float
      { new String[] { ".8", "!=", ".12" }, true },
      { new String[] { ".12", "!=", ".8" }, true },
      { new String[] { ".8", "!=", ".8" }, false },
      { new String[] { null, "!=", ".8" }, true },
      { new String[] { ".8", "!=", "" }, true },
      { new String[] { "abc", "!=", ".8" }, true },
      { new String[] { ".8", "!=", "abc" }, true },
      { new String[] { "true", "!=", ".8" }, true },
      { new String[] { ".8", "!=", "true" }, true },

      // != Boolean
      // next case is a regression for Bug 13425:
      { new String[] { "true", "!=", "true" }, false },
      { new String[] { "true", "!=", "false" }, true },
      { new String[] { "false", "!=", "true" }, true },
      { new String[] { "false", "!=", "false" }, false },

      { new String[] { null, "!=", "false" }, true },
      { new String[] { null, "!=", "true" }, true },
      { new String[] { "true", "!=", "" }, true },
      { new String[] { "false", "!=", "" }, true },

      // = Strings
      { new String[] { "ab", "!=", "a" }, true },
      { new String[] { "ab", "!=", "ab" }, false },
      { new String[] { null, "!=", "ab" }, true },
      { new String[] { "ab", "!=", "" }, true },
      { new String[] { "ab", "!=", "true" }, true },

      // ----------------------------------
      // <= Operator
      // <= Integer
      { new String[] { "8", "<=", "12" }, true },
      { new String[] { "12", "<=", "8" }, false },
      { new String[] { "8", "<=", "8" }, true },
      { new String[] { null, "<=", "8" }, false },
      { new String[] { "8", "<=", "" }, false },
      { new String[] { "abc", "<=", "8" }, false },
      { new String[] { "8", "<=", "abc" }, true },
      { new String[] { "true", "<=", "8" }, false },
      { new String[] { "8", "<=", "true" }, true },

      // <= Float
      { new String[] { ".8", "<=", ".12" }, false },
      { new String[] { ".12", "<=", ".8" }, true },
      { new String[] { ".8", "<=", ".8" }, true },
      { new String[] { null, "<=", ".8" }, false },
      { new String[] { ".8", "<=", "" }, false },
      { new String[] { "abc", "<=", ".8" }, false },
      { new String[] { ".8", "<=", "abc" }, true },
      { new String[] { "true", "<=", ".8" }, false },
      { new String[] { ".8", "<=", "true" }, true },

      // <= Strings
      { new String[] { "ab", "<=", "ac" }, true },
      { new String[] { "ab", "<=", "ab" }, true },
      { new String[] { "ac", "<=", "ab" }, false },
      { new String[] { "ab", "<=", "abc" }, true },
      { new String[] { "abc", "<=", "ab" }, false },
      { new String[] { null, "<=", "ac" }, false },
      { new String[] { "ab", "<=", "" }, false },

      // ----------------------------------
      // < Operator
      // < Integer
      { new String[] { "8", "<", "12" }, true },
      { new String[] { "12", "<", "8" }, false },
      { new String[] { "8", "<", "8" }, false },
      { new String[] { null, "<", "8" }, false },
      { new String[] { "8", "<", "" }, false },
      { new String[] { "abc", "<", "8" }, false },
      { new String[] { "8", "<", "abc" }, true },
      { new String[] { "true", "<", "8" }, false },
      { new String[] { "8", "<", "true" }, true },

      // < Float
      { new String[] { ".8", "<", ".12" }, false },
      { new String[] { ".12", "<", ".8" }, true },
      { new String[] { ".8", "<", ".8" }, false },
      { new String[] { null, "<", ".8" }, false },
      { new String[] { ".8", "<", "" }, false },
      { new String[] { "abc", "<", ".8" }, false },
      { new String[] { ".8", "<", "abc" }, true },
      { new String[] { "true", "<", ".8" }, false },
      { new String[] { ".8", "<", "true" }, true },

      // < Strings
      { new String[] { "ab", "<", "ac" }, true },
      { new String[] { "ab", "<", "ab" }, false },
      { new String[] { "ac", "<", "ab" }, false },
      { new String[] { "ab", "<", "abc" }, true },
      { new String[] { "abc", "<", "ab" }, false },
      { new String[] { null, "<", "ac" }, false },
      { new String[] { "ab", "<", "" }, false },

      // ----------------------------------
      // >= Operator
      // >= Integer
      { new String[] { "8", ">=", "12" }, false },
      { new String[] { "12", ">=", "8" }, true },
      { new String[] { "8", ">=", "8" }, true },
      { new String[] { null, ">=", "8" }, true },
      { new String[] { "8", ">=", "" }, true },
      { new String[] { "abc", ">=", "8" }, true },
      { new String[] { "8", ">=", "abc" }, false },
      { new String[] { "true", ">=", "8" }, true },
      { new String[] { "8", ">=", "true" }, false },

      // >= Float
      { new String[] { ".8", ">=", ".12" }, true },
      { new String[] { ".12", ">=", ".8" }, false },
      { new String[] { ".8", ">=", ".8" }, true },
      { new String[] { null, ">=", ".8" }, true },
      { new String[] { ".8", ">=", "" }, true },
      { new String[] { "abc", ">=", ".8" }, true },
      { new String[] { ".8", ">=", "abc" }, false },
      { new String[] { "true", ">=", ".8" }, true },
      { new String[] { ".8", ">=", "true" }, false },

      // >= Strings
      { new String[] { "ab", ">=", "ac" }, false },
      { new String[] { "ab", ">=", "ab" }, true },
      { new String[] { "ac", ">=", "ab" }, true },
      { new String[] { "ab", ">=", "abc" }, false },
      { new String[] { "abc", ">=", "ab" }, true },
      { new String[] { null, ">=", "ac" }, true },
      { new String[] { "ab", ">=", "" }, true },

      // ----------------------------------
      // > Operator
      // > Integer
      { new String[] { "8", ">", "12" }, false },
      { new String[] { "12", ">", "8" }, true },
      { new String[] { "8", ">", "8" }, false },
      { new String[] { null, ">", "8" }, true },
      { new String[] { "8", ">", "" }, true },
      { new String[] { "abc", ">", "8" }, true },
      { new String[] { "8", ">", "abc" }, false },
      { new String[] { "true", ">", "8" }, true },
      { new String[] { "8", ">", "true" }, false },

      // > Float
      { new String[] { ".8", ">", ".12" }, true },
      { new String[] { ".12", ">", ".8" }, false },
      { new String[] { ".8", ">", ".8" }, false },
      { new String[] { null, ">", ".8" }, true },
      { new String[] { ".8", ">", "" }, true },
      { new String[] { "abc", ">", ".8" }, true },
      { new String[] { ".8", ">", "abc" }, false },
      { new String[] { "true", ">", ".8" }, true },
      { new String[] { ".8", ">", "true" }, false },

      // > Strings
      { new String[] { "ab", ">", "ac" }, false },
      { new String[] { "ab", ">", "ab" }, false },
      { new String[] { "ac", ">", "ab" }, true },
      { new String[] { "ab", ">", "abc" }, false },
      { new String[] { "abc", ">", "ab" }, true },
      { new String[] { null, ">", "ac" }, true },
      { new String[] { "ab", ">", "" }, true },

      // ----------------------------------
      // =~ Operator
      { new String[] { "abc", "=~", "a|x|abc|d" }, true },
      { new String[] { "ac", "=~", "a|x|abc|d" }, false },

      // ----------------------------------
      // =! Operator
      { new String[] { "abc", "!~", "a|x|abc|d" }, false },
      { new String[] { "ac", "!~", "a|x|abc|d" }, true },
    });
  }

  @Test
  public void testComparisonFilters() {
    String propertyValue = input[0];
    String comparison = input[1];
    String compareValue = input[2];
    String expression = TEST_PROPERTY + comparison + compareValue;
    String test = propertyValue + comparison + compareValue;

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Ensure any null properties are recognized by the GameModule and null returned.
      final GameModule gm = mock(GameModule.class);
      when(gm.getProperty(Matchers.anything())).thenReturn(null);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final BasicPiece bp = new BasicPiece();
      bp.setProperty(TEST_PROPERTY, propertyValue);
      final PieceFilter filter = PropertiesPieceFilter.parse(expression);
      assertThat(test, filter.accept(bp), is(expectedResult));
    }
  }
}
