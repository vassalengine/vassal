package VASSAL.counters;

import java.util.stream.Stream;

import VASSAL.build.GameModule;

import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test basic Legacy Property Match functionality.
 * All operators are explicitly codes in VASSAL, so need to check them thoroughly.
 * NOTE: Only need to test that =~ and !~ work, no need to test Regexp functionality
 *       as this is provided by Java libraries.
 */
public class PropertiesPieceFilterParameterizedTest {

  private static final String TEST_PROPERTY = "p";

  @ParameterizedTest
  @MethodSource("testData")
  public void testComparisonFilters(String[] input, boolean expectedResult) {
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

  /**
   * The first element is an array of test inputs, the second is the expected result
   */
  private static Stream<Arguments> testData() {
    return Stream.of(
        // ----------------------------------
        // = Operator
        // = Integer
        Arguments.of(new String[] { "8", "=", "12" }, false),
        Arguments.of(new String[] { "12", "=", "8" }, false),
        Arguments.of(new String[] { "8", "=", "8" }, true),
        Arguments.of(new String[] { null, "=", "8" }, false),
        Arguments.of(new String[] { "8", "=", "" }, false),
        Arguments.of(new String[] { "abc", "=", "8" }, false),
        Arguments.of(new String[] { "8", "=", "abc" }, false),
        Arguments.of(new String[] { "true", "=", "8" }, false),
        Arguments.of(new String[] { "8", "=", "true" }, false),

        // = Float
        Arguments.of(new String[] { ".8", "=", ".12" }, false),
        Arguments.of(new String[] { ".12", "=", ".8" }, false),
        Arguments.of(new String[] { ".8", "=", ".8" }, true),
        Arguments.of(new String[] { null, "=", ".8" }, false),
        Arguments.of(new String[] { ".8", "=", "" }, false),
        Arguments.of(new String[] { "abc", "=", ".8" }, false),
        Arguments.of(new String[] { ".8", "=", "abc" }, false),
        Arguments.of(new String[] { "true", "=", ".8" }, false),
        Arguments.of(new String[] { ".8", "=", "true" }, false),

        // = Boolean
        Arguments.of(new String[] { "true", "=", "true" }, true),
        Arguments.of(new String[] { "true", "=", "false" }, false),
        Arguments.of(new String[] { "false", "=", "true" }, false),
        Arguments.of(new String[] { "false", "=", "false" }, true),

        Arguments.of(new String[] { null, "=", "false" }, false),
        Arguments.of(new String[] { null, "=", "true" }, false),
        Arguments.of(new String[] { "true", "=", "" }, false),
        Arguments.of(new String[] { "false", "=", "" }, false),

        // = Strings
        Arguments.of(new String[] { "ab", "=", "a" }, false),
        Arguments.of(new String[] { "ab", "=", "ab" }, true),
        Arguments.of(new String[] { null, "=", "ab" }, false),
        Arguments.of(new String[] { "ab", "=", "" }, false),
        Arguments.of(new String[] { "ab", "=", "true" }, false),

        // ----------------------------------
        // != Operator
        // = Integer
        Arguments.of(new String[] { "8", "!=", "12" }, true),
        Arguments.of(new String[] { "12", "!=", "8" }, true),
        Arguments.of(new String[] { "8", "!=", "8" }, false),
        Arguments.of(new String[] { null, "!=", "8" }, true),
        Arguments.of(new String[] { "8", "!=", "" }, true),
        Arguments.of(new String[] { "abc", "!=", "8" }, true),
        Arguments.of(new String[] { "8", "!=", "abc" }, true),
        Arguments.of(new String[] { "true", "!=", "8" }, true),
        Arguments.of(new String[] { "8", "!=", "true" }, true),

        // = Float
        Arguments.of(new String[] { ".8", "!=", ".12" }, true),
        Arguments.of(new String[] { ".12", "!=", ".8" }, true),
        Arguments.of(new String[] { ".8", "!=", ".8" }, false),
        Arguments.of(new String[] { null, "!=", ".8" }, true),
        Arguments.of(new String[] { ".8", "!=", "" }, true),
        Arguments.of(new String[] { "abc", "!=", ".8" }, true),
        Arguments.of(new String[] { ".8", "!=", "abc" }, true),
        Arguments.of(new String[] { "true", "!=", ".8" }, true),
        Arguments.of(new String[] { ".8", "!=", "true" }, true),

        // != Boolean
        // next case is a regression for Bug 13425:
        Arguments.of(new String[] { "true", "!=", "true" }, false),
        Arguments.of(new String[] { "true", "!=", "false" }, true),
        Arguments.of(new String[] { "false", "!=", "true" }, true),
        Arguments.of(new String[] { "false", "!=", "false" }, false),

        Arguments.of(new String[] { null, "!=", "false" }, true),
        Arguments.of(new String[] { null, "!=", "true" }, true),
        Arguments.of(new String[] { "true", "!=", "" }, true),
        Arguments.of(new String[] { "false", "!=", "" }, true),

        // = Strings
        Arguments.of(new String[] { "ab", "!=", "a" }, true),
        Arguments.of(new String[] { "ab", "!=", "ab" }, false),
        Arguments.of(new String[] { null, "!=", "ab" }, true),
        Arguments.of(new String[] { "ab", "!=", "" }, true),
        Arguments.of(new String[] { "ab", "!=", "true" }, true),

        // ----------------------------------
        // <= Operator
        // <= Integer
        Arguments.of(new String[] { "8", "<=", "12" }, true),
        Arguments.of(new String[] { "12", "<=", "8" }, false),
        Arguments.of(new String[] { "8", "<=", "8" }, true),
        Arguments.of(new String[] { null, "<=", "8" }, false),
        Arguments.of(new String[] { "8", "<=", "" }, false),
        Arguments.of(new String[] { "abc", "<=", "8" }, false),
        Arguments.of(new String[] { "8", "<=", "abc" }, true),
        Arguments.of(new String[] { "true", "<=", "8" }, false),
        Arguments.of(new String[] { "8", "<=", "true" }, true),

        // <= Float
        Arguments.of(new String[] { ".8", "<=", ".12" }, false),
        Arguments.of(new String[] { ".12", "<=", ".8" }, true),
        Arguments.of(new String[] { ".8", "<=", ".8" }, true),
        Arguments.of(new String[] { null, "<=", ".8" }, false),
        Arguments.of(new String[] { ".8", "<=", "" }, false),
        Arguments.of(new String[] { "abc", "<=", ".8" }, false),
        Arguments.of(new String[] { ".8", "<=", "abc" }, true),
        Arguments.of(new String[] { "true", "<=", ".8" }, false),
        Arguments.of(new String[] { ".8", "<=", "true" }, true),

        // <= Strings
        Arguments.of(new String[] { "ab", "<=", "ac" }, true),
        Arguments.of(new String[] { "ab", "<=", "ab" }, true),
        Arguments.of(new String[] { "ac", "<=", "ab" }, false),
        Arguments.of(new String[] { "ab", "<=", "abc" }, true),
        Arguments.of(new String[] { "abc", "<=", "ab" }, false),
        Arguments.of(new String[] { null, "<=", "ac" }, false),
        Arguments.of(new String[] { "ab", "<=", "" }, false),

        // ----------------------------------
        // < Operator
        // < Integer
        Arguments.of(new String[] { "8", "<", "12" }, true),
        Arguments.of(new String[] { "12", "<", "8" }, false),
        Arguments.of(new String[] { "8", "<", "8" }, false),
        Arguments.of(new String[] { null, "<", "8" }, false),
        Arguments.of(new String[] { "8", "<", "" }, false),
        Arguments.of(new String[] { "abc", "<", "8" }, false),
        Arguments.of(new String[] { "8", "<", "abc" }, true),
        Arguments.of(new String[] { "true", "<", "8" }, false),
        Arguments.of(new String[] { "8", "<", "true" }, true),

        // < Float
        Arguments.of(new String[] { ".8", "<", ".12" }, false),
        Arguments.of(new String[] { ".12", "<", ".8" }, true),
        Arguments.of(new String[] { ".8", "<", ".8" }, false),
        Arguments.of(new String[] { null, "<", ".8" }, false),
        Arguments.of(new String[] { ".8", "<", "" }, false),
        Arguments.of(new String[] { "abc", "<", ".8" }, false),
        Arguments.of(new String[] { ".8", "<", "abc" }, true),
        Arguments.of(new String[] { "true", "<", ".8" }, false),
        Arguments.of(new String[] { ".8", "<", "true" }, true),

        // < Strings
        Arguments.of(new String[] { "ab", "<", "ac" }, true),
        Arguments.of(new String[] { "ab", "<", "ab" }, false),
        Arguments.of(new String[] { "ac", "<", "ab" }, false),
        Arguments.of(new String[] { "ab", "<", "abc" }, true),
        Arguments.of(new String[] { "abc", "<", "ab" }, false),
        Arguments.of(new String[] { null, "<", "ac" }, false),
        Arguments.of(new String[] { "ab", "<", "" }, false),

        // ----------------------------------
        // >= Operator
        // >= Integer
        Arguments.of(new String[] { "8", ">=", "12" }, false),
        Arguments.of(new String[] { "12", ">=", "8" }, true),
        Arguments.of(new String[] { "8", ">=", "8" }, true),
        Arguments.of(new String[] { null, ">=", "8" }, true),
        Arguments.of(new String[] { "8", ">=", "" }, true),
        Arguments.of(new String[] { "abc", ">=", "8" }, true),
        Arguments.of(new String[] { "8", ">=", "abc" }, false),
        Arguments.of(new String[] { "true", ">=", "8" }, true),
        Arguments.of(new String[] { "8", ">=", "true" }, false),

        // >= Float
        Arguments.of(new String[] { ".8", ">=", ".12" }, true),
        Arguments.of(new String[] { ".12", ">=", ".8" }, false),
        Arguments.of(new String[] { ".8", ">=", ".8" }, true),
        Arguments.of(new String[] { null, ">=", ".8" }, true),
        Arguments.of(new String[] { ".8", ">=", "" }, true),
        Arguments.of(new String[] { "abc", ">=", ".8" }, true),
        Arguments.of(new String[] { ".8", ">=", "abc" }, false),
        Arguments.of(new String[] { "true", ">=", ".8" }, true),
        Arguments.of(new String[] { ".8", ">=", "true" }, false),

        // >= Strings
        Arguments.of(new String[] { "ab", ">=", "ac" }, false),
        Arguments.of(new String[] { "ab", ">=", "ab" }, true),
        Arguments.of(new String[] { "ac", ">=", "ab" }, true),
        Arguments.of(new String[] { "ab", ">=", "abc" }, false),
        Arguments.of(new String[] { "abc", ">=", "ab" }, true),
        Arguments.of(new String[] { null, ">=", "ac" }, true),
        Arguments.of(new String[] { "ab", ">=", "" }, true),

        // ----------------------------------
        // > Operator
        // > Integer
        Arguments.of(new String[] { "8", ">", "12" }, false),
        Arguments.of(new String[] { "12", ">", "8" }, true),
        Arguments.of(new String[] { "8", ">", "8" }, false),
        Arguments.of(new String[] { null, ">", "8" }, true),
        Arguments.of(new String[] { "8", ">", "" }, true),
        Arguments.of(new String[] { "abc", ">", "8" }, true),
        Arguments.of(new String[] { "8", ">", "abc" }, false),
        Arguments.of(new String[] { "true", ">", "8" }, true),
        Arguments.of(new String[] { "8", ">", "true" }, false),

        // > Float
        Arguments.of(new String[] { ".8", ">", ".12" }, true),
        Arguments.of(new String[] { ".12", ">", ".8" }, false),
        Arguments.of(new String[] { ".8", ">", ".8" }, false),
        Arguments.of(new String[] { null, ">", ".8" }, true),
        Arguments.of(new String[] { ".8", ">", "" }, true),
        Arguments.of(new String[] { "abc", ">", ".8" }, true),
        Arguments.of(new String[] { ".8", ">", "abc" }, false),
        Arguments.of(new String[] { "true", ">", ".8" }, true),
        Arguments.of(new String[] { ".8", ">", "true" }, false),

        // > Strings
        Arguments.of(new String[] { "ab", ">", "ac" }, false),
        Arguments.of(new String[] { "ab", ">", "ab" }, false),
        Arguments.of(new String[] { "ac", ">", "ab" }, true),
        Arguments.of(new String[] { "ab", ">", "abc" }, false),
        Arguments.of(new String[] { "abc", ">", "ab" }, true),
        Arguments.of(new String[] { null, ">", "ac" }, true),
        Arguments.of(new String[] { "ab", ">", "" }, true),

        // ----------------------------------
        // =~ Operator
        Arguments.of(new String[] { "abc", "=~", "a|x|abc|d" }, true),
        Arguments.of(new String[] { "ac", "=~", "a|x|abc|d" }, false),

        // ----------------------------------
        // =! Operator
        Arguments.of(new String[] { "abc", "!~", "a|x|abc|d" }, false),
        Arguments.of(new String[] { "ac", "!~", "a|x|abc|d" }, true));
  }

}
