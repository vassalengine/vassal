package VASSAL.tools.version;

import static java.lang.Integer.signum;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class VersionUtilsCompareVersionsTest {

  private static Stream<Arguments> addFixture() {
    return Stream.of(
      Arguments.of(new String[] { "1.2.3", "1.2.4" }, -1),
      Arguments.of(new String[] { "1.2.4", "1.2.3" }, 1),
      Arguments.of(new String[] { "1.2.3", "1.2.3" }, 0),
      Arguments.of(new String[] { "1.2.3-beta1", "1.2.2" }, 1),
      Arguments.of(new String[] { "1.2.3-beta1", "1.2.3" }, -1),
      Arguments.of(new String[] { "1.2.3-beta1", "1.2.4" }, -1),
      Arguments.of(new String[] { "1.2.3-beta1", "1.2.4" }, -1),
      Arguments.of(new String[] { "1.2.3-snapshot", "1.2.3" }, -1),
      Arguments.of(new String[] { "1.2.3-SNAPSHOT", "1.2.3" }, -1));
  }

  @ParameterizedTest
  @MethodSource("addFixture")
  public void testCompareVersions(String[] inputVersions, int expectedResult) {
    assertThat(
      signum(VersionUtils.compareVersions(inputVersions[0], inputVersions[1])),
      is(expectedResult));
  }

}
