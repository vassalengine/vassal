package VASSAL.tools.version;

import static VASSAL.tools.version.VersionUtils.compareVersions;
import static java.lang.Integer.signum;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class VersionUtilsCompareVersionsTest {


  private final String[] inputVersions;
  private final int expectedResult;

  public VersionUtilsCompareVersionsTest(String[] inputVersions, int expectedResult) {
    this.inputVersions = inputVersions;
    this.expectedResult = expectedResult;
  }

  @Parameterized.Parameters
  public static Collection<Object[]> testData() {
    return Arrays.asList(new Object[][]{
      { new String[] { "1.2.3", "1.2.4" }, -1 },
      { new String[] { "1.2.4", "1.2.3" }, 1 },
      { new String[] { "1.2.3", "1.2.3" }, 0 },
      { new String[] { "1.2.3-beta1", "1.2.2" }, 1 },
      { new String[] { "1.2.3-beta1", "1.2.3" }, -1 },
      { new String[] { "1.2.3-beta1", "1.2.4" }, -1 },
      { new String[] { "1.2.3-beta1", "1.2.4" }, -1 },
      { new String[] { "1.2.3-snapshot", "1.2.3" }, -1 },
      { new String[] { "1.2.3-SNAPSHOT", "1.2.3" }, -1 },
    });
  }

  @Test
  public void testCompareVersions() {
    assertThat(
      signum(compareVersions(inputVersions[0], inputVersions[1])),
      is(expectedResult));
  }

}
