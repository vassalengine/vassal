package bsh;

import java.util.List;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.is;

public class BeanShellExpressionValidatorTest {

  @Test
  public void isValid() {
    BeanShellExpressionValidator v = new BeanShellExpressionValidator("{6 * 7}");
    assertThat(v.isValid(), is(true));
    assertThat(v.getError(), is(emptyString()));

    v = new BeanShellExpressionValidator("{6 * }");
    assertThat(v.isValid(), is(false));
    assertThat(v.getError(), is(not(emptyString())));
  }

  @Test
  public void get() {
    BeanShellExpressionValidator v = new BeanShellExpressionValidator("{x + y + z.contains('p') + Random(6)}");
    assertThat(v.isValid(), is(true));
    assertThat("Find non-string variables in expression", v.getVariables(), is(equalTo(List.of("x", "y"))));
    assertThat("Find string variables in expression", v.getStringVariables(), is(equalTo(List.of("z"))));
    assertThat("Find method calls in expression", v.getMethods(), is(equalTo(List.of("Random"))));
  }
}