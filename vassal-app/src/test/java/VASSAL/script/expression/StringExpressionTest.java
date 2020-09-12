package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import org.junit.Test;

public class StringExpressionTest {

  private static final String TEST = "xyzzy";

  @Test
  public void constructor() {
    Expression e = new StringExpression(TEST);
    assertThat(e.getExpression(), is(equalTo(TEST)));
  }

  @Test
  public void evaluate() throws ExpressionException {
    Expression e = new StringExpression(TEST);
    String s = e.evaluate();
    assertThat(s, equalTo(TEST));
  }

  @Test
  public void toBeanShellString() {
    Expression e = new StringExpression(TEST);
    String s = e.toBeanShellString();
    assertThat(s, equalTo("\"" + TEST + "\""));
  }
}