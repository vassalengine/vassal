package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

public class IntExpressionTest {

  private static final int TEST_INT = 42;

  @Test
  public void constructor() {
    Expression e = IntExpression.instance(TEST_INT);
    assertThat(e.getExpression(), is(equalTo(String.valueOf(TEST_INT))));
  }

  @Test
  public void evaluate() throws ExpressionException {
    Expression e = IntExpression.instance(TEST_INT);
    String s = e.evaluate();
    assertThat(s, is(equalTo(String.valueOf(TEST_INT))));

    e = IntExpression.instance(-TEST_INT);
    s = e.evaluate();
    assertThat(s, is(equalTo(String.valueOf(-TEST_INT))));
  }

  @Test
  public void toBeanShellString() {
    Expression e = IntExpression.instance(TEST_INT);
    String s = e.toBeanShellString();
    assertThat(s, is(equalTo(String.valueOf(TEST_INT))));

    e = IntExpression.instance(-TEST_INT);
    s = e.toBeanShellString();
    assertThat(s, is(equalTo(String.valueOf(-TEST_INT))));
  }
}
