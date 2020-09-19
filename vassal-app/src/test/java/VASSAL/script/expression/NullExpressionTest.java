package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.emptyString;

import org.junit.Test;

public class NullExpressionTest {

  @Test
  public void constructor() {
    Expression e = new NullExpression();
    assertThat(e.getExpression(), is(emptyString()));
  }

  @Test
  public void evaluate() throws ExpressionException {
    Expression e = new NullExpression();
    String s = e.evaluate();
    assertThat(s, is(emptyString()));
  }

  @Test
  public void toBeanShellString() {
    Expression e = new NullExpression();
    String s = e.toBeanShellString();
    assertThat(s, is(emptyString()));
  }
}