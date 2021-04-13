package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

public class ExpressionTest {

  /**
   * See also Tests for {@link BeanShellExpression#createExpression(String)}
   * See individual classes for more detailed functional tests
   */
  @Test
  public void createExpression() {
    // Check createExpression is creating the correct types of expressions
    Expression e;

    // Null Expression
    e = Expression.createExpression("");
    assertThat(e, is(instanceOf(NullExpression.class)));

    // Int Expression
    e = Expression.createExpression("42");
    assertThat(e, is(instanceOf(IntExpression.class)));

    // Numeric Literals should create StringExpressions, not be converted to IntExpressions
    e = Expression.createExpression("1L");
    assertThat(e, is(instanceOf(StringExpression.class)));

    // Simple String Expression
    e = Expression.createExpression("abc");
    assertThat(e, is(instanceOf(StringExpression.class)));

    // Beanshell Expression
    e = Expression.createExpression("{abc}");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));

    // Formatted String Expression
    e = Expression.createExpression("Test: $abc$");
    assertThat(e, is(instanceOf(FormattedStringExpression.class)));

    // BeanShell Expression with embedded $$ varaible
    e = Expression.createExpression("{$abc$==def}");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));
  }

  @Test
  public void createPropertyExpression() {
    // Check createPropertyExpression is creating the correct types of expressions
    Expression e;

    // Null Expression
    e = Expression.createPropertyExpression("");
    assertThat(e, is(instanceOf(NullExpression.class)));

    // BeanShell Expression
    e = Expression.createPropertyExpression("{$abc$==def}");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));

    // Anything else should generate a PropertyMatchExpression
    e = Expression.createPropertyExpression("abc==def");
    assertThat(e, is(instanceOf(PropertyMatchExpression.class)));
  }

  @Test
  public void createSimplePropertyExpression() {
    // Check createSimplePropertyExpression is creating the correct types of expressions
    Expression e;

    // Null Expression
    e = Expression.createSimplePropertyExpression("");
    assertThat(e, is(instanceOf(NullExpression.class)));

    // BeanShell Expression
    e = Expression.createSimplePropertyExpression("{abc}");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));

    // Anything else should generate a PropertyMatchExpression
    e = Expression.createSimplePropertyExpression("abc");
    assertThat(e, is(instanceOf(SinglePropertyExpression.class)));
  }
}