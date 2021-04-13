package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

import VASSAL.counters.BasicPiece;
import VASSAL.counters.PieceFilter;
import org.junit.jupiter.api.Test;

public class BeanShellExpressionTest {

  private static final String EXPR1 = "abc";
  private static final String EXPR2 = "$abc$";

  @Test
  public void constructor() {
    BeanShellExpression e = new BeanShellExpression(EXPR1);
    assertThat(e.getExpression(), is(equalTo("{" + EXPR1 + "}")));
  }

  @Test
  public void strip() {
    assertThat(BeanShellExpression.strip("abc"), is(equalTo("abc")));
    assertThat(BeanShellExpression.strip("{abc"), is(equalTo("{abc")));
    assertThat(BeanShellExpression.strip("{abc}"), is(equalTo("abc")));
    assertThat(BeanShellExpression.strip("{abc}d"), is(equalTo("{abc}d")));
  }

  @Test
  public void isDynamic() {
    BeanShellExpression e;

    e = new BeanShellExpression(EXPR1);
    assertThat(e.isDynamic(), is(false));

    e = new BeanShellExpression(EXPR2);
    assertThat(e.isDynamic(), is(true));
  }

  @Test
  public void getFilter() {
    BasicPiece bp1 = new BasicPiece();
    Expression e = BeanShellExpression.createExpression("{match==\"pickMe\"}");
    PieceFilter f = e.getFilter();
    bp1.setProperty("match", "pickMe");
    assertThat(f.accept(bp1), is(true));
    bp1.setProperty("match", "dontPickMe");
    assertThat(f.accept(bp1), is(false));
  }

  @Test
  public void convertProperty() {
    assertThat(BeanShellExpression.convertProperty(""), is(emptyString()));
    assertThat(BeanShellExpression.convertProperty("{abc}"), is(equalTo("abc")));
    assertThat(BeanShellExpression.convertProperty("abc"), is(equalTo("abc")));
    assertThat(BeanShellExpression.convertProperty("abc def"), is(equalTo("GetProperty(\"abc def\")")));
  }

  @Test
  public void isBeanShellExpression() {
    assertThat(BeanShellExpression.isBeanShellExpression("abc"), is(false));
    assertThat(BeanShellExpression.isBeanShellExpression("{abc}d"), is(false));
    assertThat(BeanShellExpression.isBeanShellExpression("{abc"), is(false));
    assertThat(BeanShellExpression.isBeanShellExpression("{abc}"), is(true));
  }

  @Test
  public void createExpression() {
    Expression e;
    // Null String should generate a NullExpression
    e = BeanShellExpression.createExpression("");
    assertThat(e, is(instanceOf(NullExpression.class)));
    assertThat(e.getExpression(), is(emptyString()));

    // An Integer should generate an IntExpression
    e = BeanShellExpression.createExpression("42");
    assertThat(e, is(instanceOf(IntExpression.class)));
    assertThat(e.getExpression(), is(equalTo("42")));

    // Other types of numbers should convert ot a generic BeanShell Expression
    e = BeanShellExpression.createExpression("4.2");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));
    assertThat(e.getExpression(), is(equalTo("{4.2}")));

    e = BeanShellExpression.createExpression("abc");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));
    assertThat(e.getExpression(), is(equalTo("{abc}")));

    // Check that 1D does not get converted to 1.0
    e = BeanShellExpression.createExpression("1D");
    assertThat(e, is(instanceOf(BeanShellExpression.class)));
    assertThat(e.getExpression(), is(equalTo("{1D}")));

    // Check Strings get converted to StringExpressions
    e = BeanShellExpression.createExpression("\"abc\"");
    assertThat(e, is(instanceOf(StringExpression.class)));
    assertThat(e.getExpression(), is(equalTo("abc")));

    // Unless the alternate sig is called
    e = BeanShellExpression.createExpression("\"abc\"", true);
    assertThat(e, is(instanceOf(BeanShellExpression.class)));
    assertThat(e.getExpression(), is(equalTo("{\"abc\"}")));

  }
}