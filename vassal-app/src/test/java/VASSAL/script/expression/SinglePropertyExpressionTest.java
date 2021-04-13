package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;

import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.BasicPiece;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;


public class SinglePropertyExpressionTest {

  public static final String PROP1_KEY = "prop1";
  public static final String PROP1_VALUE = "prop1val";
  public static final String PROP1_LOCALISED_VALUE = "prop1lval";

  @Test
  public void constructor() {
    Expression e;

    e = new SinglePropertyExpression(PROP1_KEY);
    assertThat(e.getExpression(), is(equalTo(PROP1_KEY)));

    // $$ variable should converted to a straight property name
    e = new SinglePropertyExpression("$" + PROP1_KEY + "$");
    assertThat(e.getExpression(), is(equalTo(PROP1_KEY)));
  }

  @Test
  public void evaluate() throws ExpressionException {
    Expression e;
    e = new SinglePropertyExpression(PROP1_KEY);

    Source source = new Source();
    Map<String, String> props = new HashMap<>();
    props.put(PROP1_KEY, PROP1_VALUE);

    String s;
    // Property Source property value
    s = e.evaluate(source, null, false);
    assertThat(s, is(equalTo(PROP1_VALUE)));

    // Property Source localised property value
    s = e.evaluate(source, null, true);
    assertThat(s, is(equalTo(PROP1_LOCALISED_VALUE)));

    // Property Map property value
    s = e.evaluate(null, props, false);
    assertThat(s, is(equalTo(PROP1_VALUE)));

    // No value found
    s = e.evaluate(null, null, false);
    assertThat(s, is(equalTo("")));
  }

  static class Source implements PropertySource {

    @Override
    public Object getProperty(Object key) {
      if (PROP1_KEY.equals(key)) {
        return PROP1_VALUE;
      }
      return null;
    }

    @Override
    public Object getLocalizedProperty(Object key) {
      if (PROP1_KEY.equals(key)) {
        return PROP1_LOCALISED_VALUE;
      }
      return null;
    }
  }
}