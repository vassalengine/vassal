package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;

import VASSAL.build.module.properties.PropertySource;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

public class FormattedStringExpressionTest {

  public static final String PROP1_KEY = "prop1";
  public static final String PROP1_VALUE = "prop1val";
  public static final String PROP1_LOCALISED_VALUE = "prop1lval";

  public static final String PROP2_KEY = "prop2";
  public static final String PROP2_VALUE = "prop2val";

  public static final String TEST1_EXPR = "abc $test$ def";
  public static final String TEST1_RESULT = "abc test def";

  public static final String TEST2_EXPR = "abc $" + PROP1_KEY + "$ def";
  public static final String TEST2_RESULT = "abc " + PROP1_VALUE + " def";

  public static final String TEST3_EXPR = "abc $" + PROP1_KEY + "$ def";
  public static final String TEST3_RESULT = "abc " + PROP1_LOCALISED_VALUE + " def";

  public static final String TEST4_EXPR = "abc $" + PROP1_KEY + "$ def $" + PROP2_KEY + "$ ghi";
  public static final String TEST4_RESULT = "abc " + PROP1_VALUE + " def " + PROP2_VALUE + " ghi";

  public static final String TEST5_EXPR = "abc $" + PROP1_KEY + "$ def $" + PROP2_KEY + "$ ghi";
  public static final String TEST5_RESULT = "abc " + PROP1_LOCALISED_VALUE + " def " + PROP2_VALUE + " ghi";

  @Test
  public void constructor() {
    Expression e = new FormattedStringExpression(TEST1_EXPR);
    assertThat(TEST1_EXPR, is(equalTo(e.getExpression())));
  }

  @Test
  public void evaluate() throws ExpressionException {

    Source source = new Source();
    Map<String, String> props = new HashMap<>();
    props.put(PROP2_KEY, PROP2_VALUE);


    // Test 1 - Fallback behaviour Evaluation with no Property source should just strip the $'s.
    Expression e = new FormattedStringExpression(TEST1_EXPR);
    String s = e.evaluate();
    assertThat ("Fallback behaviour failing", TEST1_RESULT, is(equalTo(s)));

    // Test 1B - Property source supplied, Property not found
    e = new FormattedStringExpression(TEST1_EXPR);
    s = e.evaluate(source);
    assertThat ("Fallback behaviour failing with Property Source", TEST1_RESULT, is(equalTo(s)));

    // Test 2 - Property Source, not localised.
    e = new FormattedStringExpression(TEST2_EXPR);
    s = e.evaluate(source);
    assertThat("Property Source lookup failed", TEST2_RESULT, is(equalTo(s)));

    // Test 3 - Property Source, localised.
    e = new FormattedStringExpression(TEST3_EXPR);
    s = e.evaluate(source, true);
    assertThat("Localised Property Source lookup failed", TEST3_RESULT, is(equalTo(s)));

    // Test 4 - Property Source and Property Map, not localised
    e = new FormattedStringExpression(TEST4_EXPR);
    s = e.evaluate(source, props, false);
    assertThat("Property Source plus Map lookup failed", TEST4_RESULT, is(equalTo(s)));

    // Test 5 - Property Source and Property Map, localised
    e = new FormattedStringExpression(TEST5_EXPR);
    s = e.evaluate(source, props, true);
    assertThat("Localised Property Source plus Map lookup failed", TEST5_RESULT, is(equalTo(s)));

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