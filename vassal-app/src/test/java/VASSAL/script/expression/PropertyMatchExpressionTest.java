package VASSAL.script.expression;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import VASSAL.counters.BasicPiece;
import VASSAL.counters.PieceFilter;
import org.junit.Test;

public class PropertyMatchExpressionTest {

  private static final String PROP1 = "Army";
  private static final String VALUE1 = "German";
  private static final String PROP2 = "Strength";
  private static final String PROP3 = "Hits";
  private static final String TEST1_EXPR = PROP1 + "=" + VALUE1;
  private static final String TEST2_EXPR = PROP2 + "=$" + PROP3 + "$";

  @Test
  public void constructor() {
    Expression e = new PropertyMatchExpression(TEST1_EXPR);
    assertThat(e.getExpression(), is(equalTo(TEST1_EXPR)));
  }

  @Test
  public void getFilter() {

    Expression e = new PropertyMatchExpression(TEST1_EXPR);
    PieceFilter filter = e.getFilter();

    // Test 1 - Basic expression, no $$ variables
    BasicPiece bp = new BasicPiece();
    bp.setProperty(PROP1, VALUE1);
    assertThat(filter.accept(bp), is(true));
    bp.setProperty(PROP1, VALUE1 + "xxx");
    assertThat(filter.accept(bp), is(false));


    // Test 2 - $$ variables
    e = new PropertyMatchExpression(TEST2_EXPR);
    bp = new BasicPiece();
    bp.setProperty(PROP2, "2");
    bp.setProperty(PROP3, "2");
    filter = e.getFilter(bp);
    assertThat(filter.accept(bp), is(true));

    bp.setProperty(PROP2, "3");
    assertThat(filter.accept(bp), is(false));

    bp.setProperty(PROP2, "2");
    bp.setProperty(PROP3, "3");
    filter = e.getFilter(bp);
    assertThat(filter.accept(bp), is(false));
  }

  @Test
  public void isDynamic() {
    PropertyMatchExpression e = new PropertyMatchExpression(TEST1_EXPR);
    assertThat(e.isDynamic(), is(false));

    e = new PropertyMatchExpression(TEST2_EXPR);
    assertThat(e.isDynamic(), is(true));
  }
}