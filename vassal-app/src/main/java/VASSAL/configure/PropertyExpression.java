package VASSAL.configure;

import VASSAL.build.BadDataReport;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.script.expression.NullExpression;
import VASSAL.script.expression.PropertyMatchExpression;
import VASSAL.tools.ErrorDialog;

/*
 * Class encapsulating a Property Match Expression
 * A PropertyExpression is it's own PieceFilter.
 */
public class PropertyExpression implements PieceFilter {

  protected Expression expression = NullExpression.instance();

  public PropertyExpression() {

  }

  public PropertyExpression(String s) {
    setExpression(s);
  }

  public void setExpression(String s) {
    expression = Expression.createPropertyExpression(s);
  }

  public String getExpression() {
    return expression.getExpression();
  }

  public boolean isNull() {
    return expression == null || expression instanceof NullExpression;
  }

  public PieceFilter getFilter(PropertySource source) {
    return expression.getFilter(source);
  }

  public PieceFilter getFilter() {
    return expression.getFilter();
  }

  @Override
  public boolean accept(GamePiece piece) {
    // Classic Property Match Expressions need to use the old-style call sequence.
    if (expression instanceof PropertyMatchExpression) {
      return expression.getFilter(piece).accept(piece);
    }
    return isTrue(piece);
  }

  public boolean accept(GamePiece source, GamePiece piece) {
    return getFilter(source).accept(piece);
  }

  @Override
  public int hashCode() {
    return getExpression().hashCode();
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof PropertyExpression &&
           getExpression().equals(((PropertyExpression) o).getExpression());
  }

  /**
   * Evaluate the Property Expression as true/false using
   * a supplied property source
   *
   * @param ps Property Source   *
   * @return boolean result
   */
  public boolean isTrue(PropertySource ps) {
    String result = null;
    try {
      result = expression.evaluate(ps);
    }
    catch (ExpressionException e) {
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.expression_error"),
        "Expression=" + getExpression() + ", Error=" + e.getError(), e)); //NON-NLS
    }
    return "true".equals(result); //NON-NLS
  }

}

