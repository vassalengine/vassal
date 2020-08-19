package VASSAL.configure;

import VASSAL.build.BadDataReport;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.script.BeanShell;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.script.expression.NullExpression;
import VASSAL.tools.ErrorDialog;

/**
 * Class encapsulating a Property Match Expression
 * A PropertyExpression is it's own PieceFilter.
 * 
 * To use the PropertyExpression in a GKC type situation against a target GamePiece, use {@link #accept(GamePiece, GamePiece)}
 * To check the true/false value of the expression within a single GamePiece, use {@link #isTrue(PropertySource)}
 * 
 */
public class PropertyExpression implements PieceFilter {

  protected Expression expression = new NullExpression();

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

  /**
   * Check if a piece matches this expression. $$ variables
   * are converted to propertynames.
   *
   * This call is used to implement PropertyMatch checking on
   * a single piece.
   *
   * @param piece Target piece to check
   * @return True if expression is true for target piece.
   */
  @Override
  public boolean accept(GamePiece piece) {
    return isTrue(piece);
  }

  /**
   * Check if a piece matches this expression. Pre-evaluate
   * any $$ variables using the source piece.
   *
   * This call is used to implement PropertyMatch checking
   * for Global Key Commands and similar where a $$ variables
   * are satisfied by the source piece, but property names
   * are satisfied by the target piece.
   *
   * @param source Source Piece for evaluating $$ variables
   * @param piece Target Piece to evaluate expression on
   * @return True if target piece matches expression
   */
  public boolean accept(GamePiece source, GamePiece piece) {
    return getFilter(source).accept(piece);
  }

  public boolean equals(Object o) {
    if (o instanceof PropertyExpression) {
      return getExpression().equals(((PropertyExpression) o).getExpression());
    }
    return false;
  }

  /**
   * Evaluate the Property Expression as true/false using
   * a supplied property source.
   *
   * @param ps Property Source
   * @return boolean result
   */
  public boolean isTrue(PropertySource ps) {
    String result = null;
    try {
      result = expression.evaluate(ps);
    }
    catch (ExpressionException e) {
      ErrorDialog.dataError(new BadDataReport(Resources.getString("Error.expression_error"),
        "Expression=" + getExpression() + ", Error=" + e.getError(), e)); //$NON-NLS-1$//
    }
    return BeanShell.TRUE.equals(result); //$NON-NLS-1$//
  }

}

