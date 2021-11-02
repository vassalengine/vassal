package VASSAL.configure;

import VASSAL.build.BadDataReport;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
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

  public PieceFilter getFilter(PropertySource source, Auditable owner, String fieldKey) {
    return getFilter(source, owner, AuditTrail.create(owner, getExpression(), Resources.getString(fieldKey)));
  }

  public PieceFilter getFilter(PropertySource source, Auditable owner, AuditTrail audit) {
    return expression.getFilter(source, owner, audit);
  }

  public PieceFilter getFilter(PropertySource source) {
    return getFilter(source, null, (AuditTrail) null);
  }

  public PieceFilter getFilter() {
    return expression.getFilter(null, null);
  }

  @Override
  public boolean accept(GamePiece piece) {
    return accept(piece, null, (AuditTrail) null);
  }

  @Override
  public boolean accept(GamePiece piece, Auditable owner, String fieldKey) {
    return accept(piece, owner, AuditTrail.create(owner, getExpression(), Resources.getString(fieldKey)));
  }

  @Override
  public boolean accept(GamePiece piece, Auditable owner, AuditTrail audit) {
    // Classic Property Match Expressions need to use the old-style call sequence.
    if (expression instanceof PropertyMatchExpression) {
      return expression.getFilter(piece, owner, audit).accept(piece, owner, audit);
    }
    return isTrue(piece, owner, audit);
  }

  public boolean accept(GamePiece source, GamePiece piece) {
    return accept(source, piece, null, null);
  }

  public boolean accept(PropertySource source, GamePiece piece) {
    return accept(source, piece, null, null);
  }

  public boolean accept(GamePiece source, GamePiece piece, Auditable owner, AuditTrail trail) {
    return getFilter(source).accept(piece, owner, trail);
  }

  public boolean accept(PropertySource source, GamePiece piece, Auditable owner, AuditTrail trail) {
    return getFilter(source).accept(piece, owner, trail);
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
    return isTrue(ps, null, null);
  }

  public boolean isTrue(PropertySource ps, Auditable owner, AuditTrail audit) {
    String result = null;
    try {
      result = expression.evaluate(ps, owner, audit);
    }
    catch (ExpressionException e) {
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.expression_error"),
        "Expression=" + getExpression() + ", Error=" + e.getError(), e)); //NON-NLS
    }
    return "true".equals(result); //NON-NLS
  }

}

