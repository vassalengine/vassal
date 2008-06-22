package VASSAL.configure;

import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PropertiesPieceFilter;
import VASSAL.tools.FormattedString;

/*
 * Class encapsulating a Property Match Expression
 * A PropertyExpression is it's own PieceFilter.
 */
public class PropertyExpression implements PieceFilter {
  
  protected String expression = "";
  protected PieceFilter filter = null;
  
  public PropertyExpression() {

  }
  
  public PropertyExpression(String s) {
    setExpression(s);
  }

  public void setExpression(String expression) {
    this.expression = expression;
    filter = null;
  }

  public String getExpression() {
    return expression;
  }

  public boolean isNull() {
    return expression == null || expression.length() == 0;
  }
  
  public boolean isDynamic() {
    return expression != null && expression.indexOf('$') >= 0;
  }
  
  public PieceFilter getFilter(PropertySource source) {
    if (filter == null || isDynamic()) {
      filter = PropertiesPieceFilter.parse(new FormattedString(getExpression()).getText(source)); 
    }
    return filter;
  }

  public boolean accept(GamePiece piece) {
    return getFilter(piece).accept(piece);
  }
  
}

