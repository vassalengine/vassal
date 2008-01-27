package VASSAL.build.module.properties;

import java.util.Collection;
import java.util.Iterator;
import VASSAL.counters.GamePiece;

/**
 * For property names of the form sum(name), returns the value of
 * the named property summed over a list of pieces
 * @author rkinney
 *
 */
public class SumProperties implements PropertySource {
  protected Collection pieces;


  public SumProperties(Collection pieces) {
    this.pieces = pieces;
  }

  public Object getProperty(Object key) {
    Object value = null;
    String keyString = key.toString();
    if (keyString.startsWith("sum(") && keyString.endsWith(")")) {
      String propertyName = keyString.substring(4,keyString.length()-1);
      int sum = 0;
      for (Iterator iter = pieces.iterator(); iter.hasNext();) {
        GamePiece p = (GamePiece) iter.next();
        Object val = p.getProperty(propertyName);
        if (val != null) {
          try {
            sum += Integer.parseInt(val.toString());
          }
          catch (NumberFormatException e) {
          }
        }
      }
      value = String.valueOf(sum);
    }
    else if (pieces.size() > 0) {
      value = ((GamePiece)pieces.iterator().next()).getProperty(key);
    }
    return value;
  }
  
  public Object getLocalizedProperty(Object key) {
    return getProperty(key);
  }

}
