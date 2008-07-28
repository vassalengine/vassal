package VASSAL.build.module.properties;

import VASSAL.tools.FormattedString;

/**
 * Increments a property by a given value.  
 * The value can be specified as a FormattedString property and evaluated at runtime
 * 
 * @author rkinney
 * 
 */
public class IncrementProperty implements PropertyChanger {
  protected Constraints constraints;
  protected FormattedString format = new FormattedString();

  public IncrementProperty(String incr, Constraints constraints) {
    super();
    this.constraints = constraints;
    format.setFormat(incr);
  }

  public String getNewValue(String oldValue) {
    try {
      int value = Integer.parseInt(oldValue);
      int incr = Integer.parseInt(format.getText(constraints)); 
      if (constraints.isWrap()) {
        if (value + incr > constraints.getMaximumValue()) {
          value = constraints.getMinimumValue() + (value + incr - constraints.getMaximumValue() - 1);
        }
        else if (value + incr < constraints.getMinimumValue()) {
          value = constraints.getMaximumValue() + (value + incr - constraints.getMinimumValue() + 1);
        }
        else {
          value += incr;
        }
      }
      else {
        value += incr;
        value = Math.min(constraints.getMaximumValue(), value);
        value = Math.max(constraints.getMinimumValue(), value);
      }
      return String.valueOf(value);
    }
    // FIXME: review error message
    catch (NumberFormatException e) {
      return oldValue;
    }
  }

  public String getIncrement() {
    return format.getFormat();
  }
  
  public static interface Constraints extends PropertySource {
    int getMinimumValue();
    int getMaximumValue();
    boolean isWrap();
  }
}
