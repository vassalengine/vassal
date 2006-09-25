package VASSAL.build.module.properties;

import VASSAL.tools.FormattedString;

/**
 * Provides a fixed value
 * The value can be specified as a FormattedString property and evaluated at runtime
 * 
 * @author rkinney
 *
 */
public class PropertySetter implements PropertyChanger {
  private String newValue;
  private PropertySource propSource;
  private FormattedString format;

  public PropertySetter(String newValue, PropertySource propSource) {
    this.newValue = newValue;
    this.propSource = propSource;
    if (propSource != null) {
      format = new FormattedString();
    }
  }

  public String getRawValue() {
    return newValue; 
  }
  
  public String getNewValue(String oldValue) {
    String s = newValue;
    if (format != null) {
      format.setFormat(s);
      s = format.getText(propSource);
    }
    return s;
  }

  public void setNewValue(String newValue) {
    this.newValue = newValue;
  }


}
