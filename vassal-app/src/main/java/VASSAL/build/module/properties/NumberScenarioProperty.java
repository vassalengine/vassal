package VASSAL.build.module.properties;

import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.i18n.Resources;

public class NumberScenarioProperty extends AbstractScenarioProperty {

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.NumberScenarioProperty.component_type"); //$NON-NLS-1$
  }

  @Override
  public Class getInitialValueClass() {
    return Integer.class;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (INITIAL_VALUE.equals(key)) {
      if (value instanceof String) {
        try {
          value = Integer.parseInt(getPropertyValue());
        }
        catch (Exception ignored) {
          value = Integer.valueOf(0);
        }
      }
      initialValue = ((Integer) value).toString();
      property.setPropertyValue(initialValue);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public Configurer getOptionConfigurer() {
    // Current value of the property may not be an in integer
    int i;
    try {
      i = Integer.parseInt(getPropertyValue());
    }
    catch (Exception ignored) {
      i = 0;
    }
    return new IntConfigurer("", "", i);
  }
}
