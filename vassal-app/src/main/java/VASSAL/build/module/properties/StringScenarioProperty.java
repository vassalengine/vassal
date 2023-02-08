package VASSAL.build.module.properties;

import VASSAL.configure.Configurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;

public class StringScenarioProperty extends AbstractScenarioProperty {

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.StringScenarioProperty.component_type"); //$NON-NLS-1$
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (INITIAL_VALUE.equals(key)) {
      if (value instanceof String) {
        initialValue = (String) value;
        property.setPropertyValue(initialValue);
      }
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public Class getInitialValueClass() {
    return String.class;
  }

  @Override
  public Configurer getOptionConfigurer() {
    return new StringConfigurer("", "", getPropertyValue());
  }
}
