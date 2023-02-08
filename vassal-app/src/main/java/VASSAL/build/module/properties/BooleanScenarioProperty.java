package VASSAL.build.module.properties;

import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;

public class BooleanScenarioProperty extends AbstractScenarioProperty {

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.BooleanScenarioProperty.component_type"); //$NON-NLS-1$
  }

  @Override
  public Class getInitialValueClass() {
    return Boolean.class;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (INITIAL_VALUE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      initialValue = value == null ? "false" : ((Boolean) value).toString();
      property.setPropertyValue(initialValue);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public Configurer getOptionConfigurer() {
    return new BooleanConfigurer("", "", "true".equals(getPropertyValue()));
  }

}
