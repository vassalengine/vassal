package VASSAL.build.module.properties;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.Resources;
import org.apache.commons.lang3.ArrayUtils;

import java.util.Arrays;

public class ListScenarioProperty extends AbstractScenarioProperty {

  public static final String OPTIONS = "options";

  public String[] options = new String[0];

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ListScenarioProperty.component_type"); //$NON-NLS-1$
  }

  @Override
  public Class getInitialValueClass() {
    return validOptions.class;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.ListScenarioProperty.valid_options")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      OptionsPrompt.class
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      OPTIONS
    );
  }

  @Override
  public String getAttributeValueString(String key) {
    if (OPTIONS.equals(key)) {
      return StringArrayConfigurer.arrayToString(options);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (INITIAL_VALUE.equals(key)) {
      if (value instanceof String) {
        initialValue = (String) value;
        property.setPropertyValue(initialValue);
      }
    }
    else if (OPTIONS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      final String[] newOptions = ((String[]) value);

      if (! Arrays.equals(options, newOptions)) {
        // Clear our configurer to force the initial values list to repopulate next time it is opened
        config = null;
      }
      options = newOptions;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public Configurer getOptionConfigurer() {
    final StringEnumConfigurer c = new StringEnumConfigurer("", "", options);
    c.setValue(getPropertyValue());
    return c;
  }

  public static class OptionsPrompt implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new StringArrayConfigurer(key, name);
    }

  }

  public static class validOptions extends StringEnum {

    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return ((ListScenarioProperty) target).options;
    }
  }
}
