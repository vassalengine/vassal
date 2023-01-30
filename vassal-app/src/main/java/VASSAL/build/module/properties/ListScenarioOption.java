/*
 * Copyright (c) 2023 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.properties;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.Resources;
import org.apache.commons.lang3.ArrayUtils;

public class ListScenarioOption extends AbstractScenarioOption {

  public static final String OPTIONS = "options";

  public String[] options;

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ListScenarioOption.component_type");
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      OPTIONS
    );
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (OPTIONS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      options = ((String[]) value);
    }
    else {
      super.setAttribute(key, value);
    }
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
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.ListScenarioOption.options")
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
}
