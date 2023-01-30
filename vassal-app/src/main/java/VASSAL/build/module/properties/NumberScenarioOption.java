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

import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.i18n.Resources;

public class NumberScenarioOption extends AbstractScenarioOption {

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.NumberScenarioOption.component_type");
  }

  @Override
  public Configurer getOptionConfigurer() {
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
