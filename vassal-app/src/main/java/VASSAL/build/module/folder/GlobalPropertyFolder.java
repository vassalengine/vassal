/*
 *
 * Copyright (c) 2021-2023 by vassalengine.org, Brian Reynolds
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

package VASSAL.build.module.folder;

import VASSAL.build.AbstractFolder;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.build.module.properties.ScenarioPropertiesOptionTab;

public class GlobalPropertyFolder extends AbstractFolder {
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    final Buildable parent = getAncestor();
    if (parent instanceof GlobalProperties) {
      if (((GlobalProperties) parent).getAncestor() instanceof GameModule) {
        // Only Module-level Global Property hierachy can contain Scenario Options
        return new Class<?>[] { this.getClass(), GlobalProperty.class, ScenarioPropertiesOptionTab.class };
      }
    }
    return new Class<?>[] { this.getClass(), GlobalProperty.class };
  }
}
