/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASSAL.build.module;

import java.util.List;

import VASSAL.build.GameModule;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.configure.VisibilityCondition;

/**
 * This version of {@link MassKeyCommand} is added directly to a
 * {@link VASSAL.build.GameModule} and applies to all maps
 */
public class GlobalKeyCommand extends MassKeyCommand {

  public void apply() {
    final List<Map> l = Map.getMapList();
    GameModule.getGameModule().sendAndLog(
      globalCommand.apply(l.toArray(new Map[l.size()]), getFilter()));
  }

  // Hide 'This Map only' option
  public VisibilityCondition getAttributeVisibility(String name) {
    if (SINGLE_MAP.equals(name)) {
      return  new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }
}
