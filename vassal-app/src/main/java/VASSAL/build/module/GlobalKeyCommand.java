/*
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
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.CounterGlobalKeyCommand;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.GlobalCommandTarget;

/**
 * This version of {@link MassKeyCommand} is added directly to a {@link VASSAL.build.GameModule} and applies to all maps.
 *
 * The "Global Key Command" functionality, as the term is used in Vassal Modules, is spread out over several classes internally:
 * {@link GlobalCommand} - primary functionality for sending commands to multiple pieces based on matching parameters
 * {@link VASSAL.build.module.GlobalKeyCommand}         - Global Key Commands from a Module window
 * {@link VASSAL.build.module.StartupGlobalKeyCommand}  - Global Key Commands from a Module "At Startup"
 * {@link VASSAL.build.module.map.MassKeyCommand}       - Global Key Commands from a specific Map window
 * {@link VASSAL.build.module.map.DeckGlobalKeyCommand} - Global Key Commands from a Deck
 * {@link CounterGlobalKeyCommand}                      - Global Key Commands from a Game Piece
 *
 * Other important classes:
 * {@link GlobalCommandTarget}           - "Fast Match" parameters
 * {@link GlobalCommandTargetConfigurer} - configurer for "Fast Match" parameters
 */
public class GlobalKeyCommand extends MassKeyCommand {

  /**
   * @return Our type of Global Key Command (overrides the one from Mass Key Command). Affects what configurer options are shown.
   */
  @Override
  public GlobalCommandTarget.GKCtype getGKCtype() {
    return GlobalCommandTarget.GKCtype.MODULE;
  }

  /**
   * This version of Global Key Commands searches every map in the module
   */
  @Override
  public void apply() {
    final List<Map> l = Map.getMapList();
    GameModule.getGameModule().sendAndLog(
      globalCommand.apply(l.toArray(new Map[0]), getFilter(), target));
  }

  // Hide 'This Map only' option
  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (SINGLE_MAP.equals(name)) {
      return () -> false;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }
}
