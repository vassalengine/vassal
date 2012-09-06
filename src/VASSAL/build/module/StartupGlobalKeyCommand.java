/*
 * $Id$
 *
 * Copyright (c) 2010 by Pieter Geerkens
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

import javax.swing.SwingUtilities;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.VisibilityCondition;

/**
 * A Global Key Command that is automatically invoked on game start-up,
 * once the various Key Listeners have been started.
 * <p>
 * If multiple start-up commands need to be run, they should be combined
 * in a MultiAction Button and then launched from a single instance of
 * StartupGlobalKeyCommand, as the sequence in which multiple instances of
 * StartupGlobalKeyCommand are fired is undetermined.
 *
 * @author Pieter Geerkens
 *
 */
public class StartupGlobalKeyCommand extends GlobalKeyCommand implements GameComponent {
  public StartupGlobalKeyCommand() {
    super();
    /* These four fields pertaining to the physical representation of the
     * GKC on the toolbar are not applicable in this implementation.
     */
    launch.setAttribute(BUTTON_TEXT, "");
    launch.setAttribute(TOOLTIP, "");
    launch.setAttribute(ICON, "");
    launch.setAttribute(HOTKEY, "");
  }

  //---------------------- GlobalKeyCommand extension ---------------------
  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public static String getConfigureTypeName() {
    return "Startup Global Key Command";
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "StartupGlobalKeyCommand");
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (BUTTON_TEXT.equals(key) || TOOLTIP.equals(key) ||
        ICON.equals(key)        || HOTKEY.equals(key)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {  return false; }
      };
    }
    else {
      return super.getAttributeVisibility(key);
    }
  }

  //---------------------- GameComponent implementation ---------------------
  private boolean hasStarted = false;

  public void setup(boolean gameStarting) {
    if (gameStarting && !hasStarted){
      SwingUtilities.invokeLater(new Runnable() {
        public void run() { apply(); }
      });
    }
    hasStarted = gameStarting;
  }

  public Command getRestoreCommand() {
    return null; // No persistent state
  }
}
