/*
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
import VASSAL.i18n.Resources;

import java.util.List;

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
  @SuppressWarnings("removal")
  public StartupGlobalKeyCommand() {
    super();
    /* These four fields pertaining to the physical representation of the
     * GKC on the toolbar are not applicable in this implementation.
     */
    launch.setAttribute(BUTTON_TEXT, "");  //NON-NLS
    launch.setAttribute(TOOLTIP, "");  //NON-NLS
    launch.setAttribute(ICON, "");  //NON-NLS
    launch.setAttribute(HOTKEY, "");  //NON-NLS
  }

  //---------------------- GlobalKeyCommand extension ---------------------
  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.StartupGlobalKeyCommand.component_type");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "StartupGlobalKeyCommand"); //NON-NLS
  }

  @SuppressWarnings("removal")
  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (List.of(BUTTON_TEXT, TOOLTIP, ICON, HOTKEY).contains(key)) {
      return () -> false;
    }
    else {
      return super.getAttributeVisibility(key);
    }
  }

  //---------------------- GameComponent implementation ---------------------
  private boolean hasStarted = false;

  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting && !hasStarted) {
      SwingUtilities.invokeLater(this::apply);
    }
    hasStarted = gameStarting;
  }

  @Override
  public Command getRestoreCommand() {
    return null; // No persistent state
  }
}
