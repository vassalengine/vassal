/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

package VASSAL.launch;

import java.awt.Component;
import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;

import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.io.ZipArchive;
import VASSAL.tools.swing.SwingUtils;

public class EditModuleAction extends LoadModuleAction {
  private static final long serialVersionUID = 1L;

  public EditModuleAction(Component comp) {
    super(comp);
    putValue(NAME, Resources.getString("Main.edit_module"));
  }

  public EditModuleAction(File moduleFile) {
    super(moduleFile);
    putValue(NAME, Resources.getString("Main.edit_module"));
  }

  @Override
  protected void loadModule(File f) throws IOException {
    GameModule.init(new GameModule(new ArchiveWriter(new ZipArchive(f), ".vmod"))); //NON-NLS

    // Load the module's extensions, exactly as the Player does. Without this the
    // module in the editor is only half of itself: its player window has none of
    // the maps, counters or images the extensions supply, so loading a saved game
    // reports "Image not found" and comes up with the boards missing and only the
    // module's own counters present.
    //
    // This is safe to do here because a component an extension grafts in is
    // recorded by ExtensionElement.addTo and skipped by the module's
    // getBuildElement, so saving the module writes only what the module owns --
    // see GameModule.addExtensionComponent.
    new ExtensionsLoader().addTo(GameModule.getGameModule());
    Localization.getInstance().translate();

// FIXME: really hide the MM?
//    ModuleManagerWindow.getInstance().setVisible(false);
    final JFrame frame = GameModule.getGameModule().getPlayerWindow();
    frame.setVisible(true);

    // GameModule only produces a final buildString() once its frame displays.
    // Taken after the extensions are loaded, so the baseline matches what a save
    // would write and merely opening a module does not look like a change.
    GameModule.getGameModule().updateLastSave();

    final ModuleEditorWindow w =
      new ModuleEditorWindow(GameModule.getGameModule());
    w.setLocation(0, frame.getY() + frame.getHeight());
    w.setSize(SwingUtils.getScreenBounds(frame).width / 2, w.getHeight());
    w.setVisible(true);
  }
}
