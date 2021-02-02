/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.io.ZipArchive;
import VASSAL.tools.swing.SwingUtils;

/**
 * Loads an exiting module extension and opens it in an extension edit window
 *
 * @author rodneykinney
 */
public class EditExtensionAction extends LoadModuleAction {
  private static final long serialVersionUID = 1L;

  public EditExtensionAction(Component comp) {
    super(comp);
    putValue(NAME, Resources.getString("Editor.edit_extension"));
  }

  public EditExtensionAction(File extFile) {
    super(extFile);
    putValue(NAME, Resources.getString("Editor.edit_extension"));
  }

  @Override
  protected void loadModule(File f) throws IOException {
    final ModuleExtension ext =
      new ModuleExtension(new ArchiveWriter(new ZipArchive(f), ".vmdx")); //NON-NLS
    ext.build();
    final JFrame frame = GameModule.getGameModule().getPlayerWindow();
    final ExtensionEditorWindow w =
      new ExtensionEditorWindow(GameModule.getGameModule(), ext);
    w.setLocation(0, frame.getY() + frame.getHeight());
    w.setSize(SwingUtils.getScreenBounds(frame).width / 2, w.getHeight());
    w.setVisible(true);
  }
}
