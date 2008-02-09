/*
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
import java.util.zip.ZipFile;
import VASSAL.build.GameModule;
//import VASSAL.configure.ModuleEditWindow;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;

public class EditModuleAction extends LoadModuleAction {
  private static final long serialVersionUID = 1L;
  
  public EditModuleAction(Component comp) {
    super(comp);
    putValue(NAME, Resources.getString("Main.edit_module"));
  }

  protected void loadModule(File f) throws IOException {
    ArchiveWriter archive = new ArchiveWriter(new ZipFile(f.getPath()));
    GameModule.init(new BasicModule(archive));
    GameModule.getGameModule().getFrame().setVisible(true);
//    new ModuleEditWindow().setVisible(true);
    EditorWindow.getInstance().moduleLoading(GameModule.getGameModule());
  }
}
