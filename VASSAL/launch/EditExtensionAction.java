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
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;

/**
 * Loads an exiting module extension and opens it in an extension edit window
 * @author rodneykinney
 *
 */
public class EditExtensionAction extends LoadModuleAction {
  private static final long serialVersionUID = 1L;
  
  public EditExtensionAction(Component comp) {
    super(comp);
    putValue(NAME,Resources.getString(Resources.EDIT));
  }
  
  protected void loadModule(File f) throws IOException {
    ModuleExtension ext = new ModuleExtension(new ArchiveWriter(new ZipFile(f.getPath())));
    ext.build();
    new VASSAL.configure.ExtensionEditWindow(ext).setVisible(true);
  }

}
