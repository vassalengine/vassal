/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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

import java.awt.Desktop;
import java.io.File;

public class ModuleManagerMacOSXStartUp extends MacOSXStartUp {
  @Override
  public void initSystemProperties() {
    super.initSystemProperties();
    setupApplicationListeners();
  }

  protected void setupApplicationListeners() {
    final Desktop app = Desktop.getDesktop();
    app.setOpenFileHandler(e -> {
      final String filename = e.getFiles().get(0).toString();
      if (filename.endsWith(".vmod")) { //NON-NLS
        final LaunchRequest lr = new LaunchRequest();
        lr.mode = LaunchRequest.Mode.LOAD;
        lr.module = new File(filename);
        ModuleManager.getInstance().execute(lr);
      }
    });
  }
}
