/*
 * $Id$
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

import java.io.File;

import com.apple.eawt.Application;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;

public class ModuleManagerMacOSXStartUp extends MacOSXStartUp {
  @Override
  public void initSystemProperties() {
    super.initSystemProperties();
    setupApplicationListeners();
  }

  protected void setupApplicationListeners() {
    final Application app = Application.getApplication();
    app.addApplicationListener(new ApplicationAdapter() {
      @Override
      public void handleOpenFile(ApplicationEvent e) {
        final String filename = e.getFilename();
        if (filename.endsWith(".vmod")) {
          final LaunchRequest lr = new LaunchRequest();
          lr.mode = LaunchRequest.Mode.LOAD;
          lr.module = new File(filename);
          ModuleManager.getInstance().execute(lr);
          e.setHandled(true);
        }
        else {
          e.setHandled(false);
        }
      }

      @Override
      public void handleReOpenApplication(ApplicationEvent e) {
        final LaunchRequest lr = new LaunchRequest();
        lr.mode = LaunchRequest.Mode.MANAGE;
        ModuleManager.getInstance().execute(lr);
        e.setHandled(true);
      }
    });
  }
}
