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

import java.awt.Window;
import java.io.File;

import VASSAL.build.module.Documentation;
import VASSAL.i18n.Resources;

public class LaunchTourAction extends AbstractLaunchAction {
  private static final long serialVersionUID = 1L;

  public LaunchTourAction(Window window) {
    super(
      Resources.getString("Main.tour"),
      window,
      Player.class.getName(),
      new LaunchRequest(LaunchRequest.Mode.LOAD,
        new File(Documentation.getDocumentationBaseDir(), "tour.mod"),
        new File(Documentation.getDocumentationBaseDir(), "tour.log")
      )
    );
  }

  @Override
  protected LaunchTask getLaunchTask() {
    return new LaunchTask();
  }
}
