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
package VASSAL.launch.install;

import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class FailureScreen extends SuccessScreen {
  public FailureScreen(String msg) {
    super(msg);
  }

  public FailureScreen(Throwable t) {
    super(null);
    String msg = t.getMessage();
    if (msg == null) {
      msg = t.getClass().getName();
      msg = msg.substring(msg.lastIndexOf('.'));
    }
    setMessage(Resources.getString("Install.installation_failed", msg)); //$NON-NLS-1$
  }

  public void next() {
    System.exit(1);
  }
}