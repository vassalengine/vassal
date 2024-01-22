/*
 *
 * Copyright (c) 2008-2023 by The VASSAL Development Team
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
package VASSAL.tools.swing;

import javax.swing.JDialog;

/**
 * Close a Dialog Box after a specified delay interval
 */
public class DialogCloser implements Runnable {

  final JDialog dialog;
  final int ms;

  public DialogCloser(JDialog dialog, int ms) {
    this.dialog = dialog;
    this.ms = ms < 0 ? 500 : ms;
  }

  @Override
  public void run() {
    try {
      Thread.sleep(ms);
    }
    catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    dialog.setVisible(false);
    dialog.dispose();
  }
}
