/*
 * $Id$
 *
 * Copyright (c) 2008-2010 by Joel Uckelman
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

package VASSAL.tools.logging;

import java.awt.Font;
import java.io.File;
import java.io.IOException;

import javax.swing.JTextArea;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.io.Tailer;

public class LogPane extends JTextArea {
  private static final long serialVersionUID = 1L;

  protected final Tailer tailer;

  public LogPane(File file) {
    setEditable(false);
    setLineWrap(true);
    setWrapStyleWord(true);
    setTabSize(2);
    setFont(new Font("Monospaced", Font.PLAIN, getFont().getSize()));

    tailer = new Tailer(file);

    tailer.addEventListener(new EventListener<String>() {
      public void receive(Object src, String s) {
        // NB: JTextArea.append() is thread-safe; it can be called off-EDT.
        append(s);
      }
    });

    // tail the file only when the pane is visible
    addAncestorListener(new AncestorListener() {
      public void ancestorRemoved(AncestorEvent e) {
        tailer.stop();
      }

      public void ancestorAdded(AncestorEvent e) {
        try {
          tailer.start();
        }
        catch (IOException ex) {
          ReadErrorDialog.error(ex, tailer.getFile());
        }
      }

      public void ancestorMoved(AncestorEvent e) {}
    });
  }
}
