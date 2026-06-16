/*
 *
 * Copyright (c) 2000-2020 by Rodney Kinney, Joel Uckelman
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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Window;
import java.io.IOException;
import java.net.URL;

import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import VASSAL.build.GameModule;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ScrollPane;

public class HTMLWindowHelper implements HyperlinkListener {
  private final JEditorPane pane = new JEditorPane();

  public HTMLWindowHelper() {
    pane.setEditable(false);
    pane.setContentType("text/html");

    /*
     * Allow <src> tag to display images from the module DataArchive
     * where no pathname included in the image name.
     */
    pane.setEditorKit(new DataArchiveHTMLEditorKit(GameModule.getGameModule().getDataArchive()));
    pane.addHyperlinkListener(this);
  }

  @Override
  public void hyperlinkUpdate(HyperlinkEvent e) {
    if (HyperlinkEvent.EventType.ACTIVATED.equals(e.getEventType())) {
      if (e.getURL() != null) {
        update(e.getURL());
      }
    }
  }

  public void update(URL contents) {
    if (contents != null) {
      try {
        pane.setPage(contents);
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, contents.toString());
      }
    }
    else {
      pane.setText("");
    }
  }

  public void setup(Window w, URL contents) {
    final ScrollPane s = new ScrollPane(pane);
    final Font f = new JLabel().getFont();
    final FontMetrics fm = w.getFontMetrics(f);
    s.getVerticalScrollBar().setUnitIncrement(fm.getHeight() * 3); //BR// Mousewheel scrolls 3 lines of default JLabel font height
    w.add(s);
    update(contents);
    w.pack();

    // Suggest a reasonable initial size based on current screen bounds without forcing location.
    // Let the common window layout engine (PositionOption) perform clamping and final placement.
    final java.awt.Rectangle screen = SwingUtils.getScreenBounds(w);
    int width = Math.max(screen.width / 2, w.getSize().width);
    int height = Math.max(screen.height / 2, w.getSize().height);
    width = Math.min(width, (screen.width * 2) / 3);
    height = Math.min(height, (screen.height * 2) / 3);
    w.setSize(new Dimension(width, height));
    // Do not set explicit location here to avoid fighting with PositionOption
  }
}
