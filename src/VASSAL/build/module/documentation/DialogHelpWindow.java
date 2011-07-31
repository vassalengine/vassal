/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.documentation;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.IOException;
import java.net.URL;

import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import VASSAL.build.widget.HtmlChart;
import VASSAL.build.widget.HtmlChart.XTMLEditorKit;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ScrollPane;

/**
 * A Dialog that displays HTML content, with navigation
 */
public class DialogHelpWindow extends JDialog implements HyperlinkListener {
  private static final long serialVersionUID = 1L;

  private JEditorPane pane;

  public DialogHelpWindow(String title, URL contents, Dialog parent) {
    super(parent);
    setTitle(title);
    setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
    //setJMenuBar(MenuManager.getInstance().getMenuBarFor(this));

    pane = new JEditorPane();
    pane.setEditable(false);
    pane.addHyperlinkListener(this);

    /*
     * Allow <src> tag to display images from the module DataArchive
     * where no pathname included in the image name.
     */
    pane.setContentType("text/html"); //$NON-NLS-1$
    XTMLEditorKit myHTMLEditorKit = new HtmlChart.XTMLEditorKit();
    pane.setEditorKit(myHTMLEditorKit);

    JScrollPane scroll = new ScrollPane(pane);
    add(scroll);
    update(contents);
    pack();
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    int width = Math.max(d.width / 2, getSize().width);
    int height = Math.max(d.height / 2, getSize().height);
    width = Math.min(width, d.width * 2 / 3);
    height = Math.min(height, d.height * 2 / 3);
    setSize(width, height);
    setLocation(d.width / 2 - width / 2, 0);
  }

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
      pane.setText(""); //$NON-NLS-1$
    }
  }

}
