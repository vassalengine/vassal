/*
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
import java.net.URL;

import javax.swing.JDialog;
import javax.swing.WindowConstants;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import VASSAL.tools.swing.HTMLWindowHelper;

/**
 * A Dialog that displays HTML content, with navigation
 */
public class DialogHelpWindow extends JDialog implements HyperlinkListener {
  private static final long serialVersionUID = 1L;

  private final HTMLWindowHelper helper = new HTMLWindowHelper();

  public DialogHelpWindow(String title, URL contents, Dialog parent) {
    super(parent);
    setTitle(title);
    setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);

    helper.setup(this, contents);
  }

  @Deprecated(since = "2021-12-01", forRemoval = true)
  @Override
  public void hyperlinkUpdate(HyperlinkEvent e) {
    helper.hyperlinkUpdate(e);
  }

  public void update(URL contents) {
    helper.update(contents);
  }
}
