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
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.AbstractAction;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.swing.AboutWindow;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class AboutVASSALAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  protected final Window window;

  public AboutVASSALAction(Window w) {
    super(Resources.getString("AboutScreen.about_vassal"));
    window = w;
  }

  public void actionPerformed(ActionEvent evt) {
    final String name = "/images/Splash.png";

    BufferedImage img = null;
    try {
      img = ImageUtils.getImage(name, getClass().getResourceAsStream(name));
    }
    catch (IOException e) {
      ErrorDialog.bug(e);
      return;
    }

    final AboutWindow about = new AboutWindow(
      window,
      img,
      Resources.getString("AboutScreen.vassal_version", Info.getVersion())
    );

    about.setVisible(true);
    about.toFront();
  }
}
