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

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.AbstractAction;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.AboutScreen;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.swing.AboutWindow;

/**
 * The single About entry of a VASSAL process. On macOS it is the About
 * entry of the application menu; on Windows and Linux it is the last entry
 * of the Help menu.
 *
 * <p>With no module loaded (the Module Manager) it shows the VASSAL splash
 * and version as "About VASSAL". With a module loaded (Player or Editor)
 * it shows the module's About screen, see
 * {@link AboutScreen#showModuleAbout}, and takes its menu text from
 * {@link AboutScreen#getModuleAboutMenuText}.</p>
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class AboutVASSALAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  protected final Window window;

  public AboutVASSALAction(Window w) {
    super(getMenuText());
    window = w;
  }

  private static String getMenuText() {
    final GameModule g = GameModule.getGameModule();
    return g != null
      ? AboutScreen.getModuleAboutMenuText(g)
      : Resources.getString("AboutScreen.about_vassal");
  }

  /**
   * @return the VASSAL splash image, or null if it could not be read
   */
  public static BufferedImage getSplashImage() {
    final String name = "/images/Splash.png"; //NON-NLS
    try {
      return ImageUtils.getImage(name, AboutVASSALAction.class.getResourceAsStream(name));
    }
    catch (IOException e) {
      ErrorDialog.bug(e);
      return null;
    }
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    final GameModule g = GameModule.getGameModule();
    if (g != null) {
      AboutScreen.showModuleAbout(window, g);
      return;
    }

    final BufferedImage img = getSplashImage();
    if (img == null) return;

    final AboutWindow about = new AboutWindow(
      window,
      img,
      Resources.getString("AboutScreen.vassal_version", Info.getVersion())
    );

    about.setVisible(true);
    about.toFront();
  }
}
