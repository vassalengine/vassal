/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JWindow;
import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;

/**
 * Places an entry in the <code>Help</code> menu.  Selecting the entry
 * displays a window with a stored image on it.  Good for a splash
 * screen or an "about" screen.
 */
public class AboutScreen extends AbstractConfigurable {
  protected ImageOp op;
  protected Image image;
  protected String title;
  protected String fileName;
  protected JMenuItem launch;
  protected Window window;

  public AboutScreen() {
    launch = new JMenuItem();
    launch.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        launch();
      }
    });
  }

  public AboutScreen(ImageOp op) {
    this();
    if (op == null) throw new IllegalArgumentException();
    this.op = op;
  }

  @Deprecated
  public AboutScreen(Image i) {
    this();
    this.op = Op.load(i);
  }

  public void launch() {
    if (window == null) {
      initComponents();
    }

    window.setVisible(true);
    window.toFront();
  }

  protected void initComponents() {
    if (op == null) return;

    final JWindow w = new JWindow(GameModule.getGameModule() != null ?
      GameModule.getGameModule().getFrame() : null);
    w.getContentPane().setBackground(Color.black);
    w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.Y_AXIS));

    final JLabel l = new JLabel(new OpIcon(op));
    l.setAlignmentX(0.5F);
    w.add(l);

    w.add(createLabel(
        Resources.getString("AboutScreen.module_version",  //$NON-NLS-1$
        GameModule.getGameModule().getGameName(),
        GameModule.getGameModule().getGameVersion())));

    for (ModuleExtension ext :
         GameModule.getGameModule().getComponentsOf(ModuleExtension.class)) {
      w.add(createLabel(
          Resources.getString("AboutScreen.extension_version", ext.getName(), ext.getVersion()))); //$NON-NLS-1$
    }

    w.add(createLabel(
        Resources.getString("AboutScreen.vassal_version", Info.getVersion()))); //$NON-NLS-1$

    w.pack();

    final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    w.setLocation(d.width / 2 - w.getSize().width / 2,
                  d.height / 2 - w.getSize().height / 2);
    window = w;
    window.addMouseListener(new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        window.setVisible(false);
      }
    });
  }

  private JLabel createLabel(String text) {
    final JLabel l2 = new JLabel(text);
    l2.setBackground(Color.blue);
    l2.setForeground(Color.white);
    l2.setHorizontalAlignment(JLabel.CENTER);
    l2.setAlignmentX(0.5F);
    return l2;
  }

  public static String getConfigureTypeName() {
    return "About Screen";
  }

  /**
   * The attributes of an AboutScreen are:
   *
   * <code>TITLE</code> the text of the menu entry in the Help menu
   * <code>FILE</code> the name of an image file in the {@link
   * DataArchive}.  The image is displayed when the menu item is
   * selected
   */
  public String[] getAttributeNames() {
    return new String[]{
      TITLE,
      FILE
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Menu Entry:  ", //$NON-NLS-1$
      "Image:  " //$NON-NLS-1$
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Image.class
    };
  }

  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return title;
    }
    else if (FILE.equals(key)) {
      return fileName;
    }
    return null;
  }

  public static final String TITLE = "title"; //$NON-NLS-1$
  public static final String FILE = "fileName"; //$NON-NLS-1$

  public void setAttribute(String key, Object val) {
    if (TITLE.equals(key)) {
      title = (String) val;

      // don't permit "About VASSAL"
      if (title.equals(Resources.getString("AboutScreen.about_vassal"))) {
        title = Resources.getString("Documentation.about_module");
      } 

      setConfigureName(title);
      launch.setText(title);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      fileName = (String) val;

      op = null;
      if (fileName != null) {
        fileName = fileName.trim();
        if (fileName.length() > 0) {
          op = Op.load(fileName);
          
          try {
            // FIXME: get the wizard to cache
            GameModule.getGameModule()
                      .getWizardSupport()
                      .setBackgroundImage(op.getImage(null));
          }
          catch (CancellationException e) {
            op = null;
            ErrorLog.warn(e);
          }
          catch (InterruptedException e) {
            op = null;
            ErrorLog.warn(e);
          }
          catch (ExecutionException e) {
            op = null;
            ErrorLog.warn(e);
          }
        }
      }

      window = null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    ((Documentation) b).getHelpMenu().remove(launch);
  }

  /**
   * Expects to be added to a {@link Documentation}.  Adds an entry
   * to the <code>Help</code> menu */
  public void addTo(Buildable b) {
    final Documentation d = (Documentation) b;
    d.getHelpMenu().add(launch);

    if (op == null) {
      // use the VASSAL about image in case there is no image given
      op = Op.load("/images/Splash.png");
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.htm", "AboutScreen"); //$NON-NLS-1$ //$NON-NLS-2$
  }
}
