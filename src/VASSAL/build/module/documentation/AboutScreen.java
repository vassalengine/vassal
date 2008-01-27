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
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.ImageSourceOp;
import VASSAL.tools.imageop.OpIcon;
import VASSAL.tools.imageop.SourceOp;

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
    this.op = new ImageSourceOp(i);
  }

  public void launch() {
//    if (image == null) return;

    if (window == null) {
      initComponents();
    }

    window.setVisible(true);
    window.toFront();
  }

  protected void initComponents() {
    if (op == null) {
      return;
    }

    final Icon icon = new OpIcon(op);
    final JWindow w = new JWindow(GameModule.getGameModule() != null ?
      GameModule.getGameModule().getFrame() : null);
    w.getContentPane().setBackground(Color.black);
    w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.Y_AXIS));
    final JLabel l = new JLabel(icon);
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

  public Class[] getAttributeTypes() {
    return new Class[]{
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
        if (!fileName.isEmpty()) {
          op = new SourceOp(fileName);
          
          try {
            // FIXME: get the wizard to cache
            GameModule.getGameModule()
                      .getWizardSupport()
                      .setBackgroundImage(op.getImage(null));
          }
          catch (CancellationException e) {
// FIXME: is this possible?
            op = null;
            e.printStackTrace();
          }
          catch (InterruptedException e) {
// FIXME: is this possible?
            op = null;
            e.printStackTrace();
          }
          catch (ExecutionException e) {
            op = null;
            e.printStackTrace();
          }
        }
      }

      window = null;
/*
//      image = getImage();
//      GameModule.getGameModule().getWizardSupport().setBackgroundImage(image);
      op = fileName == null || fileName.trim().isEmpty()
         ? null : new SourceOp(fileName); 
// FIXME: setting the background in the wizard blows caching
      if (op != null) {
        GameModule.getGameModule()
                  .getWizardSupport()
                  .setBackgroundImage(op.getImage(null));
      }
      window = null;
*/
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    ((Documentation) b).getHelpMenu().remove(launch);
    //	launch.setEnabled(false);
  }

  /**
   * Expects to be added to a {@link Documentation}.  Adds an entry
   * to the <code>Help</code> menu */
  public void addTo(Buildable b) {
    final Documentation d = (Documentation) b;
    d.getHelpMenu().add(launch);
// FIXME: need to do something to check whether op will produce non-null
// output. Mabye in this case just skip this check, and complain later?
// Or just load the image now?
    if (op == null) {
      throw new IllegalBuildException(
        Resources.getString("AboutScreen.file_not_found", //$NON-NLS-1$
        fileName ,
        GameModule.getGameModule().getDataArchive().getName()));
    }
  }

/*
  private Image getImage() throws IllegalBuildException {
    Image im = null;
    if (fileName != null && fileName.length() > 0) {
      try {
        if (fileName.charAt(0) == '/') {
          im = DataArchive.getImage(getClass().getResourceAsStream(fileName));
        }
        else {
          return GameModule.getGameModule().getDataArchive().getCachedImage(fileName);
        }
      }
      catch (Exception ex) {
        ex.printStackTrace();
      }
    }
    return im;
  }
*/

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.htm", "AboutScreen"); //$NON-NLS-1$ //$NON-NLS-2$
  }
}
