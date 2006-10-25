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
import java.util.Enumeration;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
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
import VASSAL.tools.DataArchive;

/**
 * Places an entry in the <code>Help</code> menu.  Selecting the entry
 * displays a window with a stored image on it.  Good for a splash
 * screen or an "about" screen.  */
public class AboutScreen extends AbstractConfigurable {
  private Image image;
  private String title;
  private String fileName;
  private JMenuItem launch;
  private Window window;

  public AboutScreen() {
    launch = new JMenuItem();
    launch.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        launch();
      }
    });
  }

  public AboutScreen(Image i) {
    this();
    image = i;
  }

  public void launch() {
    if (window == null) {
      initComponents();
    }
    window.setVisible(true);
    window.toFront();
  }

  protected void initComponents() {
    if (image == null) {
      return;
    }
    ImageIcon icon = new ImageIcon(image);
    JWindow w = new JWindow(GameModule.getGameModule() != null ? GameModule.getGameModule().getFrame() : null);
    w.getContentPane().setBackground(Color.black);
    w.getContentPane().setLayout(new BoxLayout(w.getContentPane(), BoxLayout.Y_AXIS));
    JLabel l = new JLabel(icon);
    l.setAlignmentX(0.5F);
    w.getContentPane().add(l);
    w.getContentPane().add(createLabel(GameModule.getGameModule().getGameName()
                           + " module version "
                           + GameModule.getGameModule().getGameVersion()));
    Enumeration e = GameModule.getGameModule().getComponents(ModuleExtension.class);
    while (e.hasMoreElements()){
      ModuleExtension ext = (ModuleExtension) e.nextElement();
      w.getContentPane().add(createLabel("Extension "+ext.getName()+" version "+ext.getVersion()));
    }
    w.getContentPane().add(createLabel("VASSAL engine version "
                           + Info.getVersion()));
    w.pack();
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
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
    JLabel l2 = new JLabel(text);
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
   * selected */
  public String[] getAttributeNames() {
    return new String[]{TITLE, FILE};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Menu Entry",
                        "GIF Image"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class,
                       Image.class};
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

  public static final String TITLE = "title";
  public static final String FILE = "fileName";

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
      image = getImage();
      window = null;
    }
  }

  /*
  public Configurer[] getAttributeConfigurers() {
      Configurer config[] = new Configurer[2];
      config[0] = new StringConfigurer(TITLE,"Menu Entry");
      config[0].setValue(getConfigureName());
      listenTo(config[0]);

      config[1] = new ImageConfigurer
      (FILE,"GIF Image",
      GameModule.getGameModule().getArchiveWriter());
      config[1].setValue(fileName);
      listenTo(config[1]);

      return config;
  }
  */
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
    Documentation d = (Documentation) b;
    d.getHelpMenu().add(launch);
    if (image == null && fileName != null) {
      throw new IllegalBuildException("File " + fileName + " not found in " + GameModule.getGameModule().getDataArchive().getName());
    }
  }

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

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.htm", "AboutScreen");
  }

  public static void main(String args[]) {
    Image i = Toolkit.getDefaultToolkit().getImage("/Command Bunker/VASSAL!/latest source/images/Splash.gif");
    System.err.println("" + i);
    AboutScreen as = new AboutScreen(i);
    as.launch();
  }
}

