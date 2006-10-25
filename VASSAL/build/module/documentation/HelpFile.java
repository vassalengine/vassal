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

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import javax.swing.JMenuItem;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.tools.DataArchive;

/**
 * Places an entry in the <code>Help</code> menu.  Selecting the entry
 * displays a window with stored text on it. */
public class HelpFile extends AbstractConfigurable {
  public static final String TITLE = "title";
  public static final String FILE = "fileName";
  public static final String TYPE = "fileType";
  private static final String IMAGE = "image";

  public static final String ARCHIVE_ENTRY = "archive";
  public static final String RESOURCE = "resource";
  public static final String LOCAL_FILE = "file";

  private HelpWindow frame;
  private URL contents;
  private String title;
  private String fileName;
  private JMenuItem launch;
  private String fileType = ARCHIVE_ENTRY;

  public static String getConfigureTypeName() {
    return "Help File";
  }

  public HelpFile() {
    launch = new JMenuItem();
    setConfigureName("help");
    setMenuItem();
    launch.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        showWindow();
      }
    });
  }


  public HelpFile(String title, File contents, String ref) throws MalformedURLException {
    this();
    this.title = title;
    this.contents = new URL(toURL(contents),ref);
    setConfigureName(title);
    setMenuItem();
  }
  public HelpFile(String title, File contents) throws MalformedURLException {
    this();
    this.title = title;
    this.contents = toURL(contents);
    setConfigureName(title);
    setMenuItem();
  }

  public HelpFile(String title, URL contents) {
    this();
    this.title = title;
    this.contents = contents;
    setConfigureName(title);
    setMenuItem();
  }

  /**
   * Create and display a new HelpWindow with the contents of this HelpFile
   */
  public void showWindow() {
    HelpWindow w = getHelpWindow();
    w.setVisible(true);
    w.toFront();
  }

  protected HelpWindow getHelpWindow() {
    if (frame == null) {
      frame = new HelpWindow(title, getContents());
    }
    return frame;
  }

  public URL getContents() {
    if (contents == null) {
      if (fileName != null) {
        if (ARCHIVE_ENTRY.equals(fileType)) {
          try {
            contents = GameModule.getGameModule().getDataArchive().getURL(fileName);
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
        else if (RESOURCE.equals(fileType)) {
          contents = getClass().getResource(fileName);
        }
        else if (LOCAL_FILE.equals(fileType)) {
          File f = new File(fileName);
          if (fileName.startsWith("docs/")) {
            f = new File(Documentation.getDocumentationBaseDir(),fileName.substring("docs/".length()));
          }
          try {
            contents = toURL(f);
          }
          catch (MalformedURLException e) {
            e.printStackTrace();
          }
        }
      }
    }
    return contents;
  }

  public static URL toURL(File f) throws MalformedURLException {
    String path = f.getAbsolutePath();
    if (File.separatorChar != '/') {
      path = path.replace(File.separatorChar, '/');
    }
    if (!path.startsWith("/")) {
      path = "/" + path;
    }
    if (!path.endsWith("/") && f.isDirectory()) {
      path = path + "/";
    }
    return new URL("file", "", path);
  }


  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "HelpMenu.htm"), "#HelpFile");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  private void setMenuItem() {
    launch.setText(getConfigureName());
  }

  /**
   * The attributes of a HelpFile are:
   *
   * <code>TITLE</code> the text of the menu entry in the Help menu
   * <code>FILE</code> the name of an text file in the {@link
   * DataArchive}.  The text is displayed in a window with the same title
   */
  public String[] getAttributeNames() {
    String s[] = {TITLE, FILE, IMAGE, TYPE};
    return s;
  }

  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return title;
    }
    else if (FILE.equals(key)) {
      return fileName;
    }
    else if (TYPE.equals(key)) {
      return fileType;
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (TITLE.equals(key)) {
      title = (String) val;
      setConfigureName(title);
      launch.setText(title);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
        fileType = ARCHIVE_ENTRY;
      }
      fileName = (String) val;
      if ("Intro.txt".equals(key)) {
        fileType = RESOURCE;
      }
    }
    else if (TYPE.equals(key)) {
      fileType = (String) val;
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Menu Entry", "Text/HTML File", "Add Image"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, File.class, Image.class};
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    ((Documentation) b).getHelpMenu().remove(launch);
    launch.setEnabled(false);
  }

  public void addTo(Buildable b) {
    ((Documentation) b).getHelpMenu().add(launch);
  }
  
  public static HelpFile getReferenceManualPage(String page) {
    return getReferenceManualPage(page,null);
  }
  
  public static HelpFile getReferenceManualPage(String page, String anchor) {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return anchor == null ? new HelpFile(null,new File(dir,page)) : new HelpFile(null, new File(dir, page), "#"+anchor);
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

}

