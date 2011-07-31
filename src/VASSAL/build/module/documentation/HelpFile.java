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
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.URLUtils;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;

/**
 * Places an entry in the <code>Help</code> menu.  Selecting the entry
 * displays a window with stored text on it.
 */
public class HelpFile extends AbstractConfigurable {
  public static final String TITLE = "title"; //$NON-NLS-1$
  public static final String FILE = "fileName"; //$NON-NLS-1$
  public static final String TYPE = "fileType"; //$NON-NLS-1$
  private static final String IMAGE = "image"; //$NON-NLS-1$

  public static final String ARCHIVE_ENTRY = "archive"; //$NON-NLS-1$
  public static final String RESOURCE = "resource"; //$NON-NLS-1$
  public static final String LOCAL_FILE = "file"; //$NON-NLS-1$

  protected HelpWindow frame;
  protected DialogHelpWindow dialog;
  protected URL contents;
  protected String title;
  protected String fileName;
  protected Action launch;
  protected String fileType = ARCHIVE_ENTRY;

  public static String getConfigureTypeName() {
    return "Plain Text Help File";
  }

  public HelpFile() {
    this("help", (URL) null);
  }

  public HelpFile(String title, File contents, String ref)
                                throws MalformedURLException {
    this(title, new URL(URLUtils.toURL(contents), ref));
  }

  public HelpFile(String title, File contents) throws MalformedURLException {
    this(title, URLUtils.toURL(contents));
  }

  public HelpFile(String title, URL contents) {
    this.title = title;
    this.contents = contents;
    setConfigureName(title);

    launch = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        showWindow();
      }
    };

    launch.putValue(Action.NAME, getConfigureName());
  }

  /**
   * Create and display a new HelpWindow with the contents of this HelpFile
   */
  public void showWindow() {
    final HelpWindow w = getHelpWindow();
    w.setVisible(true);
    w.toFront();
  }

  protected HelpWindow getHelpWindow() {
    if (frame == null) {
      frame = new HelpWindow(title, getContents());
    }
    return frame;
  }

  /**
   * Create and display a new HelpWindow as a Dialog
   * with the contents of this HelpFile
   */
  public void showWindow(Dialog owner) {
    final DialogHelpWindow w = getDialogHelpWindow(owner);
    w.setVisible(true);
    w.toFront();
  }

  protected DialogHelpWindow getDialogHelpWindow(Dialog d) {
    if (dialog == null) {
      dialog = new DialogHelpWindow(title, getContents(), d);
    }
    return dialog;
  }

  public URL getContents() {
    if (contents != null || fileName == null) return contents;

    if (ARCHIVE_ENTRY.equals(fileType)) {
      try {
        contents = GameModule.getGameModule().getDataArchive().getURL(fileName);
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, fileName);
      }
    }
    else if (RESOURCE.equals(fileType)) {
      contents = getClass().getResource(fileName);
    }
    else if (LOCAL_FILE.equals(fileType)) {
      File f = new File(fileName);
      if (fileName.startsWith("docs/")) { //$NON-NLS-1$
        f = new File(Documentation.getDocumentationBaseDir(),
                     fileName.substring("docs/".length())); //$NON-NLS-1$
      }

      try {
        contents = URLUtils.toURL(f);
      }
      catch (MalformedURLException e) {
        ErrorDialog.bug(e);
      }
    }

    return contents;
  }

  /** @deprecated Use {@link URLUtils.toURL(File)} instead. */
  @Deprecated
  public static URL toURL(File f) throws MalformedURLException {
    return URLUtils.toURL(f);
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
    try {
      return new HelpFile(null, new File(dir, "HelpMenu.htm"), "#HelpFile"); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
      return null;
    }
  }

  /**
   * The attributes of a HelpFile are:
   *
   * <code>TITLE</code> the text of the menu entry in the Help menu
   * <code>FILE</code> the name of an text file in the {@link
   * DataArchive}.  The text is displayed in a window with the same title
   */
  public String[] getAttributeNames() {
    return new String[] {
      TITLE,
      FILE,
      IMAGE,
      TYPE
    };
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
      launch.putValue(Action.NAME, title);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
        fileType = ARCHIVE_ENTRY;
      }
      fileName = (String) val;
      if ("Intro.txt".equals(key)) { //$NON-NLS-1$
        fileType = RESOURCE;
      }
    }
    else if (TYPE.equals(key)) {
      fileType = (String) val;
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Menu Entry:  ",
      "Text File:  "
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      File.class
    };
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  protected MenuItemProxy launchItem;

  public void addTo(Buildable b) {
    launchItem = new MenuItemProxy(launch);
    MenuManager.getInstance().addToSection("Documentation.Module", launchItem);
    launch.setEnabled(true);
  }

  public void removeFrom(Buildable b) {
    MenuManager.getInstance()
               .removeFromSection("Documentation.Module", launchItem);
    launch.setEnabled(false);
  }

  public static HelpFile getReferenceManualPage(String page) {
    return getReferenceManualPage(page,null);
  }

  public static HelpFile getReferenceManualPage(String page, String anchor) {
    if (anchor != null && !anchor.startsWith("#")) { //$NON-NLS-1$
      anchor = "#"+anchor; //$NON-NLS-1$
    }
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
    try {
      return anchor == null ? new HelpFile(null,new File(dir, page)) :
                              new HelpFile(null, new File(dir, page), anchor);
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
      return null;
    }
  }
}

