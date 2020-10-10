/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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

import VASSAL.Info;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;
import org.apache.commons.io.file.PathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.AbstractAction;
import javax.swing.Action;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

/**
 * Unpacks a zipped directory stored in the module and displays it in an
 * external browser window.
 *
 * @author rkinney
 */
public class BrowserPDFFile extends AbstractConfigurable {
  private static final Logger logger =
    LoggerFactory.getLogger(BrowserPDFFile.class);

  public static final String TITLE = "title"; //$NON-NLS-1$
  //public static final String CONTENTS = "contents"; //$NON-NLS-1$
  public static final String PDF_FILE = "pdfFile"; //$NON-NLS-1$
  protected String name;
  protected String pdf = "";
  protected Action launch;
  protected URL url;
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected ComponentI18nData myI18nData;

  public BrowserPDFFile() {
    super();

    launch = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    };
  }

  public void launch() {
    if (url == null) {
      extractPDF();
    }
    if (url != null) {
      BrowserSupport.openURL(url.toString());
    }
  }

  /**
   * @return The entry in the module Zip file containing the HTML directory
   */
  protected String getContentsResource() {
    return name == null ? null : name.replace(' ', '_');
  }

  protected void extractPDF() {

    try (ZipInputStream in =
           new ZipInputStream(new BufferedInputStream(
             GameModule.getGameModule().getDataArchive().getInputStream(pdf)))) { //$NON-NLS-1$
      internalExtractContents(in);
    }
    catch (FileNotFoundException e) {
      logger.error("File not found in data archive: {}", pdf, e); //NON-NLS
      setFallbackUrl();
    }
    catch (IOException e) {
      logger.error("Error while reading file {} from data archive", pdf, e); //NON-NLS
      setFallbackUrl();
    }
  }

  private void setFallbackUrl() {
    try {
      url = new URL(pdf);
    }
    catch (MalformedURLException e) {
      logger.error("Malformed URL: {}", pdf, e); //NON-NLS
    }
  }

  private void internalExtractContents(ZipInputStream in) throws IOException {
    final Path p = Path.of(Info.getTempDir().getAbsolutePath(), "VASSAL", "pdf");

    if (Files.exists(p)) {
      PathUtils.deleteDirectory(p);
    }

    Files.createDirectories(p);

    final File output = p.toFile();
    ZipEntry entry;
    while ((entry = in.getNextEntry()) != null) {
      if (!pdf.equals(entry.getName())) continue;
      try (FileOutputStream fos = new FileOutputStream(new File(output, pdf))) {
        IOUtils.copy(in, fos);
        break;
      }
    }

    url = new File(output, pdf).toURI().toURL();
  }


  @Override
  public String[] getAttributeNames() {
    return new String[]{
      TITLE,
      PDF_FILE
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return name;
    }
    else if (PDF_FILE.equals(key)) {
      return PDF_FILE;
    }
    return null;
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      File.class
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      "Title",
      "File"
    };
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (TITLE.equals(key)) {
      name = (String) value;
      launch.putValue(Action.NAME, name);
      url = null;
      getI18nData().setUntranslatedValue(key, name);
    }
    else if (PDF_FILE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      pdf = (String) value;
      url = null;
    }
  }

  protected MenuItemProxy launchItem;

  @Override
  public void addTo(Buildable parent) {
    launchItem = new MenuItemProxy(launch);
    MenuManager.getInstance().addToSection("Documentation.Module", launchItem); //NON-NLS
    launch.setEnabled(true);
  }

  @Override
  public void removeFrom(Buildable parent) {
    MenuManager.getInstance()
               .removeFromSection("Documentation.Module", launchItem); //NON-NLS
    launch.setEnabled(false);
  }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String getConfigureName() {
    return name;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.html", "PDFHelpFile"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void remove(Buildable child) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.BrowserPDFFile.component_type"); //$NON-NLS-1$
  }

  @Override
  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "BrowserPDFFile." + getConfigureName(), null, //NON-NLS
          new String[] {TITLE},
          new boolean[] {true},
          new String[] {Resources.getString("Editor.menu_command")});
    }
    return myI18nData;
  }
}
