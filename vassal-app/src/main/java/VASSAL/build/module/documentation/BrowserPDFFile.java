/*
 *
 * Copyright (c) 2020 by Vassalengine.org, Brian Reynolds
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
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.DataArchive;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;

import javax.swing.AbstractAction;
import javax.swing.Action;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Easier-to-use support for opening a single PDF file in the browser (it can be done with BrowserHelpFile, but
 * requires a substantial workaround and weird confusing procedures)
 */
public class BrowserPDFFile extends AbstractConfigurable {
  private static final Logger logger =
    LoggerFactory.getLogger(BrowserPDFFile.class);

  public static final String TITLE = "title"; //$NON-NLS-1$
  public static final String PDF_FILE = "pdfFile"; //$NON-NLS-1$
  protected String menuText = Resources.getString("Editor.BrowserPDFFile.default_menu_text");
  protected String pdfFile = Resources.getString("Editor.BrowserPDFFile.default_filename");
  protected Action launch;
  protected URL url;

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
   * Extracts our PDF to a temporary file in the temp directory
   */
  protected void extractPDF() {
    final DataArchive archive = GameModule.getGameModule().getDataArchive();

    try (InputStream in = archive.getInputStream(pdfFile)) {
      final Path out = Files.createTempFile(Info.getTempDir().toPath(), "pdfhelp_", ".pdf"); //NON-NLS
      try {
        Files.copy(in, out, StandardCopyOption.REPLACE_EXISTING);
      }
      catch (IOException e) {
        logger.error("Error while copying file {} from data archive", pdfFile, e); //NON-NLS
      }
      url = out.toUri().toURL();
      out.toFile().deleteOnExit();
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      logger.error("File not found in data archive: {}", pdfFile, e); //NON-NLS
      url = null;
    }
    catch (IOException e) {
      logger.error("Error while reading file {} from data archive", pdfFile, e); //NON-NLS
      url = null;
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      TITLE,
      PDF_FILE
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.menu_command"),
      Resources.getString("Editor.BrowserPDFFile.pdf_file")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      File.class
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return menuText;
    }
    else if (PDF_FILE.equals(key)) {
      return pdfFile;
    }
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (TITLE.equals(key)) {
      menuText = (String) value;
      launch.putValue(Action.NAME, menuText);
      url = null;
    }
    else if (PDF_FILE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      pdfFile = (String) value;
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
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String getConfigureName() {
    return menuText;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.html", "PDF"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void remove(Buildable child) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.BrowserPDFFile.component_type"); //$NON-NLS-1$
  }
}
