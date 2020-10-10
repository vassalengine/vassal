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
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.DataArchive;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;
import org.apache.commons.io.file.PathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.AbstractAction;
import javax.swing.Action;
import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.zip.ZipInputStream;

public class BrowserPDFFile extends AbstractConfigurable {
  private static final Logger logger =
    LoggerFactory.getLogger(BrowserPDFFile.class);

  public static final String TITLE = "title"; //$NON-NLS-1$
  //public static final String CONTENTS = "contents"; //$NON-NLS-1$
  public static final String PDF_FILE = "pdfFile"; //$NON-NLS-1$
  protected String name;
  protected String pdfFile = "MyRulebook.pdf";
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

  protected void extractPDF() {
    final DataArchive archive = GameModule.getGameModule().getDataArchive();

    try (InputStream in = archive.getInputStream(pdfFile)) {
      Path out = Files.createTempFile(Info.getTempDir().toPath(), "pdfhelp_", ".pdf");
      try {
        Files.copy(in, out, StandardCopyOption.REPLACE_EXISTING);
      }
      catch (IOException e) {
        logger.error("Error while copying file {} from data archive", pdfFile,e); //NON-NLS
      }
      url = out.toUri().toURL();
    }
    catch (FileNotFoundException e) {
      logger.error("File not found in data archive: {}", pdfFile, e); //NON-NLS
      url = null;
      return;
    }
    catch (IOException e) {
      logger.error("Error while reading file {} from data archive", pdfFile, e); //NON-NLS
      url = null;
      return;
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
  public String getAttributeValueString(String key) {
    if (TITLE.equals(key)) {
      return name;
    }
    else if (PDF_FILE.equals(key)) {
      return pdfFile;
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
}
