/*
 * Copyright (c) 2020 by Brian Reynolds
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
package VASSAL.search;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collection;

import org.apache.commons.io.IOUtils;
import org.jsoup.Jsoup;

/**
 * Parses image tags out of an HTML file or string. Used to add the image filenames to a list, e.g. of images used.
 */
public class HTMLImageFinder {
  private String fileString;

  /**
   * @param file - Prepares to parse an HTML file in the module
   */
  public HTMLImageFinder(File file) {
    if (file != null) {
      final DataArchive mda = GameModule.getGameModule().getDataArchive();
      try (InputStream inner = mda.getInputStream(file.getPath());
           InputStream in = new BufferedInputStream(inner)) {
        fileString = IOUtils.toString(in, StandardCharsets.UTF_8);
      }
      catch (IOException e) {
        ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.not_found", "HTMLImageFinder"), file.getName(), e)); //NON-NLS
      }
    }
  }

  /**
   * @param string Prepares to parse an html string
   */
  public HTMLImageFinder(String string) {
    if (string != null) {
      if (string.toLowerCase().contains("<html>")) { //NON-NLS
        fileString = string;
      }
      else {
        fileString = "<html>" + string.trim() + "</html>"; //NON-NLS
      }
    }
  }

  /**
   * Parses the HTML and adds any image filenames to the collection
   * @param s Collection to add image filenames to.
   */
  public void addImageNames(Collection<String> s) {
    if (fileString != null) {
      s.addAll(Jsoup.parse(fileString).getElementsByTag("img").eachAttr("src")); //NON-NLS
    }
  }
}
