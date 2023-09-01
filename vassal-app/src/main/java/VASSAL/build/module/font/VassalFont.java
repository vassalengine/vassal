/*
 * Copyright (c) 2023 by The VASSAL Development Team
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

package VASSAL.build.module.font;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;

import java.awt.Font;
import java.awt.FontFormatException;
import java.awt.GraphicsEnvironment;
import java.io.BufferedInputStream;
import java.io.File;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.util.Objects;

/**
   * Classs used to encapsulate a Font loaded from a TTF/OTF file in Vassal or the Module
   */
public class VassalFont implements Comparable<VassalFont> {

  private final String fontFamily;    // Font Family Name
  private final String fontName;      // Specific Font Name
  private final String fontFile;      // File loaded from
  private boolean vassalFont = false; // true for a Font included with Vassal, false for a Font from a module
  private Font font;                  // The base loaded font, null if the Font could not be loaded
  private String loadError = "";      // Error message if the font could not be loaded
  private final boolean registered;   // Did the font successfully regiester with Jave? Main reason for failure is font is already installed on system.


  private VassalFont(String fontFamily, String fontName, String fontFile, boolean vassalFont) {
    this.fontFamily = fontFamily;
    this.fontName = fontName;
    this.fontFile = fontFile;
    this.vassalFont = vassalFont;
    this.registered = false;
  }

  /** Create an Empty Font for use by the Root Node of a Font tree */
  public VassalFont() {
    this(null, null, null, false);
  }

  /** Create a Dummy Font for use by a non-leaf nodes of a Font tree */
  public VassalFont(String fontFamily, String fontName) {
    this(fontFamily, fontName, null, false);
  }

  /**
   * Load a font from Vassal
   * @param fontFileName
   */
  public VassalFont(String fontFileName) {

    fontFile = fontFileName;

    URL url = null;
    final String subPath = FontOrganizer.FONTS_FOLDER + "/" + fontFileName;
    // Resource?
    try {
      url = GameModule.getGameModule().getDataArchive().getURL("/" + subPath);
      if (url == null) {
        url = GameModule.getGameModule().getDataArchive().getURL(subPath);
        if (url == null) {
          loadError = Resources.getString("Editor.VassalFont.find_error");
        }
      }
      else {
        vassalFont = true;
      }
    }
    catch (IOException e) {
      loadError = Resources.getString("Editor.VassalFont.find_error");
    }

    if (url != null) {
      try (InputStream stream = url.openStream()) {
        font = Font.createFont(Font.TRUETYPE_FONT, stream);
      }
      catch (FontFormatException e) {
        loadError = Resources.getString("Editor.VassalFont.invalid_format");
      }
      catch (IOException e) {
        loadError = Resources.getString("Editor.VassalFont.read_error");
      }
    }

    if (font == null) {
      fontName = null;
      fontFamily = null;
      registered = false;
      return;
    }

    fontName = font.getFontName();
    fontFamily = font.getFamily();

    // Register it with java
    registered = GraphicsEnvironment.getLocalGraphicsEnvironment().registerFont(font);

  }

  /**
   * Load a font from an External File
   *
   * @param fontFile
   */
  public VassalFont(File fontFile) {
    vassalFont = false;
    this.fontFile = fontFile.getName();

    try (BufferedInputStream stream = new BufferedInputStream(Files.newInputStream(fontFile.toPath()))) {
      font = Font.createFont(Font.TRUETYPE_FONT, stream);
    }
    catch (FileNotFoundException e) {
      loadError = Resources.getString("Editor.VassalFont.find_error");
    }
    catch (IOException e) {
      loadError = Resources.getString("Editor.VassalFont.read_error");
    }
    catch (FontFormatException e) {
      loadError = Resources.getString("Editor.VassalFont.invalid_format");
    }

    if (font == null) {
      fontFamily = null;
      fontName = null;
      registered = false;
      return;
    }

    fontName = font.getFontName();
    fontFamily = font.getFamily();

    // Register it with java
    registered = GraphicsEnvironment.getLocalGraphicsEnvironment().registerFont(font);

  }

  public String getFontFamily() {
    return fontFamily;
  }

  public String getFontName() {
    return fontName;
  }

  public String getFontFile() {
    return fontFile;
  }

  public boolean isVassalFont() {
    return vassalFont;
  }

  public Font getBaseFont() {
    return font;
  }

  public Font getDerivedFont(int style, int size) {
    return font.deriveFont(style, size);
  }

  public String getStatus() {
    if (font == null) {
      return Resources.getString("Editor.VassalFont.invalid", loadError);
    }
    return Resources.getString("Editor.VassalFont.valid", Resources.getString(registered ? "Editor.VassalFont.registered" : "Editor.VassalFont.unregistered"));
  }

  public String getLoadError() {
    return loadError;
  }


  /**
   * Comparator to sort by font name within family name
   *
   * @param o the object to be compared.
   * @return comparison result
   */
  @Override
  public int compareTo(VassalFont o) {
    // Root node, should never be compared
    if (fontFamily == null) {
      return 0;
    }

    if (o.fontFamily == null) {
      return -1;
    }

    // Family is equal, sort by font name
    if (fontFamily.compareTo(o.fontFamily) == 0) {
      if (fontName == null) {
        return o.fontName == null ? 0 : 1;
      }
      else {
        return o.fontName == null ? -1 : fontName.compareTo(o.fontName);
      }
    }

    // Family is not equal
    return fontFamily.compareTo(o.fontFamily);
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof VassalFont)) {
      return false;
    }
    return Objects.equals(fontFamily, ((VassalFont) obj).fontFamily) && Objects.equals(fontName, ((VassalFont) obj).fontName);
  }

  public boolean isFamilyInfo() {
    return fontFamily != null && fontName == null;
  }
}
