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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.folder.FontSubFolder;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.i18n.Resources;

import VASSAL.tools.swing.SwingUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Support for TrueType fonts provided by VASSAL or within the module
 * Initial implementation very simplistic, just location and caching of true type fonts supplied in Vengine.jar.
 */
public class FontOrganizer extends AbstractConfigurable {

  public static final String FONTS = "Fonts";

  private static final Logger log = LoggerFactory.getLogger(FontOrganizer.class);

  public static final String FONTS_FOLDER = "fonts";          // Folder in Vengine containing font files
  public static final String FONTS_CONFIG = "fonts.config";   // List of font files in FONTS_FOLDER
  public static final String VASSAL_EDITOR_FONT = "JetBrains Mono Regular"; // Monspaced font for editing

  private static final int COLUMNS = 5;
  private static final int FAMILY_COLUMN = 0;
  private static final int FONT_COLUMN = 1;
  private static final int FILE_COLUMN = 2;
  private static final int VASSAL_COLUMN = 3;
  private static final int STATUS_COLUMN = 4;

  private FontOrganizerConfigurer fontOrganizerConfigurer;

  private final List<VassalFont> vassalFonts = new ArrayList<>();
  private boolean dirty;

  public final java.util.Map<String, Font> fontsByFile = new HashMap<>();

  public FontOrganizerConfigurer getFontOrganizerConfigurer() {
    if (fontOrganizerConfigurer == null) {
      fontOrganizerConfigurer = new FontOrganizerConfigurer("", "", this);
    }
    return fontOrganizerConfigurer;
  }

  public void setFontOrganizerConfigurer(FontOrganizerConfigurer fontOrganizerConfigurer) {
    this.fontOrganizerConfigurer = fontOrganizerConfigurer;
  }

  /**
   * The default monospaced font for Java is usually Courier New which is not great.
   * See if we can do better
   *
   * @param style Font Style
   * @param size  Font Size
   * @return      A monospaced font
   */
  public Font getEditorFont(int style, int size) {

    final Font font = new Font(VASSAL_EDITOR_FONT, style, size);

    if (font == null) {
      return new Font(Font.MONOSPACED, style, size);
    }
    else {
      return font;
    }
  }

  /**
   * A new font has been loaded, either from Vassal, or from the module.
   * The font has been registered with Java (unless already installed) but
   * various parts of Vassal have already been built and need to be informed of the new Font
   *
   * @param font  Added Font
   */
  public static void addFontToVassal(VassalFont font) {

    if (font != null && font.getFontFamily() != null) {
      // Update the list of Fonts available for the Chat Window
      GameModule.getGameModule().getChatter().addFontFamily(font.getFontFamily());
    }
  }

  /** Return a Set of the additional font families available in this module
   *  Using a Set as multiple fonts will have the same family */
  public Set<String> getAdditionalFonts() {
    final Set<String> families = new HashSet<>();
    vassalFonts.forEach((k) -> {
      if (k.getFontName() != null) families.add(k.getFontName());
    });
    getAllDescendantComponentsOf(ModuleFont.class).forEach(
      (k) -> {
        final VassalFont font = k.getFont();
        if (font != null && font.getFontName() != null) {
          families.add(font.getFontName());
        }
      }
    );
    return families;
  }

  private List<VassalFont> getAllFonts() {
    // Sorted List
    final List<VassalFont> fonts = new ArrayList<>();

    // Add the Vassal supplied fonts
    for (final VassalFont font : vassalFonts) {
      if (font != null) {
        fonts.add(font);
      }
    }

    // Add the module specific fonts
    for (final ModuleFont mfont : getAllDescendantComponentsOf(ModuleFont.class)) {
      final VassalFont font = mfont.getFont();
      if (font != null) {
        fonts.add(font);
      }
    }

    Collections.sort(fonts);
    return fonts;
  }


  public boolean isDirty() {
    return dirty;
  }

  public void setDirty(boolean dirty) {
    this.dirty = dirty;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {FONTS};
  }

  @Override
  public void setAttribute(String key, Object value) {

  }

  @Override
  public String getAttributeValueString(String key) {
    return null;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {Resources.getString("Editor.FontOrganizer.fonts")};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class[] {FontConfig.class};
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof GameModule) {
      ((GameModule) parent).setFontOrganizer(this);
    }
    loadVassalFonts();
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (parent instanceof GameModule) {
      ((GameModule) parent).setFontOrganizer(null);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class[] getAllowableConfigureComponents() {
    return new Class[] {ModuleFont.class, FontSubFolder.class};
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.FontOrganizer.component_type"); //$NON-NLS-1$
  }

  @Override
  public String getComponentTypeName() {
    return getConfigureTypeName();
  }

  @Override
  public String getComponentName() {
    return getConfigureTypeName();
  }

  /**
   * Load any True/Open Type fonts included with Vassal. These reside in the /fonts folder of Vengine.jar (or on
   * a folder on disk if we are running via a debugger.
   * To side-step the shennanigans the IconFactory has to deal with to find all the fonts, the /fonts folder contains a
   * file fonts.config that contains a list of the individual ttf files included with Vassal
   */
  private void loadVassalFonts() {

    final URL url;
    final String fontConfigFile = "/" + FONTS_FOLDER + "/" + FONTS_CONFIG;
    try {
      url = GameModule.getGameModule().getDataArchive().getURL(fontConfigFile);
    }
    catch (IOException e) {
      log.error("VASSAL Font Config file " + fontConfigFile + " could not be found");
      return;
    }

    try (InputStream stream = url.openStream(); BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
      String line;
      while ((line = reader.readLine()) != null) {
        if (!line.trim().startsWith("#")) {

          final String fontFile = line.trim();
          final VassalFont font = new VassalFont(fontFile);
          vassalFonts.add(font);
          addFontToVassal(font);
        }
      }
    }
    catch (IOException e) {
      log.error("Error reading Font Congig File " + fontConfigFile, e);
      return;
    }
  }

  /**
   * Utility class to provide a custom configurer for the Font Organizer
   */
  public static class FontConfig implements ConfigurerFactory {

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return ((FontOrganizer) c).getFontOrganizerConfigurer();
    }
  }


  /**
   * Custom configurer for displaying a summary of all custom fonts in Vassal and the current module
   */
  public static class FontOrganizerConfigurer extends Configurer {

    private JScrollPane scroll;

    private JTable table;
    private MyTableModel tableModel;

    private JPanel controls;
    private final FontOrganizer organizer;

    public FontOrganizerConfigurer(String key, String name, FontOrganizer organizer) {
      super(key, name);
      this.organizer = organizer;
    }

    @Override
    public String getValueString() {
      return null;
    }

    @Override
    public void setValue(String s) {
      if (controls != null) {
        rebuildTable();
      }
    }

    @Override
    public Component getControls() {
      if (controls == null) {
        controls = new JPanel(new BorderLayout());
        buildTable();
        scroll = new JScrollPane(table);
        controls.add(scroll, BorderLayout.CENTER);
        scroll.setPreferredSize(new Dimension(800, scroll.getPreferredSize().width));
        SwingUtils.repack(controls);

        controls.addAncestorListener(new AncestorListener() {
          @Override
          public void ancestorAdded(AncestorEvent event) {
            if (organizer.isDirty()) {
              rebuildTable();
            }
          }

          @Override
          public void ancestorRemoved(AncestorEvent event) {
          }

          // Called when the panel become visible
          @Override
          public void ancestorMoved(AncestorEvent event) {
            if (organizer.isDirty()) {
              rebuildTable();
            }
          }
        });
      }

      return controls;
    }

    private void buildTable() {

      tableModel = new MyTableModel();
      table = new JTable(tableModel);
      table.getColumnModel().getColumn(FAMILY_COLUMN).setMinWidth(200);
      table.getColumnModel().getColumn(FONT_COLUMN).setMinWidth(200);
      table.getColumnModel().getColumn(FILE_COLUMN).setMinWidth(200);
      table.getColumnModel().getColumn(VASSAL_COLUMN).setMinWidth(50);
      table.getColumnModel().getColumn(VASSAL_COLUMN).setCellRenderer(new CellCenterRenderer());
      table.getColumnModel().getColumn(STATUS_COLUMN).setMinWidth(200);


      for (final VassalFont font : organizer.getAllFonts()) {
        addFont(font);
      }

      organizer.setDirty(false);

    }

    public void addFont(VassalFont font) {
      tableModel.addRow(new Object[] {
        font.getFontFamily(),
        font.getFontName(),
        font.getFontFile(),
        Resources.getString(font.isVassalFont() ? "Editor.FontOrganizer.vassal" : "Editor.FontOrganizer.module"),
        font.getStatus()});
    }

    private void rebuildTable() {
      buildTable();
      controls.removeAll();
      scroll = new JScrollPane(table);
      //controls.add(scroll, "grow,push");
      controls.add(scroll, BorderLayout.CENTER);
      SwingUtils.repack(controls);
    }

    private static class MyTableModel extends DefaultTableModel {

      private static final int serialVersionUID = 0;

      public MyTableModel() {
        super();
      }

      @Override
      public int getColumnCount() {
        return COLUMNS;
      }

      @Override
      public String getColumnName(int col) {
        if (col == FAMILY_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.font_family");
        }
        else if (col == FONT_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.font_name");
        }
        else if (col == FILE_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.font_file");
        }
        else if (col == VASSAL_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.vassal_heading");
        }
        else if (col == STATUS_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.status");
        }

        return "";
      }

      @Override
      public Class<?> getColumnClass(int column) {
        return String.class;
      }

      @Override
      public boolean isCellEditable(int row, int column) {
        return false;
      }
    }

    private static class CellCenterRenderer extends DefaultTableCellRenderer {
      private static final long serialVersionUID = 1L;

      public CellCenterRenderer() {
        super();
        this.setHorizontalAlignment(CENTER);
      }

      @Override
      public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        return this;
      }
    }
  }
}
