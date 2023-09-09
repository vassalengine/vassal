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
import java.util.List;
import java.util.Map;

/**
 * Support for TrueType fonts provided by VASSAL or within the module
 * Initial implementation very simplistic, just location and caching of true type fonts supplied in Vengine.jar.
 */
public class FontOrganizer extends AbstractConfigurable {

  public static final String FONTS = "Fonts";

  private static final Logger log = LoggerFactory.getLogger(FontOrganizer.class);

  public static final String FONTS_FOLDER = "fonts";                        // Folder in Vengine containing font files
  public static final String FONTS_CONFIG = "fonts.conf";                   // Contains List of font files in FONTS_FOLDER
  public static final String VASSAL_EDITOR_FONT = "JetBrains Mono Regular"; // Preferred Monospaced font for editing

  private static final int COLUMNS = 5;
  private static final int FAMILY_COLUMN = 0;
  private static final int FONT_COLUMN = 1;
  private static final int FILE_COLUMN = 2;
  private static final int VASSAL_COLUMN = 3;
  private static final int STATUS_COLUMN = 4;

  private FontOrganizerConfigurer fontOrganizerConfigurer;

  private final Map<String, VassalFont> loadedFonts = new HashMap<>();

  /** True if the Map of additional fonts needs to be reloaded */
  private boolean dirty = true;

  /** List of the fonts loaded from Vassal */
  private final List<VassalFont> vassalFonts = new ArrayList<>();

  /** Return a Configurer that displays the status of all additional fonts */
  public FontOrganizerConfigurer getFontOrganizerConfigurer() {
    if (fontOrganizerConfigurer == null) {
      fontOrganizerConfigurer = new FontOrganizerConfigurer("", "", this);
    }
    return fontOrganizerConfigurer;
  }

  /**
   * Centralized location for creating a new Font.
   * If the font is supplied by Vassal or the module, then locate the loaded Font and directly derive the
   * required font from it. Java does know about the newly loaded fonts, but you get better results if we
   * derive the new fonts specifically.
   *
   * If it's not a Vassal/Module font, just pass it to Java to find something suitable.
   *
   * NOTE: Requests for a Vassal/Module font in Swinf rendered HTML do not come through here, they are directly
   * satisified by Java
   *
   * @param fontName  Font Name or Family
   * @param style     Required style
   * @param size      Required size
   *
   * @return new Font.
   */
  public Font createFont(String fontName, int style, int size) {

    final VassalFont font = getFontMap().get(fontName);
    if (font != null && font.isValid()) {
      return font.getDerivedFont(style, size);
    }

    return new Font(fontName, style, size);
  }


  /**
   * The default monospaced font for Java is usually Courier New which is not great.
   * See if we can do better
   *
   * @param style Font Style
   * @param size  Font Size
   * @return      A monospaced font
   */
  // TODO: Make this a preference
  public Font getEditorFont(int style, int size) {

    final VassalFont font = getFontMap().get(VASSAL_EDITOR_FONT);
    if (font != null && font.isValid()) {
      return font.getDerivedFont(style, size);
    }
    return new Font(Font.MONOSPACED, style, size);
  }

  /** Return a Set of the additional font names available in this module */
  public List<String> getAdditionalFonts() {
    final List<String> fonts = new ArrayList<>();
    getAllFonts().forEach((k) -> {
      if (k.isValid()) fonts.add(k.getFontName());
    });
    return fonts;
  }

  /** Return a sorted list of all available fonts loaded from Vassal or the module */
  public List<VassalFont> getAllFonts() {
    final ArrayList<VassalFont> fonts = new ArrayList<>(getFontMap().values());
    Collections.sort(fonts);
    return fonts;
  }

  /** Return the current font Map, rebuild if potentially dirty */
  private Map<String, VassalFont> getFontMap() {
    if (dirty) {
      loadedFonts.clear();

      // Add the Vassal supplied fonts
      vassalFonts.forEach(
        (k) -> {
          if (k.isValid()) {
            loadedFonts.put(k.getFontName(), k);
          }
        }
      );

      // Add the module specific fonts
      getAllDescendantComponentsOf(ModuleFont.class).forEach(
        (k) -> {
          if (k.getFont().isValid()) {
            loadedFonts.put(k.getFont().getFontName(), k.getFont());
          }
        }
      );
    }
    return loadedFonts;
  }

  public boolean isDirty() {
    return dirty;
  }

  public void setDirty(boolean dirty) {
    this.dirty = dirty;
    if (dirty) {
      getFontOrganizerConfigurer().setDirty(dirty);
    }
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

    private boolean configurerDirty = true;

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

    public void setDirty(boolean dirty) {
      configurerDirty = dirty;
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
            if (configurerDirty) {
              rebuildTable();
            }
          }

          @Override
          public void ancestorRemoved(AncestorEvent event) {
          }

          // Called when the panel become visible
          @Override
          public void ancestorMoved(AncestorEvent event) {
            if (configurerDirty) {
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

      configurerDirty = false;

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
