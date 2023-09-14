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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.treetable.DefaultMutableTreeTableNode;
import org.jdesktop.swingx.treetable.DefaultTreeTableModel;
import org.jdesktop.swingx.treetable.TreeTableNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.Component;
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



  private static final Logger log = LoggerFactory.getLogger(FontOrganizer.class);

  // In-built VASSAL fonts
  public static final String VASSAL_SERIF_FONT = "VassalSerif";
  public static final String VASSAL_SANSERIF_FONT = "VassalSan";
  public static final String VASSAL_MONOSPACED_FONT = "VassalMono";
  public static final String VASSAL_CONDENSED_FONT = "VassalCondensed";
  public static final String VASSAL_EDITOR_FONT = "VassalEditor";

  public static final String FONTS = "Fonts";
  public static final String FONTS_FOLDER = "fonts";                        // Folder in Vengine containing font files
  public static final String FONTS_CONFIG = "fonts.yml";                    // Contains List of font files in FONTS_FOLDER

  private static final int COLUMNS = 4;
  private static final int NAME_COLUMN = 0;
  private static final int DESC_COLUMN = 1;
  private static final int SOURCE_COLUMN = 2;
  private static final int STATUS_COLUMN = 3;

  private FontOrganizerConfigurer fontOrganizerConfigurer;

  /** List of the fonts loaded from Vassal */
  private final List<VassalFont> vassalFonts = new SortedArrayList();

  /** Font family information loaded from font config file */
  private VassalFonts configuredFonts;

  /** List of the fonts loaded from the current Module */
  private final List<VassalFont> moduleFonts = new SortedArrayList();

  /** Return a Configurer that displays the status of all additional fonts */
  public FontOrganizerConfigurer getFontOrganizerConfigurer() {
    if (fontOrganizerConfigurer == null) {
      fontOrganizerConfigurer = new FontOrganizerConfigurer("", "", this);
    }
    return fontOrganizerConfigurer;
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
    return new Font(VASSAL_EDITOR_FONT, style, size);
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
    return new String[] {""};
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
      final ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
      configuredFonts = mapper.readValue(stream, VassalFonts.class);
    }
    catch (IOException e) {
      log.error("Error reading Font Congig File " + fontConfigFile, e);
      return;
    }

    configuredFonts.getLogicalFonts().forEach(logicalFont -> {
      logicalFont.getFiles().forEach(font -> {
        vassalFonts.add(new VassalFont(font.getFile(), logicalFont.getDesc()));
      });
    });
  }

  public List<VassalFont> getVassalFonts() {
    return vassalFonts;
  }

  public List<VassalFont> getModuleFonts() {
    moduleFonts.clear();
    getAllDescendantComponentsOf(ModuleFont.class).forEach(k -> moduleFonts.add(k.getFont()));
    return moduleFonts;
  }

  public void setDirty(boolean dirty) {
    if (getFontOrganizerConfigurer() != null) {
      getFontOrganizerConfigurer().setDirty(dirty);
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

    private MyTreeTable treeTable;
    private MyTreeModel treeModel;
    private MyTreeNode rootNode;

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
        controls = new JPanel(new MigLayout("ins 4", "[fill]", "[fill]"));
        buildTable();
        scroll = new JScrollPane(treeTable);
        controls.add(scroll, "grow");
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

      rootNode = new MyTreeNode();

      final List<String> families = new ArrayList<>();
      MyTreeNode lastFamilyNode = null;
      for (final VassalFont font : organizer.getVassalFonts()) {
        final MyTreeNode fontNode = new MyTreeNode(font);
        MyTreeNode parentNode;
        final String familyName = font.getFontFamily();

        if (families.contains(familyName)) {
          parentNode = lastFamilyNode;
        }
        else {
          final FontFamily family = organizer.configuredFonts.getFamily(familyName);
          final int styleCount = family.getFiles().size();
          parentNode = new MyTreeNode(
            familyName,
            family.getDesc(),
            family.getBasefont(),
            Resources.getString(
              styleCount == 1 ? "Editor.FontOrganizer.vassal" : "Editor.FontOrganizer.vassal_plural",
              styleCount));
          rootNode.add(parentNode);
          lastFamilyNode = parentNode;
          families.add(familyName);
        }
        parentNode.add(fontNode);
      }

      families.clear();
      lastFamilyNode = null;
      final Map<String, FontFamily> moduleFamilies = new HashMap<>();

      for (final VassalFont font : organizer.getModuleFonts()) {
        final String familyName = font.getFontFamily();

        final FontFamily family = moduleFamilies.computeIfAbsent(familyName, k -> {
          return new FontFamily(familyName, familyName, familyName);
        });
        family.add(new FontFile(font));
      }

      moduleFamilies.values().forEach(k -> {
        final int styleCount = k.getFiles().size();
        final MyTreeNode parentNode = new MyTreeNode(
          k.getName(),
          k.getDesc(),
          k.getBasefont(),
          Resources.getString(
            styleCount == 1 ? "Editor.FontOrganizer.vassal" : "Editor.FontOrganizer.vassal_plural",
            styleCount));
        k.getFiles().forEach(font -> parentNode.add(new MyTreeNode(font.getFont())));
        rootNode.add(parentNode);
      });

      treeModel = new MyTreeModel(rootNode);
      treeTable = new MyTreeTable(treeModel);

      treeTable.getColumnModel().getColumn(NAME_COLUMN).setMinWidth(250);
      treeTable.getColumnModel().getColumn(DESC_COLUMN).setMinWidth(250);
      treeTable.getColumnModel().getColumn(SOURCE_COLUMN).setMinWidth(200);
      treeTable.getColumnModel().getColumn(STATUS_COLUMN).setMinWidth(200);

      configurerDirty = false;

    }
    private void rebuildTable() {
      buildTable();
      controls.removeAll();
      scroll = new JScrollPane(treeTable);
      controls.add(scroll, "grow");
      SwingUtils.repack(controls);
    }

    private static class MyTreeTable extends JXTreeTable {
      private static final long serialVersionUID = 1L;

      public MyTreeTable(MyTreeModel model) {
        super(model);
        setRootVisible(false);
      }

    }

    private static class MyTreeModel extends DefaultTreeTableModel {

      public MyTreeModel(TreeTableNode root) {
        super(root);
      }

      @Override
      public int getColumnCount() {
        return COLUMNS;
      }

      @Override
      public String getColumnName(int col) {
        if (col == NAME_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.name");
        }
        else if (col == DESC_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.description");
        }
        else if (col == SOURCE_COLUMN) {
          return Resources.getString("Editor.FontOrganizer.source");
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
      public Object getValueAt(Object node, int col) {
        final MyTreeNode n = (MyTreeNode) node;
        if (col == NAME_COLUMN) {
          return n.getName();
        }
        else if (col == DESC_COLUMN) {
          return n.getDescription();
        }
        else if (col == SOURCE_COLUMN) {
          return n.getSource();
        }
        else if (col == STATUS_COLUMN) {
          return n.getStatus();
        }
        return "";
      }

      @Override
      public boolean isCellEditable(Object node, int column) {
        return false;
      }

    }

    public static class MyTreeNode extends DefaultMutableTreeTableNode {

      public enum TYPE { ROOT, FAMILY, FONT };

      private final TYPE type;
      private VassalFont font;
      private String name;
      private String description;
      private String source;
      private String status;

      public MyTreeNode() {
        super();
        type = TYPE.ROOT;
      }

      public MyTreeNode(String familyName, String description, String source, String status) {
        super();
        type = TYPE.FAMILY;
        name = familyName;
        this.description = description;
        this.source = source;
        this.status = status;
      }

      public MyTreeNode(VassalFont font) {
        super();
        type = TYPE.FONT;
        this.font = font;
        name = font.getFontName();
        description = "";
        source = font.getFontFile();
        status = font.getStatus();
      }

      public VassalFont getFont() {
        return font;
      }

      public String getName() {
        return name;
      }

      public String getDescription() {
        return description;
      }

      public String getSource() {
        return source;
      }

      public String getStatus() {
        return status;
      }

      public boolean isRoot() {
        return type == TYPE.ROOT;
      }

      public boolean isFamily() {
        return type == TYPE.FAMILY;
      }

      public boolean isFont() {
        return type == TYPE.FONT;
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

  private static class SortedArrayList extends ArrayList {

    private static final long serialVersionUID = 1L;

    @Override
    public boolean add(Object o) {
      super.add(o);
      Collections.sort(this);
      return true;
    }
  }

  /** Private class used to load yaml font config */
  private static class FontFile {
    private String file;
    private String style;
    private transient VassalFont font;

    public FontFile() {
      file = "";
      style = "";
    }

    public FontFile(String file, String style) {
      this.file = file;
      this.style = style;
    }

    public FontFile(VassalFont font) {
      this.font = font;
      this.file = font.getFontFile();
      this.style = font.getDescription();
    }

    public String getFile() {
      return file;
    }

    public void setFile(String file) {
      this.file = file;
    }

    public String getStyle() {
      return style;
    }

    public void setStyle(String style) {
      this.style = style;
    }

    public VassalFont getFont() {
      return font;
    }

    public void setFont(VassalFont font) {
      this.font = font;
    }
  }

  /** Private class used to load yaml font config */
  private static class FontFamily {
    private String name;
    private String desc;
    private String basefont;
    private List<FontFile> files = new ArrayList<>();

    public FontFamily() {
      name = "";
      desc = "";
      basefont = "";
    }

    public FontFamily(String name, String desc, String basefont) {
      this.name = name;
      this.desc = desc;
      this.basefont = basefont;
    }

    public void add(FontFile file) {
      files.add(file);
    }

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    public String getDesc() {
      return desc;
    }

    public void setDesc(String desc) {
      this.desc = desc;
    }

    public String getBasefont() {
      return basefont;
    }

    public void setBasefont(String basefont) {
      this.basefont = basefont;
    }

    public List<FontFile> getFiles() {
      return files;
    }

    public void setFiles(List<FontFile> files) {
      this.files = files;
    }
  }

  /** Private class used to load yaml font config */
  private static class VassalFonts {
    private List<FontFamily> logicalFonts;

    public List<FontFamily> getLogicalFonts() {
      return logicalFonts;
    }

    public FontFamily getFamily(String familyName) {
      for (final FontFamily family : logicalFonts) {
        if (family.getName().equals(familyName)) return family;
      }
      return null;
    }
  }

}
