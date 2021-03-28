/*
 * Copyright (c) 2000-2021 by Brent Easton, Rodney Kinney, Joel Uckelman
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

package VASSAL.launch;

import VASSAL.configure.StringConfigurer;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Stream;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.TitledBorder;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.treetable.DefaultMutableTreeTableNode;
import org.jdesktop.swingx.treetable.DefaultTreeTableModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ExtensionsManager;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.ExtensionMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.ui.ServerStatusView;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ModuleExtensionFileFilter;
import VASSAL.tools.logging.LogPane;
import VASSAL.tools.menu.CheckBoxMenuItemProxy;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuItemProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;
import VASSAL.tools.swing.Dialogs;
import VASSAL.tools.swing.SplitPane;
import VASSAL.tools.swing.SwingUtils;
import VASSAL.tools.version.UpdateCheckAction;
import VASSAL.tools.version.VersionUtils;

public class ModuleManagerWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  private static final Logger logger =
    LoggerFactory.getLogger(ModuleManagerWindow.class);

  private static final String SHOW_STATUS_KEY = "showServerStatus"; //NON-NLS
  private static final String DIVIDER_LOCATION_KEY = "moduleManagerDividerLocation"; //NON-NLS
  private static final String DEVELOPER_INFO_KEY = "moduleManagerDeveloperInfo"; //NON-NLS
  private static final String COLUMN_WIDTHS_KEY = "moduleManagerColumnWidths"; //NON-NLS
  private static final int COLUMNS = 5;
  private static final int KEY_COLUMN = 0;
  private static final int VERSION_COLUMN = 1;
  private static final int SPARE_COLUMN = 2;
  private static final int VASSAL_COLUMN = 3;
  private static final int SAVED_COLUMN = 4;
  private static final String[] columnHeadings = new String[COLUMNS];
  private static final TableColumn[] columns = new TableColumn[COLUMNS];

  private final ImageIcon moduleIcon;
  private final ImageIcon activeExtensionIcon;
  private final ImageIcon inactiveExtensionIcon;
  private final ImageIcon openGameFolderIcon;
  private final ImageIcon closedGameFolderIcon;
  private final ImageIcon fileIcon;

  private StringArrayConfigurer recentModuleConfig;
  private StringArrayConfigurer moduleConfig;
  private File selectedModule;

  private final CardLayout modulePanelLayout;
  private final JPanel moduleView;
  private final SplitPane splitPane;

  private MyTreeNode rootNode;
  private MyTree tree;
  private MyTreeTableModel treeModel;
  protected MyTreeNode selectedNode;

  private long lastExpansionTime;
  private TreePath lastExpansionPath;

  private final IntConfigurer dividerLocationConfig;

  private static final long doubleClickInterval;
  static {
    final Object dci =
      Toolkit.getDefaultToolkit().getDesktopProperty("awt.multiClickInterval");
    doubleClickInterval = dci instanceof Integer ? (Integer) dci : 200L;
  }

  public static ModuleManagerWindow getInstance() {
    return instance;
  }

  private static final ModuleManagerWindow instance = new ModuleManagerWindow();

  public ModuleManagerWindow() {
    setTitle("VASSAL " + Info.getVersion()); //NON-NLS
    setLayout(new BoxLayout(getContentPane(), BoxLayout.X_AXIS));

    ApplicationIcons.setFor(this);

    final AbstractAction shutDownAction = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        saveColumnWidths();
        final Prefs gp = Prefs.getGlobalPrefs();
        try {
          gp.close();
        }
        catch (IOException ex) {
          WriteErrorDialog.error(ex, gp.getFile());
        }

        try {
          ModuleManager.getInstance().shutDown();
        }
        catch (IOException ex) {
          ErrorDialog.bug(ex);
        }

        logger.info("Exiting"); //NON-NLS
        System.exit(0);
      }
    };
    shutDownAction.putValue(Action.NAME, Resources.getString(Resources.QUIT));

    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
       public void windowClosing(WindowEvent e) {
        shutDownAction.actionPerformed(null);
      }
    });

    // setup menubar and actions
    final MenuManager mm = MenuManager.getInstance();
    final MenuBarProxy mb = mm.getMenuBarProxyFor(this);

    // file menu
    final MenuProxy fileMenu =
      new MenuProxy(Resources.getString("General.file"));
    fileMenu.setMnemonic(Resources.getString("General.file.shortcut").charAt(0));

    fileMenu.add(mm.addKey("Main.play_module"));
    fileMenu.add(mm.addKey("Main.edit_module"));
    fileMenu.add(mm.addKey("Main.new_module"));
    fileMenu.addSeparator();

    if (!SystemUtils.IS_OS_MAC) {
      fileMenu.add(mm.addKey("Prefs.edit_preferences"));
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("General.quit"));
    }

    // tools menu
    final MenuProxy toolsMenu =
      new MenuProxy(Resources.getString("General.tools"));

    // Initialize Global Preferences
    Prefs.getGlobalPrefs().getEditor().initDialog(this);
    Prefs.initSharedGlobalPrefs();

    final BooleanConfigurer serverStatusConfig =
      new BooleanConfigurer(SHOW_STATUS_KEY, null, Boolean.FALSE);
    Prefs.getGlobalPrefs().addOption(null, serverStatusConfig);

    dividerLocationConfig = new IntConfigurer(DIVIDER_LOCATION_KEY, null, -10);
    Prefs.getGlobalPrefs().addOption(null, dividerLocationConfig);

    final BooleanConfigurer developerInfoConfig = new BooleanConfigurer(DEVELOPER_INFO_KEY, Resources.getString("Prefs.developer_info"), false);
    Prefs.getGlobalPrefs().addOption(Resources.getString("Prefs.general_tab"), developerInfoConfig);
    developerInfoConfig.addPropertyChangeListener(e1 -> updateColumnDisplay());

    toolsMenu.add(new CheckBoxMenuItemProxy(new AbstractAction(
                   Resources.getString("Chat.server_status")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        splitPane.toggleRight();
        serverStatusConfig.setValue(
          serverStatusConfig.booleanValue() ? Boolean.FALSE : Boolean.TRUE);
        if (splitPane.isRightVisible()) {
          splitPane.setDividerLocation(getPreferredDividerLocation());
        }
      }
    }, serverStatusConfig.booleanValue()));

    toolsMenu.add(mm.addKey("Main.import_module"));

    toolsMenu.add(new MenuItemProxy(new AbstractAction(
                    Resources.getString("ModuleManager.clear_tilecache")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        final int dialogResult = Dialogs.showConfirmDialog(
          ModuleManagerWindow.this,
          Resources.getString("ModuleManager.clear_tilecache_title"),
          Resources.getString("ModuleManager.clear_tilecache_heading"),
          Resources.getString("ModuleManager.clear_tilecache_message"),
          JOptionPane.WARNING_MESSAGE,
          JOptionPane.OK_CANCEL_OPTION);

        if (dialogResult == JOptionPane.OK_OPTION) {

          final File tdir = new File(Info.getConfDir(), "tiles");
          if (tdir.exists()) {
            try {
              FileUtils.forceDelete(tdir);
              FileUtils.forceMkdir(tdir);
            }
            catch (IOException e) {
              WriteErrorDialog.error(e, tdir);
            }
          }
        }
      }
    }));

    // help menu
    final MenuProxy helpMenu =
      new MenuProxy(Resources.getString("General.help"));
    helpMenu.setMnemonic(Resources.getString("General.help.shortcut").charAt(0));

    helpMenu.add(mm.addKey("General.help"));
    helpMenu.add(mm.addKey("Main.tour"));
    helpMenu.add(mm.addKey("Help.user_guide"));
    helpMenu.addSeparator();
    helpMenu.add(mm.addKey("UpdateCheckAction.update_check"));
    helpMenu.add(mm.addKey("Help.error_log"));

    if (!SystemUtils.IS_OS_MAC) {
      helpMenu.addSeparator();
      helpMenu.add(mm.addKey("AboutScreen.about_vassal"));
    }

    mb.add(fileMenu);
    mb.add(toolsMenu);
    mb.add(helpMenu);

    // add actions
    mm.addAction("Main.play_module", new Player.PromptLaunchAction(this));
    mm.addAction("Main.edit_module", new Editor.PromptLaunchAction(this));
    mm.addAction("Main.new_module", new Editor.NewModuleLaunchAction(this));
    mm.addAction("Main.import_module",
      new Editor.PromptImportLaunchAction(this));
    mm.addAction("Prefs.edit_preferences",
      Prefs.getGlobalPrefs().getEditor().getEditAction());
    mm.addAction("General.quit", shutDownAction);

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "README.html").toURI().toURL();
      mm.addAction("General.help", new ShowHelpAction(url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "userguide/userguide.pdf").toURI().toURL();
      mm.addAction("Help.user_guide",
        new ShowHelpAction("Help.user_guide", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    mm.addAction("Main.tour", new LaunchTourAction(this));
    mm.addAction("AboutScreen.about_vassal", new AboutVASSALAction(this));
    mm.addAction("UpdateCheckAction.update_check", new UpdateCheckAction(this));
    mm.addAction("Help.error_log", new ShowErrorLogAction(this));

    setJMenuBar(mm.getMenuBarFor(this));

    // Load Icons
    moduleIcon = new ImageIcon(
      getClass().getResource("/images/mm-module.png")); //NON-NLS
    activeExtensionIcon = new ImageIcon(
      getClass().getResource("/images/mm-extension-active.png")); //NON-NLS
    inactiveExtensionIcon = new ImageIcon(
      getClass().getResource("/images/mm-extension-inactive.png")); //NON-NLS
    openGameFolderIcon = new ImageIcon(
      getClass().getResource("/images/mm-gamefolder-open.png")); //NON-NLS
    closedGameFolderIcon = new ImageIcon(
      getClass().getResource("/images/mm-gamefolder-closed.png")); //NON-NLS
    fileIcon = new ImageIcon(getClass().getResource("/images/mm-file.png")); //NON-NLS

    // build module controls
    final JPanel moduleControls = new JPanel(new BorderLayout());
    modulePanelLayout = new CardLayout();
    moduleView = new JPanel(modulePanelLayout);
    buildTree();
    final JScrollPane scroll = new JScrollPane(tree);
    moduleView.add(scroll, "modules"); //NON-NLS

    final JEditorPane l = new JEditorPane("text/html",
      Resources.getString("ModuleManager.quickstart"));
    l.setEditable(false);

    // Try to get background color and font from LookAndFeel;
    // otherwise, use dummy JLabel to get color and font.
    Color bg = UIManager.getColor("control");
    Font font = UIManager.getFont("Label.font");

    if (bg == null || font == null) {
      final JLabel dummy = new JLabel();
      if (bg == null) bg = dummy.getBackground();
      if (font == null) font = dummy.getFont();
    }

    l.setBackground(bg);
    ((HTMLEditorKit) l.getEditorKit()).getStyleSheet().addRule(
      "body { font: " + font.getFamily() + " " + font.getSize() + "pt }");

    l.addHyperlinkListener(BrowserSupport.getListener());

// FIXME: use MigLayout for this!
    // this is necessary to get proper vertical alignment
    final JPanel p = new JPanel(new GridBagLayout());
    final GridBagConstraints c = new GridBagConstraints();
    c.fill = GridBagConstraints.HORIZONTAL;
    c.anchor = GridBagConstraints.CENTER;
    p.add(l, c);

    moduleView.add(p, "quickStart"); //NON-NLS
    modulePanelLayout.show(
      moduleView, getModuleCount() == 0 ? "quickStart" : "modules");
    moduleControls.add(moduleView, BorderLayout.CENTER);
    moduleControls.setBorder(new TitledBorder(
      Resources.getString("ModuleManager.recent_modules")));

    add(moduleControls);

    // build server status controls
    final ServerStatusView serverStatusControls =
      new ServerStatusView(new CgiServerStatus());
    serverStatusControls.setBorder(
      new TitledBorder(Resources.getString("Chat.server_status")));

    splitPane = new SplitPane(
      SplitPane.HORIZONTAL_SPLIT,
      moduleControls, serverStatusControls
    );

    // show the server status controls according to the prefs
    splitPane.setRightVisible(serverStatusConfig.booleanValue());
    splitPane.setResizeWeight(1.0);

    splitPane.setDividerLocation(getPreferredDividerLocation());
    splitPane.addPropertyChangeListener(
      "dividerLocation",
      e -> setPreferredDividerLocation((Integer) e.getNewValue())
    );

    add(splitPane);

    final Rectangle r = SwingUtils.getScreenBounds(this);
    serverStatusControls.setPreferredSize(
      new Dimension((int) (r.width / 3.5), 0));

    setSize(3 * r.width / 4, 3 * r.height / 4);

    // Save/load the window position and size in prefs
    final PositionOption option =
      new PositionOption(PositionOption.key + "ModuleManager", this); //NON-NLS
    Prefs.getGlobalPrefs().addOption(option);
  }

  public void setWaitCursor(boolean wait) {
    setCursor(Cursor.getPredefinedCursor(
      wait ? Cursor.WAIT_CURSOR : Cursor.DEFAULT_CURSOR
    ));
  }

  protected void setPreferredDividerLocation(int i) {
    dividerLocationConfig.setValue(i);
  }

  protected int getPreferredDividerLocation() {
    return dividerLocationConfig.getIntValue(500);
  }

  // Show/Hide the two 'developer' columns depending on the pref value
  private void updateColumnDisplay() {
    final boolean showDeveloperInfo = (Boolean) Prefs.getGlobalPrefs().getValue(DEVELOPER_INFO_KEY);
    final TableColumnModel model = tree.getColumnModel();

    if (showDeveloperInfo) {
      if (model.getColumnCount() < COLUMNS) {
        model.addColumn(columns[VASSAL_COLUMN]);
        model.addColumn(columns[SAVED_COLUMN]);
      }
    }
    else {
      model.removeColumn(columns[VASSAL_COLUMN]);
      model.removeColumn(columns[SAVED_COLUMN]);
    }
  }

  private static Pair<String, String> splitVersion(String s) {
    final int len = s.length();
    for (int i = 0; i < len; ++i) {
      final char c = s.charAt(i);
      if (0x30 <= c && c <= 0x39) {
        return Pair.of(s.substring(0, i), s.substring(i));
      }
    }
    return Pair.of(s, "");
  }

  protected void buildTree() {
    final List<ModuleInfo> moduleList = new ArrayList<>();
    final List<String> missingModules = new ArrayList<>();

    // RecentModules key was used through 3.5.1, but 3.2 can't read the
    // buildFile.xml for 3.5+ modules. Hence we've switched to the Modules
    // key, but still collect whatever is in RecentModules for compatibility.
    recentModuleConfig = new StringArrayConfigurer("RecentModules", null); //NON-NLS
    Prefs.getGlobalPrefs().addOption(null, recentModuleConfig);

    moduleConfig = new StringArrayConfigurer("Modules", null); //NON-NLS
    Prefs.getGlobalPrefs().addOption(null, moduleConfig);

    Stream.concat(
      Arrays.stream(recentModuleConfig.getStringArray()),
      Arrays.stream(moduleConfig.getStringArray())
    ).sorted().distinct().forEach(s -> {
      final ModuleInfo module = new ModuleInfo(s);
      if (module.getFile().exists() && module.isValid()) {
        moduleList.add(module);
      }
      else {
        missingModules.add(s);
      }
    });

    for (final String s : missingModules) {
      logger.info(Resources.getString("ModuleManager.removing_module", s));
      final ModuleInfo toRemove = moduleList
        .stream()
        .filter(moduleInfo -> moduleInfo.getModuleName().equals(s))
        .findFirst()
        .orElse(null);
      moduleList.remove(toRemove);
      recentModuleConfig.removeValue(s);
      moduleConfig.removeValue(s);
    }

    moduleList.sort((a, b) -> {
      // sort module names in ascending order
      int i = a.toString().compareTo(b.toString());
      if (i == 0) {
        // module names are the same
        final Pair<String, String> ap = splitVersion(a.getVersion());
        final Pair<String, String> bp = splitVersion(b.getVersion());

        i = ap.getLeft().compareTo(bp.getLeft());
        if (i == 0) {
          // leading nonnumeric part of versions are the same
          // sort version numbers in descending order
          i = -VersionUtils.compareVersions(ap.getRight(), bp.getRight());
        }
      }
      return i;
    });

    rootNode = new MyTreeNode(new RootInfo());

    for (final ModuleInfo moduleInfo : moduleList) {
      final MyTreeNode moduleNode = new MyTreeNode(moduleInfo);
      for (final ExtensionInfo ext : moduleInfo.getExtensions()) {
        final MyTreeNode extensionNode = new MyTreeNode(ext);
        moduleNode.add(extensionNode);
      }

      final ArrayList<File> missingFolders = new ArrayList<>();

      for (final File f : moduleInfo.getFolders()) {
        if (f.exists() && f.isDirectory()) {
          final GameFolderInfo folderInfo = new GameFolderInfo(f, moduleInfo);
          final MyTreeNode folderNode = new MyTreeNode(folderInfo);
          moduleNode.add(folderNode);
          final ArrayList<File> l = new ArrayList<>();

          final File[] files = f.listFiles();
          if (files == null) continue;

          for (final File f1 : files) {
            if (f1.isFile()) {
              l.add(f1);
            }
          }
          Collections.sort(l);

          for (final File f2 : l) {
            final SaveFileInfo fileInfo = new SaveFileInfo(f2, folderInfo);
            if (fileInfo.isValid() && fileInfo.belongsToModule()) {
              final MyTreeNode fileNode = new MyTreeNode(fileInfo);
              folderNode.add(fileNode);
            }
          }
        }
        else {
          missingFolders.add(f);
        }
      }

      for (final File mf : missingFolders) {
        logger.info(
          Resources.getString("ModuleManager.removing_folder", mf.getPath()));
        moduleInfo.removeFolder(mf);
      }

      rootNode.add(moduleNode);
    }

    updateModuleList();

    treeModel = new MyTreeTableModel(rootNode);
    tree = new MyTree(treeModel);

    tree.setRootVisible(false);
    tree.setEditable(false);

    tree.setTreeCellRenderer(new MyTreeCellRenderer());

    tree.addMouseListener(new MouseAdapter() {
      @Override
      public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() == 2 && SwingUtils.isMainMouseButtonDown(e)) {
          final TreePath path =
            tree.getPathForLocation(e.getPoint().x, e.getPoint().y);

          // do nothing if not on a node, or if this node was expanded
          // or collapsed during the past doubleClickInterval milliseconds
          if (path == null || (lastExpansionPath == path &&
              e.getWhen() - lastExpansionTime <= doubleClickInterval)) return;

          tree.activateOrExpandNode(path);
        }
      }

      @Override
      public void mousePressed(MouseEvent e) {
        maybePopup(e);
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        maybePopup(e);
      }

      private void maybePopup(MouseEvent e) {
        if (e.isPopupTrigger()) {
          final TreePath path =
            tree.getPathForLocation(e.getPoint().x, e.getPoint().y);
          if (path == null) return;

          final int row = tree.getRowForPath(path);
          if (row >= 0) {
            selectedNode = (MyTreeNode) path.getLastPathComponent();
            tree.clearSelection();
            tree.addRowSelectionInterval(row, row);
            final AbstractInfo target =
              (AbstractInfo) selectedNode.getUserObject();
            target.buildPopup(row).show(tree, e.getX(), e.getY());
          }
        }
      }
    });

    // We capture the time and location of clicks which would cause
    // expansion in order to distinguish these from clicks which
    // might launch a module or game.
    tree.addTreeWillExpandListener(new TreeWillExpandListener() {
      @Override
      public void treeWillCollapse(TreeExpansionEvent e) {
        lastExpansionTime = System.currentTimeMillis();
        lastExpansionPath = e.getPath();
      }

      @Override
      public void treeWillExpand(TreeExpansionEvent e) {
        lastExpansionTime = System.currentTimeMillis();
        lastExpansionPath = e.getPath();
      }
    });

    // This ensures that double-clicks always start the module but
    // doesn't prevent single-clicks on the handles from working.
    tree.setToggleClickCount(3);

    tree.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    tree.addTreeSelectionListener(e -> {
      final MyTreeNode node = (MyTreeNode) e.getPath().getLastPathComponent();
      final AbstractInfo target = node.getNodeInfo();
      if (target instanceof ModuleInfo) {
        setSelectedModule(target.getFile());
      }
      else {
        if (node.getParent() != null) {
          setSelectedModule(node.getParentModuleFile());
        }
      }
    });

    // Pref to hold the column widths.
    final StringConfigurer widthsConfig = new StringConfigurer(COLUMN_WIDTHS_KEY, null, "");
    Prefs.getGlobalPrefs().addOption(null, widthsConfig);

    setColumnWidths();

    // Set cell renderers for centered fields
    final TableColumnModel model = tree.getColumnModel();
    final DefaultTableCellRenderer centerRenderer = new CenteringCellRenderer();
    model.getColumn(VERSION_COLUMN).setCellRenderer(centerRenderer);
    model.getColumn(SAVED_COLUMN).setCellRenderer(centerRenderer);
    model.getColumn(VASSAL_COLUMN).setCellRenderer(centerRenderer);

    tree.getTableHeader().setAlignmentX(JComponent.CENTER_ALIGNMENT);

    // Save the columns so they can be removed/restored as needed
    for (int i = 0; i < COLUMNS; i++) {
      columns[i] = tree.getColumnModel().getColumn(i);
    }

    // Show/hide the developer columns
    updateColumnDisplay();
  }

  // Set the preferred widths of the columns based on the recorded preference
  // All 5 columns are guaranteed to exist at this point.
  private void setColumnWidths() {
    final String widths = Prefs.getGlobalPrefs().getValue(COLUMN_WIDTHS_KEY).toString();
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(widths, ',');
    final TableColumnModel model = tree.getColumnModel();

    model.getColumn(0).setPreferredWidth(sd.nextInt(250));
    model.getColumn(0).setMinWidth(200);
    model.getColumn(1).setPreferredWidth(sd.nextInt(100));
    model.getColumn(1).setMinWidth(100);
    model.getColumn(2).setPreferredWidth(sd.nextInt(500));
    model.getColumn(2).setMinWidth(200);
    model.getColumn(3).setPreferredWidth(sd.nextInt(100));
    model.getColumn(3).setMinWidth(100);
    model.getColumn(4).setPreferredWidth(sd.nextInt(100));
    model.getColumn(4).setMinWidth(100);
  }

  // Save the current column widths in the preference
  // The Developer columns may not exist at this point.
  private void saveColumnWidths() {
    final SequenceEncoder se = new SequenceEncoder(',');
    final TableColumnModel model = tree.getColumnModel();
    for (int i = 0; i < model.getColumnCount(); i++) {
      se.append(model.getColumn(i).getWidth());
    }
    Prefs.getGlobalPrefs().setValue(COLUMN_WIDTHS_KEY, se.toString());
  }

  public void updateRequest(File f) {
    SwingUtilities.invokeLater(() -> update(f));
  }

  /**
   * A File has been saved or created by the Player or the Editor. Update
   * the display as necessary.
   * @param f The file
   */
  public void update(File f) {
    final AbstractMetaData data = MetaDataFactory.buildMetaData(f);

    if (data instanceof ModuleMetaData) {
      // Module.
      // If we already have this module, refresh it, otherwise add it.
      final MyTreeNode moduleNode = rootNode.findNode(f);
      if (moduleNode == null) {
        addModule(f);
      }
      else {
        moduleNode.refresh();
      }
    }
    else if (data instanceof ExtensionMetaData) {
      // Extension.
      // Check if it has been saved into one of the extension directories
      // for any module we already know of. Refresh the module
      for (int i = 0; i < rootNode.getChildCount(); i++) {
        final MyTreeNode moduleNode = rootNode.getChild(i);
        final ModuleInfo moduleInfo = (ModuleInfo) moduleNode.getNodeInfo();
        for (final ExtensionInfo ext : moduleInfo.getExtensions()) {
          if (ext.getFile().equals(f)) {
            moduleNode.refresh();
            return;
          }
        }
      }
    }
    else if (data instanceof SaveMetaData) {
      // Save Game or Log file.
      // If the parent of the save file is already recorded as a Game Folder,
      // pass the file off to the Game Folder to handle. Otherwise, ignore it.
      for (int i = 0; i < rootNode.getChildCount(); i++) {
        final MyTreeNode moduleNode = rootNode.getChild(i);
        final MyTreeNode folderNode = moduleNode.findNode(f.getParentFile());
        if (folderNode != null &&
            folderNode.getNodeInfo() instanceof GameFolderInfo) {
          ((GameFolderInfo) folderNode.getNodeInfo()).update(f);
          return;
        }
      }
    }

    tree.repaint();
  }

  /**
   * Return the number of Modules added to the Module Manager
   *
   * @return Number of modules
   */
  private int getModuleCount() {
    return rootNode.getChildCount();
  }

  public File getSelectedModule() {
    return selectedModule;
  }

  private void setSelectedModule(File selectedModule) {
    this.selectedModule = selectedModule;
  }

  public void addModule(File f) {
    if (!rootNode.contains(f)) {
      final ModuleInfo moduleInfo = new ModuleInfo(f);
      if (moduleInfo.isValid()) {
        final MyTreeNode moduleNode = new MyTreeNode(moduleInfo);
        treeModel.insertNodeInto(moduleNode, rootNode,
                                 rootNode.findInsertIndex(moduleInfo));
        for (final ExtensionInfo ext : moduleInfo.getExtensions()) {
          final MyTreeNode extensionNode = new MyTreeNode(ext);
          treeModel.insertNodeInto(extensionNode, moduleNode,
              moduleNode.findInsertIndex(ext));
        }
        updateModuleList();
      }
    }
  }

  public void removeModule(File f) {
    final MyTreeNode moduleNode = rootNode.findNode(f);
    treeModel.removeNodeFromParent(moduleNode);
    updateModuleList();
  }

  public File getModuleByName(String name) {
    if (name == null) return null;

    for (int i = 0; i < rootNode.getChildCount(); i++) {
      final ModuleInfo module =
        (ModuleInfo) rootNode.getChild(i).getNodeInfo();

      if (name.equals(module.getModuleName())) return module.getFile();
    }

    return null;
  }

  private void updateModuleList() {
    final List<String> l = new ArrayList<>();
    for (int i = 0; i < rootNode.getChildCount(); i++) {
      final ModuleInfo module =
        (ModuleInfo) (rootNode.getChild(i)).getNodeInfo();
      l.add(module.encode());
    }
    recentModuleConfig.setValue(l.toArray(new String[0]));
    moduleConfig.setValue(l.toArray(new String[0]));
    modulePanelLayout.show(
      moduleView, getModuleCount() == 0 ? "quickStart" : "modules");
  }

  /** *************************************************************************
   * Custom Tree table model:-
   *  - Return column count
   *  - Return column headings
   */
  private static class MyTreeTableModel extends DefaultTreeTableModel {
    public MyTreeTableModel(MyTreeNode rootNode) {
      super(rootNode);
      columnHeadings[KEY_COLUMN] = Resources.getString("ModuleManager.module");
      columnHeadings[VERSION_COLUMN] = Resources.getString("ModuleManager.version");
      columnHeadings[VASSAL_COLUMN] = Resources.getString("ModuleManager.created_by");
      columnHeadings[SPARE_COLUMN] = Resources.getString("ModuleManager.description");
      columnHeadings[SAVED_COLUMN] = Resources.getString("ModuleManager.last_saved");
    }

    @Override
    public int getColumnCount() {
      return COLUMNS;
    }

    @Override
    public String getColumnName(int col) {
      return columnHeadings[col];
    }

    @Override
    public Object getValueAt(Object node, int column)  {
      return ((MyTreeNode) node).getValueAt(column);
    }
  }

  /**
   * Custom implementation of JXTreeTable
   * Fix for bug on startup generating illegal column numbers
   *
   */
  private static class MyTree extends JXTreeTable {
    private static final long serialVersionUID = 1L;

    public MyTree(MyTreeTableModel treeModel) {
      super(treeModel);
      createKeyBindings(this);
    }

    public void activateOrExpandNode(TreePath path) {
      final ModuleManagerWindow mmw = ModuleManagerWindow.getInstance();
      mmw.selectedNode = (MyTreeNode) path.getLastPathComponent();

      final AbstractInfo target =
        (AbstractInfo) mmw.selectedNode.getUserObject();

      final int row = mmw.tree.getRowForPath(path);
      if (row < 0) return;

      // launch module or load save, otherwise expand or collapse node
      if (target instanceof ModuleInfo) {
        final ModuleInfo modInfo = (ModuleInfo) target;
        if (modInfo.isModuleTooNew()) {
          ErrorDialog.show(
            "Error.module_too_new",
            modInfo.getFile().getPath(),
            modInfo.getVassalVersion(),
            Info.getVersion()
          );
          return;
        }
        else {
          ((ModuleInfo) target).play();
        }
      }
      else if (target instanceof SaveFileInfo) {
        ((SaveFileInfo) target).play();
      }
      else if (isExpanded(row)) {
        collapseRow(row);
      }
      else {
        expandRow(row);
      }
    }

    private void createKeyBindings(JTable table) {
      table.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "Enter");
      table.getActionMap().put("Enter", new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent ae) {
          //do something meaningful on JTable enter pressed

          final MyTree tree = ModuleManagerWindow.getInstance().tree;
          final int row = tree.getSelectedRow();

          final TreePath path = tree.getPathForRow(row);
          if (path != null) {
            tree.activateOrExpandNode(path);
          }
        }
      });
    }

// FIXME: Where's the rest of the comment???
    /**
     * There appears to be a bug/strange interaction between JXTreetable and the ComponentSplitter
     * when the Component
     */
    @Override
    public String getToolTipText(MouseEvent event) {
      if (getComponentAt(event.getPoint().x, event.getPoint().y) == null) return null;
      return super.getToolTipText(event);
    }
  }

  /**
   * Custom Tree cell renderer:-
   *   - Add file name as tooltip
   *   - Handle expanded display (some nodes use the same icon for expanded/unexpanded)
   *   - Gray out inactive extensions
   *   - Gray out Save Games that belong to other modules
   */
  private static class MyTreeCellRenderer extends DefaultTreeCellRenderer {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getTreeCellRendererComponent(
        JTree tree, Object value, boolean selected, boolean expanded,
        boolean leaf, int row, boolean hasFocus) {
      super.getTreeCellRendererComponent(
        tree, value, selected, expanded, leaf, row, hasFocus);
      final AbstractInfo info = ((MyTreeNode) value).getNodeInfo();
      setText(info.toString());
      setToolTipText(info.getToolTipText());
      setIcon(info.getIcon(expanded));
      setForeground(info.getTreeCellFgColor());
      return this;
    }
  }

  /** *************************************************************************
   * Custom cell render for Version column
   *   - Center data
   */
  private static class CenteringCellRenderer extends DefaultTableCellRenderer {
    private static final long serialVersionUID = 1L;

    public CenteringCellRenderer() {
      super();
      this.setHorizontalAlignment(CENTER);
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
      return this;
    }
  }

  /** *************************************************************************
   * Custom TreeTable Node
   */
  private static class MyTreeNode extends DefaultMutableTreeTableNode {

    public MyTreeNode(AbstractInfo nodeInfo) {
      super(nodeInfo);
      nodeInfo.setTreeNode(this);
    }

    public AbstractInfo getNodeInfo() {
      return (AbstractInfo) getUserObject();
    }

    public File getFile() {
      return getNodeInfo().getFile();
    }

    public void refresh() {
      getNodeInfo().refresh();
    }

    @Override
    public void setValueAt(Object aValue, int column) {
    }

    @Override
    public Object getValueAt(int column) {
      return getNodeInfo().getValueAt(column);
    }

    public MyTreeNode getChild(int index) {
      return (MyTreeNode) super.getChildAt(index);
    }

    public MyTreeNode findNode(File f) {
      for (int i = 0; i < getChildCount(); i++) {
        final MyTreeNode moduleNode = getChild(i);

        // NB: we canonicalize because File.equals() does not
        // always return true when one File is a relative path.
        try {
          f = f.getCanonicalFile();
        }
        catch (IOException e) {
          f = f.getAbsoluteFile();
        }

        if (f.equals(moduleNode.getNodeInfo().getFile())) {
          return moduleNode;
        }
      }
      return null;
    }

    public boolean contains(File f) {
      return findNode(f) != null;
    }

    public int findInsertIndex(AbstractInfo info) {
      for (int i = 0; i < getChildCount(); i++) {
        final MyTreeNode childNode = getChild(i);
        if (childNode.getNodeInfo().compareTo(info) >= 0) {
          return i;
        }
      }
      return getChildCount();
    }

    /**
     * Return the Module node enclosing this node
     *
     * @return Parent Tree Node
     */
    public MyTreeNode getParentModuleNode() {
      final AbstractInfo info = getNodeInfo();
      if (info instanceof RootInfo) {
        return null;
      }
      else if (info instanceof ModuleInfo) {
        return this;
      }
      else if (getParent() == null) {
        return null;
      }
      else {
        return ((MyTreeNode) getParent()).getParentModuleNode();
      }
    }

    /**
     * Return the Module file of the Module node enclosing this node
     *
     * @return Module File
     */
    public File getParentModuleFile() {
      final MyTreeNode parentNode = getParentModuleNode();
      return parentNode == null ? null : parentNode.getFile();
    }
  }

  /** *************************************************************************
   * All tree nodes encapsulate a User-defined object holding the user
   * data for that node. In the ModuleManager, all user-defined objects
   * are subclasses of AbstractInfo
   */
  private abstract class AbstractInfo implements Comparable<AbstractInfo> {
    protected File file;
    protected Icon openIcon;
    protected Icon closedIcon;
    protected boolean valid = true;
    protected String error = "";
    protected MyTreeNode node;

    public AbstractInfo(File f, Icon open, Icon closed) {
      setFile(f);
      setIcon(open, closed);
    }
    public AbstractInfo(File f, Icon i) {
      this (f, i, i);
    }

    public AbstractInfo(File f) {
      this(f, null);
    }

    public AbstractInfo() {
    }

    @Override
    public String toString() {
      return file == null ? "" : file.getName();
    }

    public File getFile() {
      return file;
    }

    public void setFile(File f) {
      if (f == null) return;

      try {
        file = f.getCanonicalFile();
      }
      catch (IOException e) {
        file = f.getAbsoluteFile();
      }
    }

    public String getToolTipText() {
      if (file == null) {
        return "";
      }
      else {
        return file.getPath();
      }
    }

    @Override
    public int compareTo(AbstractInfo info) {
      return getSortKey().compareTo(info.getSortKey());
    }

    public JPopupMenu buildPopup(int row) {
      return null;
    }

    public Icon getIcon(boolean expanded) {
      return expanded ? openIcon : closedIcon;
    }

    public void setIcon(Icon i) {
      setIcon(i, i);
    }

    public void setIcon(Icon open, Icon closed) {
      openIcon = open;
      closedIcon = closed;
    }

    public String getValueAt(int column) {
      switch (column) {
      case KEY_COLUMN:
        return toString();
      case VERSION_COLUMN:
        return getVersion();
      case VASSAL_COLUMN:
        return getVassalVersion();
      case SAVED_COLUMN:
        return getLastSaved();
      default:
        return null;
      }
    }

    public void setValid(boolean b) {
      valid = b;
    }

    public boolean isValid() {
      return valid;
    }

    public void setError(String s) {
      error = s;
    }

    public String getError() {
      return error;
    }

    public String getVersion() {
      return "";
    }

    public String getVassalVersion() {
      return "";
    }

    public String getComments() {
      return "";
    }

    public String getLastSaved() {
      return "";
    }

    public MyTreeNode getTreeNode() {
      return node;
    }

    public void setTreeNode(MyTreeNode n) {
      node = n;
    }

    /**
     * Return a String used to sort different types of AbstractInfo's that are
     * children of the same parent.
     *
     * @return sort key
     */
    public abstract String getSortKey();

    /**
     * Return the color of the text used to display the name in column 1.
     * Over-ride this to change color depending on item state.
     *
     *  @return cell text color
     */
    public Color getTreeCellFgColor() {
      return Color.black;
    }

    /**
     * Refresh yourself and any children
     */
    public void refresh() {
      refreshChildren();
    }

    public void refreshChildren() {
      for (int i = 0; i < node.getChildCount(); i++) {
        (node.getChild(i)).refresh();
      }
    }
  }

  /** *************************************************************************
   * Root Node User Information - Root node is hidden, so not much action here.
   */
  private class RootInfo extends AbstractInfo {
    public RootInfo() {
      super(null);
    }

    @Override
    public String getSortKey() {
      return "";
    }
  }

  /** *************************************************************************
   * Module Node User Information
   */
  public class ModuleInfo extends AbstractInfo {

    private final ExtensionsManager extMgr;
    private final SortedSet<File> gameFolders = new TreeSet<>();
    private ModuleMetaData metadata;

    private final Action newExtensionAction =
      new NewExtensionLaunchAction(ModuleManagerWindow.this);

    private final AbstractAction addExtensionAction = new AbstractAction(
        Resources.getString("ModuleManager.add_extension")) {

      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        final FileChooser fc = FileChooser.createFileChooser(
          ModuleManagerWindow.this, (DirectoryConfigurer)
            Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));
        if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
          final File selectedFile = fc.getSelectedFile();
          final ExtensionInfo testExtInfo = new ExtensionInfo(selectedFile, true, null);
          if (testExtInfo.isValid()) {
            final File f = getExtensionsManager().setActive(fc.getSelectedFile(), true);
            final MyTreeNode moduleNode = rootNode.findNode(selectedModule);
            final ExtensionInfo extInfo = new ExtensionInfo(f, true, (ModuleInfo) moduleNode.getNodeInfo());
            if (extInfo.isValid()) {
              final MyTreeNode extNode = new MyTreeNode(extInfo);
              treeModel.insertNodeInto(extNode, moduleNode, moduleNode.findInsertIndex(extInfo));
            }
          }
          else {
            JOptionPane.showMessageDialog(ModuleManagerWindow.this, testExtInfo.getError(), null, JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    };

    private final AbstractAction addFolderAction = new AbstractAction(
        Resources.getString("ModuleManager.add_save_game_folder")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        final FileChooser fc = FileChooser.createFileChooser(
          ModuleManagerWindow.this, (DirectoryConfigurer)
            Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY),
            FileChooser.DIRECTORIES_ONLY);
        if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
          addFolder(fc.getSelectedFile());
        }
      }
    };

    public ModuleInfo(File f) {
      super(f, moduleIcon);
      extMgr = new ExtensionsManager(f);
      loadMetaData();
    }

    protected void loadMetaData() {
      final AbstractMetaData data = MetaDataFactory.buildMetaData(file);
      if (data instanceof ModuleMetaData) {
        setValid(true);
        metadata = (ModuleMetaData) data;
      }
      else {
        setValid(false);
      }
    }

    protected boolean isModuleTooNew() {
      return metadata != null && Info.isModuleTooNew(metadata.getVassalVersion());
    }

    @Override
    public String getVassalVersion() {
      return metadata == null ? "" : metadata.getVassalVersion();
    }

    @Override
    public String getLastSaved() {
      return metadata == null ? "" : metadata.formatLastSaved();
    }

    /**
     * Initialise ModuleInfo based on a saved preference string.
     * See encode().
     *
     * @param s Preference String
     */
    public ModuleInfo(String s) {
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
      setFile(new File(sd.nextToken()));
      setIcon(moduleIcon);
      loadMetaData();
      extMgr = new ExtensionsManager(getFile());
      while (sd.hasMoreTokens()) {
        gameFolders.add(new File(sd.nextToken()));
      }
    }

    /**
     * Refresh this module and all children
     */
    @Override
    public void refresh() {
      loadMetaData();

      // Remove any missing children
      final MyTreeNode[] nodes = new MyTreeNode[getTreeNode().getChildCount()];
      for (int i = 0; i < getTreeNode().getChildCount(); i++) {
        nodes[i] = getTreeNode().getChild(i);
      }
      for (final MyTreeNode myTreeNode : nodes) {
        if (!myTreeNode.getFile().exists()) {
          treeModel.removeNodeFromParent(myTreeNode);
        }
      }

      // Refresh or add any existing children
      for (final ExtensionInfo ext : getExtensions()) {
        MyTreeNode extNode = getTreeNode().findNode(ext.getFile());
        if (extNode == null) {
          if (ext.isValid()) {
            extNode = new MyTreeNode(ext);
            treeModel.insertNodeInto(extNode, getTreeNode(), getTreeNode().findInsertIndex(ext));
          }
        }
        else {
          extNode.refresh();
        }
      }
    }

    /**
     * Encode any information which needs to be recorded in the Preference entry for this module:-
     *  - Path to Module File
     *  - Paths to any child Save Game Folders
     *
     *  @return encoded data
     */
    public String encode() {
      final SequenceEncoder se = new SequenceEncoder(file.getPath(), ';');
      for (final File f : gameFolders) {
        se.append(f.getPath());
      }
      return se.getValue();
    }

    public ExtensionsManager getExtensionsManager() {
      return extMgr;
    }

    public void addFolder(File f) {
      try {
        f = f.getCanonicalFile();
      }
      catch (final IOException e) {
        JOptionPane.showMessageDialog(
          ModuleManagerWindow.this,
          Resources.getString("Error.file_read_error", f.getPath()),
          "Error",
          JOptionPane.ERROR_MESSAGE
        );

        return;
      }

      // try to create the directory if it doesn't exist
      if (!f.exists() && !f.mkdirs()) {
        JOptionPane.showMessageDialog(
          ModuleManagerWindow.this,
          Resources.getString("Install.error_unable_to_create", f.getPath()),
          "Error", //NON-NLS
          JOptionPane.ERROR_MESSAGE
        );

        return;
      }

      gameFolders.add(f);
      final MyTreeNode moduleNode = rootNode.findNode(selectedModule);
      final GameFolderInfo folderInfo =
        new GameFolderInfo(f, (ModuleInfo) moduleNode.getNodeInfo());
      final MyTreeNode folderNode = new MyTreeNode(folderInfo);
      final int idx = moduleNode.findInsertIndex(folderInfo);
      treeModel.insertNodeInto(folderNode, moduleNode, idx);

      for (final File file : f.listFiles()) {
        if (file.isFile()) {
          final SaveFileInfo fileInfo = new SaveFileInfo(file, folderInfo);
          if (fileInfo.isValid() && fileInfo.belongsToModule()) {
            final MyTreeNode fileNode = new MyTreeNode(fileInfo);
            treeModel.insertNodeInto(fileNode, folderNode,
                                     folderNode.findInsertIndex(fileInfo));
          }
        }
      }
      updateModuleList();
    }

    public void removeFolder(File f) {
      gameFolders.remove(f);
    }

    public SortedSet<File> getFolders() {
      return gameFolders;
    }

    public List<ExtensionInfo> getExtensions() {
      final List<ExtensionInfo> l = new ArrayList<>();
      for (final File f : extMgr.getActiveExtensions()) {
        l.add(new ExtensionInfo(f, true, this));
      }
      for (final File f : extMgr.getInactiveExtensions()) {
        l.add(new ExtensionInfo(f, false, this));
      }
      Collections.sort(l);
      return l;
    }

    public void play() {
      new Player.LaunchAction(
          ModuleManagerWindow.this, file).actionPerformed(null);
    }

    @Override
    public JPopupMenu buildPopup(int row) {
      final boolean tooNew = Info.isModuleTooNew(metadata.getVassalVersion());

      final JPopupMenu m = new JPopupMenu();

      final Action playAction = new Player.LaunchAction(ModuleManagerWindow.this, file);
      playAction.setEnabled(playAction.isEnabled() && !tooNew);
      m.add(playAction);

      final Action editAction = new Editor.ListLaunchAction(ModuleManagerWindow.this, file);
      editAction.setEnabled(editAction.isEnabled() && !tooNew);
      m.add(editAction);

      m.add(new AbstractAction(Resources.getString("General.remove")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          removeModule(file);
          cleanupTileCache();
        }
      });

      m.addSeparator();

      m.add(addFolderAction);
      addFolderAction.setEnabled(!tooNew);

      m.addSeparator();

      m.add(newExtensionAction);
      newExtensionAction.setEnabled(!tooNew);

      m.add(addExtensionAction);
      addExtensionAction.setEnabled(!tooNew);

      return m;
    }

    public void cleanupTileCache() {
      final String hstr = DigestUtils.sha1Hex(
        metadata.getName() + "_" + metadata.getVersion()
      );

      final File tdir = new File(Info.getConfDir(), "tiles/" + hstr);
      if (tdir.exists()) {
        try {
          FileUtils.forceDelete(tdir);
        }
        catch (IOException e) {
          WriteErrorDialog.error(e, tdir);
        }
      }
    }

    /*
     * Is the module currently being Played or Edited?
     */
    public boolean isInUse() {
      return AbstractLaunchAction.isInUse(file) ||
             AbstractLaunchAction.isEditing(file);
    }

    @Override
    public String getVersion() {
      return metadata.getVersion();
    }

    public String getLocalizedDescription() {
      return metadata.getLocalizedDescription();
    }

    public String getModuleName() {
      return metadata.getName();
    }

    @Override
    public String toString() {
      return metadata.getLocalizedName();
    }

    @Override
    public String getValueAt(int column) {
      return column == SPARE_COLUMN ?
        getLocalizedDescription() : super.getValueAt(column);
    }

    @Override
    public String getSortKey() {
      return metadata == null ? "" : metadata.getLocalizedName();
    }

    @Override
    public Color getTreeCellFgColor() {
      return Info.isModuleTooNew(getVassalVersion()) ? Color.GRAY : Color.BLACK;
    }
  }

  /** *************************************************************************
   * Extension Node User Information
   */
  private class ExtensionInfo extends AbstractInfo {

    private boolean active;
    private final ModuleInfo moduleInfo;
    private ExtensionMetaData metadata;

    public ExtensionInfo(File file, boolean active, ModuleInfo module) {
      super(file, active ? activeExtensionIcon : inactiveExtensionIcon);
      this.active = active;
      moduleInfo = module;
      loadMetaData();
    }

    protected void loadMetaData() {
      final AbstractMetaData data = MetaDataFactory.buildMetaData(file);
      if (data instanceof ExtensionMetaData) {
        setValid(true);
        metadata = (ExtensionMetaData) data;
      }
      else {
        setError(Resources.getString("ModuleManager.invalid_extension"));
        setValid(false);
      }
    }

    @Override
    public void refresh() {
      loadMetaData();
      setActive(getExtensionsManager().isExtensionActive(getFile()));
      tree.repaint();
    }

    public boolean isActive() {
      return active;
    }

    public void setActive(boolean b) {
      active = b;
      setIcon(active ? activeExtensionIcon : inactiveExtensionIcon);
    }

    @Override
    public String getVersion() {
      return metadata == null ? "" : metadata.getVersion();
    }

    @Override
    public String getVassalVersion() {
      return metadata == null ? "" : metadata.getVassalVersion();
    }

    @Override
    public String getLastSaved() {
      return metadata == null ? "" : metadata.formatLastSaved();
    }

    public String getDescription() {
      return metadata == null ? "" : metadata.getDescription();
    }

    public ExtensionsManager getExtensionsManager() {
      return moduleInfo == null ? null : moduleInfo.getExtensionsManager();
    }

    @Override
    public String toString() {
      String s = getFile().getName();
      String st = "";
      if (metadata == null) {
        st = Resources.getString("ModuleManager.invalid");
      }
      if (!active) {
        st += st.length() > 0 ? "," : "";
        st += Resources.getString("ModuleManager.inactive");
      }
      if (st.length() > 0) {
        s += " (" + st + ")";
      }

      return s;
    }

    @Override
    public JPopupMenu buildPopup(int row) {
      final JPopupMenu m = new JPopupMenu();

      final boolean tooNew = Info.isModuleTooNew(metadata.getVassalVersion());

      final Action activateAction = new ActivateExtensionAction(
        Resources.getString(isActive() ?
        "ModuleManager.deactivate" : "ModuleManager.activate")
      );
      activateAction.setEnabled(!tooNew);
      m.add(activateAction);

      final Action editAction = new EditExtensionLaunchAction(
          ModuleManagerWindow.this, getFile(), getSelectedModule());
      editAction.setEnabled(!tooNew);
      m.add(editAction);

      return m;
    }

    @Override
    public Color getTreeCellFgColor() {
      // FIXME: should get colors from LAF
      if (isActive()) {
        return metadata == null ? Color.red : Color.black;
      }
      else {
        return metadata == null ? Color.pink : Color.gray;
      }
    }

    @Override
    public String getValueAt(int column) {
      return column == SPARE_COLUMN ?
        getDescription() : super.getValueAt(column);
    }

    /*
     * Is the extension, or its owning module currently being Played or Edited?
     */
    public boolean isInUse() {
      return AbstractLaunchAction.isInUse(file) ||
             AbstractLaunchAction.isEditing(file);
    }

    private class ActivateExtensionAction extends AbstractAction {
      private static final long serialVersionUID = 1L;

      public ActivateExtensionAction(String s) {
        super(s);
        setEnabled(!isInUse() && ! moduleInfo.isInUse());
      }

      @Override
      public void actionPerformed(ActionEvent evt) {
        setFile(getExtensionsManager().setActive(getFile(), !isActive()));
        setActive(getExtensionsManager().isExtensionActive(getFile()));
        final TreePath path = tree.getPathForRow(tree.getSelectedRow());
        final MyTreeNode extNode = (MyTreeNode) path.getLastPathComponent();
        treeModel.setValueAt("", extNode, 0);
      }
    }

    /**
     * Sort Extensions by File Name
     */
    @Override
    public String getSortKey() {
      return getFile().getName();
    }
  }

  /** *************************************************************************
   * Saved Game Folder Node User Information
   */
  private class GameFolderInfo extends AbstractInfo {
    protected String comment;
    protected ModuleInfo moduleInfo;
    protected long dtm;

    public GameFolderInfo(File f, ModuleInfo m) {
      super(f, openGameFolderIcon, closedGameFolderIcon);
      moduleInfo = m;
      dtm = f.lastModified();
    }

    @Override
    public JPopupMenu buildPopup(int row) {
      final boolean tooNew = Info.isModuleTooNew(moduleInfo.getVassalVersion());

      final JPopupMenu m = new JPopupMenu();

      final Action refreshAction = new AbstractAction(Resources.getString("General.refresh")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          refresh();
        }
      };

      refreshAction.setEnabled(!tooNew);
      m.add(refreshAction);
      m.addSeparator();

      final Action removeAction = new AbstractAction(Resources.getString("General.remove")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          final MyTreeNode moduleNode = rootNode.findNode(moduleInfo.getFile());
          final MyTreeNode folderNode = moduleNode.findNode(getFile());
          treeModel.removeNodeFromParent(folderNode);
          moduleInfo.removeFolder(getFile());
          updateModuleList();
        }
      };

      removeAction.setEnabled(!tooNew);
      m.add(removeAction);

      return m;
    }

    public ModuleInfo getModuleInfo() {
      return moduleInfo;
    }

    @Override
    public void refresh() {
      // Remove any files that no longer exist
      for (int i = getTreeNode().getChildCount() - 1; i >= 0; i--) {
        final MyTreeNode fileNode = getTreeNode().getChild(i);
        final SaveFileInfo fileInfo = (SaveFileInfo) fileNode.getNodeInfo();
        if (!fileInfo.getFile().exists()) {
          treeModel.removeNodeFromParent(fileNode);
        }
      }

      // Refresh any that are. Only include Save files belonging to this
      // module, or that are pre vassal 3.1
      final File[] files = getFile().listFiles();
      if (files == null) return;

      for (final File f : files) {
        final AbstractMetaData fdata = MetaDataFactory.buildMetaData(f);
        if (fdata != null) {
          if (fdata instanceof SaveMetaData) {
            final String moduleName = ((SaveMetaData) fdata).getModuleName();
            if (moduleName == null ||
                moduleName.length() == 0 ||
                moduleName.equals(getModuleInfo().getModuleName())) {
              update(f);
            }
          }
        }
      }
    }

    /**
     * Update the display for the specified save File, or add it in if
     * we don't already know about it.
     * @param f Save File
     */
    public void update(File f) {
      for (int i = 0; i < getTreeNode().getChildCount(); i++) {
        final SaveFileInfo fileInfo = (SaveFileInfo) (getTreeNode().getChild(i)).getNodeInfo();
        if (fileInfo.getFile().equals(f)) {
          fileInfo.refresh();
          return;
        }
      }
      final SaveFileInfo fileInfo = new SaveFileInfo(f, this);
      final MyTreeNode fileNode = new MyTreeNode(fileInfo);
      treeModel.insertNodeInto(fileNode, getTreeNode(),
          getTreeNode().findInsertIndex(fileInfo));
    }

    /**
     * Force Game Folders to sort after extensions
     */
    @Override
    public String getSortKey() {
      return "~~~" + getFile().getName();
    }
  }

  /** *************************************************************************
   * Saved Game File Node User Information
   */
  private class SaveFileInfo extends AbstractInfo {

    protected GameFolderInfo folderInfo;  // Owning Folder
    protected SaveMetaData metadata;      // Save file metadata

    public SaveFileInfo(File f, GameFolderInfo folder) {
      super(f, fileIcon);
      folderInfo = folder;
      loadMetaData();
    }

    protected void loadMetaData() {
      final AbstractMetaData data = MetaDataFactory.buildMetaData(file);
      if (data instanceof SaveMetaData) {
        metadata = (SaveMetaData) data;
        setValid(true);
      }
      else {
        setValid(false);
      }
    }

    @Override
    public void refresh() {
      loadMetaData();
      tree.repaint();
    }

    @Override
    public JPopupMenu buildPopup(int row) {
      final boolean tooNew = Info.isModuleTooNew(metadata.getVassalVersion());

      final JPopupMenu m = new JPopupMenu();
      final Action launchAction = new Player.LaunchAction(
        ModuleManagerWindow.this, getModuleFile(), file
      );
      launchAction.setEnabled(!tooNew);
      m.add(launchAction);
      return m;
    }

    protected File getModuleFile() {
      return folderInfo.getModuleInfo().getFile();
    }

    public void play() {
      new Player.LaunchAction(
        ModuleManagerWindow.this, getModuleFile(), file).actionPerformed(null);
    }

    @Override
    public String getValueAt(int column) {
      return column == SPARE_COLUMN ?
        buildComments() : super.getValueAt(column);
    }

    private String buildComments() {
      String comments = "";
      if (!belongsToModule()) {
        if (metadata != null && metadata.getModuleName().length() > 0) {
          comments = "[" + metadata.getModuleName() +  "] ";
        }
      }
      comments += (metadata == null ? "" : metadata.getDescription());
      return comments;
    }

    private boolean belongsToModule() {
      return metadata != null
        && (metadata.getModuleName().length() == 0 ||
        folderInfo.getModuleInfo().getModuleName().equals(
          metadata.getModuleName()));
    }

    @Override
    public Color getTreeCellFgColor() {
      // FIXME: should get colors from LAF
      return belongsToModule() ? Color.black : Color.gray;
    }

    @Override
    public String getVersion() {
      return metadata == null ? "" : metadata.getModuleVersion();
    }

    /**
     * Sort Save Files by file name
     */
    @Override
    public String getSortKey() {
      return this.getFile().getName();
    }
  }

  /**
   * Action to create a New Extension and edit it in another process.
   */
  private class NewExtensionLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public NewExtensionLaunchAction(Frame frame) {
      super(Resources.getString("ModuleManager.new_extension"), frame,
        Editor.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.NEW_EXT)
      );
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      lr.module = getSelectedModule();

      // register that this module is being used
      if (isEditing(lr.module)) return;
      incrementUsed(lr.module);

      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count
          decrementUsed(lr.module);
        }
      };
    }
  }

  /**
   * Action to Edit an Extension in another process
   */
  private static class EditExtensionLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public EditExtensionLaunchAction(Frame frame, File extension, File module) {
      super(Resources.getString("Editor.edit_extension"), frame,
        Editor.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.EDIT_EXT, module, extension)
      );

      setEnabled(!isInUse(module) && !isInUse(extension));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // check that neither this module nor this extension is being edited
      if (isInUse(lr.module) || isInUse(lr.extension)) return;

      // register that this module is being used
      incrementUsed(lr.module);

      // register that this extension is being edited
      markEditing(lr.module);

      super.actionPerformed(e);
      setEnabled(false);
    }

    @Override
    protected void addFileFilters(FileChooser fc) {
      fc.addChoosableFileFilter(new ModuleExtensionFileFilter());
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count for module
          decrementUsed(lr.module);

          // register that this extension is done being edited
          unmarkEditing(lr.extension);
          setEnabled(true);
        }
      };
    }
  }

  private static class ShowErrorLogAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    private final Frame frame;

    public ShowErrorLogAction(Frame frame) {
      super(Resources.getString("Help.error_log"));
      this.frame = frame;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
// FIXME: don't create a new one each time!
      final File logfile = Info.getErrorLogPath();
      final LogPane lp = new LogPane(logfile);

// FIXME: this should have its own key. Probably keys should be renamed
// to reflect what they are labeling, e.g., Help.show_error_log_menu_item,
// Help.error_log_dialog_title.
      final JDialog d =
        new JDialog(frame, Resources.getString("Help.error_log"));
      d.setLayout(new MigLayout("insets 0")); //NON-NLS
      d.add(new JScrollPane(lp), "grow, push, w 500, h 600"); //NON-NLS

      d.setLocationRelativeTo(frame);
      d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

      SwingUtils.repack(d);
      d.setVisible(true);
    }
  }
}

