/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Brent Easton, Rodney Kinney, Joel Uckelman 
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

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.border.TitledBorder;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;

import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.treetable.DefaultMutableTreeTableNode;
import org.jdesktop.swingx.treetable.DefaultTreeTableModel;

import VASSAL.Info;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ExtensionMetaData;
import VASSAL.build.module.ExtensionsManager;
import VASSAL.build.module.AbstractMetaData;
import VASSAL.build.module.ModuleMetaData;
import VASSAL.build.module.SaveMetaData;
import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.ui.ServerStatusView;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.TranslateVassalAction;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.FileChooser;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.menu.CheckBoxMenuItemProxy;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;

public class ModuleManagerWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  private static final String SHOW_STATUS_KEY = "showServerStatus";
  private static final int COLUMNS = 3;
  private static final int KEY_COLUMN = 0;
  private static final int VERSION_COLUMN = 1;
  private static final int SPARE_COLUMN = 2;
  private static final String[] columnHeadings = new String[COLUMNS];

  private final ImageIcon moduleIcon;
  private final ImageIcon activeExtensionIcon;
  private final ImageIcon inactiveExtensionIcon;
  private final ImageIcon openGameFolderIcon;
  private final ImageIcon closedGameFolderIcon;
  private final ImageIcon fileIcon;
  
  private StringArrayConfigurer recentModuleConfig;
  private File selectedModule;

  private CardLayout modulePanelLayout;
  private JPanel moduleView;
  private ComponentSplitter.SplitPane serverStatusView;
  
  private MyTreeNode rootNode;
  private JXTreeTable tree;
  private MyTreeTableModel treeModel;
  private MyTreeNode selectedNode;

  public static ModuleManagerWindow getInstance() {
    return instance;
  }

  private static final ModuleManagerWindow instance = new ModuleManagerWindow();

  public ModuleManagerWindow() {
    setTitle("VASSAL");
    setLayout(new BoxLayout(getContentPane(), BoxLayout.X_AXIS));

    final AbstractAction shutDownAction = new AbstractAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        if (!AbstractLaunchAction.shutDown()) return;
        try {
          Prefs.getGlobalPrefs().write();
          Prefs.getGlobalPrefs().close();
        }
        catch (IOException ex) {
          ex.printStackTrace();
        }
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
    
    fileMenu.add(mm.addKey("Main.play_module"));
    fileMenu.add(mm.addKey("Main.edit_module"));
    fileMenu.add(mm.addKey("Main.new_module"));
    fileMenu.add(mm.addKey("Editor.import_module"));

    if (!Info.isMacOSX()) {
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("General.quit"));
    }

    // tools menu
    final MenuProxy toolsMenu =
      new MenuProxy(Resources.getString("General.tools"));
    
    toolsMenu.add(new CheckBoxMenuItemProxy(new AbstractAction(
                   Resources.getString("Chat.server_status")) {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        serverStatusView.toggleVisibility();
        final BooleanConfigurer config = (BooleanConfigurer)
          Prefs.getGlobalPrefs().getOption(SHOW_STATUS_KEY);
        if (config != null) {
          config.setValue(config.booleanValue() ? Boolean.FALSE : Boolean.TRUE);
        }
      }
    }));
 
   toolsMenu.add(mm.addKey("Editor.ModuleEditor.translate_vassal"));

    // help menu
    final MenuProxy helpMenu =
      new MenuProxy(Resources.getString("General.help"));

    helpMenu.add(mm.addKey("General.help"));
    helpMenu.add(mm.addKey("Main.tour"));

    if (!Info.isMacOSX()) {
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
    mm.addAction("Editor.import_module",
      new Editor.PromptImportLaunchAction(this));
    mm.addAction("General.quit", shutDownAction);

    mm.addAction("Editor.ModuleEditor.translate_vassal",
                 new TranslateVassalAction(this));

    URL url = null; 
    try {
      url = new File(Documentation.getDocumentationBaseDir(),
                     "README.html").toURI().toURL();
    }
    catch (MalformedURLException e) {
      ErrorLog.warn(e);
    }
    mm.addAction("General.help", new ShowHelpAction(url, null));
    
    mm.addAction("Main.tour", new LaunchTourAction(this));
    mm.addAction("AboutScreen.about_vassal", AboutVASSAL.getAction());

    setJMenuBar(mm.getMenuBarFor(this));

    // Load Icons
    moduleIcon = new ImageIcon(
      getClass().getResource("/images/mm-module.png"));
    activeExtensionIcon = new ImageIcon(
      getClass().getResource("/images/mm-extension-active.png"));
    inactiveExtensionIcon = new ImageIcon(
      getClass().getResource("/images/mm-extension-inactive.png"));
    openGameFolderIcon = new ImageIcon(
      getClass().getResource("/images/mm-gamefolder-open.png"));
    closedGameFolderIcon = new ImageIcon(
      getClass().getResource("/images/mm-gamefolder-closed.png"));
    fileIcon = new ImageIcon(getClass().getResource("/images/mm-file.png"));
    
    // build module controls 
    final JPanel moduleControls = new JPanel(new BorderLayout());
    modulePanelLayout = new CardLayout();
    moduleView = new JPanel(modulePanelLayout);
    buildTree();
    JScrollPane scroll = new JScrollPane(tree);
    moduleView.add(scroll, "modules");

    final JEditorPane l = new JEditorPane("text/html",
      Resources.getString("ModuleManager.quickstart"));
    l.setEditable(false);

    // pick up background color and font from JLabel
    l.setBackground(UIManager.getColor("control"));
    final Font font = UIManager.getFont("Label.font");
    ((HTMLEditorKit) l.getEditorKit()).getStyleSheet().addRule(
      "body { font: " + font.getFamily() + " " + font.getSize() + "pt }");
    
    l.addHyperlinkListener(new HyperlinkListener() {
      public void hyperlinkUpdate(HyperlinkEvent e) {
        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
          BrowserSupport.openURL(e.getURL().toString());
        }
      }
    });

    // this is necessary to get proper vertical alignment
    final JPanel p = new JPanel(new GridBagLayout());
    final GridBagConstraints c = new GridBagConstraints();
    c.fill = GridBagConstraints.HORIZONTAL;
    c.anchor = GridBagConstraints.CENTER;
    p.add(l, c);

    moduleView.add(p, "quickStart");
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

    serverStatusView = new ComponentSplitter().splitRight(
      moduleControls, serverStatusControls, false);
    serverStatusView.revalidate();

    final Rectangle r = Info.getScreenBounds(this);
    serverStatusControls.setPreferredSize(
      new Dimension((int) (r.width / 3.5), 0));

    setSize(3 * r.width / 4, 3 * r.height / 4);
  }
  
  protected void buildTree() {
    recentModuleConfig = new StringArrayConfigurer("RecentModules", null);
    Prefs.getGlobalPrefs().addOption(null, recentModuleConfig);
    final List<String> missingModules = new ArrayList<String>();
    final List<ModuleInfo> moduleList = new ArrayList<ModuleInfo>();
    for (String s : recentModuleConfig.getStringArray()) {
      final ModuleInfo module = new ModuleInfo(s);
      if (module.getFile().exists() && module.isValid()) {
        moduleList.add(module);
      }
      else {
        missingModules.add(s);
      }
    }

    for (String s : missingModules) {
      moduleList.remove(s);
      recentModuleConfig.removeValue(s);
    }

    Collections.sort(moduleList, new Comparator<ModuleInfo>() {
      public int compare(ModuleInfo f1, ModuleInfo f2) {
        return f1.compareTo(f2);
      }
    });
    
    rootNode = new MyTreeNode (new RootInfo());
    
    for (ModuleInfo moduleInfo : moduleList) {    
      final MyTreeNode moduleNode = new MyTreeNode(moduleInfo);
      for (ExtensionInfo ext : moduleInfo.getExtensions()) {
        final MyTreeNode extensionNode = new MyTreeNode(ext);
        moduleNode.add(extensionNode);
      }
      for (File f : moduleInfo.getFolders()) {
        final GameFolderInfo folderInfo = new GameFolderInfo(f, moduleInfo);
        final MyTreeNode folderNode = new MyTreeNode(folderInfo);
        moduleNode.add(folderNode);
        final ArrayList<File> l = new ArrayList<File>();
        for (File f1 : f.listFiles()) {
          if (f1.isFile()) {
            l.add(f1);
          }
        }
        Collections.sort(l);
        for (File f2 : l) {
          final SaveFileInfo fileInfo = new SaveFileInfo(f2, folderInfo);
          if (fileInfo.isValid() && fileInfo.belongsToModule()) {
            final MyTreeNode fileNode = new MyTreeNode(fileInfo);          
            folderNode.add(fileNode);
          }
        }
      }
      rootNode.add(moduleNode);
    }
    
    treeModel = new MyTreeTableModel(rootNode);
    tree = new JXTreeTable(treeModel);
    
    tree.setRootVisible(false);
    tree.setEditable(false);
    
    tree.setTreeCellRenderer(new MyTreeCellRenderer());
    
    tree.addMouseListener(new MouseAdapter() {
      @Override
      public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() == 2) {
          final TreePath path =
            tree.getPathForLocation(e.getPoint().x, e.getPoint().y);
          if (path == null) return;

          selectedNode = (MyTreeNode) path.getLastPathComponent();

          final int row = tree.getRowForPath(path);          
          if (row < 0) return;

          final AbstractInfo target =
            (AbstractInfo) selectedNode.getUserObject();

          // launch module or load save, otherwise expand or collapse node
          if (target instanceof ModuleInfo) {
            ((ModuleInfo) target).play();
          }
          else if (target instanceof SaveFileInfo) {
            ((SaveFileInfo) target).play();
          }
          else if (tree.isExpanded(row)) {
            tree.collapseRow(row);
          }
          else {
            tree.expandRow(row); 
          }
        }
      } 

      @Override
      public void mouseReleased(MouseEvent e) {
        final TreePath path =
          tree.getPathForLocation(e.getPoint().x, e.getPoint().y);
        if (path == null) return;
              
        selectedNode = (MyTreeNode) path.getLastPathComponent();

        if (e.isMetaDown()) {
          final int row = tree.getRowForPath(path);          
          if (row >= 0) {
            tree.clearSelection();
            tree.addRowSelectionInterval(row, row);
            final AbstractInfo target =
              (AbstractInfo) selectedNode.getUserObject();
            target.buildPopup(row).show(tree, e.getX(), e.getY());
          }
        }
      }
    });

    // This ensures that double-clicks always start the module but
    // doesn't prevent single-clicks on the handles from working. 
    tree.setToggleClickCount(3); 

    tree.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    tree.addTreeSelectionListener(new TreeSelectionListener() {
      public void valueChanged(TreeSelectionEvent e) {
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
      }
    });
    
    columnHeadings[KEY_COLUMN] = "";
    columnHeadings[VERSION_COLUMN] = Resources.getString("ModuleManager.version");
    columnHeadings[SPARE_COLUMN] = "";
    
    // FIXME: Width handling needs improvement. Also save in prefs
    tree.getColumnModel().getColumn(KEY_COLUMN).setMinWidth(250);
    
    tree.getColumnModel().getColumn(VERSION_COLUMN)
                         .setCellRenderer(new VersionCellRenderer());
    tree.getColumnModel().getColumn(VERSION_COLUMN).setMinWidth(75);
    
    tree.getColumnModel().getColumn(SPARE_COLUMN).setMinWidth(10);
    tree.getColumnModel().getColumn(SPARE_COLUMN).setPreferredWidth(600);
    
    // FIXME: How to set alignment of individual header components?
    tree.getTableHeader().setAlignmentX(JComponent.CENTER_ALIGNMENT);
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
      final MyTreeNode moduleNode = new MyTreeNode(moduleInfo);
      treeModel.insertNodeInto(moduleNode, rootNode,
                               rootNode.findInsertIndex(moduleInfo));
      for (ExtensionInfo ext : moduleInfo.getExtensions()) {
        final MyTreeNode extensionNode = new MyTreeNode(ext);
        treeModel.insertNodeInto(extensionNode, moduleNode,
            moduleNode.findInsertIndex(ext));
      }
      updateModuleList();
    }
  }

  public void removeModule(File f) {
    final MyTreeNode moduleNode = rootNode.findNode(f);
    treeModel.removeNodeFromParent(moduleNode);
    updateModuleList();
  }
  
  private void updateModuleList() {
    final List<String> l = new ArrayList<String>();
    for (int i = 0; i < rootNode.getChildCount(); i++) {
      final ModuleInfo module =
        (ModuleInfo) ((MyTreeNode) rootNode.getChildAt(i)).getNodeInfo();
      l.add(module.encode());
    }
    recentModuleConfig.setValue(l.toArray(new String[l.size()]));
    modulePanelLayout.show(
      moduleView, getModuleCount() == 0 ? "quickStart" : "modules");
  }
  
  /** *************************************************************************
   * Custom Tree table model:-
   *  - Return column count
   *  - Return column headings
   */
  private class MyTreeTableModel extends DefaultTreeTableModel {
  
    public MyTreeTableModel(MyTreeNode rootNode) {
      super(rootNode);
    }

    public int getColumnCount() {
      return COLUMNS;
    }
    
    public String getColumnName(int col) {
      switch (col) {
      case VERSION_COLUMN : return Resources.getString("ModuleManager.version");
      default: return "";
      }
    }
    
    public Object getValueAt(Object node, int column)  {
      return ((MyTreeNode) node).getValueAt(column);
    }
    
  }
  
  /** *************************************************************************
   * Custom Tree cell renderer:-
   *   - Add file name as tooltip
   *   - Handle expanded display (some nodes use the same icon for expanded/unexpanded)
   *   - Gray out inactve extensions
   *   - Gray out Save Games that belong to other modules
   */
  private class MyTreeCellRenderer extends DefaultTreeCellRenderer {

    private static final long serialVersionUID = 1L;
    
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
  private class VersionCellRenderer extends DefaultTableCellRenderer  {
    
    private static final long serialVersionUID = 1L;
    
    public VersionCellRenderer() {
      super();
      this.setHorizontalAlignment(CENTER); 
    }
    
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
    }

    public AbstractInfo getNodeInfo() {
      return (AbstractInfo) getUserObject();
    }
    
    public File getFile() {
      return getNodeInfo().getFile();
    }
    
    public void setValueAt(Object aValue, int column) {

    }
    
    public Object getValueAt(int column) {
      return getNodeInfo().getValueAt(column);
    }
    
    public MyTreeNode findNode(File f) {
      for (int i = 0; i < getChildCount(); i++) {
        final MyTreeNode moduleNode = (MyTreeNode) getChildAt(i);

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
        final MyTreeNode childNode = (MyTreeNode) getChildAt(i);
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
    
    /*
     * Sort by Type, then File name.
     */
    public int compareTo(AbstractInfo info) {
      final int typeCompare = info.getSortKey().compareTo(getSortKey());
      return typeCompare == 0 ? getFile().getName().toLowerCase().compareTo(
        info.getFile().getName().toLowerCase()) : typeCompare;
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
    
    public String getComments() {
      return "";
    }
    
    /**
     * Return a String used to sort different types of AbstractInfo's that are
     * children of the same parent.
     * 
     * @return sort key
     */
    public String getSortKey() {
      return "5";
    }
    
    /**
     * Return the color of the text used to display the name in column 1.
     * Over-ride this to change color depending on item state.
     * 
     *  @return cell text color
     */
    public Color getTreeCellFgColor() {
      return Color.black;
    }
  }
  
  /** *************************************************************************
   * Root Node User Information - Root node is hidden, so not much action here.
   */
  private class RootInfo extends AbstractInfo {
    public RootInfo() {
      super(null);
    } 
  }
  
  /** *************************************************************************
   * Module Node User Information
   */
  public class ModuleInfo extends AbstractInfo {

    private ExtensionsManager extMgr;
    private List<File> gameFolders = new ArrayList<File>();
    private ModuleMetaData metadata;
    
    private Action newExtensionAction =
      new NewExtensionLaunchAction(ModuleManagerWindow.this);
    
    private AbstractAction addExtensionAction =
      new AbstractAction(Resources.getString("ModuleManager.add_extension")) {

      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        final FileChooser fc = FileChooser.createFileChooser(
          ModuleManagerWindow.this, (DirectoryConfigurer)
            Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));
        if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
          File selectedFile = fc.getSelectedFile();
          ExtensionInfo testExtInfo = new ExtensionInfo(selectedFile, true, null);
          if (testExtInfo.isValid()) {
            File f = getExtensionsManager().setActive(fc.getSelectedFile(), true);
            MyTreeNode moduleNode = rootNode.findNode(selectedModule);
            ExtensionInfo extInfo = new ExtensionInfo(f, true, (ModuleInfo) moduleNode.getNodeInfo());
            if (extInfo.isValid()) {
              MyTreeNode extNode = new MyTreeNode(extInfo);
              treeModel.insertNodeInto(extNode, moduleNode, moduleNode.findInsertIndex(extInfo));
            }
          }
          else {
            JOptionPane.showMessageDialog(null, testExtInfo.getError(), null, JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    };
    
    private AbstractAction addFolderAction = new AbstractAction(
        Resources.getString("ModuleManager.add_save_game_folder")) {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        final FileChooser fc = FileChooser.createFileChooser(
          ModuleManagerWindow.this, (DirectoryConfigurer)
            Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY),
            FileChooser.DIRECTORIES_ONLY);
        if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
          final File f = fc.getSelectedFile();
          if (!gameFolders.contains(f)) {
            addFolder(f);
          }
        }
      }
    };
    
    public ModuleInfo(File f) {
      super(f, moduleIcon);
      AbstractMetaData data = AbstractMetaData.buildMetaData(file);
      if (data != null && data instanceof ModuleMetaData) {
        setValid(true);
        metadata = (ModuleMetaData) data;
      }
      else {
        setValid(false);
        return;
      }
      extMgr = new ExtensionsManager(f);   
    }
    
    /**
     * Initialise ModuleInfo based on a saved preference string.
     * See encode().
     * 
     * @param s Preference String
     */
    public ModuleInfo(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
      setFile(new File(sd.nextToken()));
      setIcon(moduleIcon);
      AbstractMetaData data = AbstractMetaData.buildMetaData(getFile());
      if (data != null && data instanceof ModuleMetaData) {
        setValid(true);
        metadata = (ModuleMetaData) data;
      }
      else {
        setValid(false);
        return;
      }
      extMgr = new ExtensionsManager(getFile());  
      while (sd.hasMoreTokens()) {
        gameFolders.add(new File(sd.nextToken()));
      }
      Collections.sort(gameFolders);
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
      for (File f : gameFolders) {
        se.append(f.getPath());
      }
      return se.getValue();
    }
    
    public ExtensionsManager getExtensionsManager() {
      return extMgr;
    }

    public void addFolder(File f) {
      gameFolders.add(f);
      final MyTreeNode moduleNode = rootNode.findNode(selectedModule);
      final GameFolderInfo folderInfo =
        new GameFolderInfo(f, (ModuleInfo) moduleNode.getNodeInfo());
      final MyTreeNode folderNode = new MyTreeNode(folderInfo);
      final int idx = moduleNode.findInsertIndex(folderInfo);
      treeModel.insertNodeInto(folderNode, moduleNode, idx);

      for (File file : f.listFiles()) {
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
    
    public List<File> getFolders() {
      return gameFolders;
    }
    
    public List<ExtensionInfo> getExtensions() {
      final List<ExtensionInfo> l = new ArrayList<ExtensionInfo>();
      for (File f : extMgr.getActiveExtensions()) {
        l.add(new ExtensionInfo(f, true, this));
      }
      for (File f : extMgr.getInactiveExtensions()) {
        l.add(new ExtensionInfo(f, false, this));
      }
      Collections.sort(l);
      return l;
    }
    
    public void play() {
      new Player.LaunchAction(
          ModuleManagerWindow.this, file).actionPerformed(null);
    }
    
    public JPopupMenu buildPopup(int row) {
      final JPopupMenu m = new JPopupMenu();
      m.add(new Player.LaunchAction(ModuleManagerWindow.this, file));
      m.add(new Editor.ListLaunchAction(ModuleManagerWindow.this, file));
      m.add(new AbstractAction(Resources.getString("General.remove")) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          removeModule(file);
        }
      });

      m.addSeparator();
      m.add(addFolderAction);
      m.addSeparator();
      m.add(newExtensionAction);
      m.add(addExtensionAction);
      return m;
    }
    
    /*
     * Is the module currently being Played or Edited?
     */
    public boolean isInUse() {
      return AbstractLaunchAction.isInUse(file) || AbstractLaunchAction.isEditing(file);
    }
    
    public String getVersion() {
      return metadata.getVersion();
    }
    
    public String getLocalizedDescription() {
      return metadata.getLocalizedDescription();
    }
    
    public String getModuleName() {
      return metadata.getName();
    }
    
    public String toString() {
      return metadata.getLocalizedName();
    }
    
    public String getValueAt(int column) {
      return column == SPARE_COLUMN ? getLocalizedDescription() : super.getValueAt(column);
    }
  }
  
  /** *************************************************************************
   * Extension Node User Information
   */
  private class ExtensionInfo extends AbstractInfo {
    
    private boolean active;
    private ModuleInfo moduleInfo;
    private ExtensionMetaData metadata;
    
    public ExtensionInfo(File file, boolean active, ModuleInfo module) {
      super(file, active ? activeExtensionIcon : inactiveExtensionIcon);
      this.active = active;
      moduleInfo = module;
      AbstractMetaData data = AbstractMetaData.buildMetaData(file);
      if (data != null && data instanceof ExtensionMetaData) {
        setValid(true);
        metadata = (ExtensionMetaData) data;
      }
      else {
        setValid(false);
      }
    }
    
    public boolean isActive() {
      return active;
    }
    
    public void setActive(boolean b) {
      active = b;
      setIcon(active ? activeExtensionIcon : inactiveExtensionIcon);
    }
    
    public String getVersion() {
      return metadata == null ? "" : metadata.getVersion();
    }
    
    public String getDescription() {
      return metadata == null ? "" : metadata.getDescription();
    }
    
    public ExtensionsManager getExtensionsManager() {
      return moduleInfo == null ? null : moduleInfo.getExtensionsManager();
    }

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
    
    public JPopupMenu buildPopup(int row) {
      final JPopupMenu m = new JPopupMenu();
      m.add(new ActivateExtensionAction(Resources.getString(isActive() ?
            "ModuleManager.deactivate" : "ModuleManager.activate")));

      m.add(new EditExtensionLaunchAction(
        ModuleManagerWindow.this, getFile(), getSelectedModule()));
      return m;
    }
    
    public Color getTreeCellFgColor() {
      // FIXME: should get colors from LAF
      if (isActive()) {
        return metadata == null ? Color.red : Color.black;
      }
      else {
        return metadata == null ? Color.pink : Color.gray;
      }
    }
    
    public String getValueAt(int column) {
      return column == SPARE_COLUMN ? getDescription() : super.getValueAt(column);
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

      public ActivateExtensionAction (String s) {
        super(s);
        setEnabled(!isInUse() && ! moduleInfo.isInUse());
      }
      
      public void actionPerformed(ActionEvent evt) {
        setFile(getExtensionsManager().setActive(getFile(), !isActive()));          
        setActive(getExtensionsManager().isExtensionActive(getFile()));
        final TreePath path = tree.getPathForRow(tree.getSelectedRow());
        final MyTreeNode extNode = (MyTreeNode) path.getLastPathComponent();
        treeModel.setValueAt("", extNode, 0);
      }
    }
  }
  
  /** *************************************************************************
   * Saved Game Folder Node User Information
   */
  private class GameFolderInfo extends AbstractInfo {
    protected String comment;
    protected ModuleInfo moduleInfo;
    
    public GameFolderInfo(File f, ModuleInfo m) {
      super(f, openGameFolderIcon, closedGameFolderIcon);
      moduleInfo = m;
    }
    
    public JPopupMenu buildPopup(int row) {
      final JPopupMenu m = new JPopupMenu();
      m.add(new AbstractAction(Resources.getString("General.refresh")) {
        private static final long serialVersionUID = 1L;
        
        public void actionPerformed(ActionEvent e) {
          refresh();
        }        
      });
      m.addSeparator();
      m.add(new AbstractAction(Resources.getString("General.remove")) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          final MyTreeNode moduleNode = rootNode.findNode(moduleInfo.getFile());
          final MyTreeNode folderNode = moduleNode.findNode(getFile());
          treeModel.removeNodeFromParent(folderNode);
          moduleInfo.removeFolder(getFile());
          updateModuleList();
        }
      });

      return m;
    }
    
    public ModuleInfo getModuleInfo() {
      return moduleInfo;
    }
    
    public void refresh() {
      MyTreeNode moduleNode = rootNode.findNode(moduleInfo.getFile());
      MyTreeNode folderNode = moduleNode.findNode(getFile());
      treeModel.removeNodeFromParent(folderNode);
      moduleInfo.removeFolder(getFile());
      moduleInfo.addFolder(getFile());
    }
    
    /*
     * Force Game Folders to sort after extensions
     */
    public String getSortKey() {
      return "3";
    }
  }
  
  /** *************************************************************************
   * Saved Game File Node User Information
   */
  private class SaveFileInfo extends AbstractInfo {

    protected GameFolderInfo folderInfo;  // Owning Folder
    protected SaveMetaData metadata;      // Save file metadata
    protected ModuleMetaData moduleMetadata; // Module metadata
    
    public SaveFileInfo(File f, GameFolderInfo folder) {
      super(f, fileIcon);  
      folderInfo = folder;
      AbstractMetaData data = AbstractMetaData.buildMetaData(file);
      if (data != null && data instanceof SaveMetaData) {
        metadata = (SaveMetaData) data;
        setValid(true);
      }
      else {
        setValid(false);
      }
    }
    
    public JPopupMenu buildPopup(int row) {
      final JPopupMenu m = new JPopupMenu();
      m.add(new Player.LaunchAction(
        ModuleManagerWindow.this, getModuleFile(), file));
      return m;
    }
    
    protected File getModuleFile() {
      return folderInfo.getModuleInfo().getFile();
    }
    
    public void play() {
      new Player.LaunchAction(
        ModuleManagerWindow.this, getModuleFile(), file).actionPerformed(null);
    } 
    
    public String getValueAt(int column) {
      return column == SPARE_COLUMN ? buildComments() : super.getValueAt(column);
    }
    
    private String buildComments() {
      String comments = "";
      if (!belongsToModule()) {
        if (metadata.getModuleName().length() > 0) {
          comments = "[" + metadata.getModuleName() +  "] ";
        }
      }
      comments += metadata.getDescription();
      return comments;
    }
    
    private boolean belongsToModule() {
      return metadata.getModuleName().length() == 0 || folderInfo.getModuleInfo().getModuleName().equals(metadata.getModuleName());
    }
    
    public Color getTreeCellFgColor() {
      // FIXME: should get colors from LAF
       return belongsToModule() ? Color.black : Color.gray;
     }
    
    public String getVersion() {
      return metadata.getModuleVersion();
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
      if (editing.contains(lr.module)) return;
      Integer count = using.get(lr.module);
      using.put(lr.module, count == null ? 1 : ++count);

      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count
          Integer count = using.get(mod);
          if (count == 1) using.remove(mod);
          else using.put(mod, --count);
        }

/*
        @Override
        protected void process(List<Void> chunks) {
          super.process(chunks);
          ((ModuleManagerWindow) frame).addModule(mod);
        }
*/
      };
    }
  }

  /** *************************************************************************
   * Action to Edit an Extension in another process
   */
  private class EditExtensionLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public EditExtensionLaunchAction(Frame frame, File extension, File module) {
      super(Resources.getString("Editor.edit_extension"), frame,
        Editor.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.EDIT_EXT, module, extension)
      );

      setEnabled(!using.containsKey(module) &&
                 !editing.contains(module) &&
                 !editing.contains(extension) &&
                 !using.containsKey(extension));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // check that neither this module nor this extension is being edited
      if (editing.contains(lr.module) || editing.contains(lr.extension)) return;

      // register that this module is being used
      Integer count = using.get(lr.module);
      using.put(lr.module, count == null ? 1 : ++count);

      // register that this extension is being edited 
      editing.add(lr.extension);

      super.actionPerformed(e);
      setEnabled(false);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count for module
          Integer count = using.get(mod);
          if (count == 1) using.remove(mod);
          else using.put(mod, --count);

          // reduce that this extension is done being edited
          editing.remove(lr.extension);
          setEnabled(true);
        }

/*
        @Override
        protected void process(List<Void> chunks) {
          super.process(chunks);
          ((ModuleManagerWindow) frame).addModule(mod);
        }
*/
      };
    } 
  }
 
}
