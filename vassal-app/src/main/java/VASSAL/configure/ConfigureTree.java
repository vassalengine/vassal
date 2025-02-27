/*
 *
 * Copyright (c) 2000-2022 by Rodney Kinney, The Vassal Development Team
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
package VASSAL.configure;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GlobalKeyCommand;
import VASSAL.build.module.KeyNamer;
import VASSAL.build.module.Plugin;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.folder.GlobalPropertyFolder;
import VASSAL.build.module.folder.PrototypeFolder;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.DeckGlobalKeyCommand;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.properties.ChangePropertyButton;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.build.module.properties.GlobalTranslatableMessage;
import VASSAL.build.module.properties.ZoneProperty;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.MassPieceLoader;
import VASSAL.counters.Properties;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateAction;
import VASSAL.launch.EditorWindow;
import VASSAL.preferences.Prefs;
import VASSAL.search.SearchTarget;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ReflectionUtils;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.DropMode;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.awt.Component;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.IntStream;

import static VASSAL.build.GameModule.MODULE_NAME_PROPERTY;
import static VASSAL.build.GameModule.DESCRIPTION;
import static VASSAL.build.GameModule.MODULE_DESCRIPTION_PROPERTY;
import static VASSAL.build.GameModule.MODULE_OTHER1_PROPERTY;
import static VASSAL.build.GameModule.MODULE_OTHER2_PROPERTY;

import static java.util.regex.Pattern.CASE_INSENSITIVE;

/**
 * The beating heart of the Editor, this class handles the Configuration Tree
 * that appears in the Configuration window when editing a VASSAL module. Each
 * node in the tree structure is a {@link VASSAL.build.Configurable} object,
 * whose child nodes are obtained via {@link VASSAL.build.Configurable#getConfigureComponents}.
 * When we're running as the Extension Editor, this is subclassed by {@link ExtensionTree}, which
 * overrides some methods to handle extension-specific differences.
 */
public class ConfigureTree extends JTree implements PropertyChangeListener, MouseListener, MouseMotionListener, TreeSelectionListener, TreeExpansionListener {
  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(ConfigureTree.class);

  protected Map<Configurable, DefaultMutableTreeNode> nodes = new HashMap<>();
  protected DefaultMutableTreeNode copyData;
  protected DefaultMutableTreeNode cutData;
  protected HelpWindow helpWindow;
  protected EditorWindow editorWindow;
  protected Configurable selected;
  protected int selectedRow;
  protected String searchCmd;
  protected String moveCmd;
  protected String deleteCmd;
  protected String pasteCmd;
  protected String copyCmd;
  protected String cutCmd;
  protected String helpCmd;
  protected String propertiesCmd;
  protected String translateCmd;
  protected String duplicateCmd;
  protected KeyStroke cutKey;
  protected KeyStroke copyKey;
  protected KeyStroke pasteKey;
  protected KeyStroke deleteKey;
  protected KeyStroke moveKey;
  protected KeyStroke searchKey;
  protected KeyStroke helpKey;
  protected KeyStroke propertiesKey;
  protected KeyStroke translateKey;
  protected KeyStroke duplicateKey;
  protected Action cutAction;
  protected Action copyAction;
  protected Action pasteAction;
  protected Action deleteAction;
  protected Action moveAction;
  protected Action searchAction;
  protected Action propertiesAction;
  protected Action translateAction;
  protected Action helpAction;
  protected Action duplicateAction;

  protected JDialog searchDialog;
  protected JTextField searchField;
  protected JRadioButton searchFiltered;

  protected JCheckBox searchAdvanced;

  private final SearchParameters searchParameters;
  protected static Chatter chatter;

  @Deprecated(since = "2022-08-08", forRemoval = true)
  public static final Font POPUP_MENU_FONT = new Font(Font.DIALOG, Font.PLAIN, 11);
  protected static final List<AdditionalComponent> additionalComponents = new ArrayList<>();

  // Internal names of module classes; used where system literals are required rather than translate codes (shown in comment)
  private static final String CLASS_MODULE = "Module";        // Editor.GameModule.component_type
  private static final String CLASS_HELP_MENU = "Help Menu";  // Editor.Documentation.component_type

  /**
   * Creates new ConfigureTree
   */
  public ConfigureTree(Configurable root, HelpWindow helpWindow) {
    this(root, helpWindow, null, false);
  }

  public ConfigureTree(Configurable root, HelpWindow helpWindow, EditorWindow editorWindow) {
    this(root, helpWindow, editorWindow, false);
  }

  public ConfigureTree(Configurable root, HelpWindow helpWindow, EditorWindow editorWindow, boolean disableDragAndDrop) {
    toggleClickCount = 3;
    this.helpWindow = helpWindow;
    this.editorWindow = editorWindow;
    setShowsRootHandles(true);
    setModel(new DefaultTreeModel(buildTreeNode(root)));
    setCellRenderer(buildRenderer());
    addMouseListener(this);
    addMouseMotionListener(this);
    addTreeSelectionListener(this);
    addTreeExpansionListener(this);
    searchCmd = Resources.getString("Editor.search"); //$NON-NLS-1$
    moveCmd = Resources.getString("Editor.move"); //$NON-NLS-1$
    deleteCmd = Resources.getString("Editor.delete"); //$NON-NLS-1$
    pasteCmd = Resources.getString("Editor.paste"); //$NON-NLS-1$
    copyCmd = Resources.getString("Editor.copy"); //$NON-NLS-1$
    cutCmd = Resources.getString("Editor.cut"); //$NON-NLS-1$
    propertiesCmd = Resources.getString("Editor.properties"); //$NON-NLS-1$
    translateCmd = Resources.getString("Editor.ModuleEditor.translate"); //$NON-NLS-1$
    helpCmd = Resources.getString("Editor.ModuleEditor.component_help"); //$NON-NLS-1$
    duplicateCmd = Resources.getString("Editor.duplicate");
    final int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx();
    cutKey = KeyStroke.getKeyStroke(KeyEvent.VK_X, mask);
    copyKey = KeyStroke.getKeyStroke(KeyEvent.VK_C, mask);
    pasteKey = KeyStroke.getKeyStroke(KeyEvent.VK_V, mask);

    // Workaround to https://github.com/vassalengine/vassal/issues/11559
    // - make the Edit Delete shortcut a modified key on Mac.
    // FIXME: A neater solution would be if the Delete key could be directed to what should be the active, foreground window - i.e. the File Dialog (if open)
    // That is the expected behaviour, as seen on Windows but not on MacOS, where the Editor intercepts deleteKey (and others) regardless
    deleteKey = KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, SystemUtils.IS_OS_MAC ? InputEvent.META_DOWN_MASK : 0);

    moveKey = KeyStroke.getKeyStroke(KeyEvent.VK_M, mask);
    searchKey = KeyStroke.getKeyStroke(KeyEvent.VK_F, mask);
    propertiesKey = KeyStroke.getKeyStroke(KeyEvent.VK_P, mask);
    translateKey = KeyStroke.getKeyStroke(KeyEvent.VK_T, mask);
    helpKey = KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0);
    duplicateKey = KeyStroke.getKeyStroke(KeyEvent.VK_D, mask);
    copyAction = new KeyAction(copyCmd, copyKey);
    pasteAction = new KeyAction(pasteCmd, pasteKey);
    cutAction = new KeyAction(cutCmd, cutKey);
    deleteAction = new KeyAction(deleteCmd, deleteKey);
    moveAction = new KeyAction(moveCmd, moveKey);
    searchAction = new KeyAction(searchCmd, searchKey);
    propertiesAction = new KeyAction(propertiesCmd, propertiesKey);
    translateAction = new KeyAction(translateCmd, translateKey);
    helpAction = new KeyAction(helpCmd, helpKey);
    duplicateAction = new KeyAction(duplicateCmd, duplicateKey);
    /*
     * Cut, Copy and Paste will not work unless I add them to the JTree input and action maps. Why??? All the others
     * work fine.
     */
    getInputMap().put(cutKey, cutCmd);
    getInputMap().put(copyKey, copyCmd);
    getInputMap().put(pasteKey, pasteCmd);
    getInputMap().put(deleteKey, deleteCmd);
    getInputMap().put(duplicateKey, duplicateCmd);
    getActionMap().put(cutCmd, cutAction);
    getActionMap().put(copyCmd, copyAction);
    getActionMap().put(pasteCmd, pasteAction);
    getActionMap().put(deleteCmd, deleteAction);
    getActionMap().put(duplicateCmd, duplicateAction);
    this.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

    searchParameters = new SearchParameters();

    final TreePath path = new TreePath(((DefaultMutableTreeNode) (getModel().getRoot())).getPath());
    setSelectionPath(path);
    scrollPathToVisible(path);

    chatter = GameModule.getGameModule().getChatter();

    createKeyBindings();

    if (!disableDragAndDrop) {
      setDragEnabled(true);
      setDropMode(DropMode.ON_OR_INSERT);
      setTransferHandler(new TreeTransferHandler());
    }
  }

  public static String noHTML(String text) {
    return text.replaceAll("<", "&lt;")  //NON-NLS // This prevents any unwanted tag from functioning
               .replaceAll(">", "&gt;"); //NON-NLS // This makes sure > doesn't break any of our legit <div> tags
  }

  protected static void chat(String text) {
    if (chatter != null) {
      chatter.show("- " + text);
    }
  }


  protected JDialog getSearchDialog() {
    return searchDialog;
  }

  protected void setSearchDialog(JDialog searchDialog) {
    this.searchDialog = searchDialog;
  }

  protected JTextField getSearchField() {
    return searchField;
  }

  protected void setSearchField(JTextField searchField) {
    this.searchField = searchField;
  }

  protected void setSearchAdvanced(JRadioButton searchFiltered) {
    this.searchFiltered = searchFiltered;
  }

  // FIXME: Attempting to remove these now unused items yields build errors.
  protected void setSearchAdvanced(JCheckBox searchAdvanced) {
  }

  protected JCheckBox getSearchAdvanced() {
    return searchAdvanced;
  }


  public JFrame getFrame() {
    return editorWindow;
  }

  /**
   * Create a key binding for ENTER key to do meaningful things.
   */
  private void createKeyBindings() {
    getInputMap(WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "Enter");
    getActionMap().put("Enter", new AbstractAction() {
      @Override
      public void actionPerformed(ActionEvent ae) {
        // Do something meaningful when Enter key pressed
        final TreePath path = getSelectionPath();
        if (path == null) { //BR// Apparently this can happen.
          return;
        }
        final DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
        if (isExpanded(path) || (node.getChildCount() == 0)) {
          final Configurable target = (Configurable) ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
          if ((target != null) && (target.getConfigurer() != null)) {
            final Action a = buildEditAction(target);
            if (a != null) {
              a.actionPerformed(new ActionEvent(ae.getSource(), ActionEvent.ACTION_PERFORMED, "Edit")); //NON-NLS
            }
          }
        }
        else {
          setExpandedState(path, true);
        }
      }
    });
  }

  @Override
  public void treeExpanded(TreeExpansionEvent event) {

  }

  /**
   * A cell has been collapsed. Reset the edit flag on all the children owned by this node
   * @param event Expansion event
   */
  @Override
  public void treeCollapsed(TreeExpansionEvent event) {
    final ConfigureTreeNode node = (ConfigureTreeNode) event.getPath().getLastPathComponent();
    node.resetChildEditFlags();
  }


  class KeyAction extends AbstractAction {
    private static final long serialVersionUID = 1L;
    protected String actionName;

    public KeyAction(String name, KeyStroke key) {
      super(name);
      actionName = name;
      putValue(ACCELERATOR_KEY, key);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      doKeyAction(actionName);
    }
  }

  protected Renderer buildRenderer() {
    return new Renderer();
  }

  /**
   * Tell our enclosing EditorWindow that we are now clean
   * or dirty.
   *
   * @param changed true = state is not dirty
   */
  protected void notifyStateChanged(boolean changed) {
    if (editorWindow != null) {
      editorWindow.treeStateChanged(changed);
    }
  }

  protected Configurable getTarget(int x, int y) {
    final TreePath path = getPathForLocation(x, y);
    Configurable target = null;
    if (path != null) {
      target = (Configurable) ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
    }
    return target;
  }

  protected DefaultMutableTreeNode buildTreeNode(Configurable c) {
    c.addPropertyChangeListener(this);
    final DefaultMutableTreeNode node = new ConfigureTreeNode(c);
    final Configurable[] children = c.getConfigureComponents();
    for (final Configurable child : children) {
      if (! (child instanceof Plugin)) { // Hide Plug-ins
        node.add(buildTreeNode(child));
      }
    }
    nodes.put(c, node);
    return node;
  }

  protected void addAction(JPopupMenu menu, Action a) {
    if (a != null) {
      menu.add(a);
    }
  }

  protected void addSubMenu(JPopupMenu menu, String name, List<Action> l) {
    if ((l != null) && !l.isEmpty()) {
      final JMenu subMenu = new JMenu(name);
      for (final Action a: l) {
        subMenu.add(a);
      }
      menu.add(subMenu);
      l.clear();
    }
  }


  private void addActionGroup(JPopupMenu menu, List<Action> l) {
    boolean empty = true;
    for (final Action a : l) {
      if (a != null) {
        menu.add(a);
        empty = false;
      }
    }
    if (!empty) {
      menu.addSeparator();
    }
    l.clear();
  }

  protected JPopupMenu buildPopupMenu(final Configurable target) {
    final JPopupMenu popup = new JPopupMenu();
    final List<Action> l = new ArrayList<>();
    l.add(buildEditAction(target));
    addActionGroup(popup, l);
    l.add(buildTranslateAction(target));
    addActionGroup(popup, l);
    l.add(buildHelpAction(target));
    addActionGroup(popup, l);
    l.add(buildSearchAction(target));
    addActionGroup(popup, l);
    l.add(buildDeleteAction(target));
    l.add(buildCutAction(target));
    l.add(buildCopyAction(target));
    l.add(buildPasteAction(target));
    l.add(buildMoveAction(target));

    addActionGroup(popup, l);
    final List<Action> inserts = new ArrayList<>();
    final List<Action> adds = buildAddActionsFor(target, inserts);
    for (final Action a : adds) {
      addAction(popup, a);
    }
    addSubMenu(popup, Resources.getString("Editor.ConfigureTree.insert"), inserts);
    if (hasChild(target, PieceSlot.class) || hasChild(target, CardSlot.class)) {
      addAction(popup, buildMassPieceLoaderAction(target));
    }

    addAction(popup, buildImportAction(target));

    final boolean canExport = getTreeNode(target).getParent() != null;
    final boolean canImport = (target.getAllowableConfigureComponents().length > 0);

    if (canImport || canExport || (target instanceof DrawPile)) {
      popup.addSeparator();
    }

    if (canExport) {
      addAction(popup, buildExportTreeAction(target));
    }

    if (canImport) {
      addAction(popup, buildImportTreeAction(target));
    }

    if (target instanceof DrawPile) {
      addAction(popup, buildImportDeckAction(target));
    }

    final Action aOpen = buildOpenPiecesAction(target);
    final Action aEdit = buildEditPiecesAction(target);

    if ((aOpen != null) || (aEdit != null)) {
      popup.addSeparator();
    }

    addAction(popup, aOpen);
    addAction(popup, aEdit);

    return popup;
  }

  public static final String defaultExportExtension = ".xml"; //NON-NLS

  protected boolean exportTreeBranch(AbstractBuildable target) {
    final FileChooser fc = FileChooser.createFileChooser(
      GameModule.getGameModule().getPlayerWindow(),
      (DirectoryConfigurer) Prefs.getGlobalPrefs()
        .getOption(Prefs.MODULES_DIR_KEY));
    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) {
      return false;
    }
    String filename = fc.getSelectedFile().getPath();

    if (!StringUtils.isEmpty(defaultExportExtension) && (filename.lastIndexOf('.') < 0)) {
      filename = filename + defaultExportExtension;
      if (new File(filename).exists() && JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(GameModule.getGameModule().getPlayerWindow(), Resources.getString("Editor.ConfigureTree.export_overwrite", filename), Resources.getString("Editor.ConfigureTree.export_exists"), JOptionPane.YES_NO_OPTION)) {
        return false;
      }
    }

    try (Writer w = Files.newBufferedWriter(Path.of(filename), StandardCharsets.UTF_8)) {
      w.write(target.buildString());
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, e, filename);
      return false;
    }

    return true;
  }

  protected boolean importTreeBranch(Configurable target) {

    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) {
      return false;
    }

    String filename = fc.getSelectedFile().getPath();

    if (fc.getSelectedFile().getName().indexOf('.') < 0) {
      filename += defaultExportExtension;
    }

    try {
      Buildable b = Builder.create(DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new File(filename)).getDocumentElement(), target);

      if (b instanceof Configurable) {
        b = convertChild(target, (Configurable)b);
      }
      else {
        GameModule.getGameModule().warn(Resources.getString("Editor.ConfigureTree.import_invalid_file"));
        ErrorDialog.show("Error.import_invalid_file", b.toString());
        return false;
      }

      boolean allowed = false;
      for (final Class c : target.getAllowableConfigureComponents()) {
        if (c.isInstance(b)) {
          allowed = true;
          break;
        }
      }

      if (allowed) {
        insert(target, (Configurable)b, getTreeNode(target).getChildCount());
      }
      else {
        GameModule.getGameModule().warn(Resources.getString("Editor.ConfigureTree.import_not_allowed", b.toString()));
        ErrorDialog.show("Error.import_not_allowed", b.toString());
        return false;
      }
    }
    catch (IllegalBuildException | SAXException e) {
      GameModule.getGameModule().warn(Resources.getString("Editor.ConfigureTree.import_failed_message", filename, target.getConfigureName()));
      ErrorDialog.show("Editor.ConfigureTree.import_failed", filename, target.getConfigureName());
      return false;
    }
    catch (ParserConfigurationException e) {
      ErrorDialog.bug(e);
      return false;
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, filename);
      return false;
    }

    GameModule.getGameModule().warn(Resources.getString("Editor.ConfigureTree.import_successful"));
    return true;
  }

  /**
   * Enumerates our configure tree in preparation for searching it
   * @param root - root of our module's tree.
   * @return a list of search nodes
   */
  private List<DefaultMutableTreeNode> getSearchNodes(DefaultMutableTreeNode root) {
    final List<DefaultMutableTreeNode> searchNodes = new ArrayList<>();

    final Enumeration<?> e = root.preorderEnumeration();
    while (e.hasMoreElements()) {
      searchNodes.add((DefaultMutableTreeNode)e.nextElement());
    }
    return searchNodes;
  }

  private static int getBookmark(List<DefaultMutableTreeNode> searchNodes, DefaultMutableTreeNode targetNode) {
    return IntStream
            .range(0, searchNodes.size())
            .filter(i -> searchNodes.get(i) == targetNode)
            .findFirst()
            .orElse(-1);
  }


  private void notifyUpdate(final Configurable target) {
    if (editorWindow != null) {
      if (target instanceof AbstractConfigurable) {
        if (editorWindow.getListKeyCommands() != null) {
          editorWindow.getListKeyCommands().updateConfigurable((AbstractConfigurable)target);
        }
      }
    }
  }

  private void notifyDelete(final Configurable target) {
    if (editorWindow != null) {
      if (target instanceof AbstractConfigurable) {
        if (editorWindow.getListKeyCommands() != null) {
          editorWindow.getListKeyCommands().deleteConfigurable((AbstractConfigurable)target);
        }
      }
    }
  }

  /**
   * @return Search action - runs search dialog box, then searches
   */
  protected Action buildSearchAction(final Configurable target) {
    final Action a = new SearchAction(this, searchParameters);
    a.setEnabled(true);
    return a;
  }

  protected Action buildExportTreeAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction(Resources.getString("Editor.ConfigureTree.export_object")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          if (target instanceof AbstractBuildable) {
            exportTreeBranch((AbstractBuildable) target);
          }
        }
      };
    }
    return a;
  }


  protected Action buildImportTreeAction(final Configurable target) {
    return new AbstractAction(Resources.getString("Editor.ConfigureTree.import_object")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        importTreeBranch(target);
      }
    };
  }


  protected Action buildMoveAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction(moveCmd) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), true);
          d.setTitle(target.getConfigureName() == null ? moveCmd : moveCmd + " " + target.getConfigureName());
          d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
          final Box box = Box.createHorizontalBox();
          box.add(new JLabel(Resources.getString("Editor.ConfigureTree.move_to_position")));
          box.add(Box.createHorizontalStrut(10));
          final JComboBox<String> select = new JComboBox<>();
          final TreeNode parentNode = getTreeNode(target).getParent();
          for (int i = 0; i < parentNode.getChildCount(); ++i) {
            final Configurable c = (Configurable) ((DefaultMutableTreeNode) parentNode.getChildAt(i)).getUserObject();
            final String name = (c.getConfigureName() != null ? c.getConfigureName() : "") + " [" + getConfigureName(c.getClass()) + "]";
            select.addItem((i + 1) + ":  " + name);
          }
          final DefaultMutableTreeNode targetNode = getTreeNode(target);
          final int currentIndex = targetNode.getParent().getIndex(targetNode);
          select.setSelectedIndex(currentIndex);
          box.add(select);
          final JButton ok = new JButton(Resources.getString(Resources.OK));
          ok.addActionListener(e1 -> {
            final int index = select.getSelectedIndex();
            if (currentIndex != index) {
              final Configurable parent = getParent(targetNode);
              if (remove(parent, target)) {
                insert(parent, target, index);
              }
            }
            d.dispose();
          });
          d.add(box);
          d.add(ok);

          // Default actions for Enter/Esc
          SwingUtils.setDefaultButtons(d.getRootPane(), ok, ok); // ESC also does OK

          SwingUtils.repack(d);
          d.setLocationRelativeTo(d.getParent());
          d.setVisible(true);
        }
      };
      a.setEnabled(isMoveAllowed(target));
    }
    return a;
  }

  protected Action buildCutAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction(cutCmd) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          cutData = getTreeNode(target);
          copyData = null;
          updateEditMenu();
        }
      };
      a.setEnabled(isDeleteAllowed(target));
    }
    return a;
  }

  protected Action buildCopyAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction(copyCmd) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          copyData = getTreeNode(target);
          cutData = null;
          updateEditMenu();
        }
      };
      a.setEnabled(true);
    }
    return a;
  }


  /**
   * If there are items that need to be adjusted after being pasted (to make sure they don't contain illegal values that will be hard to debug later),
   * we can make them here.
   * @param target Item we just pasted
   */
  protected void postPasteFixups(final Configurable target) {
    // SetupStacks (and thus DrawPiles) pasted to a new map, but whose owning board setting doesn't exist for this map, are forced to "any"
    if (target instanceof SetupStack) {
      final SetupStack ss = (SetupStack)target;
      final String owning = ss.getOwningBoardName();
      if (owning != null) {
        if (!ss.getValidOwningBoards().contains(owning)) {
          chat(Resources.getString("Editor.convert_setupstack_or_deck", (target instanceof DrawPile) ? DrawPile.getConfigureTypeName() : SetupStack.getConfigureTypeName(), ss.getConfigureName(), owning));
          ss.setOwningBoardName(null);
        }
      }
    }

    // PrototypeFolder needs any prototype children added to the main prototype definition list
    if (target instanceof PrototypeFolder) {
      final PrototypeFolder folder = (PrototypeFolder)target;
      final Buildable ancestor = folder.getNonFolderAncestor();
      if (ancestor instanceof PrototypesContainer) {
        final PrototypesContainer protos = (PrototypesContainer)ancestor;
        for (final PrototypeDefinition child : folder.getAllDescendantComponentsOf(PrototypeDefinition.class)) {
          protos.addDefinition(child);
        }
      }
    }

    // Global properties need some extra validation onto their parents, because the new ones get built w/o full knowledge of ancestors
    if (target instanceof GlobalPropertyFolder) {
      for (final GlobalProperty child : ((GlobalPropertyFolder) target).getAllDescendantComponentsOf(GlobalProperty.class)) {
        child.addTo(child.getAncestor());
      }
    }

    // CounterDetailViewers in folders may need to be "acquainted" with their new maps.
    if (target instanceof CounterDetailViewer) {
      final CounterDetailViewer cdv = (CounterDetailViewer) target;
      if (cdv.getMap() == null) {
        cdv.addTo(cdv.getAncestor());
      }
    }
  }

  protected Action buildPasteAction(final Configurable target) {
    final Action a = new AbstractAction(pasteCmd) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        if (cutData != null) {
          final DefaultMutableTreeNode targetNode = getTreeNode(target);
          if (targetNode.isNodeAncestor(cutData)) {
            chat(Resources.getString("Editor.cant_cut_ancestor_to_child"));
            return;
          }
          final Configurable cutObj = (Configurable) cutData.getUserObject();
          final Configurable convertedCutObj = convertChild(target, cutObj);
          if ((getParent(cutData) == null) || remove(getParent(cutData), cutObj)) {
            insert(target, convertedCutObj, targetNode.getChildCount());
            postPasteFixups(convertedCutObj);
          }
          copyData = getTreeNode(convertedCutObj);
        }
        else if (copyData != null) {
          final Configurable copyBase = (Configurable) copyData.getUserObject();
          Configurable clone = null;
          try {
            clone = convertChild(target, copyBase.getClass().getConstructor().newInstance());
          }
          catch (Throwable t) {
            ReflectionUtils.handleNewInstanceFailure(t, copyBase.getClass());
          }

          if (clone != null) {
            clone.build(copyBase.getBuildElement(Builder.createNewDocument()));
            insert(target, clone, getTreeNode(target).getChildCount());
            updateGpIds(clone);
            postPasteFixups(clone);
          }
        }
        cutData = null;
        updateEditMenu();
      }
    };
    a.setEnabled(isValidPasteTarget(target));
    return a;
  }


  protected boolean isValidPasteTarget(Configurable target, DefaultMutableTreeNode sourceNode) {
    if (sourceNode == null) {
      return false;
    }

    final Configurable pasteComponent = (Configurable) sourceNode.getUserObject();

    // Do not allow Immobile components to be moved
    if (!pasteComponent.isMovable()) {
      return false;
    }

    // Do not allow a Unique component to be pasted into a node that already has an instance of that component
    if (pasteComponent.isUnique() && target instanceof AbstractBuildable) {
      if (!((AbstractBuildable) target).getComponentsOf(pasteComponent.getClass()).isEmpty()) {
        return false;
      }
    }

    // We can always be dragged/pasted onto our own parent.
    final DefaultMutableTreeNode parent = (DefaultMutableTreeNode) sourceNode.getParent();
    return (parent != null && parent.getUserObject().equals(target)) ||
      isValidParent(target, (Configurable) sourceNode.getUserObject());
  }

  protected boolean isValidPasteTarget(Configurable target) {
    return isValidPasteTarget(target, cutData) || isValidPasteTarget(target, copyData);
  }

  /**
   * Some components need to be converted to a new type before insertion.
   *
   * Currently, this is used to allow cut and paste of CardSlots and PieceSlots
   * between Decks and GamePiece Palette components.
   *
   * @param parent Parent Configurable
   * @param child Child Configurable
   * @return new Child
   */
  protected Configurable convertChild(Configurable parent, Configurable child) {
    // Convert between CardSlots and PieceSlots
    if (child.getClass() == PieceSlot.class && isAllowedChildClass(parent, CardSlot.class)) {
      return new CardSlot((PieceSlot) child);
    }
    else if (child.getClass() == CardSlot.class && isAllowedChildClass(parent, PieceSlot.class)) {
      return new PieceSlot((CardSlot) child);
    }
    // Handle the conversion between zone properties & global/map properties
    else if (child.getClass() == ZoneProperty.class && isAllowedChildClass(parent, GlobalProperty.class)) {
      return new GlobalProperty((GlobalProperty) child);
    }
    else if (child.getClass() == GlobalProperty.class && isAllowedChildClass(parent, ZoneProperty.class)) {
      return new ZoneProperty((GlobalProperty) child);
    }
    // Handle conversion between different types of GKCs. Note startup GKC's cannot be created this way.
    // Any non-Startup GKC pasted at the module level will be transformed to a GlobalKeyCommand
    // Startup GKCs will always be pasted as themselves at the module level
    // The order of this if is important as all GKCs subclass MassKeyCommand
    else if (MassKeyCommand.class.isAssignableFrom(child.getClass())) {
      if (isAllowedChildClass(parent, DeckGlobalKeyCommand.class)) {
        // Convert other types to Deck GKC. Do not convert an existing Deck GKC
        return child instanceof DeckGlobalKeyCommand ? child : new DeckGlobalKeyCommand((MassKeyCommand) child);
      }
      else if (isAllowedChildClass(parent, GlobalKeyCommand.class)) {
        // Convert Mass GKC and Deck GKC to Global. Do not convert an existing Global GKC, or its subclass Startup GKC
        return child instanceof GlobalKeyCommand ? child : new GlobalKeyCommand((MassKeyCommand) child);
      }
      else if (isAllowedChildClass(parent, MassKeyCommand.class)) {
        // Convert other types to Mass GKX
        // Need to check MassKeyCommand class specifically as all other GKCs subclass it
        return child.getClass().equals(MassKeyCommand.class) ? child : new MassKeyCommand((MassKeyCommand) child);
      }
    }
    return child;
  }

  protected boolean isAllowedChildClass(Configurable parent, Class<?> childClass) {
    final Class<?>[] allowableClasses = parent.getAllowableConfigureComponents();
    for (final Class<?> allowableClass : allowableClasses) {
      if (allowableClass == childClass) {
        return true;
      }
    }
    return false;
  }

  /**
   * Allocate new PieceSlot Ids to any PieceSlot subcomponents
   *
   * @param c Configurable to update
   */
  public void updateGpIds(Configurable c) {
    if (c instanceof PieceSlot) {
      ((PieceSlot) c).updateGpId(GameModule.getGameModule());
    }
    else {
      for (final Configurable comp : c.getConfigureComponents()) updateGpIds(comp);
    }
  }

  protected class ImportAction extends AddAction {
    private static final long serialVersionUID = 1L;

    public ImportAction(Configurable target, String name) {
      super(target, null, name, -1, null);
    }

    @Override
    protected Configurable getChild() {
      return importConfigurable();
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
      final Configurable child = getChild();
      try {
        doIt(child);
      }
      // FIXME: review error message
      catch (Exception ex) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Error adding " + getConfigureName(child) + " to " + getConfigureName(target) + "\n" //NON-NLS
            + ex.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE); //NON-NLS
      }
    }
  }

  protected Action buildImportAction(Configurable target) {
    return new ImportAction(
      target,
      Resources.getString("Editor.ConfigureTree.add_imported_class")
    );
  }

  protected Action buildImportDeckAction(final Configurable target) {
    final ConfigureTree tree = this;
    return new AbstractAction(Resources.getString("Editor.ConfigureTree.import_deck_file")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        ((DrawPile)(target)).importDeck(tree);
      }
    };
  }

  protected Action buildMassPieceLoaderAction(final Configurable target) {
    Action a = null;
    final ConfigureTree tree = this;
    if (getTreeNode(target).getParent() != null) {
      Resources.getString("Editor.ConfigureTree.add_cards");
      final String desc = hasChild(target, CardSlot.class) ? Resources.getString("Editor.ConfigureTree.add_cards") : Resources.getString("Editor.ConfigureTree.add_pieces");
      a = new AbstractAction(desc) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          new MassPieceLoader(tree, target).load();
        }
      };
    }
    return a;
  }

  protected boolean hasChild(Configurable parent, Class<?> childClass) {
    for (final Class<?> c : parent.getAllowableConfigureComponents()) {
      if (c.equals(childClass)) {
        return true;
      }
    }
    return false;
  }

  protected List<Action> buildAddActionsFor(final Configurable target) {
    return buildAddActionsFor(target, null);
  }


  protected List<Action> buildAddActionsFor(final Configurable target, List<Action> peerInserts) {
    final List<Action> l = new ArrayList<>();

    if (target instanceof AbstractConfigurable) {
      final DefaultMutableTreeNode targetNode = getTreeNode(target);
      if (targetNode != null) {
        final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) targetNode.getParent();
        if (parentNode != null) {

          if (isDuplicateAllowed(target)) {
            l.add(buildAddAction((Configurable) parentNode.getUserObject(), target.getClass(), "Editor.ConfigureTree.add_duplicate", parentNode.getIndex(targetNode) + 1, target));
          }

          if (peerInserts != null) {
            final Configurable parent = ((Configurable)parentNode.getUserObject());
            for (final Class<? extends Buildable> newConfig : parent.getAllowableConfigureComponents()) {
              peerInserts.add(buildAddAction(parent, newConfig, "Editor.ConfigureTree.add_peer", parentNode.getIndex(targetNode), null));
            }
          }
        }
      }
    }

    for (final Class<? extends Buildable> newConfig :
      target.getAllowableConfigureComponents()) {
      l.add(buildAddAction(target, newConfig));
    }

    for (final AdditionalComponent add : additionalComponents) {
      if (target.getClass().equals(add.getParent())) {
        final Class<? extends Buildable> newConfig = add.getChild();
        l.add(buildAddAction(target, newConfig));
      }
    }

    return l;
  }

  protected Action buildAddAction(final Configurable target, final Class<? extends Buildable> newConfig) {
    return buildAddAction(target, newConfig, "Editor.ConfigureTree.add_component", -1, null);
  }

  protected class AddAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    protected final Configurable target;
    private final Class<? extends Buildable> newConfig;
    private final int index;
    private final Configurable duplicate;

    public AddAction(Configurable target, Class<? extends Buildable> newConfig, String name, int index, Configurable duplicate) {
      super(name);

      this.target = target;
      this.newConfig = newConfig;
      this.index = index;
      this.duplicate = duplicate;
    }

    protected Configurable getChild() {
      try {
        return (Configurable) newConfig.getConstructor().newInstance();
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, newConfig);
        return null;
      }
    }

    protected void doIt(Configurable child) {
      if (child == null) {
        return;
      }

      //BR// We do an early & extra set of the ancestor before build so that if, during the addTo() sequence,
      //BR// an item in an AbstractFolder needs to know its "first non-folder ancestor", it can walk up the
      //BR// tree as necessary.
      if (child instanceof AbstractBuildable) {
        ((AbstractBuildable) child).setAncestor(target);
      }

      child.build((duplicate != null) ? duplicate.getBuildElement(Builder.createNewDocument()) : null);

      if (child instanceof PieceSlot) {
        ((PieceSlot) child).updateGpId(GameModule.getGameModule());
      }

      final int finalIndex = (index < 0) ? getTreeNode(target).getChildCount() : checkMinimumIndex(getTreeNode(target), index);

      if (child.getConfigurer() != null) {
        if (insert(target, child, finalIndex)) {
          if (duplicate != null) {
            updateGpIds(child);
          }

          // expand the new node
          final TreePath path = new TreePath(getTreeNode(child).getPath());
          expandPath(path);

          final PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
            private static final long serialVersionUID = 1L;

// HERE
            @Override
            public void cancel() {
              // Child could have been already deleted or dragged elsewhere
              final DefaultMutableTreeNode currentParent = (DefaultMutableTreeNode)getTreeNode(child).getParent();
              if (currentParent != null) {
                ConfigureTree.this.delete(child);
              }
              dispose();
            }

            @Override
            public void save() {
              notifyUpdate(child);
              //BR// If we've just created a new duplicate and saved it, then select the duplicate rather than leaving the original selected
              if (duplicate != null) {
                final DefaultMutableTreeNode node = getTreeNode(child);
                if (node != null) {
                  final TreePath path = new TreePath(node.getPath());
                  setSelectionPath(path);
                  scrollPathToVisible(path);
                }
              }
              super.save();
            }
          };
          w.setVisible(true);
        }
      }
      else {
        insert(target, child, finalIndex);
        if (duplicate != null) {
          updateGpIds(child);
        }
      }
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
      doIt(getChild());
    }
  }

  protected Action buildAddAction(Configurable target, Class<? extends Buildable> newConfig, String key, int index, Configurable duplicate) {
    return new AddAction(
      target,
      newConfig,
      Resources.getString(key, getConfigureName(newConfig)),
      index,
      duplicate
    );
  }

  protected Action buildHelpAction(final Configurable target) {
    final Action showHelp;
    final HelpFile helpFile = target.getHelpFile();
    if (helpFile == null) {
      showHelp = new ShowHelpAction(null, null);
      showHelp.setEnabled(false);
    }
    else {
      showHelp = new ShowHelpAction(helpFile.getContents(), null);
    }
    return showHelp;
  }

  protected Action buildCloneAction(final Configurable target) {
    final DefaultMutableTreeNode targetNode = getTreeNode(target);
    if (targetNode.getParent() != null) {
      return new AbstractAction(Resources.getString("Editor.ConfigureTree.clone")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent evt) {
          Configurable clone = null;
          try {
            clone = target.getClass().getConstructor().newInstance();
          }
          catch (Throwable t) {
            ReflectionUtils.handleNewInstanceFailure(t, target.getClass());
          }

          if (clone != null) {
            clone.build(target.getBuildElement(Builder.createNewDocument()));
            insert(getParent(targetNode), clone,
              targetNode.getParent().getIndex(targetNode) + 1);
          }
        }
      };
    }
    else {
      return null;
    }
  }

  protected Configurable getParent(final DefaultMutableTreeNode targetNode) {
    final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) targetNode.getParent();
    return parentNode == null ? null : (Configurable) parentNode.getUserObject();
  }

  protected Action buildDeleteAction(final Configurable target) {
    final DefaultMutableTreeNode targetNode = getTreeNode(target);
    if (targetNode.getParent() != null) {
      final Action a = new AbstractAction(deleteCmd) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent evt) {
          final int row = selectedRow;
          delete(target);
          if (row < getRowCount()) {
            setSelectionRow(row);
          }
          else {
            setSelectionRow(row - 1);
          }
        }
      };
      a.setEnabled(isDeleteAllowed(target));
      return a;
    }
    else {
      return null;
    }
  }

  protected Action buildEditPiecesAction(final Configurable target) {
    if (canContainGamePiece(target)) {
      return new EditContainedPiecesAction(target, this);
    }
    else {
      return null;
    }
  }

  protected Action buildOpenPiecesAction(final Configurable target) {
    if (canContainGamePiece(target)) {
      return new OpenContainedPiecesAction(target, helpWindow, (Frame) SwingUtilities.getAncestorOfClass(Frame.class, this), this);
    }
    else {
      return null;
    }
  }

  protected Action buildEditAction(final Configurable target) {
    return new EditPropertiesAction(target, helpWindow, (Frame) SwingUtilities.getAncestorOfClass(Frame.class, this), this);
  }

  protected Action buildTranslateAction(final Configurable target) {
    final Action a = new TranslateAction(target, helpWindow, this);
    a.setEnabled(target.getI18nData().isTranslatable());
    return a;
  }

  public boolean canContainGamePiece(final Configurable target) {
    boolean canContainPiece = false;
    for (final Class<?> c : target.getAllowableConfigureComponents()) {
      if (PieceSlot.class.isAssignableFrom(c)) {
        canContainPiece = true;
        break;
      }
    }
    return canContainPiece;
  }

  /**
   * Delete removes an item from the tree but ALSO traverses the tree throwing all the children's children manually out
   * the airlock, one by one. Lest they return and live on as zombies...
   */
  protected boolean delete(Configurable target) {
    boolean result = true;

    final Enumeration<?> e = getTreeNode(target).postorderEnumeration();
    final List<DefaultMutableTreeNode> victims = new ArrayList<>();
    while (e.hasMoreElements()) {
      victims.add((DefaultMutableTreeNode) e.nextElement());
    }

    for (final DefaultMutableTreeNode victim : victims) {
      result &= remove((Configurable) ((DefaultMutableTreeNode)victim.getParent()).getUserObject(), (Configurable) victim.getUserObject());
    }
    return result;
  }

  protected boolean remove(Configurable parent, Configurable child) {
    try {
      child.removeFrom(parent);
      parent.remove(child);
      ((DefaultTreeModel) getModel()).removeNodeFromParent(getTreeNode(child));
      notifyDelete(child);
      notifyStateChanged(true);
      return true;
    }
    // FIXME: review error message
    catch (IllegalBuildException err) {
      JOptionPane.showMessageDialog(getTopLevelAncestor(), "Cannot delete " + getConfigureName(child) + " from " + getConfigureName(parent) + "\n" //NON-NLS
        + err.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE); //NON-NLS
      return false;
    }
  }

  protected boolean insert(Configurable parent, Configurable child, int index) {
    // Check if the child needs to be converted to a compatible type for this parent
    final Configurable theChild = convertChild(parent, child);

    final DefaultMutableTreeNode childNode = buildTreeNode(theChild);
    final DefaultMutableTreeNode parentNode = getTreeNode(parent);
    final Configurable[] oldContents = parent.getConfigureComponents();
    final List<Configurable> moveToBack = new ArrayList<>();
    for (int i = index; i < oldContents.length; ++i) {
      try {
        parent.remove(oldContents[i]);
      }
      // FIXME: review error message
      catch (IllegalBuildException err) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't insert " + getConfigureName(theChild) + " before " + getConfigureName(oldContents[i]), //NON-NLS
          "Illegal configuration", JOptionPane.ERROR_MESSAGE); //NON-NLS
        for (int j = index; j < i; ++j) {
          parent.add(oldContents[j]);
        }
        return false;
      }
      moveToBack.add(oldContents[i]);
    }

    boolean succeeded = true;
    try {
      theChild.addTo(parent);
      parent.add(theChild);

      // Update ancestor tree for child
      if (theChild instanceof AbstractBuildable) {
        ((AbstractBuildable)theChild).setAncestor(parent);
      }

      parentNode.insert(childNode, index);
      final int[] childI = new int[1];
      childI[0] = index;
      ((DefaultTreeModel) getModel()).nodesWereInserted(parentNode, childI);
    }
    // FIXME: review error message
    catch (IllegalBuildException err) {
      JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't add " + getConfigureName(child) + "\n" + err.getMessage(), "Illegal configuration", //NON-NLS
        JOptionPane.ERROR_MESSAGE);
      succeeded = false;
    }

    for (final Configurable c : moveToBack) {
      parent.add(c);
    }

    notifyStateChanged(true);
    notifyUpdate(child);
    return succeeded;
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    final DefaultMutableTreeNode newValue = getTreeNode((Configurable) evt.getSource());
    ((DefaultTreeModel) getModel()).nodeChanged(newValue);
  }

  /**
   * Custom Tree Cell Renderer
   * Change the font to italic if the Configurable held by the node for this cell has been edited.
   */
  static class Renderer extends javax.swing.tree.DefaultTreeCellRenderer {
    private static final long serialVersionUID = 1L;
    private Font plainFont;
    private Font italicFont;

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
      final JLabel label = (JLabel) super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
      if (plainFont == null) {
        plainFont = label.getFont();
      }
      if (value instanceof ConfigureTreeNode) {
        final ConfigureTreeNode c = (ConfigureTreeNode) value;
        if (c.isEdited()) {
          if (italicFont == null) {
            final Font f = label.getFont();
            italicFont = new Font(f.getFontName(), Font.ITALIC, f.getSize());
          }
          label.setFont(italicFont);
        }
        else {
          label.setFont(plainFont);
        }
      }
      return label;
    }
  }

  /**
   * Returns the name of the class for display purposes. Reflection is
   * used to call <code>getConfigureTypeName()</code>, which should be
   * a static method if it exists in the given class. (This is necessary
   * because static methods are not permitted in interfaces.)
   *
   * @param c the class whose configure name will be returned
   * @return the configure name of the class
   */
  public static String getConfigureName(Class<?> c) {
    try {
      return (String) c.getMethod("getConfigureTypeName").invoke(null);
    }
    catch (NoSuchMethodException e) {
      // Ignore. This is normal, since some classes won't have this method.
    }
    catch (IllegalAccessException | ExceptionInInitializerError
      | NullPointerException | InvocationTargetException | IllegalArgumentException e) {
      ErrorDialog.bug(e);
    }

    return c.getName().substring(c.getName().lastIndexOf('.') + 1);
  }

  public static String getConfigureName(Configurable c) {
    if (c.getConfigureName() != null && c.getConfigureName().length() > 0) {
      return c.getConfigureName();
    }
    else {
      return getConfigureName(c.getClass());
    }
  }

  protected Configurable importConfigurable() {
    final String className = JOptionPane.showInputDialog(
      getTopLevelAncestor(),
      Resources.getString("Editor.ConfigureTree.java_name"));

    if (className == null) {
      return null;
    }

    Object o = null;
    try {
      o = GameModule.getGameModule().getDataArchive()
        .loadClass(className).getConstructor().newInstance();
    }
    catch (Throwable t) {
      ReflectionUtils.handleImportClassFailure(t, className);
    }

    if (o == null) {
      return null;
    }

    if (o instanceof Configurable) {
      return (Configurable) o;
    }

    ErrorDialog.show("Error.not_a_configurable", className); //$NON-NLS-1$//
    return null;
  }

  protected void maybePopup(MouseEvent e) {
    final Configurable target = getTarget(e.getX(), e.getY());
    if (target == null) {
      return;
    }

    setSelectionRow(getClosestRowForLocation(e.getX(), e.getY()));
    final JPopupMenu popup = buildPopupMenu(target);
    popup.show(this, e.getX(), e.getY());
    popup.addPopupMenuListener(new PopupMenuListener() {
      @Override
      public void popupMenuCanceled(PopupMenuEvent evt) {
        repaint();
      }

      @Override
      public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) {
        repaint();
      }

      @Override
      public void popupMenuWillBecomeVisible(PopupMenuEvent evt) {
      }
    });
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (e.isPopupTrigger()) {
      maybePopup(e);
    }
  }

  // FIXME: should clicked handling be in mouseClicked()?
  @Override
  public void mouseReleased(MouseEvent e) {
    if (e.isPopupTrigger()) {
      maybePopup(e);
    }
    else if (e.getClickCount() == 2 && SwingUtils.isMainMouseButtonDown(e)) {
      final Configurable target = getTarget(e.getX(), e.getY());
      if (target == null) {
        return;
      }

      if (target.getConfigurer() != null) {
        final Action a = buildEditAction(target);
        if (a != null) {
          a.actionPerformed(new ActionEvent(e.getSource(), ActionEvent.ACTION_PERFORMED, "Edit", e.getModifiersEx())); //NON-NLS
        }
      }
    }
  }

  public DefaultMutableTreeNode getTreeNode(Configurable target) {
    return nodes.get(target);
  }

  @Override
  public void mouseDragged(MouseEvent evt) {
  }

  /** Is the Component allowed to be moved? */
  public boolean isMoveAllowed(Configurable target) {
    return target.isMovable();
  }

  /**
   * Is the Component allowed to be deleted?
   * A mandatory component can be deleted if there is more than one of them with the same parent
   * */
  protected boolean isDeleteAllowed(Configurable target) {
    if (!target.isMandatory()) {
      return true;
    }

    if (target instanceof AbstractBuildable) {
      final Buildable parent = ((AbstractBuildable) target).getAncestor();
      if (parent instanceof AbstractBuildable) {
        final int count = ((AbstractBuildable) parent).getComponentsOf(target.getClass()).size();
        return count > 1;
      }
    }

    return false;
  }

  /** Is the Component allowed to be duplicated? */
  protected boolean isDuplicateAllowed(Configurable target) {
    return !target.isUnique();
  }

  protected boolean isValidParent(Configurable parent, Configurable child) {

    if (parent != null && child != null) {
      final Class<?>[] c = parent.getAllowableConfigureComponents();
      for (final Class<?> aClass : c) {
        if (aClass.isAssignableFrom(child.getClass()) ||
          ((aClass == CardSlot.class) && (child.getClass() == PieceSlot.class)) || // Allow PieceSlots to be pasted to Decks
          ((aClass == ZoneProperty.class) && (child.getClass() == GlobalProperty.class)) || // Allow Global Properties to be saved as Zone Properties
          ((MassKeyCommand.class.isAssignableFrom(aClass)) && (MassKeyCommand.class.isAssignableFrom(child.getClass())))// All types of GKC's are inter-assignable
        ) {
          return true;
        }
      }
    }
    return false;
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mouseMoved(MouseEvent e) {
  }

  /*
   * Refresh the display of a node
   */
  public void nodeUpdated(Configurable target) {
    final DefaultMutableTreeNode node = getTreeNode(target);
    final Configurable parent = getParent(node);
    if (remove(parent, target)) {
      insert(parent, target, 0);
    }
    ((DefaultTreeModel) getModel()).nodeChanged(node);
  }

  public void jumpToTarget(Configurable target) {
    final DefaultMutableTreeNode node = getTreeNode(target);
    final TreePath path = new TreePath(node.getPath());
    setSelectionPath(path);
    scrollPathToVisible(path);
  }

  /**
   * Configurers that add or remove their own children directly should implement the Mutable interface so that
   * ConfigureTree can refresh the changed node.
   */
  public interface Mutable {
  }

  /**
   * Build an AddAction and execute it to request a new component from the user
   *
   * @param parent
   *          Target Parent
   * @param child
   *          Type to add
   */
  public void externalInsert(Configurable parent, Configurable child) {
    insert(parent, child, getTreeNode(parent).getChildCount());
  }

  public Action getHelpAction() {
    return helpAction;
  }

  public void populateEditMenu(EditorWindow ew) {
    final MenuManager mm = MenuManager.getInstance();

    mm.addAction("Editor.delete", deleteAction);
    mm.addAction("Editor.cut", cutAction);
    mm.addAction("Editor.copy", copyAction);
    mm.addAction("Editor.paste", pasteAction);
    mm.addAction("Editor.move", moveAction);
    mm.addAction("Editor.search", searchAction);
    mm.addAction("Editor.properties", propertiesAction);
    mm.addAction("Editor.ModuleEditor.translate", translateAction);
    mm.addAction("Editor.duplicate", duplicateAction);

    updateEditMenu();
  }

  /**
   * Handle main Edit menu selections/accelerators
   *
   * @param action
   *          Edit command name
   */
  protected void doKeyAction(String action) {
    final DefaultMutableTreeNode targetNode = (DefaultMutableTreeNode) this.getLastSelectedPathComponent();
    if (targetNode != null) {
      final Configurable target = (Configurable) targetNode.getUserObject();
      Action a = null;
      if (cutCmd.equals(action)) {
        a = buildCutAction(target);
      }
      else if (copyCmd.equals(action)) {
        a = buildCopyAction(target);
      }
      else if (pasteCmd.equals(action) || action.equals(String.valueOf(pasteKey.getKeyChar()))) {
        a = buildPasteAction(target);
      }
      else if (deleteCmd.equals(action)) {
        a = buildDeleteAction(target);
      }
      else if (moveCmd.equals(action)) {
        a = buildMoveAction(target);
      }
      else if (searchCmd.equals(action)) {
        a = buildSearchAction(target);
      }
      else if (propertiesCmd.equals(action)) {
        a = buildEditAction(target);
      }
      else if (translateCmd.equals(action)) {
        a = buildTranslateAction(target);
      }
      else if (helpCmd.equals(action)) {
        a = buildHelpAction(target);
      }
      else if (duplicateCmd.equals(action)) {
        final DefaultMutableTreeNode targetNode2 = getTreeNode(target);
        if (targetNode2 != null) {
          final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) targetNode2.getParent();
          if (parentNode != null) {
            a = buildAddAction((Configurable) parentNode.getUserObject(), target.getClass(), Resources.getString("Editor.duplicate"), parentNode.getIndex(targetNode2) + 1, target);
          }
        }
      }
      if (a != null) {
        a.actionPerformed(null);
      }
    }
  }

  /**
   * Tree selection changed, record info about the currently selected component
   */
  @Override
  public void valueChanged(TreeSelectionEvent e) {
    selected = null;
    final TreePath path = e.getPath();
    if (path != null) {
      final DefaultMutableTreeNode selectedNode = (DefaultMutableTreeNode) path.getLastPathComponent();
      selected = (Configurable) selectedNode.getUserObject();
      selectedRow = getRowForPath(path);
      updateEditMenu();
      ((DefaultTreeModel) getModel()).nodeChanged(selectedNode);

      // tree path change will affect indexing for an existing search; detect / prepare for that here
      newNodeSelected = selectedNode != lastFoundNode;

      // if node changes, we'll need the current Index.
      if (newNodeSelected) {
        selectedNodeIndex = getBookmark(
          (List<DefaultMutableTreeNode>) getSearchNodes(
            (DefaultMutableTreeNode) selectedNode.getRoot()
          ),
          selectedNode
        );
      }
    }
  }

  // tracks when search must be reset as node has changed
  protected static boolean newNodeSelected = false;
  protected static DefaultMutableTreeNode lastFoundNode;
  protected static int selectedNodeIndex;

  protected void updateEditMenu() {
    deleteAction.setEnabled(selected != null && isDeleteAllowed(selected));
    cutAction.setEnabled(selected != null && isDeleteAllowed(selected));
    copyAction.setEnabled(selected != null);
    pasteAction.setEnabled(selected != null && isValidPasteTarget(selected));
    moveAction.setEnabled(selected != null && isMoveAllowed(selected));
    duplicateAction.setEnabled(selected != null && isDuplicateAllowed(selected));
    searchAction.setEnabled(true);
    // Check the cached Configurer in the TreeNode, not the Configurable as Configurable.getConfigurer()
    // is very expensive and resets the Configurer causing label truncation issues in the JTree
    propertiesAction.setEnabled(selected != null && selected.getConfigurer() != null);
    translateAction.setEnabled(selected != null);
  }

  /**
   * Find the parent Configurable of a specified Configurable
   *
   * @param target target Configurable
   * @return parent
   */
  protected Configurable getParent(Configurable target) {
    final DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) getTreeNode(target).getParent();
    return (Configurable) parentNode.getUserObject();
  }

  public String getSearchCmd() {
    return searchCmd;
  }

  /**
   * Record additional available components to add to the popup menu.
   *
   * @param parent Parent Class
   * @param child Child Class
   */
  public static void addAdditionalComponent(Class<? extends Buildable> parent, Class<? extends Buildable> child) {
    additionalComponents.add(new AdditionalComponent(parent, child));
  }

  protected static class AdditionalComponent {
    Class<? extends Buildable> parent;
    Class<? extends Buildable> child;

    public AdditionalComponent(Class<? extends Buildable> p, Class<? extends Buildable> c) {
      parent = p;
      child = c;
    }

    public Class<? extends Buildable> getParent() {
      return parent;
    }

    public Class<? extends Buildable> getChild() {
      return child;
    }
  }

  /**
   * Container for search parameters
   */
  private static class SearchParameters {
    public static final String SEARCH_STRING = "searchString"; //$NON-NLS-1$//
    public static final String SEARCH_NORMAL    = "optNormal"; //$NON-NLS-1$//
    public static final String SEARCH_WORD   = "optWord"; //$NON-NLS-1$//
    public static final String SEARCH_REGEX   = "optRegex"; //$NON-NLS-1$//
    public static final String MATCH_CASE    = "matchCase"; //$NON-NLS-1$//
    public static final String MATCH_MODULE    = "matchModule"; //$NON-NLS-1$//
    public static final String MATCH_NAMES   = "matchNames"; //$NON-NLS-1$//
    public static final String MATCH_TYPES   = "matchTypes"; //$NON-NLS-1$//
    public static final String MATCH_SIMPLE     = "matchSimple";
    public static final String MATCH_FULL     = "matchFull";
    public static final String MATCH_ADVANCED     = "matchAdvanced";
    public static final String MATCH_TRAITS       = "matchTraits"; //$NON-NLS-1$//
    public static final String MATCH_EXPRESSIONS  = "matchExpressions"; //$NON-NLS-1$//
    public static final String MATCH_PROPERTIES   = "matchProperties"; //$NON-NLS-1$//
    public static final String MATCH_KEYS         = "matchKeys"; //$NON-NLS-1$//
    public static final String MATCH_MENUS        = "matchMenus"; //$NON-NLS-1$//
    public static final String MATCH_MESSAGES     = "matchMessages"; //$NON-NLS-1$//

    /** Current search string */
    private String searchString;

    /** Radio Button setting  */
    private boolean optNormal;
    private boolean optWord;
    private boolean optRegex;

    /** True if case-sensitive */
    private boolean matchCase;

    /** Option to include module components when searching an extension */
    private boolean matchModule;

    /** True if match configurable names */
    private boolean matchNames;

    /** True if match class names */
    private boolean matchTypes;

    /** Control of which data types get checked; filters == true enables subsequent detailed filter items */
    private boolean matchSimple;
    private boolean matchFull;
    private boolean matchAdvanced;

    /** True if match traits (names, descriptions, menu commands) */
    private boolean matchTraits;

    /** True if match expressions */
    private boolean matchExpressions;

    /** True if match Properties */
    private boolean matchProperties;

    /** True if match key commands */
    private boolean matchKeys;

    /** True if match context menu text */
    private boolean matchMenus;

    /** True if match Message Formats */
    private boolean matchMessages;

    /** Attach to our module preferences, if relevant */
    private static Prefs prefs;

    /**
     * Constructs a new search parameters object, using the preferences.
     */
    public SearchParameters() {

      // Attach to our module preferences if constructed this way. This also marks that we will write them when modified
      prefs = GameModule.getGameModule().getPrefs();

      prefs.addOption(null, new StringConfigurer(SEARCH_STRING, null, ""));
      prefs.addOption(null, new BooleanConfigurer(SEARCH_NORMAL,   null, true));
      prefs.addOption(null, new BooleanConfigurer(SEARCH_WORD,  null, false));
      prefs.addOption(null, new BooleanConfigurer(SEARCH_REGEX,  null, false));
      prefs.addOption(null, new BooleanConfigurer(MATCH_CASE,   null, false));
      prefs.addOption(null, new BooleanConfigurer(MATCH_MODULE,   null, false));
      prefs.addOption(null, new BooleanConfigurer(MATCH_NAMES,  null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_TYPES,  null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_SIMPLE, null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_FULL, null, false));
      prefs.addOption(null, new BooleanConfigurer(MATCH_ADVANCED, null, false));
      prefs.addOption(null, new BooleanConfigurer(MATCH_TRAITS,      null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_EXPRESSIONS, null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_PROPERTIES, null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_KEYS,        null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_MENUS, null, true));
      prefs.addOption(null, new BooleanConfigurer(MATCH_MESSAGES, null, true));

      // reset at module start
      searchString = "";
      optNormal = true;
      optWord = false;
      optRegex = false;
      matchCase    = false;
      matchModule = false;     // default for extension will be false; module over-rides to true and blocks from change

      // Radio buttons; belt & braces to ensure setup is consistent
      matchSimple       = (Boolean)prefs.getValue(MATCH_SIMPLE);
      matchFull       = (Boolean)prefs.getValue(MATCH_FULL) && !matchSimple;
      matchAdvanced      = (Boolean)prefs.getValue(MATCH_ADVANCED) && !(matchFull || matchSimple);

      matchNames   = (Boolean)prefs.getValue(MATCH_NAMES);
      matchTypes       = (Boolean)prefs.getValue(MATCH_TYPES);
      matchTraits      = (Boolean)prefs.getValue(MATCH_TRAITS);
      matchExpressions = (Boolean)prefs.getValue(MATCH_EXPRESSIONS);
      matchProperties  = (Boolean)prefs.getValue(MATCH_PROPERTIES);
      matchKeys        = (Boolean)prefs.getValue(MATCH_KEYS);
      matchMenus       = (Boolean)prefs.getValue(MATCH_MENUS);
      matchMessages    = (Boolean)prefs.getValue(MATCH_MESSAGES);
    }

    /**
     * Constructs a new search parameters object
     */
    public SearchParameters(String searchString, boolean optNormal, boolean optWord, boolean optRegex,
                            boolean matchCase, boolean matchModule, boolean matchNames, boolean matchTypes,
                            boolean matchSimple, boolean matchFull, boolean matchAdvanced, boolean matchTraits,
                            boolean matchExpressions, boolean matchProperties, boolean matchKeys, boolean matchMenus,
                            boolean matchMessages) {
      this.searchString = searchString;
      this.optNormal = optNormal;
      this.optWord = optWord;
      this.optRegex = optRegex;
      this.matchCase    = matchCase;
      this.matchModule = matchModule;
      this.matchNames   = matchNames;
      this.matchTypes   = matchTypes;
      this.matchSimple    = matchSimple;
      this.matchFull   = matchFull;
      this.matchAdvanced    = matchAdvanced;
      this.matchTraits      = matchTraits;
      this.matchExpressions = matchExpressions;
      this.matchProperties  = matchProperties;
      this.matchKeys        = matchKeys;
      this.matchMenus       = matchMenus;
      this.matchMessages    = matchMessages;
    }

    public String getSearchString() {
      return searchString;
    }

    public void setSearchString(String searchString) {
      this.searchString = searchString;
      writePrefs();
    }

    public boolean isOptNormal() {
      return optNormal;
    }

    public boolean isOptWord() {
      return optWord;
    }

    public boolean isOptRegex() {
      return optRegex;
    }

    public boolean isMatchCase() {
      return matchCase;
    }

    public void setMatchCase(boolean matchCase) {
      this.matchCase = matchCase;
      writePrefs();
    }
    public boolean isMatchModule() {
      return matchModule;
    }

    public void setMatchModule(boolean matchModule) {
      this.matchModule = matchModule;
      writePrefs();
    }

    public boolean isMatchNames() {
      return matchNames;
    }

    public void setMatchNames(boolean matchNames) {
      this.matchNames = matchNames;
      writePrefs();
    }

    public boolean isMatchTypes() {
      return matchTypes;
    }

    public void setMatchTypes(boolean matchTypes) {
      this.matchTypes = matchTypes;
      writePrefs();
    }

    public boolean isMatchSimple() {
      return matchSimple;
    }
    public void setMatchSimple(boolean matchSimple) {
      this.matchFull = matchSimple;
      writePrefs();
    }

    public boolean isMatchFull() {
      return matchFull;
    }
    public void setMatchFull(boolean matchFull) {
      this.matchFull = matchFull;
      writePrefs();
    }

    public boolean isMatchAdvanced() {
      return matchAdvanced;
    }

    public void setMatchAdvanced(boolean matchAdvanced) {
      this.matchAdvanced = matchAdvanced;
      writePrefs();
    }

    public boolean isMatchTraits() {
      return matchTraits;
    }

    public void setMatchTraits(boolean matchTraits) {
      this.matchTraits = matchTraits;
      writePrefs();
    }

    public boolean isMatchExpressions() {
      return matchExpressions;
    }

    public void setMatchExpressions(boolean matchExpressions) {
      this.matchExpressions = matchExpressions;
      writePrefs();
    }

    public boolean isMatchProperties() {
      return matchProperties;
    }

    public void setMatchProperties(boolean matchProperties) {
      this.matchProperties = matchProperties;
      writePrefs();
    }

    public boolean isMatchKeys() {
      return matchKeys;
    }

    public void setMatchKeys(boolean matchKeys) {
      this.matchKeys = matchKeys;
      writePrefs();
    }

    public boolean isMatchMenus() {
      return matchMenus;
    }

    public void setMatchMenus(boolean matchMenus) {
      this.matchMenus = matchMenus;
      writePrefs();
    }

    public boolean isMatchMessages() {
      return matchMessages;
    }

    public void setMatchMessages(boolean matchMessages) {
      this.matchMessages = matchMessages;
      writePrefs();
    }

    public void setFrom(final SearchParameters searchParameters) {
      searchString = searchParameters.getSearchString();
      optNormal = searchParameters.isOptNormal();
      optWord = searchParameters.isOptWord();
      optRegex = searchParameters.isOptRegex();
      matchCase = searchParameters.isMatchCase();
      matchModule = searchParameters.isMatchModule();
      matchNames = searchParameters.isMatchNames();
      matchTypes = searchParameters.isMatchTypes();
      matchSimple = searchParameters.isMatchSimple();
      matchFull   = searchParameters.isMatchFull();
      matchAdvanced   = searchParameters.isMatchAdvanced();
      matchTraits      = searchParameters.isMatchTraits();
      matchExpressions = searchParameters.isMatchExpressions();
      matchProperties  = searchParameters.isMatchProperties();
      matchKeys        = searchParameters.isMatchKeys();
      matchMenus       = searchParameters.isMatchMenus();
      matchMessages    = searchParameters.isMatchMessages();
      writePrefs();
    }

    public void writePrefs() {
      if (prefs != null) {
        prefs.setValue(SEARCH_STRING, searchString);
        prefs.setValue(SEARCH_NORMAL, optNormal);
        prefs.setValue(SEARCH_WORD,   optWord);
        prefs.setValue(SEARCH_REGEX, optRegex);
        prefs.setValue(MATCH_CASE, matchCase);
        prefs.setValue(MATCH_MODULE, matchModule);
        prefs.setValue(MATCH_NAMES,       matchNames);
        prefs.setValue(MATCH_TYPES, matchTypes);
        prefs.setValue(MATCH_SIMPLE,    matchSimple);
        prefs.setValue(MATCH_FULL,    matchFull);
        prefs.setValue(MATCH_ADVANCED,    matchAdvanced);
        prefs.setValue(MATCH_TRAITS,      matchTraits);
        prefs.setValue(MATCH_EXPRESSIONS, matchExpressions);
        prefs.setValue(MATCH_PROPERTIES,  matchProperties);
        prefs.setValue(MATCH_KEYS,        matchKeys);
        prefs.setValue(MATCH_MENUS,       matchMenus);
        prefs.setValue(MATCH_MESSAGES,    matchMessages);
      }
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      final SearchParameters that = (SearchParameters) o;
      return isMatchCase() == that.isMatchCase() &&
        isMatchModule() == that.isMatchModule() &&
        isMatchNames() == that.isMatchNames() &&
        isMatchTypes() == that.isMatchTypes() &&
        isMatchTraits() == that.isMatchTraits() &&
        isMatchSimple() == that.isMatchSimple() &&
        isMatchFull() == that.isMatchFull() &&
        isMatchAdvanced() == that.isMatchAdvanced() &&
        isMatchExpressions() == that.isMatchExpressions() &&
        isMatchProperties() == that.isMatchProperties() &&
        isMatchKeys() == that.isMatchKeys() &&
        isMatchMenus() == that.isMatchMenus() &&
        isMatchMessages() == that.isMatchMessages() &&
        getSearchString().equals(that.getSearchString()) &&
        isOptNormal() == that.isOptNormal() &&
        isOptWord() == that.isOptWord() &&
        isOptRegex() == that.isOptRegex();
    }

    @Override
    public int hashCode() {
      return Objects.hash(getSearchString(),
              isOptNormal(), isOptWord(), isOptRegex(), isMatchCase(), isMatchModule(),
              isMatchNames(), isMatchTypes(), isMatchSimple(), isMatchFull(), isMatchAdvanced(),
              isMatchTraits(), isMatchExpressions(), isMatchProperties(), isMatchKeys(),
              isMatchMenus(), isMatchMessages());
    }
  }

  private static class SearchAction extends AbstractAction {

    private static final long serialVersionUID = 1L;
    private final ConfigureTree configureTree;
    private final SearchParameters searchParameters;
    private Pattern regexPattern;
    private int nodeListIndex;
    private int traitIndex;
    private final List<Integer> breadCrumbs = new ArrayList<>();

    /**
     * Constructs a new {@link SearchAction}
     *
     * @param configureTree back reference to the {@link ConfigureTree}
     * @param searchParameters reference to the search parameter object
     */
    public SearchAction(ConfigureTree configureTree, SearchParameters searchParameters) {
      super(configureTree.getSearchCmd());
      this.configureTree = configureTree;
      this.searchParameters = searchParameters;
    }

    @Override
    public void actionPerformed(ActionEvent e) {

      JDialog d = configureTree.getSearchDialog();
      final JTextField search;
      if (d != null) {
        search = configureTree.getSearchField();
        search.selectAll();
      }
      else {
        d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, configureTree), false);
        configureTree.setSearchDialog(d);

        d.setTitle(configureTree.getSearchCmd());

        search = new HintTextField(32, Resources.getString("Editor.search_string"));
        search.setText(searchParameters.getSearchString());
        configureTree.setSearchField(search);
        search.selectAll();

        final JRadioButton normal = new JRadioButton(Resources.getString("Editor.search_optNormal"), searchParameters.isOptNormal());
        final JRadioButton word = new JRadioButton(Resources.getString("Editor.search_optWord"), searchParameters.isOptWord());
        final JRadioButton regex = new JRadioButton(Resources.getString("Editor.search_optRegex"), searchParameters.isOptRegex());

        final JCheckBox sensitive = new JCheckBox(Resources.getString("Editor.search_case"), searchParameters.isMatchCase());

        final JCheckBox includeModule = new JCheckBox(Resources.getString("Editor.search_module_tree"), !(configureTree instanceof ExtensionTree) || searchParameters.isMatchModule());

        final JRadioButton simple  = new JRadioButton(Resources.getString("Editor.search_simple"), searchParameters.isMatchSimple());
        final JRadioButton full  = new JRadioButton(Resources.getString("Editor.search_full"), searchParameters.isMatchFull());
        final JRadioButton filters  = new JRadioButton(Resources.getString("Editor.search_advanced"), searchParameters.isMatchAdvanced());

        final JLabel filtersPrompt = new JLabel((Resources.getString("Editor.search_filters")));

        final JCheckBox names = new JCheckBox(Resources.getString("Editor.search_names"), searchParameters.isMatchNames());
        final JCheckBox types = new JCheckBox(Resources.getString("Editor.search_types"), searchParameters.isMatchTypes());

        final JCheckBox traits = new JCheckBox(Resources.getString("Editor.search_traits"), searchParameters.isMatchTraits());
        final JCheckBox expressions = new JCheckBox(Resources.getString("Editor.search_expressions"), searchParameters.isMatchExpressions());
        final JCheckBox properties = new JCheckBox(Resources.getString("Editor.search_properties"), searchParameters.isMatchProperties());

        final JCheckBox keys = new JCheckBox(Resources.getString("Editor.search_keys"), searchParameters.isMatchKeys());
        final JCheckBox menus = new JCheckBox(Resources.getString("Editor.search_menus"), searchParameters.isMatchMenus());
        final JCheckBox messages = new JCheckBox(Resources.getString("Editor.search_messages"), searchParameters.isMatchMessages());

        final Consumer<Boolean> visSetter = visible -> {
          filtersPrompt.setVisible(visible);
          names.setVisible(visible);
          types.setVisible(visible);
          traits.setVisible(visible);
          expressions.setVisible(visible);
          properties.setVisible(visible);
          keys.setVisible(visible);
          menus.setVisible(visible);
          messages.setVisible(visible);
        };

        visSetter.accept(filters.isSelected());

        configureTree.setSearchAdvanced(filters);

        final JButton prev = new JButton(Resources.getString("Editor.search_prev"));
        prev.setToolTipText(Resources.getString("Editor.search_prevTip"));
        prev.setEnabled(false);

        // enable Page Up to trigger the Prev button
        final InputMap prevMap = prev.getInputMap(WHEN_IN_FOCUSED_WINDOW);
        prevMap.put(KeyStroke.getKeyStroke("PAGE_UP"), "PgUp");
        prev.getActionMap().put("PgUp", new AbstractAction() {
          @Override
          public void actionPerformed(ActionEvent e21) {
            for (final ActionListener a : prev.getActionListeners()) {
              a.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, null));
            }
          }
        });

        prev.addActionListener(ePrev -> {
          final SearchParameters parametersSetInDialog =
                  new SearchParameters(search.getText(), normal.isSelected(), word.isSelected(), regex.isSelected(), sensitive.isSelected(), includeModule.isSelected(), names.isSelected(), types.isSelected(),
                          simple.isSelected(), full.isSelected(), filters.isSelected(),
                          traits.isSelected(), expressions.isSelected(), properties.isSelected(), keys.isSelected(), menus.isSelected(), messages.isSelected());

          if (!searchParameters.equals(parametersSetInDialog)) {
            // previous relies on a prior forward search - search setting changes will block the Prev button but this is to catch the search string itself
            prev.setEnabled(false);
            chat("~" + Resources.getString("Editor.search_prev_invalid"));
          }
          else {
            // in event of a cursor move, breadCrumbs list may need culling before backward step...
            if (newNodeSelected && !breadCrumbs.isEmpty()) {
              while (nodeListIndex > 0 && breadCrumbs.get(nodeListIndex - 1) >= selectedNodeIndex) {
                breadCrumbs.remove(--nodeListIndex);
              }
            }
            else {
              if (nodeListIndex > 0) {
                breadCrumbs.remove(--nodeListIndex);
              }
            }

            // stop once we get to the first item
            if (nodeListIndex > 0) {
              // Rewind to previous match, and move the pointer back
              final DefaultMutableTreeNode node = setNode(breadCrumbs.get(nodeListIndex - 1));
              if (node != null) {
                selectPath(node);
                showHitList(node, regexPattern);
              }
            }
            prev.setEnabled(nodeListIndex > 1); // disable button once at start
          }
        });

         // Any search option changes immediately disables prev (invalidates backtracking). A renewed search will enable it.
        // Note Text field input requires that return is pressed before the event actions so is omitted. Other actions are immediate.
        final ActionListener checkChanges = e12 -> {
          final SearchParameters parametersSetInDialog =
                  new SearchParameters(search.getText(), normal.isSelected(), word.isSelected(), regex.isSelected(), sensitive.isSelected(), includeModule.isSelected(), names.isSelected(), types.isSelected(),
                          simple.isSelected(), full.isSelected(), filters.isSelected(),
                          traits.isSelected(), expressions.isSelected(), properties.isSelected(), keys.isSelected(), menus.isSelected(), messages.isSelected());
          prev.setEnabled(searchParameters.equals(parametersSetInDialog));
        };

        //search.addActionListener(checkChanges); // don't intercept search  - allow return/enter to trigger the next button instead
        normal.addActionListener(checkChanges);
        word.addActionListener(checkChanges);
        regex.addActionListener(checkChanges);
        sensitive.addActionListener(checkChanges);
        includeModule.addActionListener(checkChanges);
        simple.addActionListener(checkChanges);
        full.addActionListener(checkChanges);
        filters.addActionListener(checkChanges);
        names.addActionListener(checkChanges);
        types.addActionListener(checkChanges);
        traits.addActionListener(checkChanges);
        expressions.addActionListener(checkChanges);
        properties.addActionListener(checkChanges);
        keys.addActionListener(checkChanges);
        menus.addActionListener(checkChanges);
        messages.addActionListener(checkChanges);

        // The Advanced option...
        filters.addChangeListener(l -> {
          visSetter.accept(filters.isSelected());
          SwingUtils.repack(configureTree.getSearchDialog());
        });

        final JButton find = new JButton(Resources.getString("Editor.search_next"));
        find.setToolTipText(Resources.getString("Editor.search_nextTip"));

        // enable Page Down to trigger the Find/Next button
        final InputMap findMap = find.getInputMap(WHEN_IN_FOCUSED_WINDOW);
        findMap.put(KeyStroke.getKeyStroke("PAGE_DOWN"), "PgDn");
        find.getActionMap().put("PgDn", new AbstractAction() {
          @Override
          public void actionPerformed(ActionEvent e22) {
            for (final ActionListener a : find.getActionListeners()) {
              a.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, null));
            }
          }
        });

        find.addActionListener(eNext -> {
          final SearchParameters parametersSetInDialog =
                  new SearchParameters(search.getText(), normal.isSelected(), word.isSelected(), regex.isSelected(), sensitive.isSelected(), includeModule.isSelected(), names.isSelected(), types.isSelected(),
                          simple.isSelected(), full.isSelected(), filters.isSelected(),
                          traits.isSelected(), expressions.isSelected(), properties.isSelected(), keys.isSelected(), menus.isSelected(), messages.isSelected());

          final boolean anyChanges = !searchParameters.equals(parametersSetInDialog);

          if (anyChanges) {

            searchParameters.setFrom(parametersSetInDialog);

            // If custom filters is selected but no search parameters are selected, turn at least one on (and print warning)
            if (searchParameters.isMatchAdvanced() && !searchParameters.isMatchNames() && !searchParameters.isMatchTypes()
                    && !searchParameters.isMatchTraits() && !searchParameters.isMatchExpressions() && !searchParameters.isMatchProperties()
                    && !searchParameters.isMatchKeys() && !searchParameters.isMatchMenus() && !searchParameters.isMatchMessages()) {
              searchParameters.setMatchNames(true);
              names.setSelected(true);
              chat(Resources.getString("Editor.search_all_off"));
            }
          }

          if (searchParameters.getSearchString().isEmpty()) {
            prev.setEnabled(false);
          }
          else {
            if (anyChanges || newNodeSelected) {

              if (anyChanges) {
                regexPattern = setupRegexSearch(searchParameters.getSearchString());

                if (regexPattern != null) {

                  chatter.show(""); // line space at start of search

                  // Compute & display hit count as heading, no indent
                  final int matches = getNumMatches(regexPattern);

                  chatter.show(!searchParameters.isOptRegex() ? Resources.getString((searchParameters.isOptNormal() ? "Editor.search_count" : "Editor.search_countWord"), matches, noHTML(searchParameters.getSearchString())) :
                          Resources.getString("Editor.search_countRegex", matches, noHTML(regexPattern.toString())));

                  if (matches > 0) {
                    resetPath();  // Search needs to start from current position; if nothing found, cursor stays where it was
                  }
                }
                else {
                  return; // setupRegexSearch has failed and will have output an error message to chat
                }
              }

              final DefaultMutableTreeNode node = findNode(regexPattern);

              if (node != null) {
                selectPath(node);
                nodeListIndex = initSearchPosition(regexPattern);  //  maintains search index at  arbitrary start position
                showHitList(node, regexPattern);
              }
            }
            else {
              if (breadCrumbs.isEmpty()) {
                chat(Resources.getString(searchParameters.optNormal ? "Editor.search_none_found" : searchParameters.optWord ? "Editor.search_noWord_match" : "Editor.search_noRegex_match",
                      noHTML(!searchParameters.optRegex ? searchParameters.getSearchString() : regexPattern.toString())));
              }
              else {
                // get next...
                final DefaultMutableTreeNode node = findNode(regexPattern);
                if (node != null) {
                  selectPath(node);
                  showHitList(node, regexPattern);
                }
              }
            }
            prev.setEnabled(nodeListIndex > 1); // on second+ items, it's possible to traverse back the list
          }

        });

        final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
        cancel.addActionListener(e1 -> configureTree.getSearchDialog().setVisible(false));

        final JButton help = new JButton(Resources.getString(Resources.HELP));
        help.addActionListener(e2 -> showSearchHelp());

        d.setLayout(new MigLayout("", "[fill]")); // NON-NLS
        final JPanel panel = new JPanel(new MigLayout("wrap 1, gapy 4", "[fill]")); // NON-NLS
        panel.setBorder(BorderFactory.createEtchedBorder());

        // top row
        panel.add(search, "grow");

        final ButtonGroup searchType = new ButtonGroup();
        searchType.add(normal);
        searchType.add(word);
        searchType.add(regex);

        final JPanel optLine1 = new JPanel(new MigLayout("gapy 4, ins 0", "[]rel[]rel[]rel[]push")); // NON-NLS
        optLine1.add(new JLabel((Resources.getString("Editor.search_optLabel"))));
        optLine1.add(normal);
        optLine1.add(word);
        optLine1.add(regex);
        panel.add(optLine1, "grow"); // NON-NLS

        // options
        final JPanel optLine2 = new JPanel(new MigLayout("gapy 4, ins 0", "[]rel[]rel[]push")); // NON-NLS
        optLine2.add(sensitive);
        panel.add(optLine2, "grow"); // NON-NLS

        final JSeparator sep = new JSeparator();
        sep.setOrientation(SwingConstants.HORIZONTAL);
        panel.add(sep);

        // scope of search
        final ButtonGroup searchScope = new ButtonGroup();
        searchScope.add(simple);
        searchScope.add(full);
        searchScope.add(filters);

        final JPanel scopePanel = new JPanel(new MigLayout(ConfigurerLayout.STANDARD_INSETS, "[]rel[]rel[]rel[]push")); // NON-NLS
        scopePanel.add(new JLabel((Resources.getString("Editor.search_scopeLabel"))));
        scopePanel.add(simple);
        scopePanel.add(full);
        scopePanel.add(filters);
        scopePanel.add(includeModule);
        includeModule.setVisible(configureTree instanceof ExtensionTree);
        panel.add(scopePanel, "grow"); // NON-NLS

        // Filters
        // PDS can be set to refresh specific items only, based on a regex
        final JPanel filtersTop = new JPanel(new MigLayout("hidemode 3,wrap 1," + ConfigurerLayout.STANDARD_INSETS_GAPY, "[]rel[]")); // NON-NLS
        filtersTop.add(filtersPrompt);
        filtersTop.add(names);

        final JPanel filterList = new JPanel(new MigLayout("hidemode 3,wrap 1," + ConfigurerLayout.STANDARD_INSETS_GAPY, "push[]")); // NON-NLS
        filterList.add(filtersTop);
        filterList.add(types);
        filterList.add(traits);
        filterList.add(expressions);
        filterList.add(properties);
        filterList.add(keys);
        filterList.add(menus);
        filterList.add(messages);
        panel.add(filterList, "grow"); // NON-NLS

        // buttons row
        final JPanel bPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]rel[]push")); // NON-NLS
        bPanel.add(prev, "tag prev,sg 1"); //$NON-NLS-1$//
        bPanel.add(find, "tag next,sg 1"); //$NON-NLS-1$//
        bPanel.add(cancel, "tag cancel,sg 1"); //$NON-NLS-1$//
        bPanel.add(help, "tag help,sg 1"); // NON-NLS
        panel.add(bPanel, "grow"); // NON-NLS

        d.add(panel, "grow"); // NON-NLS

        d.getRootPane().setDefaultButton(find); // Enter key activates search (see also Page_Down)

         // Esc Key cancels
        final KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        d.getRootPane().registerKeyboardAction(ee -> configureTree.getSearchDialog().setVisible(false), k, WHEN_IN_FOCUSED_WINDOW);
      }

      search.requestFocus(); // Start w/ focus in search string field

      if (!d.isVisible()) {
        d.setLocationRelativeTo(d.getParent());
        SwingUtils.repack(d);
        d.setVisible(true);
      }
    }

    /**
     * Select path to node and display in editor
     *
     * @param node - node to position at
     */
    private void selectPath(DefaultMutableTreeNode node) {
      final TreePath path = new TreePath(node.getPath());
      configureTree.setSelectionPath(path);
      configureTree.scrollPathToVisible(path);
    }

    /**
     * Resets path so that an initial search will hit first on the currently selected item.
     */
    private void resetPath() {
      final List<DefaultMutableTreeNode> searchNodes =
              configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot());
      final DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode)configureTree.getLastSelectedPathComponent();

      int bookmark = 0;

      // Position at the current node
      if (currentNode != null) {
        bookmark =
                IntStream
                        .range(0, searchNodes.size())
                        .filter(i -> searchNodes.get(i) == currentNode)
                        .findFirst()
                        .orElse(-1);
      }

      // take a step back
      if (--bookmark < 0) {
        bookmark = searchNodes.size() - 1;
      }
      final TreePath path = new TreePath(setNode(bookmark).getPath());
      configureTree.setSelectionPath(path);
    }

    private void showSearchHelp() {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
      final File theFile = new File(dir, "Search.html"); //$NON-NLS-1$
      HelpFile h = null;
      try {
        h = new HelpFile(null, theFile, "#top"); //$NON-NLS-1$
      }
      catch (MalformedURLException e) {
        ErrorDialog.bug(e);
      }
      BrowserSupport.openURL(h.getContents().toString());
    }

    /**
     * Search through the tree, starting at the currently selected location (and wrapping around if needed)
     * Compare nodes until we find our search string (or have searched everything we can search)
     * @param regexPattern Match pattern for checkNode()
     * @return the node we found, or null if none
     */
    private DefaultMutableTreeNode findNode(Pattern regexPattern) {

      if (regexPattern == null) {
        return null;
      }

      final List<DefaultMutableTreeNode> searchNodes =
        configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot());
      final DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode)configureTree.getLastSelectedPathComponent();

      int bookmark = -1;

      // Position at the current node
      if (currentNode != null) {
        bookmark = getBookmark(searchNodes, currentNode);
      }

      // find the next node
      final Predicate<DefaultMutableTreeNode> nodeMatchesSearchString = node -> checkNode(node, regexPattern);

      lastFoundNode =
        searchNodes
          .stream()
          .skip(bookmark + 1)
          .filter(nodeMatchesSearchString)
          .findFirst()
          .orElse(null);

      if (lastFoundNode == null) {
        lastFoundNode =
                searchNodes
                        .stream()
                        .limit(bookmark + 1)
                        .filter(nodeMatchesSearchString)
                        .findFirst()
                        .orElse(null);

        breadCrumbs.clear();
        nodeListIndex = 0;
      }

      // Determine precise bookmark for back-track record (might be a child of the node within which the search was performed)
      if (lastFoundNode != null) {
        bookmark = getBookmark(searchNodes, lastFoundNode);
      }

      // track the node just found
      selectedNodeIndex = bookmark;
      breadCrumbs.add(bookmark);
      ++nodeListIndex;

      return lastFoundNode;
    }

    private DefaultMutableTreeNode setNode(int bookmark) {
      selectedNodeIndex = bookmark;
      lastFoundNode = configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot()).get(bookmark);
      return lastFoundNode;
    }

    /**
    * @return how many total nodes match the search string
    */
    private int getNumMatches(Pattern regexPattern) {
      final List<DefaultMutableTreeNode> searchNodes = configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot());
      return (int) searchNodes.stream().filter(node -> checkNode(node, regexPattern)).count();
    }

    /**
     * Position and initialise for start of a search. Purpose is to maintain a consistent display index regardless of starting position.
     * @param regexPattern - our regex pattern derived from the search panel
     * @return value of nodeListIndex
     */
    private int initSearchPosition(Pattern regexPattern) {
      final List<DefaultMutableTreeNode> searchNodes = configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot());
      final DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode)configureTree.getLastSelectedPathComponent();

      breadCrumbs.clear();
      int i = 0;

      do {
        if (checkNode(searchNodes.get(i), regexPattern)) {
          breadCrumbs.add(i);
        }
      } while (searchNodes.get(i++) != currentNode);

      return  breadCrumbs.size();
    }

    /**
     * @param st - Search target (usually Decorator or AbstractConfigurable)
     * @param regexPattern - our regex pattern derived from the search panel
     * @return true if the node matches our searchString based on search configuration
     */
    private boolean checkSearchTarget(SearchTarget st, Pattern regexPattern) {
      if (searchParameters.isMatchExpressions() || searchParameters.isMatchFull()) {
        final List<String> exps = st.getExpressionList();
        if (exps != null) {
          for (final String s : exps) {
            if (!StringUtils.isEmpty(s) && checkString(s, regexPattern)) {
              return true;
            }
          }
        }
      }

      if (searchParameters.isMatchProperties() || searchParameters.isMatchFull()) {
        final List<String> props = st.getPropertyList();
        if (props != null) {
          for (final String s : props) {
            if (!StringUtils.isEmpty(s) && checkString(s, regexPattern)) {
              return true;
            }
          }
        }
      }

      if (searchParameters.isMatchKeys() || searchParameters.isMatchFull()) {
        final List<NamedKeyStroke> keys = st.getNamedKeyStrokeList();
        if (keys != null) {
          for (final NamedKeyStroke k : keys) {
            if (k != null) {
              final String s = k.isNamed() ? k.getName() : KeyNamer.getKeyString(k.getStroke());
              if (!StringUtils.isEmpty(s) && checkString(s, regexPattern)) {
                return true;
              }
            }
          }
        }
      }

      if (searchParameters.isMatchMenus() || searchParameters.isMatchFull()) {
        final List<String> menus = st.getMenuTextList();
        if (menus != null) {
          for (final String s : menus) {
            if (!StringUtils.isEmpty(s) && checkString(s, regexPattern)) {
              return true;
            }
          }
        }
      }

      if (searchParameters.isMatchMessages() || searchParameters.isMatchFull()) {
        final List<String> msgs = st.getFormattedStringList();
        if (msgs != null) {
          for (final String s : msgs) {
            if (!StringUtils.isEmpty(s) && checkString(s, regexPattern)) {
              return true;
            }
          }
        }
      }

      return false;
    }

    /**
     * @param node - any node of our module tree
     * @param regexPattern - our search pattern derived from the configuration panel
     * @return true if the node matches our search
     */
    private boolean checkNode(DefaultMutableTreeNode node, Pattern regexPattern) {
      final Configurable c = (Configurable) node.getUserObject();
      final boolean showName = (searchParameters.isMatchNames() || !searchParameters.isMatchAdvanced());  // name is default (i.e. unless filtered out)
      final boolean showTypes = (searchParameters.isMatchTypes() || !searchParameters.isMatchAdvanced());  // type [class] is default (i.e. unless filtered out)

      // If we're searching an extension, only extension components will be included by default
      if (configureTree instanceof ExtensionTree && !searchParameters.isMatchModule()) {
        final ExtensionTree xTree = (ExtensionTree) configureTree;
        if (!xTree.isEditable(node)) {
          return false;
        }
      }

      if (showName) {
        final String objectName = c.getConfigureName();
        if (objectName != null && checkString(objectName, regexPattern)) {
          return true;
        }
        if (c instanceof ComponentDescription) {
          // Selecting names includes description in detection.
          final String desc = ((ComponentDescription) c).getDescription();
          if ((desc != null) && checkString(desc, regexPattern)) {
            return true;
          }
        }
      }

      if (showTypes) {
        final String className = getConfigureName(c.getClass());
        if ((className != null) && checkString(className, regexPattern)) {
          return true;
        }
      }

      if (searchParameters.isMatchSimple()) {
        return false;
      }

      //  Special processing to include select items in full search despite not being a SearchTarget
      // Is module descriptor ?
      if (getConfigureName(c.getClass()).equals(CLASS_MODULE)) {
        // [Module] - Name, Description & Additional infos fields within the Module component
        if (searchParameters.isMatchFull() || searchParameters.isMatchMenus()) {
          // check UI content

          if (!showName) {       // not if already captured by name
            final String objectName = c.getConfigureName();
            if (objectName != null && checkString(objectName, regexPattern)) {
              return true;
            }
          }

          final String desc = c.getAttributeValueString(DESCRIPTION);
          if (desc != null && checkString(desc, regexPattern)) {
            return true;
          }

          final String moduleOther1 = c.getAttributeValueString(MODULE_OTHER1_PROPERTY);
          if (moduleOther1 != null && checkString(moduleOther1, regexPattern)) {
            return true;
          }

          final String moduleOther2 = c.getAttributeValueString(MODULE_OTHER2_PROPERTY);
          if (moduleOther2 != null) {
            return checkString(moduleOther2, regexPattern);
          }
        }
      }
      else {
        final DefaultMutableTreeNode pNode = (DefaultMutableTreeNode) node.getParent();
        final Configurable p = (Configurable) pNode.getUserObject();

        if (getConfigureName(p.getClass()).equals(CLASS_HELP_MENU)) {
          // [Help Menu]
          // Menu is also the Name, so only catch here if not already caught as Name

          if (!showName && (searchParameters.isMatchFull() || searchParameters.isMatchMenus())) {
            return checkString(getConfigureName(c), regexPattern);
          }

          if (searchParameters.isMatchFull() || searchParameters.isMatchExpressions()) {
            // content will be categorised as Expressions... each is a separate component
            final String startPage = c.getAttributeValueString("startingPage");
            if (startPage != null) {
              return checkString(startPage, regexPattern);
            }

            final String pdfFile = c.getAttributeValueString("pdfFile");
            if (pdfFile != null) {
              return checkString(pdfFile, regexPattern);
            }

            final String textFile = c.getAttributeValueString("textFile");
            if (textFile != null) {
              return checkString(textFile, regexPattern);
            }
          }
        }
      }

      if (!(c instanceof SearchTarget)) {
        return false;
      }

      // From here down we are only searching inside SearchTarget objects (Piece/Prototypes, or searchable AbstractConfigurables)
      GamePiece p;
      boolean protoskip;
      if (c instanceof GamePiece) {
        p = (GamePiece) c;
        protoskip = false;
      }
      else if (c instanceof PieceSlot) {
        p = ((PieceSlot)c).getPiece();
        protoskip = false;
      }
      else if (c instanceof PrototypeDefinition) {
        p = ((PrototypeDefinition)c).getPiece();
        protoskip = true;
      }
      else {
        return checkSearchTarget((SearchTarget) c, regexPattern);
      }

      // We're going to search Decorator from inner-to-outer (BasicPiece-on-out), so that user sees the traits hit in
      // the same order they're listed in the PieceDefiner window. So we first traverse them in the "normal" direction
      // outer to inner and make a list in the order we want to traverse it (for architectural reasons, just traversing
      // with getOuter() would take us inside of prototypes inside a piece, which we don't want).
      final List<GamePiece> pieces = new ArrayList<>();
      pieces.add(p);
      while (p instanceof Decorator) {
        p = ((Decorator) p).getInner();
        pieces.add(p);
      }
      Collections.reverse(pieces);

      for (final GamePiece piece : pieces) {
        if (!protoskip) { // Skip the fake "Basic Piece" on a Prototype definition
          if (searchParameters.isMatchTraits() && (piece instanceof EditablePiece)) {
            final String desc = ((EditablePiece) piece).getDescription();
            if ((desc != null) && checkString(desc, regexPattern)) {
              return true;
            }
          }

          if (piece instanceof SearchTarget) {
            if (checkSearchTarget((SearchTarget)piece, regexPattern)) {
              return true;
            }
          }
        }
        protoskip = false;
      }

      return false;
    }

    /**
     * Tracks how we are progressing through searching a target GamePiece or Configurable and its traits/attributes, and whether we need to display headers
     */
    private class TargetProgress {
      public boolean targetShown = false;
      public boolean traitShown = false;

      /**
       * When we're starting a new trait within the piece, clear the traitShown flag.
       */
      void startNewTrait() {
        traitShown = false;
      }

      /**
       * Checks and displays the piece header if needed
       * @param matchString our match string
       */
      void checkShowPiece(String matchString) {
        if (!targetShown) {
          targetShown = true;
          chat("<font color=blue>" + matchString + "</font>");
        }
      }

      /**
       * Checks and displays the piece header & trait/component headers, if needed
       * @param matchString our matched string
       * @param regexPattern the search pattern
       * @param idString trait or component name
       * @param desc trait description
       */
      void checkShowTrait(String matchString, Pattern regexPattern, String idString, String desc) {
        checkShowPiece(matchString);
        if (!traitShown) {
          traitShown = true;
          printFind(7, idString, desc, regexPattern);
        }
      }

      /**
       * Checks and displays the piece header & trait/component headers with additional info
       * @param matchString our matched string
       * @param regexPattern the search pattern
       * @param idString trait or component name
       * @param desc trait description
       * @param suffix raw string that is not searched and appends to the output
       */
      void checkShowTrait(String matchString, Pattern regexPattern, String idString, String desc, String suffix) {
        checkShowPiece(matchString);
        if (!traitShown) {
          traitShown = true;
          printFind(7, idString, desc, regexPattern, suffix);
        }
      }
    }

    private void hitCheck(String s, Pattern regexPattern, String matchString, String item, String desc, String show, TargetProgress progress) {
      if (!StringUtils.isEmpty(s) && checkString(s, regexPattern)) {
        progress.checkShowTrait(matchString, regexPattern, item, desc);
        printFind(10, show, s, regexPattern);
      }
    }

    private void stringListHits(Boolean flag, List<String> strings, Pattern regexPattern, String matchString, String item, String desc, String show, TargetProgress progress) {
      if (!flag || (strings == null)) {
        return;
      }
      for (final String s : strings) {
        hitCheck(s, regexPattern, matchString, item, desc, show, progress);
      }
    }

    private void keyListHits(Boolean flag, List<NamedKeyStroke> keys, Pattern regexPattern, String matchString, String item, String desc, String show, TargetProgress progress) {
      if (!flag || (keys == null)) {
        return;
      }

      for (final NamedKeyStroke k : keys) {
        if (k != null) {
          final String s = k.isNamed() ? k.getName() : KeyNamer.getKeyString(k.getStroke());
          hitCheck(s, regexPattern, matchString, item, desc, show, progress);
        }
      }
    }

    @Deprecated (since = "2023-10-21", forRemoval = true)
    private void showConfigurableHitList(DefaultMutableTreeNode node, Pattern regexPattern) {
      final Configurable c = (Configurable) node.getUserObject();
      final String item = getConfigureName(c.getClass());
      final String name = StringUtils.defaultString(c.getConfigureName());
      final String matchString = Resources.getString("Editor.search_matches", nodeListIndex) + "<b>" + noHTML(name + " [" + item + "]") + "</b>: ";
      final TargetProgress progress = new TargetProgress();
      showConfigurableHitList(node, regexPattern, matchString, progress);
    }

    /**
     * Called from showHitList to generate detailed output for Configurables.
     * @param node - any node of our module tree
     * @param regexPattern - our search string
     * @param progress - tracks whether component has matched already (ensures only first-time hit generates header output)
     */
    private void showConfigurableHitList(DefaultMutableTreeNode node, Pattern regexPattern, String matchString, TargetProgress progress) {
      final Configurable c = (Configurable) node.getUserObject();
      final String item = getConfigureName(c.getClass());
      final boolean showName = (searchParameters.isMatchNames() || !searchParameters.isMatchAdvanced());  // name is default (i.e. unless filtered out)


      if (getConfigureName(c.getClass()).equals(CLASS_MODULE)) {
        // [Module] - Name, Description & Additional info fields within the Module component
        if (searchParameters.isMatchFull() || searchParameters.isMatchMenus()) {
          // display matched UI content

          if (!showName) {       // not if already captured by name
            stringListHits(true, Collections.singletonList(getConfigureName(c)), regexPattern, matchString, item, "", MODULE_NAME_PROPERTY, progress); //NON-NLS
          }

          final String desc = c.getAttributeValueString(DESCRIPTION);
          final String moduleOther1 = c.getAttributeValueString(MODULE_OTHER1_PROPERTY);
          final String moduleOther2 = c.getAttributeValueString(MODULE_OTHER2_PROPERTY);
          stringListHits(true, Collections.singletonList(desc), regexPattern, matchString, item, "", MODULE_DESCRIPTION_PROPERTY, progress); //NON-NLS
          stringListHits(true, Collections.singletonList(moduleOther1), regexPattern, matchString, item, "", MODULE_OTHER1_PROPERTY, progress); //NON-NLS
          stringListHits(true, Collections.singletonList(moduleOther2), regexPattern, matchString, item, "", MODULE_OTHER2_PROPERTY, progress); //NON-NLS
        }
      }
      else {
        // Help ?
        final DefaultMutableTreeNode pNode = (DefaultMutableTreeNode) node.getParent();
        final Configurable p = (Configurable) pNode.getUserObject();

        if (getConfigureName(p.getClass()).equals(CLASS_HELP_MENU)) {
          // Help - special processing to include in full search despite not being a SearchTarget
          // Menu is also the Name, so only display here if not already reported as Name

          if (!showName && (searchParameters.isMatchFull() || searchParameters.isMatchMenus())) {
            stringListHits(true, Collections.singletonList(getConfigureName(c)), regexPattern, matchString, item, "", "UI Text", progress); //NON-NLS
          }
          if (searchParameters.isMatchFull() || searchParameters.isMatchExpressions()) {
            // content refs will be categorised as Expressions
            final String startPage = c.getAttributeValueString("startingPage");
            final String pdfFile = c.getAttributeValueString("pdfFile");
            final String textFile = c.getAttributeValueString("textFile");
            stringListHits(true, Collections.singletonList(startPage), regexPattern, matchString, item, "", "Starting page", progress); //NON-NLS
            stringListHits(true, Collections.singletonList(pdfFile), regexPattern, matchString, item, "", "PDF file", progress); //NON-NLS
            stringListHits(true, Collections.singletonList(textFile), regexPattern, matchString, item, "", "Text File", progress); //NON-NLS
          }
        }
      }

      if (!(c instanceof SearchTarget)) {
        return;
      }

      // Go deeper for a full or filtered search on SearchTarget data
      final SearchTarget st = (SearchTarget) c;

      stringListHits(searchParameters.isMatchExpressions() || searchParameters.isMatchFull(), st.getExpressionList(),      regexPattern, matchString, item, "", "Expression",    progress); //NON-NLS
      // Avoid duplicate output when Name is also the key  value here...
      if (!item.equals(Resources.getString("Editor.GlobalProperty.component_type")) || !showName) {
        stringListHits(searchParameters.isMatchProperties() || searchParameters.isMatchFull(), st.getPropertyList(), regexPattern, matchString, item, "", "Property", progress); //NON-NLS
      }
      stringListHits(searchParameters.isMatchMenus() || searchParameters.isMatchFull(),       st.getMenuTextList(),        regexPattern, matchString, item, "", "UI Text",       progress); //NON-NLS
      stringListHits(searchParameters.isMatchMessages() || searchParameters.isMatchFull(),    st.getFormattedStringList(), regexPattern, matchString, item, "", "Message/Field", progress); //NON-NLS

      keyListHits(searchParameters.isMatchKeys() || searchParameters.isMatchFull(),           st.getNamedKeyStrokeList(),  regexPattern, matchString, item, "", "KeyCommand",    progress); //NON-NLS
    }

    /**
     * Generates any common output (Names & Types) then, if this node contains a Game Piece of some kind,
     * displays a list of Trait information from the piece that matches our search parameters.
     * Otherwise, continues via showConfigurableHitList for details of Configurables.
     * @param node - any node of our module tree
     * @param regexPattern - our search string
     */
    private void showHitList(DefaultMutableTreeNode node, Pattern regexPattern) {
      final Configurable c = (Configurable) node.getUserObject();
      final String item = getConfigureName(c.getClass());
      final String name = StringUtils.defaultString(getConfigureName(c));
      final TargetProgress progress = new TargetProgress();

      // name & type (class) are default search categories and only excluded via Advanced Filters
      final boolean showName = searchParameters.isMatchNames() || !searchParameters.isMatchAdvanced();
      final boolean showTypes = searchParameters.isMatchTypes() || !searchParameters.isMatchAdvanced();

      // Heading without highlighting matches
      final String matchString = Resources.getString("Editor.search_matches", nodeListIndex)
              + "<b>" + noHTML(name) + " [" + noHTML(item) + "]" + "</b>: ";

      stringListHits(showName, Collections.singletonList(getConfigureName(c)), regexPattern, matchString, item, "", "Name", progress); //NON-NLS
      stringListHits(showTypes, List.of(item),                 regexPattern, matchString, item, "", "Type", progress); //NON-NLS

      // Component description is displayed in Simple mode searches or when Name is selected
      if (c instanceof ComponentDescription) {
        stringListHits(showName, Collections.singletonList(((ComponentDescription) c).getDescription()), regexPattern, matchString, item, "", "Description", progress); //NON-NLS
      }

      if (searchParameters.isMatchSimple()) {
        return;
      }

      // Details...
      GamePiece p;
      boolean protoskip;
      if (c instanceof GamePiece) {
        p = (GamePiece)c;
        protoskip = false; // This is a "real" GamePiece, so we will look at the BasicPiece too
      }
      else if (c instanceof PieceSlot) {
        p = ((PieceSlot)c).getPiece();
        protoskip = false; // This is a "real" GamePiece, so we will look at the BasicPiece too
      }
      else if (c instanceof PrototypeDefinition) {
        p = ((PrototypeDefinition)c).getPiece();
        protoskip = true; // This is a prototype definition, so we will ignore the BasicPiece entry
      }
      else {
        showConfigurableHitList(node, regexPattern, matchString, progress); // If no GamePiece, try searching for regular Configurable details
        return;
      }

      // We're going to search Decorator from inner-to-outer (BasicPiece-on-out), so that user sees the traits hit in
      // the same order they're listed in the PieceDefiner window. So we first traverse them in the "normal" direction
      // outer to inner and make a list in the order we want to traverse it (for architectural reasons, just traversing
      // with getOuter() would take us inside of prototypes inside a piece, which we don't want).
      final List<GamePiece> pieces = new ArrayList<>();
      pieces.add(p);
      while (p instanceof Decorator) {
        p = ((Decorator) p).getInner();
        pieces.add(p);
      }
      Collections.reverse(pieces);

      traitIndex = 0;

      for (final GamePiece piece : pieces) {
        if (!protoskip && (piece instanceof EditablePiece)) { // Skip the fake "Basic Piece" on a Prototype definition;

          final String desc = ((EditablePiece) piece).getDescription();
          progress.startNewTrait();    // A new trait, so reset our "trait progress".
          traitIndex++;

          if (searchParameters.isMatchTraits() || searchParameters.isMatchFull() && ((piece instanceof Decorator) || (!searchParameters.isMatchNames() && searchParameters.isMatchAdvanced()))) {
            if ((desc != null) && checkString(desc, regexPattern)) {

              final String[] traitType = piece.getType().split(";"); // splitting the string at ";"

              if ((traitType[0] + ";").equals(BasicPiece.ID)) {
                // Basic Piece gets bonus info that is otherwise excluded from search
                // processing Type string piece;;;image;BasicName - assumes the extra fields don't need calls to noHTML()
                final String pieceInfo = "image: " + (traitType.length < 4 ? "" : traitType[3]) + "&nbsp;".repeat(3) + "gpid: " + p.getProperty(Properties.PIECE_ID);
                progress.checkShowTrait(matchString, regexPattern, "Trait", desc, pieceInfo); //NON-NLS
              }
              else {
                progress.checkShowTrait(matchString, regexPattern, "Trait", desc); //NON-NLS
              }
            }
          }

          if (piece instanceof Decorator) { // Not the Basic Piece trait

            final Decorator d = (Decorator) piece;
            stringListHits(searchParameters.isMatchExpressions() || searchParameters.isMatchFull(), d.getExpressionList(), regexPattern, matchString, "Trait", desc, "Expression", progress); //NON-NLS
            stringListHits(searchParameters.isMatchProperties() || searchParameters.isMatchFull(), d.getPropertyList(), regexPattern, matchString, "Trait", desc, "Property", progress); //NON-NLS
            stringListHits(searchParameters.isMatchMenus() || searchParameters.isMatchFull(), d.getMenuTextList(), regexPattern, matchString, "Trait", desc, "UI Text", progress); //NON-NLS
            stringListHits(searchParameters.isMatchMessages() || searchParameters.isMatchFull(), d.getFormattedStringList(), regexPattern, matchString, "Trait", desc, "Message/Field", progress); //NON-NLS

            keyListHits(searchParameters.isMatchKeys() || searchParameters.isMatchFull(), d.getNamedKeyStrokeList(), regexPattern, matchString, "Trait", desc, "KeyCommand", progress); //NON-NLS

          }
        }
        protoskip = false;
      }
    }

    /**
     * Checks a single string against our search parameters
     * @param target - string to check
     * @param regexPattern - Regex search pattern
     * @return true if this is a match based on the pattern derived from search parameters in setupRegexSearch().
     */
    private boolean checkString(String target, Pattern regexPattern) {

      // Regular Expression check - match on pattern established in setupRegexPattern()
      return regexPattern.matcher(target).find();
    }

    /**
     * Prints search output (found details)
     * @param padding - left margin spaces
     * @param id - trait or component
     * @param str - item details
     */
    private void printFind(int padding, String id, String str, Pattern regexPattern) {

      final String printStr = (StringUtils.isEmpty(str) ? "" : highlightFinds(str, regexPattern));

      chat((id.equals("Trait") ? (traitIndex < 10 ? "&nbsp;&nbsp;<b>" : traitIndex < 100 ? "&nbsp;<b>" : "<b>") + traitIndex + "&gt</b>" + "&nbsp;".repeat(padding - 6) : "&nbsp;".repeat(padding)) + "{" + highlightFinds(id, regexPattern) + "} " + printStr); //NON-NLS
    }

    /**
     * Prints search output (found details) with additional information
     * @param padding - left margin spaces
     * @param id - trait or component
     * @param str - item details
     * @param strSuffix - appended output that bypassing highlighting, offset and in italics (not null)
     */
    private void printFind(int padding, String id, String str, Pattern regexPattern, String strSuffix) {

      final String printStr = (StringUtils.isEmpty(str) ? "" : highlightFinds(str, regexPattern));

      chat((id.equals("Trait") ? (traitIndex < 10 ? "&nbsp;&nbsp;<b>" : traitIndex < 100 ? "&nbsp;<b>" : "<b>") + traitIndex + "&gt</b>" + "&nbsp;".repeat(padding - 6) : "&nbsp;".repeat(padding)) + "{" + highlightFinds(id, regexPattern) + "} " + printStr + "&nbsp;".repeat(10) + "<i>" + strSuffix + "</i>"); //NON-NLS
    }

    /**
     * Formats an output string, neutralising original HTML and adding highlighting around matched segment(s)
     * @param rawStr - string to format
     * @param regexPattern - Regex search pattern
     * @return fmtStr - string formatted with highlighted matches
     */
    private String highlightFinds(String rawStr, Pattern regexPattern) {

      final String htmlHighlighter = "<font bgcolor=yellow>";
      final StringBuilder fmtStr = new StringBuilder();
      int lastEnd = 0;  // set to ensure that an unmatched first segment will be picked up

      final Matcher match = regexPattern.matcher(rawStr);

      // loop through matches merging unmatched and highlighted matched output
      while (match.find()) {
        final int start = match.start();
        final int end = match.end();

        // add in next unmatched segment followed by formatted matched segment
        if (start > lastEnd) {
          fmtStr.append(noHTML(rawStr.substring(lastEnd, start))).append(htmlHighlighter).append(noHTML(rawStr.substring(start, end))).append("</font>");
        }
        else {
          // rarely executed....
          // a contiguous or overlapping matched segment; ignore the duplicate sub-segment, and insert the remainder ahead of the </font> tag
          if (fmtStr.length() > 12) {
            fmtStr.replace(fmtStr.length() - 7, fmtStr.length() - 1, ""); // remove prior </font>, skip if this is first pass
          }
          else {
            fmtStr.append(htmlHighlighter); // first pass match at start of string
          }
          fmtStr.append(noHTML(rawStr.substring(lastEnd, end))).append("</font>");
        }

        lastEnd = end;
      }

      // final unmatched string, if any
      if (rawStr.length() > lastEnd) {
        fmtStr.append(noHTML(rawStr.substring(lastEnd)));
      }

      return fmtStr.toString();
    }

    /**
     * Initialise a Pattern for subsequent Matcher / Matches
     * @param searchString - Regex search string
     * @return Pattern for searches, with an applied default
     */
    private Pattern setupRegexSearch(String searchString) {

      final int flags = searchParameters.isMatchCase() ? 0 : CASE_INSENSITIVE;

      // "non-regex" search string still has a use for regex
      if (searchParameters.isOptNormal()) {
        // matching on whatever is provided
        return Pattern.compile(searchString, flags + Pattern.LITERAL);
      }

      // pre-pack regex for word start search
      if (searchParameters.isOptWord()) {
        try {
          // matching on a word boundary.
          return Pattern.compile("\\b\\Q" + searchString + "\\E", flags);
        }
        catch (PatternSyntaxException e) {
          // something went wrong - a \E in the search input followed by invalid Regex will end up here
          logger.error("~" + Resources.getString("Editor.search_badWord", noHTML(e.getMessage()))); //NON-NLS
          return null;
        }
      }

      // search string is full regex
      try {
        return Pattern.compile(searchString, flags);
      }
      catch (PatternSyntaxException e) {
        chat("~" + Resources.getString("Editor.search_badRegex", noHTML(e.getMessage()))); //NON-NLS
        return null;
      }
    }
  }

  /**
   * Called when the Configurable held by a node has been edited.
   * Set the edit status and re-build the Jtree label
   * @param target Edited Configurable
   */
  public void nodeEdited(Configurable target) {
    final ConfigureTreeNode node = (ConfigureTreeNode) getTreeNode(target);
    node.setEdited(true);
    notifyUpdate(target);
    ((DefaultTreeModel) getModel()).nodeChanged(node);
  }

  /**
   * Custom TreeNode
   *  - Determine description for the node
   *  - Track this node has been edited
   */
  private static class ConfigureTreeNode extends DefaultMutableTreeNode {
    private static final long serialVersionUID = 1L;

    private boolean edited;

    public ConfigureTreeNode(Object userObject) {
      super(userObject);
      edited = false;
    }

    @Override
    public String toString() {
      String description = "";

      final Configurable c =  (Configurable) getUserObject();
      if (c != null) {
        description = (c.getConfigureName() != null ? c.getConfigureName() : "");
        if (c instanceof GlobalProperty) {
          final String desc = ((GlobalProperty)c).getDescription();
          if (!desc.isEmpty()) {
            description += " - " + desc;
          }
        }

        if (c instanceof GlobalTranslatableMessage) {
          final String desc = ((GlobalTranslatableMessage)c).getDescription();
          if (!desc.isEmpty()) {
            description += " - " + desc;
          }
        }
        description += " [" + getConfigureName(c.getClass()) + "]";

        if (c instanceof ComponentDescription) {
          final String desc = ((ComponentDescription) c).getDescription();
          if ((desc != null) && !desc.isEmpty()) {
            description += " - " + desc;
          }
        }

        if (c instanceof PrototypeDefinition) {
          final GamePiece p = ((PrototypeDefinition) c).getPiece();
          if (p != null) {
            final String basicName = (String) (p.getProperty(BasicPiece.BASIC_NAME));
            if ((basicName != null) && !basicName.isEmpty()) {
              description += " - " + basicName;
            }
          }
        }

        if (c instanceof ChangePropertyButton) {
          final ChangePropertyButton cpb = (ChangePropertyButton)c;
          final String desc = cpb.getAttributeValueString(ChangePropertyButton.DESCRIPTION);
          if ((desc != null) && !desc.isEmpty()) {
            description += " - " + desc;
          }
        }
      }
      return description;
    }

    public boolean isEdited() {
      return edited;
    }

    public void setEdited(boolean edited) {
      this.edited = edited;
    }

    /**
     * Reset the edit flags on this node and all Children of this node
     */
    private void resetEditFlags() {
      setEdited(false);
      resetChildEditFlags();
    }

    /**
     * Reset the edit flags on all Children of this node
     */
    public void resetChildEditFlags() {
      if (getChildCount() > 0) {
        for (final TreeNode node : children) {
          ((ConfigureTreeNode) node).resetEditFlags();
        }
      }
    }
  }

  /**
   * Allows ExtensionEditor to override and control what indexes are allowed
   */
  public int checkMinimumIndex(DefaultMutableTreeNode targetNode, int index) {
    return index;
  }

  /**
   * Tree Transfer Handler provides drag-and-drop support for editor items. Heavily adapted from Oracle "tutorial" and
   * various StackOverflow and Code Ranch articles, but yeah, google ftw. (Brian Reynolds Apr 4 2021)
   */
  class TreeTransferHandler extends TransferHandler {
    private static final long serialVersionUID = 1L;

    DataFlavor nodesFlavor;
    DataFlavor[] flavors = new DataFlavor[1];

    public TreeTransferHandler() {
      try {
        final String mimeType = DataFlavor.javaJVMLocalObjectMimeType +
          ";class=\"" +
          DefaultMutableTreeNode[].class.getName() +
          "\"";
        nodesFlavor = new DataFlavor(mimeType);
        flavors[0] = nodesFlavor;
      }
      catch (ClassNotFoundException e) {
        logger.error("Class Not Found: " + e.getMessage()); //NON-NLS
      }
    }

    /**
     * @param support info on the drag/drop in question (called continuously as item is dragged over various targets)
     * @return true if drag/drop is "legal", false if the "No Drag For You!" icon should be shown
     */
    @Override
    public boolean canImport(TransferHandler.TransferSupport support) {
      if (!support.isDrop()) {
        return false;
      }
      support.setShowDropLocation(true);
      if (!support.isDataFlavorSupported(nodesFlavor)) {
        return false;
      }

      // Do not allow a drop on the drag source selections.
      final JTree.DropLocation dl = (JTree.DropLocation)support.getDropLocation();
      final JTree tree = (JTree)support.getComponent();
      final int dropRow = tree.getRowForPath(dl.getPath());
      final int[] selRows = tree.getSelectionRows();
      if (selRows == null) {
        return false;
      }
      for (final int selRow : selRows) {
        if (selRow == dropRow) {
          return false;
        }
      }

      // Only allow drag to a valid parent (same logic as our regular cut-and-paste)
      final TreePath dest = dl.getPath();
      final DefaultMutableTreeNode targetNode = (DefaultMutableTreeNode)dest.getLastPathComponent();
      final TreePath path = tree.getPathForRow(selRows[0]);
      final DefaultMutableTreeNode firstNode = (DefaultMutableTreeNode)path.getLastPathComponent();
      final Configurable target = (Configurable)targetNode.getUserObject();
      if (!isValidPasteTarget(target, firstNode)) {
        return false;
      }

      // If we're editing an extension, components owned by the module can never be dragged
      if (tree instanceof ExtensionTree) {
        final ExtensionTree xTree = (ExtensionTree) tree;
        if (!xTree.isEditable(firstNode)) {
          return false;
        }
      }

      final int action = support.getDropAction();
      // Don't allow move (cut) to our own descendant
      return action != MOVE || !targetNode.isNodeAncestor(firstNode);
    }

    /**
     * @param c The component being dragged
     * @return Package describing data to be moved/copied
     */
    @Override
    protected Transferable createTransferable(JComponent c) {
      final JTree tree = (JTree)c;
      final TreePath[] paths = tree.getSelectionPaths();
      if (paths != null) {
        // Array w/ node to be transferred
        final DefaultMutableTreeNode node = (DefaultMutableTreeNode)paths[0].getLastPathComponent();
        final DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[1];
        nodes[0] = node;
        return new NodesTransferable(nodes);
      }
      return null;
    }

    /**
     * Nothing to do here, as we do our removal in the import stage (original algorithm made a copy and then deleted
     * the source, but that would make our "unique ID" code barf a mighty barf)
     */
    @Override
    protected void exportDone(JComponent source, Transferable data, int action) {
      //
    }

    @Override
    public int getSourceActions(JComponent c) {
      return COPY_OR_MOVE;
    }

    /**
     * Actually performs the data transfer for a move or copy operation
     * @param support Info on the drag being performed
     * @return true if drag/drop operation was successful
     */
    @Override
    public boolean importData(TransferHandler.TransferSupport support) {
      if (!canImport(support)) {
        return false;
      }

      // Extract transfer data.
      DefaultMutableTreeNode[] nodes = null;
      try {
        final Transferable t = support.getTransferable();
        nodes = (DefaultMutableTreeNode[])t.getTransferData(nodesFlavor);
      }
      catch (UnsupportedFlavorException ufe) {
        logger.error("Unsupported Flavor: " + ufe.getMessage()); //NON-NLS
      }
      catch (IOException ioe) {
        logger.error("I/O error: " + ioe.getMessage()); //NON-NLS
      }
      catch (InvalidDnDOperationException id) {
        logger.error("Invalid DND Operation: " + id.getMessage()); //NON-NLS
      }

      if (nodes == null) {
        return false;
      }

      // Get drop location info.
      final JTree.DropLocation dl = (JTree.DropLocation)support.getDropLocation();
      final TreePath dest = dl.getPath();
      final DefaultMutableTreeNode targetNode = (DefaultMutableTreeNode)dest.getLastPathComponent();
      final Configurable target = (Configurable)targetNode.getUserObject();
      final DefaultMutableTreeNode sourceNode = nodes[0];

      // What new index shall we drop it at? -1 means it was dropped directly on the parent item.
      int childIndex = dl.getChildIndex();
      if (childIndex < 0) {
        if (sourceNode.getParent() == targetNode) {
          childIndex = 0; // If we've been dragged up to our same parent then drop at top of list.
        }
        else {
          childIndex = targetNode.getChildCount(); // Otherwise we drop it at the bottom
        }
      }

      // This is for Extension editor to override
      childIndex = checkMinimumIndex(targetNode, childIndex);

      if (childIndex > targetNode.getChildCount()) {
        childIndex = targetNode.getChildCount();
      }

      if ((support.getDropAction() & MOVE) == MOVE) {
        if (!targetNode.isNodeAncestor(sourceNode)) {
          // Here's we're "moving", therefore "cutting" the source object from its original location (plain drag)
          final Configurable cutObj = (Configurable) sourceNode.getUserObject();
          final Configurable convertedCutObj = convertChild(target, cutObj);

          if (sourceNode.getParent() == targetNode) {
            final int oldIndex = targetNode.getIndex(sourceNode);
            //BR// If we're being dragged to our same parent, but lower down the list, adjust index to account for the
            //BR// fact we're about to be cut from it. Such humiliations are the price of keeping the Unique ID manager
            //BR// copacetic.
            if (childIndex > oldIndex) {
              childIndex--;
            }
            // If just moving to same place, report success without doing anything.
            if (childIndex == oldIndex) {
              return true;
            }
          }

          if (remove(getParent(sourceNode), cutObj)) {
            postRemoveProcessing(getParent(sourceNode), cutObj);
            insert(target, convertedCutObj, childIndex);
            postInsertProcessing(target, convertedCutObj);
            postPasteFixups(convertedCutObj);
          }
        }
      }
      else {
        // Or, if we're actually making a new copy (ctrl-drag)
        final Configurable copyBase = (Configurable) sourceNode.getUserObject();
        Configurable clone = null;
        try {
          clone = convertChild(target, copyBase.getClass().getConstructor().newInstance());
        }
        catch (Throwable t) {
          ReflectionUtils.handleNewInstanceFailure(t, copyBase.getClass());
        }

        if (clone != null) {
          clone.build(copyBase.getBuildElement(Builder.createNewDocument()));
          insert(target, clone, childIndex);
          postInsertProcessing(target, clone);
          updateGpIds(clone);
        }
      }

      return true;
    }

    @Override
    public String toString() {
      return getClass().getName();
    }

    /**
     * Self-important data blurb that describes a potential drag/drop source. But there's stuff
     * about flavor, and who doesn't like flavor?
     */
    public class NodesTransferable implements Transferable {
      DefaultMutableTreeNode[] nodes;

      public NodesTransferable(DefaultMutableTreeNode[] nodes) {
        this.nodes = nodes;
      }

      @SuppressWarnings("NullableProblems") //BR// Because I just have no idea
      @Override
      public Object getTransferData(DataFlavor flavor)
        throws UnsupportedFlavorException {
        if (!isDataFlavorSupported(flavor)) {
          throw new UnsupportedFlavorException(flavor);
        }
        return nodes;
      }

      @Override
      public DataFlavor[] getTransferDataFlavors() {
        return flavors;
      }

      @Override
      public boolean isDataFlavorSupported(DataFlavor flavor) {
        return nodesFlavor.equals(flavor);
      }
    }
  }

  // ExtensionTree to override
  protected void postInsertProcessing(Configurable parent, Configurable child) {

  }

  // ExtensionTree to override
  protected void postRemoveProcessing(Configurable parent, Configurable child) {

  }
}
