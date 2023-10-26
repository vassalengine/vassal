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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DropMode;
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
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.KeyStroke;
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
import java.awt.event.KeyEvent;
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
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.IntStream;

/**
 * The beating heart of the Editor, this class handles the Configuration Tree
 * that appears in the Configuration window when editing a VASSAL module. Each
 * node in the tree structure is a {@link VASSAL.build.Configurable} object,
 * whose child nodes are obtained via {@link VASSAL.build.Configurable#getConfigureComponents}.
 *
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
  protected JCheckBox searchAdvanced;

  private final SearchParameters searchParameters;
  protected static Chatter chatter;

  @Deprecated(since = "2022-08-08", forRemoval = true)
  public static final Font POPUP_MENU_FONT = new Font(Font.DIALOG, Font.PLAIN, 11);
  protected static final List<AdditionalComponent> additionalComponents = new ArrayList<>();

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
    deleteKey = KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0);
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

  protected void setSearchAdvanced(JCheckBox searchAdvanced) {
    this.searchAdvanced = searchAdvanced;
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
    getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "Enter");
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
      putValue(Action.ACCELERATOR_KEY, key);
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
    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return false;
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
    if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) return false;

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
    catch (ParserConfigurationException | SAXException e) {
      ErrorDialog.bug(e);
      return false;
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, filename);
      return false;
    }

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
    // SetupStacks (and thus DrawPiles) pasted to a new map, but whose owning board setting doesnt exist for this map, are forced to "any"
    if (target instanceof SetupStack) {
      final SetupStack ss = (SetupStack)target;
      final String owning = ss.getOwningBoardName();
      if (owning != null) {
        if (!ss.getValidOwningBoards().contains(owning)) {
          ConfigureTree.chat(Resources.getString("Editor.convert_setupstack_or_deck", (target instanceof DrawPile) ? DrawPile.getConfigureTypeName() : SetupStack.getConfigureTypeName(), ss.getConfigureName(), owning));
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
    if (parent != null && parent.getUserObject().equals(target)) {
      return true;
    }

    return isValidParent(target, (Configurable) sourceNode.getUserObject());
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
        // Convert Mass GKC and Deck GKC to Global. Do not convert an existing Global GKC, or it subclass Startup GKC
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
   * Allocate new PieceSlot Id's to any PieceSlot sub-components
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
    final ArrayList<Action> l = new ArrayList<>();

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
   * Delete removes an item from the tree but ALSO traverses the tree throwing all the childrens' children manually out
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
    final ArrayList<Configurable> moveToBack = new ArrayList<>();
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

    return c.getName().substring(c.getName().lastIndexOf(".") + 1);
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

    if (className == null) return null;

    Object o = null;
    try {
      o = GameModule.getGameModule().getDataArchive()
        .loadClass(className).getConstructor().newInstance();
    }
    catch (Throwable t) {
      ReflectionUtils.handleImportClassFailure(t, className);
    }

    if (o == null) return null;

    if (o instanceof Configurable) return (Configurable) o;

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
    }
  }

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
    public static final String MATCH_CASE    = "matchCase"; //$NON-NLS-1$//
    public static final String MATCH_REGEX    = "matchRegex"; //$NON-NLS-1$//
    public static final String MATCH_NAMES   = "matchNames"; //$NON-NLS-1$//
    public static final String MATCH_TYPES   = "matchTypes"; //$NON-NLS-1$//
    public static final String MATCH_ADVANCED     = "matchAdvanced"; //$NON-NLS-1$//
    public static final String MATCH_TRAITS       = "matchTraits"; //$NON-NLS-1$//
    public static final String MATCH_EXPRESSIONS  = "matchExpressions"; //$NON-NLS-1$//
    public static final String MATCH_PROPERTIES   = "matchProperties"; //$NON-NLS-1$//
    public static final String MATCH_KEYS         = "matchKeys"; //$NON-NLS-1$//
    public static final String MATCH_MENUS        = "matchMenus"; //$NON-NLS-1$//
    public static final String MATCH_MESSAGES     = "matchMessages"; //$NON-NLS-1$//

    /** Current search string */
    private String searchString;

    /** True if case-sensitive */
    private boolean matchCase;

    /** True if matching on a Regular Expression */
    private boolean matchRegex;

    /** True if match configurable names */
    private boolean matchNames;

    /** True if match class names */
    private boolean matchTypes;

    /** True if using advanced search (enables subsequent items) */
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

      prefs.addOption(null, new StringConfigurer(SearchParameters.SEARCH_STRING, null, ""));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_CASE,   null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_REGEX,   null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_NAMES,  null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_TYPES,  null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_ADVANCED, null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_TRAITS,      null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_EXPRESSIONS, null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_PROPERTIES, null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_KEYS,        null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_MENUS, null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_MESSAGES, null, true));

      searchString = (String) prefs.getValue(SearchParameters.SEARCH_STRING);
      matchCase    = (Boolean)prefs.getValue(SearchParameters.MATCH_CASE);
      matchRegex    = (Boolean)prefs.getValue(SearchParameters.MATCH_REGEX);
      matchNames   = (Boolean)prefs.getValue(SearchParameters.MATCH_NAMES);
      matchTypes       = (Boolean)prefs.getValue(SearchParameters.MATCH_TYPES);
      matchAdvanced    = (Boolean)prefs.getValue(SearchParameters.MATCH_ADVANCED);
      matchTraits      = (Boolean)prefs.getValue(SearchParameters.MATCH_TRAITS);
      matchExpressions = (Boolean)prefs.getValue(SearchParameters.MATCH_EXPRESSIONS);
      matchProperties  = (Boolean)prefs.getValue(SearchParameters.MATCH_PROPERTIES);
      matchKeys        = (Boolean)prefs.getValue(SearchParameters.MATCH_KEYS);
      matchMenus       = (Boolean)prefs.getValue(SearchParameters.MATCH_MENUS);
      matchMessages    = (Boolean)prefs.getValue(SearchParameters.MATCH_MESSAGES);
    }

    /**
     * Constructs a new search parameters object
     */
    public SearchParameters(String searchString, boolean matchCase, boolean matchRegex, boolean matchNames, boolean matchTypes, boolean matchAdvanced, boolean matchTraits, boolean matchExpressions, boolean matchProperties, boolean matchKeys, boolean matchMenus, boolean matchMessages) {
      this.searchString = searchString;
      this.matchCase    = matchCase;
      this.matchRegex    = matchRegex;
      this.matchNames   = matchNames;
      this.matchTypes   = matchTypes;
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

    public boolean isMatchCase() {
      return matchCase;
    }

    public boolean isMatchRegex() {
      return matchRegex;
    }

    public void setMatchCase(boolean matchCase) {
      this.matchCase = matchCase;
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
      matchCase = searchParameters.isMatchCase();
      matchRegex = searchParameters.isMatchRegex();
      matchNames = searchParameters.isMatchNames();
      matchTypes = searchParameters.isMatchTypes();
      matchAdvanced    = searchParameters.isMatchAdvanced();
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
        prefs.setValue(MATCH_CASE, matchCase);
        prefs.setValue(MATCH_REGEX, matchRegex);
        prefs.setValue(MATCH_NAMES,       matchNames);
        prefs.setValue(MATCH_TYPES, matchTypes);
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
        isMatchRegex() == that.isMatchRegex() &&
        isMatchNames() == that.isMatchNames() &&
        isMatchTypes() == that.isMatchTypes() &&
        isMatchTraits() == that.isMatchTraits() &&
        isMatchAdvanced() == that.isMatchAdvanced() &&
        isMatchExpressions() == that.isMatchExpressions() &&
        isMatchProperties() == that.isMatchProperties() &&
        isMatchKeys() == that.isMatchKeys() &&
        isMatchMenus() == that.isMatchMenus() &&
        isMatchMessages() == that.isMatchMessages() &&
        getSearchString().equals(that.getSearchString());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getSearchString(), isMatchCase(), isMatchRegex(), isMatchNames(), isMatchTypes(), isMatchAdvanced(),
        isMatchTraits(), isMatchExpressions(), isMatchProperties(), isMatchKeys(),
        isMatchMenus(), isMatchMessages());
    }
  }

  private static class SearchAction extends AbstractAction {

    private static final long serialVersionUID = 1L;

    private final ConfigureTree configureTree;
    private final SearchParameters searchParameters;

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

        final JCheckBox sensitive = new JCheckBox(Resources.getString("Editor.search_case"), searchParameters.isMatchCase());
        final JCheckBox regex = new JCheckBox(Resources.getString("Editor.search_regex"), searchParameters.isMatchRegex());
        final JCheckBox advanced  = new JCheckBox(Resources.getString("Editor.search_advanced"), searchParameters.isMatchAdvanced());

        final JCheckBox names = new JCheckBox(Resources.getString("Editor.search_names"), searchParameters.isMatchNames());
        final JCheckBox types = new JCheckBox(Resources.getString("Editor.search_types"), searchParameters.isMatchTypes());

        final JCheckBox traits = new JCheckBox(Resources.getString("Editor.search_traits"), searchParameters.isMatchTraits());
        final JCheckBox expressions = new JCheckBox(Resources.getString("Editor.search_expressions"), searchParameters.isMatchExpressions());
        final JCheckBox properties = new JCheckBox(Resources.getString("Editor.search_properties"), searchParameters.isMatchProperties());

        final JCheckBox keys = new JCheckBox(Resources.getString("Editor.search_keys"), searchParameters.isMatchKeys());
        final JCheckBox menus = new JCheckBox(Resources.getString("Editor.search_menus"), searchParameters.isMatchMenus());
        final JCheckBox messages = new JCheckBox(Resources.getString("Editor.search_messages"), searchParameters.isMatchMessages());

        final Consumer<Boolean> visSetter = visible -> {
          names.setVisible(visible);
          types.setVisible(visible);
          traits.setVisible(visible);
          expressions.setVisible(visible);
          properties.setVisible(visible);
          keys.setVisible(visible);
          menus.setVisible(visible);
          messages.setVisible(visible);
        };

        advanced.addChangeListener(l -> {
          visSetter.accept(advanced.isSelected());
          SwingUtils.repack(configureTree.getSearchDialog());
        });

        visSetter.accept(advanced.isSelected());

        configureTree.setSearchAdvanced(advanced);

        final JButton find = new JButton(Resources.getString("Editor.search_next"));
        find.addActionListener(e12 -> {
          final SearchParameters parametersSetInDialog =
            new SearchParameters(search.getText(), sensitive.isSelected(), regex.isSelected(), names.isSelected(), types.isSelected(), true, traits.isSelected(), expressions.isSelected(), properties.isSelected(), keys.isSelected(), menus.isSelected(), messages.isSelected());

          final boolean anyChanges = !searchParameters.equals(parametersSetInDialog);

          if (anyChanges) {
            searchParameters.setFrom(parametersSetInDialog);
          }

          // If literally no search parameters are selectable, turn at least one on (and print warning)
          if (!searchParameters.isMatchNames() && !searchParameters.isMatchTypes() && searchParameters.isMatchAdvanced() && (!searchParameters.isMatchTraits() && !searchParameters.isMatchExpressions() && !searchParameters.isMatchProperties() && !searchParameters.isMatchKeys() && !searchParameters.isMatchMenus() && !searchParameters.isMatchMessages())) {
            searchParameters.setMatchNames(true);
            names.setSelected(true);
            ConfigureTree.chat(Resources.getString("Editor.search_all_off"));
          }

          if (!searchParameters.getSearchString().isEmpty() && (!searchParameters.isMatchRegex() || isValidRegex(searchParameters.getSearchString()))) {
            if (anyChanges) {
              // Unless we're just continuing to the next match in an existing search, compute & display hit count
              final int matches = getNumMatches(searchParameters.getSearchString());
              chat(matches + " " + Resources.getString("Editor.search_count") + noHTML(searchParameters.getSearchString()));
            }

            // Find first match
            final DefaultMutableTreeNode node = findNode(searchParameters.getSearchString());

            // Assuming *something* matched, scroll to it and show any "trait hits"
            if (node != null) {
              final TreePath path = new TreePath(node.getPath());
              configureTree.setSelectionPath(path);
              configureTree.scrollPathToVisible(path);
              if (searchParameters.isMatchAdvanced()) {
                showHitList(node, searchParameters.getSearchString());
              }
            }
            else {
              chat(Resources.getString("Editor.search_none_found") + noHTML(searchParameters.getSearchString()));
            }
          }
        });

        final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
        cancel.addActionListener(e1 -> configureTree.getSearchDialog().setVisible(false));

        final JButton help = new JButton(Resources.getString(Resources.HELP));
        help.addActionListener(e2 -> showSearchHelp());

        d.setLayout(new MigLayout("", "[fill]")); // NON-NLS
        final JPanel panel = new JPanel(new MigLayout("hidemode 3,wrap 1" + "," + ConfigurerLayout.STANDARD_GAPY, "[fill]")); // NON-NLS
        panel.setBorder(BorderFactory.createEtchedBorder());

        // top row
        panel.add(search);

        // options row
        panel.add(sensitive);
        panel.add(regex);
        panel.add(advanced);

        // Advanced 1
        panel.add(names);
        panel.add(types);

        // Advanced 2
        panel.add(traits);
        panel.add(expressions);
        panel.add(properties);

        // Advanced 3
        panel.add(keys);
        panel.add(menus);
        panel.add(messages);

        // buttons row
        final JPanel bPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
        bPanel.add(find, "tag ok,sg 1"); //$NON-NLS-1$//
        bPanel.add(cancel, "tag cancel,sg 1"); //$NON-NLS-1$//
        bPanel.add(help, "tag help,sg 1"); // NON-NLS
        panel.add(bPanel, "grow"); // NON-NLS

        d.add(panel, "grow"); // NON-NLS

        d.getRootPane().setDefaultButton(find); // Enter key activates search

        // Esc Key cancels
        final KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        d.getRootPane().registerKeyboardAction(ee -> configureTree.getSearchDialog().setVisible(false), k, JComponent.WHEN_IN_FOCUSED_WINDOW);
      }

      search.requestFocus(); // Start w/ focus in search string field

      if (!d.isVisible()) {
        d.setLocationRelativeTo(d.getParent());
        SwingUtils.repack(d);
        d.setVisible(true);
      }
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
     * @return the node we found, or null if none
     */
    private DefaultMutableTreeNode findNode(String searchString) {
      final List<DefaultMutableTreeNode> searchNodes =
        configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot());
      final DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode)configureTree.getLastSelectedPathComponent();

      int bookmark = -1;

      if (currentNode != null) {
        bookmark =
          IntStream
            .range(0, searchNodes.size())
            .filter(i -> searchNodes.get(i) == currentNode)
            .findFirst()
            .orElse(-1);
      }

      final Predicate<DefaultMutableTreeNode> nodeMatchesSearchString = node -> checkNode(node, searchString);

      final DefaultMutableTreeNode foundNode =
        searchNodes
          .stream()
          .skip(bookmark + 1)
          .filter(nodeMatchesSearchString)
          .findFirst()
          .orElse(null);

      if (foundNode != null) {
        return foundNode;
      }

      return
        searchNodes
          .stream()
          .limit(bookmark + 1)
          .filter(nodeMatchesSearchString)
          .findFirst()
          .orElse(null);
    }

    /**
     * @return how many total nodes match the search string
     */
    private int getNumMatches(String searchString) {
      final List<DefaultMutableTreeNode> searchNodes = configureTree.getSearchNodes((DefaultMutableTreeNode)configureTree.getModel().getRoot());
      return (int) searchNodes.stream().filter(node -> checkNode(node, searchString)).count();
    }


    /**
     * @param st - Search target (usually Decorator or AbstractConfigurable)
     * @param searchString - our search string
     * @return true if the node matches our searchString based on search configuration ("match" checkboxes)
     */
    private boolean checkSearchTarget(SearchTarget st, String searchString) {
      if (searchParameters.isMatchExpressions()) {
        final List<String> exps = st.getExpressionList();
        if (exps != null) {
          for (final String s : exps) {
            if (!StringUtils.isEmpty(s) && checkString(s, searchString)) {
              return true;
            }
          }
        }
      }

      if (searchParameters.isMatchProperties()) {
        final List<String> props = st.getPropertyList();
        if (props != null) {
          for (final String s : props) {
            if (!StringUtils.isEmpty(s) && checkString(s, searchString)) {
              return true;
            }
          }
        }
      }

      if (searchParameters.isMatchKeys()) {
        final List<NamedKeyStroke> keys = st.getNamedKeyStrokeList();
        if (keys != null) {
          for (final NamedKeyStroke k : keys) {
            if (k != null) {
              final String s = k.isNamed() ? k.getName() : KeyNamer.getKeyString(k.getStroke());
              if (!StringUtils.isEmpty(s) && checkString(s, searchString)) {
                return true;
              }
            }
          }
        }
      }

      if (searchParameters.isMatchMenus()) {
        final List<String> menus = st.getMenuTextList();
        if (menus != null) {
          for (final String s : menus) {
            if (!StringUtils.isEmpty(s) && checkString(s, searchString)) {
              return true;
            }
          }
        }
      }

      if (searchParameters.isMatchMessages()) {
        final List<String> msgs = st.getFormattedStringList();
        if (msgs != null) {
          for (final String s : msgs) {
            if (!StringUtils.isEmpty(s) && checkString(s, searchString)) {
              return true;
            }
          }
        }
      }

      return false;
    }

    /**
     * @param node - any node of our module tree
     * @param searchString - our search string
     * @return true if the node matches our searchString based on search configuration ("match" checkboxes)
     */
    private boolean checkNode(DefaultMutableTreeNode node, String searchString) {
      final Configurable c = (Configurable) node.getUserObject();

      if (searchParameters.isMatchNames() || !searchParameters.isMatchAdvanced()) {
        final String objectName = c.getConfigureName();
        if (objectName != null && checkString(objectName, searchString)) {
          return true;
        }
      }

      if (searchParameters.isMatchTypes() || !searchParameters.isMatchAdvanced()) {
        final String className = getConfigureName(c.getClass());
        if ((className != null) && checkString(className, searchString)) {
          return true;
        }
      }

      if ((searchParameters.isMatchNames() && searchParameters.isMatchTypes()) || !searchParameters.isMatchAdvanced()) {
        if (c instanceof ComponentDescription) {
          final String desc = ((ComponentDescription) c).getDescription();
          if ((desc != null) && checkString(desc, searchString)) {
            return true;
          }
        }
      }

      if (!searchParameters.isMatchAdvanced()) {
        return false;
      }

      // From here down we are only searching inside of SearchTarget objects (Piece/Prototypes, or searchable AbstractConfigurables)
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
      else if (c instanceof SearchTarget) {
        return checkSearchTarget((SearchTarget) c, searchString);
      }
      else {
        return false;
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
          if (searchParameters.isMatchTraits()) {
            if (piece instanceof EditablePiece) {
              final String desc = ((EditablePiece) piece).getDescription();
              if ((desc != null) && checkString(desc, searchString)) {
                return true;
              }
            }
          }

          if (piece instanceof SearchTarget) {
            if (checkSearchTarget((SearchTarget)piece, searchString)) {
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
    private static class TargetProgress {
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
          chat(matchString);
        }
      }

      /**
       * Checks and displays the piece header & trait/component headers, if needed
       * @param matchString our match string
       * @param desc trait description
       */
      void checkShowTrait(String matchString, String idString, String desc) {
        checkShowPiece(matchString);
        if (!traitShown) {
          traitShown = true;
          chat("&nbsp;&nbsp;{" + idString + "} " + ((desc != null) ? desc : "")); //NON-NLS
        }
      }
    }

    private void hitCheck(String s, String searchString, String matchString, String item, String desc, String show, TargetProgress progress) {
      if (!StringUtils.isEmpty(s) && checkString(s, searchString)) {
        progress.checkShowTrait(matchString, item, desc);
        chat("&nbsp;&nbsp;&nbsp;&nbsp;{" + show + "} " + noHTML(s)); //NON-NLS
      }
    }

    private void stringListHits(Boolean flag, List<String> strings, String searchString, String matchString, String item, String desc, String show, TargetProgress progress) {
      if (!flag || (strings == null)) {
        return;
      }
      for (final String s : strings) {
        hitCheck(s, searchString, matchString, item, desc, show, progress);
      }
    }

    private void keyListHits(Boolean flag, List<NamedKeyStroke> keys, String searchString, String matchString, String item, String desc, String show, TargetProgress progress) {
      if (!flag || (keys == null)) {
        return;
      }
      for (final NamedKeyStroke k : keys) {
        if (k != null) {
          final String s = k.isNamed() ? k.getName() : KeyNamer.getKeyString(k.getStroke());
          hitCheck(s, searchString, matchString, item, desc, show, progress);
        }
      }
    }

    private void showConfigurableHitList(DefaultMutableTreeNode node, String searchString) {
      final Configurable c = (Configurable) node.getUserObject();
      if (!(c instanceof SearchTarget)) {
        return;
      }

      final String name = (c.getConfigureName() != null ? c.getConfigureName() : "") +
        " [" + getConfigureName(c.getClass()) + "]";
      final String matchString = "<b><u>Matches for " + noHTML(name) + ": </u></b>"; //NON-NLS

      final SearchTarget st = (SearchTarget) c;
      final String item = getConfigureName(c.getClass());

      final TargetProgress progress = new TargetProgress();

      stringListHits(searchParameters.isMatchNames() || !searchParameters.isMatchAdvanced(), Arrays.asList(c.getConfigureName()), searchString, matchString, item, "", "Name", progress); //NON-NLS
      stringListHits(searchParameters.isMatchTypes() || !searchParameters.isMatchAdvanced(), Arrays.asList(item),                 searchString, matchString, item, "", "Type", progress); //NON-NLS

      if (c instanceof ComponentDescription) {
        stringListHits((searchParameters.isMatchNames() && searchParameters.isMatchTypes()) || !searchParameters.isMatchAdvanced(), Arrays.asList(((ComponentDescription) c).getDescription()), searchString, matchString, item, "", "Description", progress); //NON-NLS
      }

      stringListHits(searchParameters.isMatchExpressions(), st.getExpressionList(),      searchString, matchString, item, "", "Expression",    progress); //NON-NLS
      stringListHits(searchParameters.isMatchProperties(),  st.getPropertyList(),        searchString, matchString, item, "", "Property",      progress); //NON-NLS
      stringListHits(searchParameters.isMatchMenus(),       st.getMenuTextList(),        searchString, matchString, item, "", "UI Text",       progress); //NON-NLS
      stringListHits(searchParameters.isMatchMessages(),    st.getFormattedStringList(), searchString, matchString, item, "", "Message/Field", progress); //NON-NLS

      keyListHits(searchParameters.isMatchKeys(),           st.getNamedKeyStrokeList(),  searchString, matchString, item, "", "KeyCommand",    progress); //NON-NLS
    }

    /**
     * If this node contains a Game Piece of some kind, displays a list of Trait information from the piece that
     * matches our search parameters.
     * @param node - any node of our module tree
     * @param searchString - our search string
     */
    private void showHitList(DefaultMutableTreeNode node, String searchString) {
      final Configurable c = (Configurable) node.getUserObject();

      GamePiece p;
      boolean protoskip;
      if (c instanceof GamePiece) {
        p = (GamePiece)c;
        protoskip = false; // This is a "real" GamePiece so we will look at the BasicPiece too
      }
      else if (c instanceof PieceSlot) {
        p = ((PieceSlot)c).getPiece();
        protoskip = false; // This is a "real" GamePiece so we will look at the BasicPiece too
      }
      else if (c instanceof PrototypeDefinition) {
        p = ((PrototypeDefinition)c).getPiece();
        protoskip = true; // This is a prototype definition, so we will ignore the BasicPiece entry
      }
      else {
        showConfigurableHitList(node, searchString); // If no GamePiece, try regular Configurable search.
        return;
      }

      final String name = (c.getConfigureName() != null ? c.getConfigureName() : "") +
        " [" + getConfigureName(c.getClass()) + "]";

      final TargetProgress progress = new TargetProgress();
      final String matchString = "<b><u>Matches for " + name + ": </u></b>"; //NON-NLS

      stringListHits(searchParameters.isMatchNames(), Arrays.asList(c.getConfigureName()),           searchString, matchString, protoskip ? "Prototype Definition" : "Game Piece", "", "Name", progress); //NON-NLS
      stringListHits(searchParameters.isMatchTypes(), Arrays.asList(getConfigureName(c.getClass())), searchString, matchString, protoskip ? "Prototype Definition" : "Game Piece", "", "Type", progress); //NON-NLS

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
        if (!protoskip && (piece instanceof EditablePiece) && (piece instanceof Decorator)) { // Skip the fake "Basic Piece" on a Prototype definition
          final String desc = ((EditablePiece) piece).getDescription();
          final Decorator d = (Decorator)piece;
          progress.startNewTrait();    // A new trait, so reset our "trait progress".

          if (searchParameters.isMatchTraits()) {
            if ((desc != null) && checkString(desc, searchString)) {
              progress.checkShowTrait(matchString, "Trait", desc); //NON-NLS
            }
          }

          stringListHits(searchParameters.isMatchExpressions(), d.getExpressionList(), searchString, matchString, "Trait", desc, "Expression", progress); //NON-NLS
          stringListHits(searchParameters.isMatchProperties(), d.getPropertyList(), searchString, matchString, "Trait", desc, "Property", progress); //NON-NLS
          stringListHits(searchParameters.isMatchMenus(), d.getMenuTextList(), searchString, matchString, "Trait", desc, "UI Text", progress); //NON-NLS
          stringListHits(searchParameters.isMatchMessages(), d.getFormattedStringList(), searchString, matchString, "Trait", desc, "Message/Field", progress); //NON-NLS

          keyListHits(searchParameters.isMatchKeys(), d.getNamedKeyStrokeList(), searchString, matchString, "Trait", desc, "KeyCommand", progress); //NON-NLS
        }
        protoskip = false;
      }
    }

    private boolean isValidRegex(String searchString) { // avoid exceptions by checking the Regex before use
      try {
        return "".matches(searchString) || true;
      }
      catch (java.util.regex.PatternSyntaxException e) {
        chat("Search string is not a valid Regular Expression: " + e.getMessage()); //NON-NLS
        return false;
      }
    }

    /**
     * Checks a single string against our search parameters
     * @param target - string to check
     * @param searchString - our search string
     * @return true if this is a match based on our "matchCase" & "matchRegex"checkboxes.
     */
    private boolean checkString(String target, String searchString) {
      if (searchParameters.isMatchRegex()) {
        if (searchParameters.isMatchCase()) {
          return target.matches(searchString);
        }
        else {
          return target.toLowerCase().matches(searchString.toLowerCase());
        }
      }
      else {
        if (searchParameters.isMatchCase()) {
          return target.contains(searchString);
        }
        else {
          return target.toLowerCase().contains(searchString.toLowerCase());
        }
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
      if (selRows == null) return false;
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
      if (action == MOVE) {
        // Don't allow move (cut) to our own descendant
        return !targetNode.isNodeAncestor(firstNode);
      }

      return true;
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
          // Here's we're "moving", so therefore "cutting" the source object from its original location (plain drag)
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
     * Self-important data blurp that describes a potential drag/drop source. But there's stuff
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
