/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import VASSAL.build.module.KeyNamer;
import VASSAL.search.SearchTarget;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import java.awt.Component;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.IntStream;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import net.miginfocom.swing.MigLayout;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Plugin;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.build.module.properties.ZoneProperty;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.MassPieceLoader;
import VASSAL.counters.Properties;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateAction;
import VASSAL.launch.EditorWindow;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReflectionUtils;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * The beating heart of the Editor, this class handles the Configuration Tree
 * that appears in the Configuration window when editing a VASSAL module. Each
 * node in the tree structure is a {@link VASSAL.build.Configurable} object,
 * whose child nodes are obtained via {@link VASSAL.build.Configurable#getConfigureComponents}.
 *
 * When we're running as the Extension Editor, this is subclassed by {@link ExtensionTree}, which
 * overrides some methods to handle extension-specific differences.
 */
public class ConfigureTree extends JTree implements PropertyChangeListener, MouseListener, MouseMotionListener, TreeSelectionListener {
  private static final long serialVersionUID = 1L;

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
  protected KeyStroke cutKey;
  protected KeyStroke copyKey;
  protected KeyStroke pasteKey;
  protected KeyStroke deleteKey;
  protected KeyStroke moveKey;
  protected KeyStroke searchKey;
  protected KeyStroke helpKey;
  protected KeyStroke propertiesKey;
  protected KeyStroke translateKey;
  protected Action cutAction;
  protected Action copyAction;
  protected Action pasteAction;
  protected Action deleteAction;
  protected Action moveAction;
  protected Action searchAction;
  protected Action propertiesAction;
  protected Action translateAction;
  protected Action helpAction;

  protected JDialog searchDialog;
  protected JTextField searchField;
  protected JCheckBox searchAdvanced;

  private final SearchParameters searchParameters;
  protected static Chatter chatter;

  public static final Font POPUP_MENU_FONT = new Font(Font.DIALOG, Font.PLAIN, 11);
  protected static final List<AdditionalComponent> additionalComponents = new ArrayList<>();

  /**
   * Creates new ConfigureTree
   */
  public ConfigureTree(Configurable root, HelpWindow helpWindow) {
    this(root, helpWindow, null);
  }

  public ConfigureTree(Configurable root, HelpWindow helpWindow, EditorWindow editorWindow) {
    toggleClickCount = 3;
    this.helpWindow = helpWindow;
    this.editorWindow = editorWindow;
    setShowsRootHandles(true);
    setModel(new DefaultTreeModel(buildTreeNode(root)));
    setCellRenderer(buildRenderer());
    addMouseListener(this);
    addMouseMotionListener(this);
    addTreeSelectionListener(this);
    searchCmd = Resources.getString("Editor.search"); //$NON-NLS-1$
    moveCmd = Resources.getString("Editor.move"); //$NON-NLS-1$
    deleteCmd = Resources.getString("Editor.delete"); //$NON-NLS-1$
    pasteCmd = Resources.getString("Editor.paste"); //$NON-NLS-1$
    copyCmd = Resources.getString("Editor.copy"); //$NON-NLS-1$
    cutCmd = Resources.getString("Editor.cut"); //$NON-NLS-1$
    propertiesCmd = Resources.getString("Editor.properties"); //$NON-NLS-1$
    translateCmd = Resources.getString("Editor.ModuleEditor.translate"); //$NON-NLS-1$
    helpCmd = Resources.getString("Editor.ModuleEditor.component_help"); //$NON-NLS-1$
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
    copyAction = new KeyAction(copyCmd, copyKey);
    pasteAction = new KeyAction(pasteCmd, pasteKey);
    cutAction = new KeyAction(cutCmd, cutKey);
    deleteAction = new KeyAction(deleteCmd, deleteKey);
    moveAction = new KeyAction(moveCmd, moveKey);
    searchAction = new KeyAction(searchCmd, searchKey);
    propertiesAction = new KeyAction(propertiesCmd, propertiesKey);
    translateAction = new KeyAction(translateCmd, translateKey);
    helpAction = new KeyAction(helpCmd, helpKey);
    /*
     * Cut, Copy and Paste will not work unless I add them to the JTree input and action maps. Why??? All the others
     * work fine.
     */
    getInputMap().put(cutKey, cutCmd);
    getInputMap().put(copyKey, copyCmd);
    getInputMap().put(pasteKey, pasteCmd);
    getInputMap().put(deleteKey, deleteCmd);
    getActionMap().put(cutCmd, cutAction);
    getActionMap().put(copyCmd, copyAction);
    getActionMap().put(pasteCmd, pasteAction);
    getActionMap().put(deleteCmd, deleteAction);
    this.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

    searchParameters = new SearchParameters();

    final TreePath path = new TreePath(((DefaultMutableTreeNode) (getModel().getRoot())).getPath());
    setSelectionPath(path);
    scrollPathToVisible(path);

    chatter = GameModule.getGameModule().getChatter();

    createKeyBindings();
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
              a.actionPerformed(new ActionEvent(ae.getSource(), ActionEvent.ACTION_PERFORMED, "Edit"));
            }
          }
        }
        else {
          setExpandedState(path, true);
        }
      }
    });
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
    final DefaultMutableTreeNode node = new DefaultMutableTreeNode(c);
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
      menu.add(a).setFont(POPUP_MENU_FONT);
    }
  }

  private void addActionGroup(JPopupMenu menu, List<Action> l) {
    boolean empty = true;
    for (final Action a : l) {
      if (a != null) {
        menu.add(a).setFont(POPUP_MENU_FONT);
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
    final ArrayList<Action> l = new ArrayList<>();
    l.add(buildEditAction(target));
    l.add(buildEditPiecesAction(target));
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
    for (final Action a : buildAddActionsFor(target)) {
      addAction(popup, a);
    }
    if (hasChild(target, PieceSlot.class) || hasChild(target, CardSlot.class)) {
      addAction(popup, buildMassPieceLoaderAction(target));
    }
    addAction(popup, buildImportAction(target));
    return popup;
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


  /**
   * @return Search action - runs search dialog box, then searches
   */
  protected Action buildSearchAction(final Configurable target) {
    final Action a = new SearchAction(this, searchParameters);
    a.setEnabled(true);
    return a;
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
          SwingUtils.repack(d);
          d.setLocationRelativeTo(d.getParent());
          d.setVisible(true);
        }
      };
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
    }
    return a;
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
          if (remove(getParent(cutData), cutObj)) {
            insert(target, convertedCutObj, targetNode.getChildCount());
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
          }
        }
        cutData = null;
        updateEditMenu();
      }
    };
    a.setEnabled(isValidPasteTarget(target));
    return a;
  }

  protected boolean isValidPasteTarget(Configurable target) {
    return (cutData != null &&
            isValidParent(target, (Configurable) cutData.getUserObject())) ||
           (copyData != null &&
            isValidParent(target, (Configurable) copyData.getUserObject()));
  }

  /**
   * Some components need to be converted to a new type before insertion.
   *
   * Currently this is used to allow cut and paste of CardSlots and PieceSlots
   * between Decks and GamePiece Palette components.
   *
   * @param parent Parent Configurable
   * @param child Child Configurable
   * @return new Child
   */
  protected Configurable convertChild(Configurable parent, Configurable child) {
    if (child.getClass() == PieceSlot.class && isAllowedChildClass(parent, CardSlot.class)) {
      return new CardSlot((PieceSlot) child);
    }
    else if (child.getClass() == CardSlot.class && isAllowedChildClass(parent, PieceSlot.class)) {
      return new PieceSlot((CardSlot) child);
    }
    else {
      return child;
    }
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

  protected Action buildImportAction(final Configurable target) {
    return new AbstractAction(Resources.getString("Editor.ConfigureTree.add_imported_class")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        final Configurable child = importConfigurable();
        if (child != null) {
          try {
            child.build(null);
            if (child.getConfigurer() != null) {
              final PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                @Override
                public void save() {
                  super.save();
                  insert(target, child, getTreeNode(target).getChildCount());
                }

                @Override
                public void cancel() {
                  dispose();
                }
              };
              w.setVisible(true);
            }
            else {
              insert(target, child, getTreeNode(target).getChildCount());
            }
          }
          // FIXME: review error message
          catch (Exception ex) {
            JOptionPane.showMessageDialog(getTopLevelAncestor(), "Error adding " + getConfigureName(child) + " to " + getConfigureName(target) + "\n" //NON-NLS
                + ex.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE); //NON-NLS
          }
        }
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
    final ArrayList<Action> l = new ArrayList<>();
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

  /**
   * @deprecated Use {@link #buildAddActionsFor(Configurable)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected Enumeration<Action> buildAddActions(final Configurable target) {
    ProblemDialog.showDeprecated("2020-08-06");
    return Collections.enumeration(buildAddActionsFor(target));
  }

  protected Action buildAddAction(final Configurable target, final Class<? extends Buildable> newConfig) {
    return new AbstractAction(Resources.getString("Editor.ConfigureTree.add_component", getConfigureName(newConfig))) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        Configurable ch = null;
        try {
          ch = (Configurable) newConfig.getConstructor().newInstance();
        }
        catch (Throwable t) {
          ReflectionUtils.handleNewInstanceFailure(t, newConfig);
        }

        if (ch != null) {
          final Configurable child = ch;
          child.build(null);

          if (child instanceof PieceSlot) {
            ((PieceSlot) child).updateGpId(GameModule.getGameModule());
          }

          if (child.getConfigurer() != null) {
            if (insert(target, child, getTreeNode(target).getChildCount())) {
              // expand the new node
              final TreePath path = new TreePath(getTreeNode(child).getPath());
              expandPath(path);

              final PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                @Override
                public void cancel() {
                  ConfigureTree.this.remove(target, child);
                  dispose();
                }
              };
              w.setVisible(true);
            }
          }
          else {
            insert(target, child, getTreeNode(target).getChildCount());
          }
        }
      }
    };
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
    final Configurable parent = getParent(targetNode);
    if (targetNode.getParent() != null) {
      return new AbstractAction(deleteCmd) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent evt) {
          final int row = selectedRow;
          remove(parent, target);
          if (row < getRowCount()) {
            setSelectionRow(row);
          }
          else {
            setSelectionRow(row - 1);
          }
        }
      };
    }
    else {
      return null;
    }
  }

  protected Action buildEditPiecesAction(final Configurable target) {
    if (canContainGamePiece(target)) {
      return new EditContainedPiecesAction(target);
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

  protected boolean remove(Configurable parent, Configurable child) {
    try {
      child.removeFrom(parent);
      parent.remove(child);
      ((DefaultTreeModel) getModel()).removeNodeFromParent(getTreeNode(child));
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
    Configurable theChild = child;
    // Convert subclasses of GlobalProperty to an actual GlobalProperty before inserting into the GlobalProperties container
    if (parent.getClass() == GlobalProperties.class && child.getClass() == ZoneProperty.class) {
      theChild = new GlobalProperty((GlobalProperty) child);
    }
    if (parent.getClass() == Zone.class && child.getClass() == GlobalProperty.class) {
      theChild = new ZoneProperty((GlobalProperty) child);
    }
    final DefaultMutableTreeNode childNode = buildTreeNode(theChild);
    final DefaultMutableTreeNode parentNode = getTreeNode(parent);
    final Configurable[] oldContents = parent.getConfigureComponents();
    final ArrayList<Configurable> moveToBack = new ArrayList<>();
    for (int i = index; i < oldContents.length; ++i) {
      try {
        oldContents[i].removeFrom(parent);
        parent.remove(oldContents[i]);
      }
      // FIXME: review error message
      catch (IllegalBuildException err) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't insert " + getConfigureName(theChild) + " before " + getConfigureName(oldContents[i]), //NON-NLS
            "Illegal configuration", JOptionPane.ERROR_MESSAGE); //NON-NLS
        for (int j = index; j < i; ++j) {
          parent.add(oldContents[j]);
          oldContents[j].addTo(parent);
        }
        return false;
      }
      moveToBack.add(oldContents[i]);
    }

    boolean succeeded = true;
    try {
      theChild.addTo(parent);
      parent.add(theChild);
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
      c.addTo(parent);
    }

    notifyStateChanged(true);
    return succeeded;
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    final DefaultMutableTreeNode newValue = getTreeNode((Configurable) evt.getSource());
    ((DefaultTreeModel) getModel()).nodeChanged(newValue);
  }

  static class Renderer extends javax.swing.tree.DefaultTreeCellRenderer {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
      if (value instanceof DefaultMutableTreeNode) {
        final Configurable c =
          (Configurable) ((DefaultMutableTreeNode) value).getUserObject();
        if (c != null) {
          leaf = c.getAllowableConfigureComponents().length == 0;
          value = (c.getConfigureName() != null ? c.getConfigureName() : "") +
                  " [" + getConfigureName(c.getClass()) + "]";
        }
      }

      return super.getTreeCellRendererComponent(
        tree, value, sel, expanded, leaf, row, hasFocus
      );
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
          a.actionPerformed(new ActionEvent(e.getSource(), ActionEvent.ACTION_PERFORMED, "Edit"));
        }
      }
    }
  }

  /*
   * protected void performDrop(Configurable target) { DefaultMutableTreeNode dragNode = getTreeNode(dragging);
   * DefaultMutableTreeNode targetNode = getTreeNode(target); Configurable parent = null; int index = 0; if
   * (isValidParent(target, dragging)) { parent = target; index = targetNode.getChildCount(); if (dragNode.getParent() ==
   * targetNode) { index--; } } else if (targetNode.getParent() != null && isValidParent(getParent(targetNode),
   * dragging)) { parent = (Configurable) ((DefaultMutableTreeNode) targetNode.getParent()).getUserObject(); index =
   * targetNode.getParent().getIndex(targetNode); } if (parent != null) { remove(getParent(dragNode), dragging);
   * insert(parent, dragging, index); } dragging = null; }
   */
  public DefaultMutableTreeNode getTreeNode(Configurable target) {
    return nodes.get(target);
  }

  @Override
  public void mouseDragged(MouseEvent evt) {
  }


  protected boolean isValidParent(Configurable parent, Configurable child) {
    if (parent != null && child != null) {
      final Class<?>[] c = parent.getAllowableConfigureComponents();
      for (final Class<?> aClass : c) {
        if (aClass.isAssignableFrom(child.getClass()) ||
                ((aClass == CardSlot.class) && (child.getClass() == PieceSlot.class)) || // Allow PieceSlots to be pasted to Decks
                ((aClass == ZoneProperty.class) && (child.getClass() == GlobalProperty.class)) // Allow Global Properties to be saved as Zone Properties
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
      selected = (Configurable) ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
      selectedRow = getRowForPath(path);
      updateEditMenu();
    }
  }

  protected void updateEditMenu() {
    deleteAction.setEnabled(selected != null);
    cutAction.setEnabled(selected != null);
    copyAction.setEnabled(selected != null);
    pasteAction.setEnabled(selected != null && isValidPasteTarget(selected));
    moveAction.setEnabled(selected != null);
    searchAction.setEnabled(true);
    propertiesAction.setEnabled(selected != null &&
                                selected.getConfigurer() != null);
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
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_NAMES,  null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_TYPES,  null, true));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_ADVANCED, null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_TRAITS,      null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_EXPRESSIONS, null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_PROPERTIES, null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_KEYS,        null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_MENUS, null, false));
      prefs.addOption(null, new BooleanConfigurer(SearchParameters.MATCH_MESSAGES, null, false));

      searchString = (String) prefs.getValue(SearchParameters.SEARCH_STRING);
      matchCase    = (Boolean)prefs.getValue(SearchParameters.MATCH_CASE);
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
    public SearchParameters(String searchString, boolean matchCase, boolean matchNames, boolean matchTypes, boolean matchAdvanced, boolean matchTraits, boolean matchExpressions, boolean matchProperties, boolean matchKeys, boolean matchMenus, boolean matchMessages) {
      this.searchString = searchString;
      this.matchCase    = matchCase;
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
      return Objects.hash(getSearchString(), isMatchCase(), isMatchNames(), isMatchTypes(), isMatchAdvanced(),
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
      }
      else {
        d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, configureTree), false);
        configureTree.setSearchDialog(d);

        d.setTitle(configureTree.getSearchCmd());

        final JLabel searchLabel = new JLabel("String to find: ");
        search = new JTextField(searchParameters.getSearchString(), 32);
        configureTree.setSearchField(search);
        search.select(0, searchParameters.getSearchString().length()); // Pre-select all the search text when opening the dialog
        searchLabel.setLabelFor(search);

        final JCheckBox sensitive = new JCheckBox(Resources.getString("Editor.search_case"), searchParameters.isMatchCase());
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
            new SearchParameters(search.getText(), sensitive.isSelected(), names.isSelected(), types.isSelected(), true, traits.isSelected(), expressions.isSelected(), properties.isSelected(), keys.isSelected(), menus.isSelected(), messages.isSelected());

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

          if (!searchParameters.getSearchString().isEmpty()) {
            if (anyChanges) {
              // Unless we're just continuing to the next match in an existing search, compute & display hit count
              final int matches = getNumMatches(searchParameters.getSearchString());
              chat(matches + " " + Resources.getString("Editor.search_count") + searchParameters.getSearchString());
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
              chat(Resources.getString("Editor.search_none_found") + searchParameters.getSearchString());
            }
          }
        });

        final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
        cancel.addActionListener(e1 -> configureTree.getSearchDialog().setVisible(false));

        d.setLayout(new MigLayout("insets dialog, nogrid", "", "[]unrel[]unrel:push[]")); //$NON-NLS-1$//

        // top row
        d.add(searchLabel, "align right, gapx rel"); //$NON-NLS-1$//
        d.add(search, "pushx, growx, wrap"); //$NON-NLS-1$//

        // options row
        d.add(sensitive, "align center, gapx unrel, span"); //$NON-NLS-1$//
        d.add(advanced, "wrap"); //NON-NLS

        // Advanced 1
        d.add(names, "align center, gapx unrel, span"); //$NON-NLS-1$//
        d.add(types, "wrap"); //$NON-NLS-1$//

        // Advanced 2
        d.add(traits, "align center, gapx unrel, span"); //$NON-NLS-1$//
        d.add(expressions, "gapx unrel"); //$NON-NLS-1$//
        d.add(properties, "wrap"); //$NON-NLS-1$//

        // Advanced 3
        d.add(keys, "align center, gapx unrel, span"); //$NON-NLS-1$//
        d.add(menus, "gapx unrel"); //$NON-NLS-1$//
        d.add(messages, "wrap"); //$NON-NLS-1$//

        // buttons row
        d.add(find, "tag ok, split"); //$NON-NLS-1$//
        d.add(cancel, "tag cancel"); //$NON-NLS-1$//

        d.getRootPane().setDefaultButton(find); // Enter key activates search

        // Esc Key cancels
        final KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        d.getRootPane().registerKeyboardAction(ee -> configureTree.getSearchDialog().setVisible(false), k, JComponent.WHEN_IN_FOCUSED_WINDOW);
      }

      search.requestFocus(); // Start w/ focus in search string field

      if (!d.isVisible()) {
        SwingUtils.repack(d);
        d.setLocationRelativeTo(d.getParent());
        d.setVisible(true);
      }
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

      if (!searchParameters.isMatchAdvanced()) {
        return false;
      }

      // From here down we are only searching inside of SearchTarget objects (Piece/Prototypes, or searchable AbstractConfigurables)
      GamePiece p;
      boolean protoskip;
      if (c instanceof GamePiece) {
        p = (GamePiece)c;
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

      p = Decorator.getInnermost(p); // Head to the innermost trait, which would be the BasicPiece in a regular piece i.e. the "top" of the list.
      do {
        if (!protoskip) { // Skip the fake "Basic Piece" on a Prototype definition
          if (searchParameters.isMatchTraits()) {
            if (p instanceof EditablePiece) {
              final String desc = ((EditablePiece) p).getDescription();
              if ((desc != null) && checkString(desc, searchString)) {
                return true;
              }
            }
          }

          if (p instanceof SearchTarget) {
            if (checkSearchTarget((SearchTarget)p, searchString)) {
              return true;
            }
          }
        }
        protoskip = false;
        p = (GamePiece)p.getProperty(Properties.OUTER); // Continue traversing traits list from inner to outer
      } while (p != null);

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
        chat("&nbsp;&nbsp;&nbsp;&nbsp;{" + show + "} " + s); //NON-NLS
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
      final String matchString = "<b><u>Matches for " + name + ": </u></b>";

      final SearchTarget st = (SearchTarget) c;
      final String item = getConfigureName(c.getClass());

      final TargetProgress progress = new TargetProgress();
      stringListHits(searchParameters.isMatchExpressions(), st.getExpressionList(),      searchString, matchString, item, "", "Expression",    progress);
      stringListHits(searchParameters.isMatchProperties(),  st.getPropertyList(),        searchString, matchString, item, "", "Property",      progress);
      stringListHits(searchParameters.isMatchMenus(),       st.getMenuTextList(),        searchString, matchString, item, "", "UI Text",       progress);
      stringListHits(searchParameters.isMatchMessages(),    st.getFormattedStringList(), searchString, matchString, item, "", "Message/Field", progress);

      keyListHits(searchParameters.isMatchKeys(),           st.getNamedKeyStrokeList(),  searchString, matchString, item, "", "KeyCommand",    progress);
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
      final String matchString = "<b><u>Matches for " + name + ": </u></b>";

      // We're going to search Decorator from inner-to-outer (BasicPiece-on-out), so that user sees the traits hit in
      // the same order they're listed in the PieceDefiner window.

      p = Decorator.getInnermost(p); // Head to the innermost trait, which would be the BasicPiece in a regular piece i.e. the "top" of the list.
      do {
        if (!protoskip && (p instanceof EditablePiece) && (p instanceof Decorator)) { // Skip the fake "Basic Piece" on a Prototype definition
          final String desc = ((EditablePiece) p).getDescription();
          final Decorator d = (Decorator)p;
          progress.startNewTrait();    // A new trait, so reset our "trait progress".

          if (searchParameters.isMatchTraits()) {
            if ((desc != null) && checkString(desc, searchString)) {
              progress.checkShowTrait(matchString, "Trait", desc);
            }
          }

          stringListHits(searchParameters.isMatchExpressions(), d.getExpressionList(),      searchString, matchString, "Trait", desc, "Expression",    progress);
          stringListHits(searchParameters.isMatchProperties(),  d.getPropertyList(),        searchString, matchString, "Trait", desc, "Property",      progress);
          stringListHits(searchParameters.isMatchMenus(),       d.getMenuTextList(),        searchString, matchString, "Trait", desc, "UI Text",       progress);
          stringListHits(searchParameters.isMatchMessages(),    d.getFormattedStringList(), searchString, matchString, "Trait", desc, "Message/Field", progress);

          keyListHits(searchParameters.isMatchKeys(),           d.getNamedKeyStrokeList(),  searchString, matchString, "Trait", desc, "KeyCommand",    progress);
        }
        protoskip = false;
        p = (GamePiece)p.getProperty(Properties.OUTER); // Continue traversing traits list from inner to outer
      } while (p != null);
    }

    /**
     * Checks a single string against our search parameters
     * @param target - string to check
     * @param searchString - our search string
     * @return true if this is a match based on our "matchCase" checkbox
     */
    private boolean checkString(String target, String searchString) {
      if (searchParameters.isMatchCase()) {
        return target.contains(searchString);
      }
      else {
        return target.toLowerCase().contains(searchString.toLowerCase());
      }
    }
  }
}
