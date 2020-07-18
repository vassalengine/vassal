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

import java.awt.Component;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Plugin;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.build.module.properties.ZoneProperty;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.MassPieceLoader;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateAction;
import VASSAL.launch.EditorWindow;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReflectionUtils;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;

/**
 * This is the Configuration Tree that appears in the Configuration window
 * when editing a VASSAL module. Each node in the tree structure is a
 * {@link VASSAL.build.Configurable} object, whose child nodes are obtained
 * via {@link VASSAL.build.Configurable#getConfigureComponents}.
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
  
  // Search parameters
  protected String searchString;
  protected boolean matchCase;
  protected boolean matchNames;
  protected boolean matchTypes;

  public static Font POPUP_MENU_FONT = new Font("Dialog", 0, 11);
  protected static List<AdditionalComponent> additionalComponents = new ArrayList<>();

  /** Creates new ConfigureTree */
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
    propertiesCmd = Resources.getString("Editor.ModuleEditor.properties"); //$NON-NLS-1$
    translateCmd = Resources.getString("Editor.ModuleEditor.translate"); //$NON-NLS-1$
    helpCmd = Resources.getString("Editor.ModuleEditor.component_help"); //$NON-NLS-1$
    int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx();
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
    
    //FIXME: remember these search parameters from session to session
    searchString="";
    matchCase = false;
    matchTypes = true;
    matchNames = true;
  }

  public JFrame getFrame() {
    return editorWindow;
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
  
  
  private Chatter chatter;
  
  private void chat (String text) {
    if (chatter == null) {
      chatter = GameModule.getGameModule().getChatter();
    }
    if (chatter != null) {
      chatter.show("- " + text);
    }
  }
  

  protected Configurable getTarget(int x, int y) {
    TreePath path = getPathForLocation(x, y);
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
    for (Configurable child : children) {
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

  private void addActionGroup(JPopupMenu menu, ArrayList<Action> l) {
    boolean empty = true;
    for (Action a : l) {
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
    for (Action a : buildAddActionsFor(target)) {
      addAction(popup, a);
    }
    if (hasChild(target, PieceSlot.class) || hasChild(target, CardSlot.class)) {
      addAction(popup, buildMassPieceLoaderAction(target));
    }
    addAction(popup, buildImportAction(target));
    return popup;
  }
  
  
  // Enumerates our configure tree in preparation for searching it
  private final List<DefaultMutableTreeNode> getSearchNodes(DefaultMutableTreeNode root) {
    List<DefaultMutableTreeNode> searchNodes = new ArrayList<DefaultMutableTreeNode>();

    Enumeration<?> e = root.preorderEnumeration();
    while(e.hasMoreElements()) {
      searchNodes.add((DefaultMutableTreeNode)e.nextElement());
    }
    return searchNodes;
  }
  
  
  // Checks a single string against our search parameters
  public final boolean checkString(String target, String searchString) {
    if (matchCase) {
      return target.contains(searchString);
    } 
    else {
      return target.toLowerCase().contains(searchString.toLowerCase());
    }         
  }
  
  
  // Checks a node of the tree against our search parameters
  public final boolean checkNode(DefaultMutableTreeNode node, String searchString) {
    Configurable c = null;
    c = (Configurable) node.getUserObject();

    if (matchNames) {
      String objectName = c.getConfigureName();
      if (objectName != null && checkString(objectName, searchString)) {
        return true;
      }
    }
    
    if (matchTypes) {
      String className = getConfigureName(c.getClass());
      if (className != null && checkString(className, searchString)) {
        return true;
      }
    }
    
    return false;
  }
  
  
  // Search through the tree, starting at the currently selected location (and wrapping around if needed)
  // Compare nodes until we find our search string (or have searched everything we can search)
  public final DefaultMutableTreeNode findNode(String searchString) {
    List<DefaultMutableTreeNode> searchNodes = getSearchNodes((DefaultMutableTreeNode)getModel().getRoot());
    DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode)getLastSelectedPathComponent();

    DefaultMutableTreeNode foundNode = null;
    int bookmark = -1;    

    if (currentNode != null) {
      for (int index = 0; index < searchNodes.size(); index++) {
        if (searchNodes.get(index) == currentNode) {
          bookmark = index;
          break;
        }
      }
    }

    for (int index = bookmark + 1; index < searchNodes.size(); index++) {  
      if (checkNode(searchNodes.get(index), searchString)) {
        foundNode = searchNodes.get(index);    
        break;
      }
    }

    if (foundNode == null) {
      for (int index = 0; index <= bookmark; index++) {    
        if (checkNode(searchNodes.get(index), searchString)) {
          foundNode = searchNodes.get(index);    
          break;
        }
      }
    }
    
    return foundNode;
  }   
  
    
  // Search action - runs search dialog box, then searches
  protected Action buildSearchAction(final Configurable target) {
    final Action a = new AbstractAction(searchCmd) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        
        final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), true);
        d.setTitle(searchCmd);
        d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
        Box box = Box.createHorizontalBox();
        box.add(new JLabel("String to find: "));
        box.add(Box.createHorizontalStrut(10));
        
        final JTextField search = new JTextField(searchString, 32);
        box.add(search);
        
        d.add(box);

        box = Box.createHorizontalBox();
        final JCheckBox sensitive = new JCheckBox(Resources.getString("Editor.search_case"), matchCase);
        box.add(sensitive);
        final JCheckBox names = new JCheckBox(Resources.getString("Editor.search_names"), matchNames);
        box.add(names);        
        final JCheckBox types = new JCheckBox(Resources.getString("Editor.search_types"), matchTypes);
        box.add(types);        
        d.add(box);
        
        box = Box.createHorizontalBox();     
        JButton find = new JButton(Resources.getString("Editor.search_next"));
        find.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            searchString = search.getText();
            matchCase    = sensitive.isSelected();
            matchNames   = names.isSelected();
            matchTypes   = types.isSelected();
            
            if (!matchNames && !matchTypes) {              
              matchNames = true;
              names.setSelected(true);
              chat (Resources.getString("Editor.search_all_off"));                            
            }
            
            if (!searchString.isEmpty()) {              
              DefaultMutableTreeNode node = findNode(searchString);                
              if (node != null) {
                TreePath path = new TreePath(node.getPath());
                setSelectionPath(path);
                scrollPathToVisible(path);
              } 
              else {
                chat (Resources.getString("Editor.search_none_found") + searchString);
              }
            }            
          }
        });
        
        
        JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
        cancel.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            d.dispose();
          }
        });
        
        box.add(find);
        box.add(cancel);
        d.add(box);
        
        d.getRootPane().setDefaultButton(find); // Enter key activates search
        
        // Esc Key cancels
        KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        int w = JComponent.WHEN_IN_FOCUSED_WINDOW;
        d.getRootPane().registerKeyboardAction(ee -> d.dispose(), k, w);
        
        d.pack();
        d.setLocationRelativeTo(d.getParent());
        d.setVisible(true);                
      }
    };
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
          Box box = Box.createHorizontalBox();
          box.add(new JLabel("Move to position"));
          box.add(Box.createHorizontalStrut(10));
          final JComboBox<String> select = new JComboBox<>();
          TreeNode parentNode = getTreeNode(target).getParent();
          for (int i = 0; i < parentNode.getChildCount(); ++i) {
            Configurable c = (Configurable) ((DefaultMutableTreeNode) parentNode.getChildAt(i)).getUserObject();
            String name = (c.getConfigureName() != null ? c.getConfigureName() : "") + " [" + getConfigureName(c.getClass()) + "]";
            select.addItem((i + 1) + ":  " + name);
          }
          final DefaultMutableTreeNode targetNode = getTreeNode(target);
          final int currentIndex = targetNode.getParent().getIndex(targetNode);
          select.setSelectedIndex(currentIndex);
          box.add(select);
          JButton ok = new JButton(Resources.getString(Resources.OK));
          ok.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
              int index = select.getSelectedIndex();
              if (currentIndex != index) {
                Configurable parent = getParent(targetNode);
                if (remove(parent, target)) {
                  insert(parent, target, index);
                }
              }
              d.dispose();
            }
          });
          d.add(box);
          d.add(ok);
          d.pack();
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
   * @param parent
   * @param child
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
    for (Class<?> allowableClass : allowableClasses) {
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
      for (Configurable comp : c.getConfigureComponents()) updateGpIds(comp);
    }
  }

  protected Action buildImportAction(final Configurable target) {
    Action a = new AbstractAction("Add Imported Class") {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent evt) {
        final Configurable child = importConfigurable();
        if (child != null) {
          try {
            child.build(null);
            final Configurable c = target;
            if (child.getConfigurer() != null) {
              PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                @Override
                public void save() {
                  super.save();
                  insert(c, child, getTreeNode(c).getChildCount());
                }

                @Override
                public void cancel() {
                  dispose();
                }
              };
              w.setVisible(true);
            }
            else {
              insert(c, child, getTreeNode(c).getChildCount());
            }
          }
          // FIXME: review error message
          catch (Exception ex) {
            JOptionPane.showMessageDialog(getTopLevelAncestor(), "Error adding " + getConfigureName(child) + " to " + getConfigureName(target) + "\n"
                + ex.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    };
    return a;
  }


  protected Action buildMassPieceLoaderAction(final Configurable target) {
    Action a = null;
    final ConfigureTree tree = this;
    if (getTreeNode(target).getParent() != null) {
      String desc = "Add Multiple " + (hasChild(target, CardSlot.class) ? "Cards" : "Pieces");
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
    for (Class<?> c : parent.getAllowableConfigureComponents()) {
      if (c.equals(childClass)) {
        return true;
      }
    }
    return false;
  }

  protected List<Action> buildAddActionsFor(final Configurable target) {
    final ArrayList<Action> l = new ArrayList<>();
    for (Class<? extends Buildable> newConfig :
            target.getAllowableConfigureComponents()) {
      l.add(buildAddAction(target, newConfig));
    }

    for (AdditionalComponent add : additionalComponents) {
      if (target.getClass().equals(add.getParent())) {
        final Class<? extends Buildable> newConfig = add.getChild();
        l.add(buildAddAction(target, newConfig));
      }
    }
    return l;
  }

  /**
   * @deprecated Use {@link #buildAddActionsFor(final Configurable)} instead.
   */
  @Deprecated
  protected Enumeration<Action> buildAddActions(final Configurable target) {
    return Collections.enumeration(buildAddActionsFor(target));
  }

  protected Action buildAddAction(final Configurable target, final Class<? extends Buildable> newConfig) {
    AbstractAction action = new AbstractAction("Add " + getConfigureName(newConfig)) {
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

          final Configurable c = target;
          if (child.getConfigurer() != null) {
            if (insert(target, child, getTreeNode(target).getChildCount())) {
              PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                @Override
                public void save() {
                  super.save();
                }

                @Override
                public void cancel() {
                  ConfigureTree.this.remove(c, child);
                  dispose();
                }
              };
              w.setVisible(true);
            }
          }
          else {
            insert(c, child, getTreeNode(c).getChildCount());
          }
        }
      }
    };
    return action;
  }

  protected Action buildHelpAction(final Configurable target) {
    Action showHelp;
    HelpFile helpFile = target.getHelpFile();
    if (helpFile == null) {
      showHelp = new ShowHelpAction(null,null);
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
      return new AbstractAction("Clone") {
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
    DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) targetNode.getParent();
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
          int row = selectedRow;
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
    Action a = new TranslateAction(target, helpWindow, this);
    a.setEnabled(target.getI18nData().isTranslatable());
    return a;
  }

  public boolean canContainGamePiece(final Configurable target) {
    boolean canContainPiece = false;
    for (Class<?> c : target.getAllowableConfigureComponents()) {
      if (VASSAL.build.widget.PieceSlot.class.isAssignableFrom(c)) {
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
      JOptionPane.showMessageDialog(getTopLevelAncestor(), "Cannot delete " + getConfigureName(child) + " from " + getConfigureName(parent) + "\n"
          + err.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE);
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
    DefaultMutableTreeNode childNode = buildTreeNode(theChild);
    DefaultMutableTreeNode parentNode = getTreeNode(parent);
    Configurable[] oldContents = parent.getConfigureComponents();
    boolean succeeded = true;
    ArrayList<Configurable> moveToBack = new ArrayList<>();
    for (int i = index; i < oldContents.length; ++i) {
      try {
        oldContents[i].removeFrom(parent);
        parent.remove(oldContents[i]);
      }
      // FIXME: review error message
      catch (IllegalBuildException err) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't insert " + getConfigureName(theChild) + " before " + getConfigureName(oldContents[i]),
            "Illegal configuration", JOptionPane.ERROR_MESSAGE);
        for (int j = index; j < i; ++j) {
          parent.add(oldContents[j]);
          oldContents[j].addTo(parent);
        }
        return false;
      }
      moveToBack.add(oldContents[i]);
    }
    try {
      theChild.addTo(parent);
      parent.add(theChild);
      parentNode.insert(childNode, index);
      int[] childI = new int[1];
      childI[0] = index;
      ((DefaultTreeModel) getModel()).nodesWereInserted(parentNode, childI);
    }
    // FIXME: review error message
    catch (IllegalBuildException err) {
      JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't add " + getConfigureName(child) + "\n" + err.getMessage(), "Illegal configuration",
          JOptionPane.ERROR_MESSAGE);
      succeeded = false;
    }

    for (Configurable c : moveToBack) {
      parent.add(c);
      c.addTo(parent);
    }

    notifyStateChanged(true);
    return succeeded;
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    DefaultMutableTreeNode newValue = getTreeNode((Configurable) evt.getSource());
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
   * @param the class whose configure name will be returned
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
      "Enter fully-qualified name of Java class to import");

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

    ErrorDialog.show("Error.not_a_configurable", className);
    return null;
  }

  protected void maybePopup(MouseEvent e) {
    Configurable target = getTarget(e.getX(), e.getY());
    if (target == null) {
      return;
    }

    setSelectionRow(getClosestRowForLocation(e.getX(), e.getY()));
    JPopupMenu popup = buildPopupMenu(target);
    popup.show(ConfigureTree.this, e.getX(), e.getY());
    popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
      @Override
      public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
        repaint();
      }

      @Override
      public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
        repaint();
      }

      @Override
      public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
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
    else if (e.getClickCount() == 2 && SwingUtils.isLeftMouseButton(e)) {
      Configurable target = getTarget(e.getX(), e.getY());
      if (target == null) {
        return;
      }

      if (target.getConfigurer() != null) {
        Action a = buildEditAction(target);
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
      for (Class<?> aClass : c) {
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
    DefaultMutableTreeNode node = getTreeNode(target);
    Configurable parent = getParent(node);
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
   * @param type
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
    mm.addAction("Editor.ModuleEditor.properties", propertiesAction);
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
    DefaultMutableTreeNode targetNode = (DefaultMutableTreeNode) this.getLastSelectedPathComponent();
    if (targetNode != null) {
      Configurable target = (Configurable) targetNode.getUserObject();
      Action a = null;
      if (cutCmd.equals(action)) {
        a = buildCutAction(target);
      }
      else if (copyCmd.equals(action)) {
        a = buildCopyAction(target);
      }
      else if (pasteCmd.equals(action) || action.equals(pasteKey.getKeyChar())) {
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
    TreePath path = e.getPath();
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
   * @param target
   * @return parent
   */
  protected Configurable getParent(Configurable target) {
    DefaultMutableTreeNode parentNode = (DefaultMutableTreeNode) getTreeNode(target).getParent();
    return (Configurable) parentNode.getUserObject();
  }

  /**
   * Record additional available components to add to the popup menu.
   *
   * @param parent
   * @param child
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
}
