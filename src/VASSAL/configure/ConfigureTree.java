/*
 * $Id$
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
import java.util.ArrayList;
import java.util.Collection;
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
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
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

import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Plugin;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.widget.PieceSlot;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateAction;

/**
 * This is the Configuration Tree that appears in the Configuration window
 * when editing a VASSAL module. Each node in the tree structure is a
 * {@link VASSAL.build.Configurable} object, whose child nodes are obtained
 * via {@link VASSAL.build.Configurable#getConfigureComponents}.
 */
public class ConfigureTree extends JTree implements PropertyChangeListener, MouseListener, MouseMotionListener, TreeSelectionListener {
  private static final long serialVersionUID = 1L;

  protected Map<Configurable, DefaultMutableTreeNode> nodes =
    new HashMap<Configurable, DefaultMutableTreeNode>();
  protected DefaultMutableTreeNode copyData;
  protected DefaultMutableTreeNode cutData;
  protected HelpWindow helpWindow;
  protected Configurable selected;
  protected int selectedRow;
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
  protected KeyStroke helpKey;
  protected KeyStroke propertiesKey;
  protected KeyStroke translateKey;
  protected Action cutAction;
  protected Action copyAction;
  protected Action pasteAction;
  protected Action deleteAction;
  protected Action moveAction;
  protected Action propertiesAction;
  protected Action translateAction;
  protected Action helpAction;
  protected JMenuItem cutItem;
  protected JMenuItem copyItem;
  protected JMenuItem pasteItem;
  protected JMenuItem deleteItem;
  protected JMenuItem moveItem;
  protected JMenuItem propertiesItem;
  protected JMenuItem translateItem;
  public static java.awt.Font POPUP_MENU_FONT =
    new java.awt.Font("Dialog", 0, 11);
  protected JMenu editMenu;
  protected static List<AdditionalComponent> additionalComponents =
    new ArrayList<AdditionalComponent>();
  
  /** Creates new ConfigureTree */
  public ConfigureTree(Configurable root, HelpWindow helpWindow) {
    toggleClickCount = 3;
    this.helpWindow = helpWindow;
    setShowsRootHandles(true);
    setModel(new DefaultTreeModel(buildTreeNode(root)));
    setCellRenderer(buildRenderer());
    addMouseListener(this);
    addMouseMotionListener(this);
    addTreeSelectionListener(this);
    moveCmd = Resources.getString("Editor.move"); //$NON-NLS-1$
    deleteCmd = Resources.getString("Editor.delete"); //$NON-NLS-1$
    pasteCmd = Resources.getString("Editor.paste"); //$NON-NLS-1$
    copyCmd = Resources.getString("Editor.copy"); //$NON-NLS-1$
    cutCmd = Resources.getString("Editor.cut"); //$NON-NLS-1$
    propertiesCmd = Resources.getString("Editor.ModuleEditor.properties"); //$NON-NLS-1$
    translateCmd = Resources.getString("Editor.ModuleEditor.translate"); //$NON-NLS-1$
    helpCmd = Resources.getString("Editor.ModuleEditor.component_help"); //$NON-NLS-1$
    int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    cutKey = KeyStroke.getKeyStroke(KeyEvent.VK_X, mask);
    copyKey = KeyStroke.getKeyStroke(KeyEvent.VK_C, mask);
    pasteKey = KeyStroke.getKeyStroke(KeyEvent.VK_V, mask);
    deleteKey = KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0);
    moveKey = KeyStroke.getKeyStroke(KeyEvent.VK_M, mask);
    propertiesKey = KeyStroke.getKeyStroke(KeyEvent.VK_P, mask);
    translateKey = KeyStroke.getKeyStroke(KeyEvent.VK_T, mask);
    helpKey = KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0);
    copyAction = new KeyAction(copyCmd, copyKey);
    pasteAction = new KeyAction(pasteCmd, pasteKey);
    cutAction = new KeyAction(cutCmd, cutKey);
    deleteAction = new KeyAction(deleteCmd, deleteKey);
    moveAction = new KeyAction(moveCmd, moveKey);
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
    getActionMap().put(cutCmd, cutAction);
    getActionMap().put(copyCmd, copyAction);
    getActionMap().put(pasteCmd, pasteAction);
    this.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
  }
  class KeyAction extends AbstractAction {
    private static final long serialVersionUID = 1L;
    protected String actionName;

    public KeyAction(String name, KeyStroke key) {
      super(name);
      actionName = name;
      putValue(Action.ACCELERATOR_KEY, key);
    }

    public void actionPerformed(ActionEvent e) {
      doKeyAction(actionName);
    }
  }

  protected Renderer buildRenderer() {
    return new Renderer();
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
    final ArrayList<Action> l = new ArrayList<Action>();
    l.add(buildEditAction(target));
    l.add(buildEditPiecesAction(target));
    addActionGroup(popup, l);
    l.add(buildTranslateAction(target));
    addActionGroup(popup, l);
    l.add(buildHelpAction(target));
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
    addAction(popup, buildImportAction(target));
    return popup;
  }

  protected Action buildMoveAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction(moveCmd) {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), true);
          d.setTitle(target.getConfigureName() == null ? moveCmd : moveCmd + " " + target.getConfigureName());
          d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
          Box box = Box.createHorizontalBox();
          box.add(new JLabel("Move to position"));
          box.add(Box.createHorizontalStrut(10));
          final JComboBox select = new JComboBox();
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
    Action a = new AbstractAction(pasteCmd) {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        if (cutData != null) {
          DefaultMutableTreeNode targetNode = getTreeNode(target);
          if (remove(getParent(cutData), (Configurable) cutData.getUserObject())) {
            insert(target, (Configurable) cutData.getUserObject(), targetNode.getChildCount());
          }
        }
        else if (copyData != null) {
          try {
            Configurable copyBase = (Configurable) copyData.getUserObject();
            Configurable clone = copyBase.getClass().newInstance();
            clone.build(copyBase.getBuildElement(Builder.createNewDocument()));
            insert(target, clone, getTreeNode(target).getChildCount());
            updateGpIds(clone);
          }
          catch (InstantiationException e1) {
            e1.printStackTrace();
            JOptionPane.showMessageDialog(getTopLevelAncestor(), "Cannot copy " + getConfigureName(target), "Copy failed", JOptionPane.ERROR_MESSAGE);
          }
          catch (IllegalAccessException e1) {
            e1.printStackTrace();
            JOptionPane.showMessageDialog(getTopLevelAncestor(), "Cannot copy " + getConfigureName(target), "Copy failed", JOptionPane.ERROR_MESSAGE);
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
    return (cutData != null && isValidParent(target, (Configurable) cutData.getUserObject()))
        || (copyData != null && isValidParent(target, (Configurable) copyData.getUserObject()));
  }

  /**
   * Allocate new PieceSlot Id's to any PieceSlot sub-components
   * 
   * @param c Configurable to update
   */
  protected void updateGpIds(Configurable c) {
  if (c instanceof PieceSlot) {
    ((PieceSlot) c).updateGpId(GameModule.getGameModule());
  }
  else {
      Configurable[] components = c.getConfigureComponents();
      for (int i=0; i < components.length; updateGpIds(components[i++])) {
      }
    }
  }
  
  protected Action buildImportAction(final Configurable target) {
    Action a = new AbstractAction("Add Imported Class") {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent evt) {
        final Configurable child = importConfigurable();
        if (child != null) {
          try {
            child.build(null);
            final Configurable c = target;
            if (child.getConfigurer() != null) {
              PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                public void save() {
                  super.save();
                  insert(c, child, getTreeNode(c).getChildCount());
                }

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
          catch (Exception ex) {
            JOptionPane.showMessageDialog(getTopLevelAncestor(), "Error adding " + getConfigureName(child) + " to " + getConfigureName(target) + "\n"
                + ex.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    };
    return a;
  }

  protected Collection<Action> buildAddActionsFor(final Configurable target) {
    final ArrayList<Action> l = new ArrayList<Action>();
    for (Class newConfig : target.getAllowableConfigureComponents()) {
      final Action action = buildAddAction(target, newConfig);
      l.add(action);
    }

    for (AdditionalComponent add : additionalComponents) {
      if (target.getClass().equals(add.getParent())) {
        final Class newConfig = add.getChild();
        final Action action = buildAddAction(target, newConfig);
        l.add(action);
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

  protected Action buildAddAction(final Configurable target, final Class newConfig) {
    AbstractAction action = new AbstractAction("Add " + getConfigureName(newConfig)) {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent evt) {
        try {
          final Configurable child = (Configurable) newConfig.newInstance();
          child.build(null);
          if (child instanceof PieceSlot) {
            ((PieceSlot) child).updateGpId(GameModule.getGameModule());
          }
          final Configurable c = target;
          if (child.getConfigurer() != null) {
            if (insert(target, child, getTreeNode(target).getChildCount())) {
              PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                public void save() {
                  super.save();
                }

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
        catch (Exception err) {
          err.printStackTrace();
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

        public void actionPerformed(ActionEvent evt) {
          try {
            Configurable clone = target.getClass().newInstance();
            clone.build(target.getBuildElement(Builder.createNewDocument()));
            insert(getParent(targetNode), clone, targetNode.getParent().getIndex(targetNode) + 1);
          }
          catch (Exception err) {
            err.printStackTrace();
            JOptionPane.showMessageDialog(getTopLevelAncestor(), "Cannot clone " + getConfigureName(target), "Clone failed", JOptionPane.ERROR_MESSAGE);
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
    Class[] sub = target.getAllowableConfigureComponents();
    for (int i = 0; i < sub.length; ++i) {
      if (VASSAL.build.widget.PieceSlot.class.isAssignableFrom(sub[i])) {
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
      return true;
    }
    catch (IllegalBuildException err) {
      JOptionPane.showMessageDialog(getTopLevelAncestor(), "Cannot delete " + getConfigureName(child) + " from " + getConfigureName(parent) + "\n"
          + err.getMessage(), "Illegal configuration", JOptionPane.ERROR_MESSAGE);
      return false;
    }
  }

  protected boolean insert(Configurable parent, Configurable child, int index) {
    DefaultMutableTreeNode childNode = buildTreeNode(child);
    DefaultMutableTreeNode parentNode = getTreeNode(parent);
    Configurable[] oldContents = parent.getConfigureComponents();
    boolean succeeded = true;
    ArrayList<Configurable> moveToBack = new ArrayList<Configurable>();
    for (int i = index; i < oldContents.length; ++i) {
      try {
        oldContents[i].removeFrom(parent);
        parent.remove(oldContents[i]);
      }
      catch (IllegalBuildException err) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't insert " + getConfigureName(child) + " before " + getConfigureName(oldContents[i]),
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
      child.addTo(parent);
      parent.add(child);
      parentNode.insert(childNode, index);
      int[] childI = new int[1];
      childI[0] = index;
      ((DefaultTreeModel) getModel()).nodesWereInserted(parentNode, childI);
    }
    catch (IllegalBuildException err) {
      JOptionPane.showMessageDialog(getTopLevelAncestor(), "Can't add " + getConfigureName(child) + "\n" + err.getMessage(), "Illegal configuration",
          JOptionPane.ERROR_MESSAGE);
      succeeded = false;
    }
    for (Configurable c : moveToBack) {
      parent.add(c);
      c.addTo(parent);
    }
    return succeeded;
  }

  public void propertyChange(PropertyChangeEvent evt) {
    DefaultMutableTreeNode newValue = getTreeNode((Configurable) evt.getSource());
    ((DefaultTreeModel) getModel()).nodeChanged(newValue);
  }
  static class Renderer extends javax.swing.tree.DefaultTreeCellRenderer {
    private static final long serialVersionUID = 1L;

    public Renderer() {
    }

    public java.awt.Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
      Configurable c = null;
      if (value instanceof DefaultMutableTreeNode) {
        c = (Configurable) ((DefaultMutableTreeNode) value).getUserObject();
        leaf = c.getAllowableConfigureComponents().length == 0;
      }
      super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
      if (c != null) {
        setText((c.getConfigureName() != null ? c.getConfigureName() : "") + " [" + getConfigureName(c.getClass()) + "]");
      }
      return this;
    }
  }

  public static String getConfigureName(Class<?> c) {
    try {
      return (String) c.getMethod("getConfigureTypeName", new Class[0]).invoke(null, new Object[0]);
    }
    catch (Exception err) {
      return c.getName().substring(c.getName().lastIndexOf(".") + 1);
    }
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
    String className = JOptionPane.showInputDialog(getTopLevelAncestor(), "Enter fully-qualified name of Java class to import");
    Configurable config = null;
    if (className != null) {
      config = null;
      try {
        Class c = GameModule.getGameModule().getDataArchive().loadClass(className);
        Object o = c.newInstance();
        if (o instanceof Configurable) {
          config = (Configurable) o;
        }
        else {
          JOptionPane.showMessageDialog(getTopLevelAncestor(), "Class must implement the Configurable interface.", "Class error", JOptionPane.ERROR_MESSAGE);
        }
      }
      catch (ClassNotFoundException noClass) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Couldn't find class.\nClass file must exist in module zipfile with correct package structure.",
            "Class not found", JOptionPane.ERROR_MESSAGE);
      }
      catch (InstantiationException iex) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Couldn't instantiate class.\nClass must have a no-argument constructor.",
            "Class not initialized", JOptionPane.ERROR_MESSAGE);
      }
      catch (IllegalAccessException ilex) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Error accessing class", "Access error", JOptionPane.ERROR_MESSAGE);
      }
      catch (NoSuchMethodError noMethod) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Couldn't instantiate class.\nClass must have a no-argument constructor.",
            "Class not initialized", JOptionPane.ERROR_MESSAGE);
      }
    }
    return config;
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
    Configurable target = getTarget(e.getX(), e.getY());
    if (target != null) {
      if (e.getClickCount() == 2 && !e.isMetaDown()) {
        if (target.getConfigurer() != null) {
          Action a = buildEditAction(target);
          if (a != null) {
            a.actionPerformed(new ActionEvent(e.getSource(), ActionEvent.ACTION_PERFORMED, "Edit"));
          }
        }
      }
      else if (e.isMetaDown()) {
        setSelectionRow(getClosestRowForLocation(e.getX(), e.getY()));
        JPopupMenu popup = buildPopupMenu(target);
        popup.show(ConfigureTree.this, e.getX(), e.getY());
        popup.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
          public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
            repaint();
          }

          public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
            repaint();
          }

          public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
          }
        });
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

  public void mouseDragged(MouseEvent evt) {
  }

  protected boolean isValidParent(Configurable parent, Configurable child) {
    if (parent != null && child != null) {
      Class c[] = parent.getAllowableConfigureComponents();
      for (int i = 0; i < c.length; ++i) {
        if (c[i] == child.getClass()) {
          return true;
        }
      }
    }
    return false;
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

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

  public JMenu getEditMenu() {
    if (editMenu == null) {
      editMenu = new JMenu(Resources.getString(Resources.EDIT));
      deleteItem = new JMenuItem(deleteAction);
      editMenu.add(deleteItem);
      cutItem = new JMenuItem(cutAction);
      editMenu.add(cutItem);
      copyItem = new JMenuItem(copyAction);
      editMenu.add(copyItem);
      pasteItem = new JMenuItem(pasteAction);
      editMenu.add(pasteItem);
      moveItem = new JMenuItem(moveAction);
      editMenu.add(moveItem);
      editMenu.addSeparator();
      propertiesItem = new JMenuItem(propertiesAction);
      editMenu.add(propertiesItem);
      translateItem = new JMenuItem(translateAction);
      editMenu.add(translateItem);
      updateEditMenu();
    }
    return editMenu;
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
    if (editMenu != null) {
      deleteItem.setEnabled(selected != null);
      cutItem.setEnabled(selected != null);
      cutAction.setEnabled(selected != null);
      copyItem.setEnabled(selected != null);
      copyAction.setEnabled(selected != null);
      pasteItem.setEnabled(selected != null && isValidPasteTarget(selected));
      pasteAction.setEnabled(selected != null && isValidPasteTarget(selected));
      moveItem.setEnabled(selected != null);
      propertiesItem.setEnabled(selected != null && selected.getConfigurer() != null);
      translateItem.setEnabled(selected != null);
    }
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
  public static void addAdditionalComponent(Class parent, Class child) {
    additionalComponents.add(new AdditionalComponent(parent, child));
  }
  
  protected static class AdditionalComponent {
    Class parent;
    Class child;
    
    public AdditionalComponent(Class p, Class c) {
      parent = p;
      child = c;
    }
    
    public Class getParent() {
      return parent;
    }
    
    public Class getChild() {
      return child;
    }     
  }
}
