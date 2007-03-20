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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpWindow;

/**
 * This is the Configuration Tree that appears in the Configuration window when editing a VASSAL module.
 * Each node in the tree structure is a {@link VASSAL.build.Configurable} object, whose
 * child nodes are obtained via {@link VASSAL.build.Configurable#getConfigureComponents}
 */
public class ConfigureTree extends JTree implements PropertyChangeListener, MouseListener, MouseMotionListener {
  private static final long serialVersionUID = 1L;

  private Hashtable nodes = new Hashtable();
  protected DefaultMutableTreeNode copyData;
  protected DefaultMutableTreeNode cutData;
  protected HelpWindow helpWindow;

  public static java.awt.Font POPUP_MENU_FONT = new java.awt.Font("Dialog", 0, 11);

  /** Creates new ConfigureTree */
  public ConfigureTree(Configurable root, HelpWindow helpWindow) {
    toggleClickCount = 3;
    this.helpWindow = helpWindow;
    setShowsRootHandles(true);

    setModel(new DefaultTreeModel(buildTreeNode(root)));
    setCellRenderer(buildRenderer());
    addMouseListener(this);
    addMouseMotionListener(this);
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
    DefaultMutableTreeNode node = new DefaultMutableTreeNode(c);
    Configurable[] child = c.getConfigureComponents();
    for (int i = 0; i < child.length; ++i) {
      node.add(buildTreeNode(child[i]));
    }
    nodes.put(c, node);
    return node;
  }

  protected void addAction(JPopupMenu menu, Action a) {
    if (a != null) {
      menu.add(a).setFont(POPUP_MENU_FONT);
    }
  }

  private void addActionGroup(JPopupMenu menu, Vector v) {
    boolean empty = true;
    for (int i = 0; i < v.size(); ++i) {
      if (v.elementAt(i) != null) {
        menu.add((Action) v.elementAt(i)).setFont(POPUP_MENU_FONT);
        empty = false;
      }
    }
    if (!empty) {
      menu.addSeparator();
    }
    v.removeAllElements();
  }

  protected JPopupMenu buildPopupMenu(final Configurable target) {
    JPopupMenu popup = new JPopupMenu();
    Vector v = new Vector();
    v.addElement(buildEditAction(target));
    v.addElement(buildEditPiecesAction(target));
    addActionGroup(popup, v);

    v.addElement(buildHelpAction(target));
    addActionGroup(popup, v);

    v.addElement(buildDeleteAction(target));
    v.addElement(buildCutAction(target));
    v.addElement(buildCopyAction(target));
    v.addElement(buildPasteAction(target));
    v.addElement(buildMoveAction(target));
    addActionGroup(popup, v);

    for (Enumeration e = buildAddActions(target); e.hasMoreElements();) {
      addAction(popup, (Action) e.nextElement());
    }
    addAction(popup, buildImportAction(target));
    return popup;
  }

  protected Action buildMoveAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction("Move") {
        private static final long serialVersionUID = 1L; 

        public void actionPerformed(ActionEvent e) {
          final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ConfigureTree.this), true);
          d.setTitle(target.getConfigureName() == null ? "Move" : "Move " + target.getConfigureName());
          d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
          Box box = Box.createHorizontalBox();
          box.add(new JLabel("Move to position"));
          box.add(Box.createHorizontalStrut(10));
          final JComboBox select = new JComboBox();
          TreeNode parentNode = getTreeNode(target).getParent();
          for (int i = 0; i < parentNode.getChildCount(); ++i) {
            Configurable c = (Configurable) ((DefaultMutableTreeNode) parentNode.getChildAt(i)).getUserObject();
            String name = (c.getConfigureName() != null ?
                c.getConfigureName() : "") + " [" + getConfigureName(c.getClass()) + "]";
            select.addItem((i + 1) + ":  " + name);
          }
          final DefaultMutableTreeNode targetNode = getTreeNode(target);
          final int currentIndex = targetNode.getParent().getIndex(targetNode);
          select.setSelectedIndex(currentIndex);
          box.add(select);
          JButton ok = new JButton("Ok");
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
          d.getContentPane().add(box);
          d.getContentPane().add(ok);
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
      a = new AbstractAction("Cut") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          cutData = getTreeNode(target);
          copyData = null;
        }
      };
    }
    return a;
  }

  protected Action buildCopyAction(final Configurable target) {
    Action a = null;
    if (getTreeNode(target).getParent() != null) {
      a = new AbstractAction("Copy") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          copyData = getTreeNode(target);
          cutData = null;
        }
      };
    }
    return a;
  }

  protected Action buildPasteAction(final Configurable target) {
    Action a = new AbstractAction("Paste") {
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
          }
          catch (InstantiationException e1) {
            e1.printStackTrace();
            JOptionPane.showMessageDialog
                (getTopLevelAncestor(),
                 "Cannot copy " + getConfigureName(target),
                 "Copy failed",
                 JOptionPane.ERROR_MESSAGE);

          }
          catch (IllegalAccessException e1) {
            e1.printStackTrace();
            JOptionPane.showMessageDialog
                (getTopLevelAncestor(),
                 "Cannot copy " + getConfigureName(target),
                 "Copy failed",
                 JOptionPane.ERROR_MESSAGE);

          }
        }
        cutData = null;
      }
    };
    a.setEnabled((cutData != null && isValidParent(target, (Configurable) cutData.getUserObject()))
                 || (copyData != null && isValidParent(target, (Configurable) copyData.getUserObject())));
    return a;
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
            JOptionPane.showMessageDialog
                (getTopLevelAncestor(),
                 "Error adding " + getConfigureName(child) +
                 " to " + getConfigureName(target) + "\n" + ex.getMessage(),
                 "Illegal configuration",
                 JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    };
    return a;
  }

  protected Enumeration buildAddActions(final Configurable target) {
    Vector v = new Vector();
    Class[] allow = target.getAllowableConfigureComponents();
    for (int i = 0; i < allow.length; ++i) {
      final Class newConfig = allow[i];
      Action action = buildAddAction(target, newConfig);
      v.addElement(action);
    }
    return v.elements();
  }

  protected Action buildAddAction(final Configurable target, final Class newConfig) {
    AbstractAction action = new AbstractAction("Add " + getConfigureName(newConfig)) {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent evt) {
        try {
          final Configurable child = (Configurable) newConfig.newInstance();
          child.build(null);
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
    Action showHelp = new ShowHelpAction(helpWindow, target.getHelpFile(), null);
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
            JOptionPane.showMessageDialog
                (getTopLevelAncestor(),
                 "Cannot clone " + getConfigureName(target),
                 "Clone failed",
                 JOptionPane.ERROR_MESSAGE);
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
    final Configurable parent
        = getParent(targetNode);
    if (targetNode.getParent() != null) {
      return new AbstractAction("Delete") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent evt) {
          remove(parent, target);
        }
      };
    }
    else {
      return null;
    }
  }

  protected Action buildEditPiecesAction(final Configurable target) {
    if (canContainGamePiece(target)) {
      return new EditContainedPiecesAction(target, helpWindow);
    }
    else {
      return null;
    }
  }

  protected Action buildEditAction(final Configurable target) {
    return new EditPropertiesAction(target, helpWindow, (Frame) SwingUtilities.getAncestorOfClass(Frame.class, this), this);
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
      JOptionPane.showMessageDialog
          (getTopLevelAncestor(),
           "Cannot delete " + getConfigureName(child)
           + " from " + getConfigureName(parent)
           + "\n" + err.getMessage(),
           "Illegal configuration",
           JOptionPane.ERROR_MESSAGE);
      return false;
    }
  }

  protected boolean insert(Configurable parent, Configurable child, int index) {
    DefaultMutableTreeNode childNode = buildTreeNode(child);
    DefaultMutableTreeNode parentNode = getTreeNode(parent);
    Configurable[] oldContents = parent.getConfigureComponents();

    boolean succeeded = true;
    Vector moveToBack = new Vector();
    for (int i = index; i < oldContents.length; ++i) {
      try {
        oldContents[i].removeFrom(parent);
        parent.remove(oldContents[i]);
      }
      catch (IllegalBuildException err) {
        JOptionPane.showMessageDialog
            (getTopLevelAncestor(),
             "Can't insert " + getConfigureName(child)
             + " before "
             + getConfigureName(oldContents[i]),
             "Illegal configuration",
             JOptionPane.ERROR_MESSAGE);
        for (int j = index; j < i; ++j) {
          parent.add(oldContents[j]);
          oldContents[j].addTo(parent);
        }
        return false;
      }
      moveToBack.addElement(oldContents[i]);
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
      JOptionPane.showMessageDialog
          (getTopLevelAncestor(),
           "Can't add " + getConfigureName(child) + "\n" + err.getMessage(),
           "Illegal configuration",
           JOptionPane.ERROR_MESSAGE);
      succeeded = false;
    }
    while (moveToBack.size() > 0) {
      Configurable c = (Configurable) moveToBack.firstElement();
      parent.add(c);
      c.addTo(parent);
      moveToBack.removeElementAt(0);
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

    public java.awt.Component getTreeCellRendererComponent(javax.swing.JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
      Configurable c = null;
      if (value instanceof DefaultMutableTreeNode) {
        c = (Configurable) ((DefaultMutableTreeNode) value).getUserObject();
        leaf = c.getAllowableConfigureComponents().length == 0;
      }
      super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
      if (c != null) {
        setText((c.getConfigureName() != null ?
                 c.getConfigureName() : "") + " [" + getConfigureName(c.getClass()) + "]");
      }
      return this;
    }
  }

  public static String getConfigureName(Class c) {
    try {
      return (String) c.getMethod("getConfigureTypeName", new Class[0]).invoke(null, new Object[0]);
    }
    catch (Exception err) {
      return c.getName().substring(c.getName().lastIndexOf(".") + 1);
    }
  }

  public static String getConfigureName(Configurable c) {
    if (c.getConfigureName() != null
      && c.getConfigureName().length() > 0) {
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
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Couldn't find class.\nClass file must exist in module zipfile with correct package structure.", "Class not found", JOptionPane.ERROR_MESSAGE);
      }
      catch (InstantiationException iex) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Couldn't instantiate class.\nClass must have a no-argument constructor.", "Class not initialized", JOptionPane.ERROR_MESSAGE);
      }
      catch (IllegalAccessException ilex) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Error accessing class", "Access error", JOptionPane.ERROR_MESSAGE);
      }
      catch (NoSuchMethodError noMethod) {
        JOptionPane.showMessageDialog(getTopLevelAncestor(), "Couldn't instantiate class.\nClass must have a no-argument constructor.", "Class not initialized", JOptionPane.ERROR_MESSAGE);
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
  protected void performDrop(Configurable target) {
    DefaultMutableTreeNode dragNode = getTreeNode(dragging);
    DefaultMutableTreeNode targetNode = getTreeNode(target);
    Configurable parent = null;
    int index = 0;
    if (isValidParent(target, dragging)) {
      parent = target;
      index = targetNode.getChildCount();
      if (dragNode.getParent() == targetNode) {
        index--;
      }
    }
    else if (targetNode.getParent() != null
      && isValidParent(getParent(targetNode), dragging)) {
      parent = (Configurable) ((DefaultMutableTreeNode) targetNode.getParent()).getUserObject();
      index = targetNode.getParent().getIndex(targetNode);
    }
    if (parent != null) {
      remove(getParent(dragNode), dragging);
      insert(parent, dragging, index);
    }
    dragging = null;
  }
  */

  public DefaultMutableTreeNode getTreeNode(Configurable target) {
    return (DefaultMutableTreeNode) nodes.get(target);
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
  
  /*
   * Configurers that add or remove their own children directly should implement the Mutable interface 
   * so that ConfigureTree can refresh the changed node.
   */
  public interface Mutable {
  
  }
}
