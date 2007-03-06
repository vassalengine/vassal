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
import java.awt.event.MouseEvent;
import java.util.Enumeration;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.module.ExtensionElement;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.documentation.HelpWindow;

/**
 * The configuration tree for editing a module extension
 */
public class ExtensionTree extends ConfigureTree {
  private static final long serialVersionUID = 1L;

  private ModuleExtension extension;

  public ExtensionTree(Configurable root, HelpWindow helpWindow, ModuleExtension extention) {
    super(root, helpWindow);
    this.extension = extention;
  }

  private boolean isEditable(DefaultMutableTreeNode node) {
    if (node != null) {
      for (Enumeration e = extension.getComponents(ExtensionElement.class); e.hasMoreElements();) {
        ExtensionElement el = (ExtensionElement) e.nextElement();
        if (el.getExtension() == node.getUserObject()) {
          return true;
        }
      }
      if (node.getParent() instanceof DefaultMutableTreeNode) {
        return isEditable((DefaultMutableTreeNode) node.getParent());
      }
    }
    return false;
  }

  public Configurable[] getPath(DefaultMutableTreeNode node) {
    Object[] nodePath = node.getUserObjectPath();
    Configurable[] path = new Configurable[nodePath.length - 1];
    for (int i = 0; i < path.length; ++i) {
      path[i] = (Configurable) nodePath[i + 1];
    }
    return path;
  }

  protected Action buildAddAction(final Configurable target, final Class newConfig) {
    AbstractAction action = new AbstractAction("Add " + getConfigureName(newConfig)) {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent evt) {
        try {
          final Configurable child = (Configurable) newConfig.newInstance();
          child.build(null);
          if (child.getConfigurer() != null) {
            if (insert(target, child, getTreeNode(target).getChildCount())) {
              PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ExtensionTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                public void save() {
                  super.save();
                  if (!isEditable(target)) {
                    ExtensionElement el = new ExtensionElement(child, getPath(getTreeNode(target)));
                    extension.add(el);
                  }
                }

                public void cancel() {
                  ExtensionTree.this.remove(target, child);
                  dispose();
                }
              };
              w.setVisible(true);
            }
          }
          else {
            boolean inserted = insert(target, child, getTreeNode(target).getChildCount());
            if (inserted && !isEditable(getTreeNode(target))) {
              extension.add(new ExtensionElement(child, getPath(getTreeNode(target))));
            }
          }
        }
        catch (Exception err) {
          err.printStackTrace();
        }
      }
    };
    return action;
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
              PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ExtensionTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                public void save() {
                  super.save();
                  insert(c, child, getTreeNode(c).getChildCount());
                  if (!isEditable(target)) {
                    ExtensionElement el = new ExtensionElement(child, getPath(getTreeNode(target)));
                    extension.add(el);
                  }
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

  private boolean isEditable(final Configurable target) {
    return isEditable(getTreeNode(target));
  }

  protected Action buildEditAction(Configurable target) {
    return isEditable(target) ? super.buildEditAction(target) : null;
  }

  protected Action buildCutAction(Configurable target) {
    return isEditable(target) ? super.buildCutAction(target) : null;
  }

  protected Action buildPasteAction(final Configurable target) {
    Action a = null;
    if (isEditable(target)) {
      a = super.buildPasteAction(target);
    }
    else {
      a = new AbstractAction("Paste") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent e) {
          if (cutData != null) {
            DefaultMutableTreeNode targetNode = getTreeNode(target);
            Configurable cutTarget = (Configurable) cutData.getUserObject();
            if (remove(getParent(cutData), cutTarget)) {
              if (insert(target, cutTarget, targetNode.getChildCount())) {
                extension.add(new ExtensionElement(cutTarget, getPath(targetNode)));
              }
            }
          }
          else if (copyData != null) {
            try {
              Configurable copyTarget = (Configurable) copyData.getUserObject();
              Configurable clone = (Configurable) copyTarget.getClass().newInstance();
              clone.build(copyTarget.getBuildElement(Builder.createNewDocument()));
              if (insert(target, clone, getTreeNode(target).getChildCount())) {
                extension.add(new ExtensionElement(clone, getPath(getTreeNode(target))));
              }
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
      a.setEnabled((cutData != null
                    && super.isValidParent(target, (Configurable) cutData.getUserObject())
                    && isEditable(getParent(cutData)))
                   || (copyData != null
                       && super.isValidParent(target, (Configurable) copyData.getUserObject())));
    }
    return a;
  }

  protected Action buildMoveAction(Configurable target) {
    return isEditable((DefaultMutableTreeNode) getTreeNode(target).getParent()) ? super.buildMoveAction(target) : null;
  }

  protected Action buildDeleteAction(final Configurable target) {
    Action action = null;
    final DefaultMutableTreeNode targetNode = getTreeNode(target);
    if (targetNode.getParent() != null
        && isEditable(targetNode)) {
      final Configurable parent = (Configurable) ((DefaultMutableTreeNode) targetNode.getParent()).getUserObject();
      action = new AbstractAction("Delete") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent evt) {
          boolean removed = remove(parent, target);
          if (removed && !isEditable(parent)) {
            // We've removed an ExtensionElement
            for (Enumeration e = extension.getComponents(ExtensionElement.class); e.hasMoreElements();) {
              ExtensionElement el = (ExtensionElement) e.nextElement();
              if (el.getExtension() == target) {
                extension.remove(el);
                break;
              }
            }
          }
        }
      };
    }
    return action;
  }

  protected Action buildCloneAction(final Configurable target) {
    final DefaultMutableTreeNode targetNode = getTreeNode(target);
    if (isEditable((DefaultMutableTreeNode) targetNode.getParent())) {
      return super.buildCloneAction(target);
    }
    if (targetNode.getParent() != null) {
      return new AbstractAction("Clone") {
        private static final long serialVersionUID = 1L;

        public void actionPerformed(ActionEvent evt) {
          try {
            Configurable clone = (Configurable) target.getClass().newInstance();
            clone.build(target.getBuildElement(Builder.createNewDocument()));
            if (insert((Configurable) ((DefaultMutableTreeNode) targetNode.getParent()).getUserObject(), clone, targetNode.getParent().getChildCount())) {
              extension.add(new ExtensionElement(clone, getPath((DefaultMutableTreeNode) targetNode.getParent())));
            }
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

  protected Action buildEditPiecesAction(Configurable target) {
    Action a = null;
    if (isEditable(target)) {
      a = super.buildEditPiecesAction(target);
    }
    return a;
  }

  public void mousePressed(MouseEvent e) {
    TreePath path = getPathForLocation(e.getX(), e.getY());
    if (path != null) {
      if (isEditable((DefaultMutableTreeNode) path.getLastPathComponent())) {
        super.mousePressed(e);
      }
    }
  }

  protected boolean isValidParent(Configurable parent, Configurable child) {
    return super.isValidParent(parent, child) && isEditable(parent);
  }
}
