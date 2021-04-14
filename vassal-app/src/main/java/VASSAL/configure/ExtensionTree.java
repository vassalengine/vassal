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

import java.awt.Color;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JOptionPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.module.ExtensionElement;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.widget.PieceSlot;
import VASSAL.i18n.Resources;
import VASSAL.launch.EditorWindow;
import VASSAL.tools.ReflectionUtils;

/**
 * The configuration tree for editing a module extension
 */
public class ExtensionTree extends ConfigureTree {
  private static final long serialVersionUID = 1L;

  private final ModuleExtension extension;

  public ExtensionTree(Configurable root, HelpWindow helpWindow, ModuleExtension extension, EditorWindow editorWindow) {
    super(root, helpWindow, editorWindow);
    this.extension = extension;
    setCellRenderer(new ExtensionRenderer());
  }

  /**
   * Allows ExtensionEditor to override and control what indexes are available during drag and drop
   */
  @Override
  public int checkMinimumIndex(DefaultMutableTreeNode targetNode, int index) {
    if (isEditable(targetNode)) {
      return index;
    }

    // Don't allow dragging things up in among the greyed-out (owned by module, not our extension) items.
    while ((index < targetNode.getChildCount()) && !isEditable((DefaultMutableTreeNode)targetNode.getChildAt(index))) {
      index++;
    }

    return index;
  }
  

  public boolean isEditable(DefaultMutableTreeNode node) {
    if (extension == null) {
      return false;
    }
    if (node != null) {
      for (final ExtensionElement el :
           extension.getComponentsOf(ExtensionElement.class)) {
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
    final Object[] nodePath = node.getUserObjectPath();
    final Configurable[] path = new Configurable[nodePath.length - 1];
    for (int i = 0; i < path.length; ++i) {
      path[i] = (Configurable) nodePath[i + 1];
    }
    return path;
  }

  @Override
  public void externalInsert(Configurable parent, Configurable child) {
    super.externalInsert(parent, child);
    if (!isEditable(parent)) {
      extension.add(new ExtensionElement(child, getPath(getTreeNode(parent))));
    }
  }

  @Override
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
            ((PieceSlot) child).updateGpId(extension);
          }
          if (child.getConfigurer() != null) {
            if (insert(target, child, getTreeNode(target).getChildCount())) {
              final PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ExtensionTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                @Override
                public void save() {
                  super.save();
                  if (!isEditable(target)) {
                    final ExtensionElement el = new ExtensionElement(child, getPath(getTreeNode(target)));
                    extension.add(el);
                  }
                }

                @Override
                public void cancel() {
                  ExtensionTree.this.remove(target, child);
                  dispose();
                }
              };
              w.setVisible(true);
            }
          }
          else {
            final boolean inserted = insert(target, child, getTreeNode(target).getChildCount());
            if (inserted && !isEditable(getTreeNode(target))) {
              extension.add(new ExtensionElement(child, getPath(getTreeNode(target))));
            }
          }
        }
      }
    };
  }

  @Override
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
              final PropertiesWindow w = new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class, ExtensionTree.this), false, child, helpWindow) {
                private static final long serialVersionUID = 1L;

                @Override
                public void save() {
                  super.save();
                  insert(target, child, getTreeNode(target).getChildCount());
                  if (!isEditable(target)) {
                    final ExtensionElement el = new ExtensionElement(child, getPath(getTreeNode(target)));
                    extension.add(el);
                  }
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
            JOptionPane.showMessageDialog(
              getTopLevelAncestor(),
              "Error adding " + getConfigureName(child) + //NON-NLS
                " to " + getConfigureName(target) + "\n" + ex.getMessage(), //NON-NLS
              "Illegal configuration", //NON-NLS
              JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    };
  }

  private boolean isEditable(final Configurable target) {
    return isEditable(getTreeNode(target));
  }

  @Override
  protected Action buildEditAction(Configurable target) {
    return isEditable(target) ? super.buildEditAction(target) : null;
  }

  @Override
  protected Action buildCutAction(Configurable target) {
    return isEditable(target) ? super.buildCutAction(target) : null;
  }

  @Override
  protected Action buildPasteAction(final Configurable target) {
    final Action a;
    if (isEditable(target)) {
      a = super.buildPasteAction(target);
    }
    else {
      a = new AbstractAction(Resources.getString("Editor.ConfigureTree.paste")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          if (cutData != null) {
            final DefaultMutableTreeNode targetNode = getTreeNode(target);
            final Configurable cutTarget = (Configurable) cutData.getUserObject();
            if (remove(getParent(cutData), cutTarget)) {
              if (insert(target, cutTarget, targetNode.getChildCount())) {
                extension.add(new ExtensionElement(cutTarget, getPath(targetNode)));
              }
            }
          }
          else if (copyData != null) {
            final Configurable copyTarget =
              (Configurable) copyData.getUserObject();

            Configurable clone = null;
            try {
              clone = copyTarget.getClass().getConstructor().newInstance();
            }
            catch (Throwable t) {
              ReflectionUtils.handleNewInstanceFailure(
                t, copyTarget.getClass());
            }

            if (clone != null) {
              clone.build(
                  copyTarget.getBuildElement(Builder.createNewDocument()));

              if (insert(target, clone, getTreeNode(target).getChildCount())) {
                updateGpIds(clone);
                extension.add(
                  new ExtensionElement(clone, getPath(getTreeNode(target))));
              }
            }
          }
          cutData = null;
          updateEditMenu();
        }
      };
      a.setEnabled(isValidPasteTarget(target));
    }
    return a;
  }


  @Override
  protected boolean isValidPasteTarget(Configurable target) {
    return isValidPasteTarget(target, cutData) || isValidPasteTarget(target, copyData);
  }


  @Override
  protected boolean isValidPasteTarget(Configurable target, DefaultMutableTreeNode sourceNode) {
    if (sourceNode == null) {
      return false;
    }
    return super.isValidParent(target, (Configurable) sourceNode.getUserObject());
  }


  /**
   * Allocate new PieceSlot Id's to any PieceSlot sub-components
   *
   * @param c Configurable to update
   */
  @Override
  public void updateGpIds(Configurable c) {
    if (c instanceof PieceSlot) {
      ((PieceSlot) c).updateGpId(extension);
    }
    else {
      for (final Configurable conf : c.getConfigureComponents()) updateGpIds(conf);
    }
  }

  @Override
  protected Action buildMoveAction(Configurable target) {
    return isEditable((DefaultMutableTreeNode) getTreeNode(target).getParent()) ? super.buildMoveAction(target) : null;
  }

  @Override
  protected Action buildDeleteAction(final Configurable target) {
    Action action = null;
    final DefaultMutableTreeNode targetNode = getTreeNode(target);
    if (targetNode.getParent() != null
        && isEditable(targetNode)) {
      final Configurable parent = (Configurable) ((DefaultMutableTreeNode) targetNode.getParent()).getUserObject();
      action = new AbstractAction(Resources.getString("Editor.ConfigureTree.delete")) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent evt) {
          final boolean removed = remove(parent, target);
          if (removed && !isEditable(parent)) {
            // We've removed an ExtensionElement
            for (final ExtensionElement el :
                 extension.getComponentsOf(ExtensionElement.class)) {
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

  @Override
  protected Action buildCloneAction(final Configurable target) {
    final DefaultMutableTreeNode targetNode = getTreeNode(target);
    final DefaultMutableTreeNode parentNode =
      (DefaultMutableTreeNode) targetNode.getParent();

    if (isEditable(parentNode)) {
      return super.buildCloneAction(target);
    }

    if (parentNode != null) {
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

            if (insert((Configurable) parentNode.getUserObject(),
                       clone, parentNode.getChildCount())) {
              extension.add(new ExtensionElement(clone, getPath(parentNode)));
            }
          }
        }
      };
    }
    else {
      return null;
    }
  }

  @Override
  protected Action buildEditPiecesAction(Configurable target) {
    Action a = null;
    if (isEditable(target)) {
      a = super.buildEditPiecesAction(target);
    }
    return a;
  }

  @Override
  protected boolean isValidParent(Configurable parent, Configurable child) {
    return super.isValidParent(parent, child) && isEditable(parent);
  }

  @Override
  protected void updateEditMenu() {
    super.updateEditMenu();
    deleteAction.setEnabled(selected != null && isEditable(selected));
    cutAction.setEnabled(selected != null && isEditable(selected));
//    cutAction.setEnabled(selected != null && isEditable(selected));
    propertiesAction.setEnabled(selected != null && isEditable(selected) && selected.getConfigurer() != null);
  }

  /**
   * Change color of component names based on editable status
   */
  class ExtensionRenderer extends Renderer {

    private static final long serialVersionUID = 1L;

    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel,
        boolean expanded, boolean leaf, int row, boolean hasFocus) {

      final Component c = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row,
          hasFocus);
      c.setForeground(isEditable((DefaultMutableTreeNode) value) ? Color.BLACK : Color.GRAY);

      return c;
    }

  }
}
