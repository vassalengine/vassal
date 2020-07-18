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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.configure.ConfigureTree;
import VASSAL.tools.ScrollPane;

/**
 * Dialog that prompts the user to select a component from the {@link ConfigureTree}
 */
public class ChooseComponentDialog extends JDialog implements TreeSelectionListener {
  private static final long serialVersionUID = 1L;

  private Configurable target;
  private Class<? extends Buildable> targetClass;
  private JButton okButton;
  private VASSAL.configure.ConfigureTree tree;

  public ChooseComponentDialog(Frame owner, Class<? extends Buildable> targetClass) {
    super(owner, true);
    this.targetClass = targetClass;
    setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    tree = new ConfigureTree(GameModule.getGameModule(), null) {
      private static final long serialVersionUID = 1L;

      @Override
      public void mousePressed(MouseEvent e) {
      }

      @Override
      public void mouseReleased(MouseEvent e) {
      }
    };
    tree.addTreeSelectionListener(this);
    add(new ScrollPane(tree));
    Box b = Box.createHorizontalBox();
    okButton = new JButton("Ok");
    okButton.setEnabled(false);
    okButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        dispose();
      }
    });
    JButton cancelButton = new JButton("Cancel");
    cancelButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        target = null;
        dispose();
      }
    });
    b.add(okButton);
    b.add(cancelButton);
    add(b);
    pack();
  }

  @Override
  public void valueChanged(TreeSelectionEvent e) {
    boolean enabled = false;
    target = null;
    TreePath path = tree.getSelectionPath();
    if (path != null) {
      Object selected = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
      enabled = isValidTarget(selected);
      if (enabled) {
        target = (Configurable) selected;
      }
    }
    okButton.setEnabled(enabled);
  }

  protected boolean isValidTarget(Object selected) {
    return targetClass.isInstance(selected);
  }

  public Configurable getTarget() {
    return target;
  }
}
