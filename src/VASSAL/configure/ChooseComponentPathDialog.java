/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import VASSAL.build.Configurable;

/**
 * Widget for selecting the full path of a Component in the Buildable hierarchy
 */
public class ChooseComponentPathDialog extends ChooseComponentDialog {
  private static final long serialVersionUID = 1L;

  private Configurable[] path;

  public ChooseComponentPathDialog(Frame owner, Class targetClass) {
    super(owner, targetClass);
  }

  public void valueChanged(TreeSelectionEvent e) {
    super.valueChanged(e);
    TreePath p = e.getPath();
    if (p != null) {
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) p.getLastPathComponent();
      Object[] userObjectPath = node.getUserObjectPath();
      path = new Configurable[userObjectPath.length - 1];
      System.arraycopy(userObjectPath, 1, path, 0, userObjectPath.length - 1);
    }
    else {
      path = null;
    }
  }

  public Configurable[] getPath() {
    return path;
  }

}
