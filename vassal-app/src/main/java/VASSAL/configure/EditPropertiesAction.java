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

import VASSAL.tools.swing.SwingUtils;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.AbstractAction;

import VASSAL.build.Configurable;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.i18n.Resources;

/**
 * Action to edit the Properties of a component
 */
public class EditPropertiesAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  protected Configurable target;
  protected HelpWindow helpWindow;
  protected static final Map<Configurable, PropertiesWindow> openWindows = new HashMap<>();
  protected Frame dialogOwner;
  protected ConfigureTree tree;

  public EditPropertiesAction(Configurable target, HelpWindow helpWindow, Frame dialogOwner) {
    super(Resources.getString("Editor.properties")); //$NON-NLS-1$
    this.helpWindow = helpWindow;
    this.target = target;
    this.dialogOwner = dialogOwner;
    setEnabled(target.getConfigurer() != null);
  }

  /*
   * Used by ConfigureTree where Configurers may change the children of a node
   */
  public EditPropertiesAction(Configurable target, HelpWindow helpWindow, Frame dialogOwner, ConfigureTree tree) {
    this(target, helpWindow, dialogOwner);
    this.tree = tree;
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    PropertiesWindow w = openWindows.get(target);
    if (w == null) {
      w = new PropertiesWindow(dialogOwner, false, target, helpWindow);
      w.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosed(WindowEvent e) {
          openWindows.remove(target);
          if (tree != null) {
            if (target instanceof ConfigureTree.Mutable) {
              tree.nodeUpdated(target);
            }
            tree.nodeEdited(target);
          }
        }
      });
      openWindows.put(target, w);
      w.setVisible(true);
      SwingUtils.ensureOnScreen(w);
      if (tree != null) {
        tree.notifyStateChanged(true);
        tree.nodeEdited(target);
      }
    }
    w.toFront();
  }
}
