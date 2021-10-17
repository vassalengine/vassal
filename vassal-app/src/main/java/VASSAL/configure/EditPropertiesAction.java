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
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
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

      //BR// If a modifier key was held with double-clicking, displace the window to a corner.
      final boolean alt = (evt.getModifiers() & MouseEvent.ALT_DOWN_MASK) != 0;
      final boolean shift = (evt.getModifiers() & MouseEvent.SHIFT_DOWN_MASK) != 0;
      final boolean ctrl = (evt.getModifiers() & MouseEvent.CTRL_DOWN_MASK) != 0;
      if (alt || shift || ctrl) {
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice defaultScreen = ge.getDefaultScreenDevice();
        final Rectangle rect = defaultScreen.getDefaultConfiguration().getBounds();

        final int x, y;

        if (alt) {
          if (ctrl && shift) { // ctrl+alt+shift == lower left
            x = 30;
            y = (int) rect.getMaxY() - w.getHeight() - 30;
          }
          else if (ctrl) {     // alt+ctrl = lower right
            x = (int) rect.getMaxX() - w.getWidth() - 30;
            y = (int) rect.getMaxY() - w.getHeight() - 30;
          }
          else if (shift) { // alt+shift = upper right;
            x = ((int) rect.getMaxX() - w.getWidth() - 30);
            y = 30;
          }
          else { // alt = center right
            x = ((int) rect.getMaxX() - w.getWidth() - 30);
            y = (int) (rect.getMaxY() - w.getHeight()) / 2;
          }
        }
        else if (shift) {
          if (ctrl) { // ctrl+shift = upper left
            x = 30;
            y = 30;
          }
          else {  // shift = center
            x = ((int)rect.getMaxX() - w.getWidth()) / 2;
            y = ((int)rect.getMaxY() - w.getHeight()) / 2;
          }
        }
        else {  // Ctrl = center left
          x = 30;
          y = ((int)rect.getMaxY() - w.getHeight()) / 2;
        }
        w.setLocation(x, y);
      }

      SwingUtils.ensureOnScreen(w);

      if (tree != null) {
        tree.notifyStateChanged(true);
        tree.nodeEdited(target);
      }
    }
    w.toFront();
  }
}
