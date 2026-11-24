/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.tools.filechooser;

import java.awt.Component;
import java.awt.Frame;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

/**
 * Keeps VASSAL's menu shortcuts away from a native macOS file dialog while
 * it is open.
 *
 * <p>On macOS {@link java.awt.FileDialog} is a Cocoa open or save panel, and
 * VASSAL's menus are on the screen menu bar. While the panel is key, Cocoa
 * offers every key press with a modifier, and some without one, to the menu
 * bar before the panel's text field sees it, and the JDK runs the matching
 * Swing menu item's action because the key window is not an AWT window. So
 * Delete in the file name field deleted the selected component in the
 * Editor, Cmd+V pasted into the Editor's tree, and Cmd+A once opened Save
 * Module over Save Game. See vassalengine/vassal#11559 and #9359.</p>
 *
 * <p>Taking the accelerators off every menu item for as long as the dialog
 * is open lets those keys reach the dialog. A disabled item would do as
 * well, but enabled states are owned by their actions; accelerators are
 * not, so they can be put back exactly as they were. The menus themselves
 * are greyed out as well, because the JDK leaves them usable behind a
 * modal dialog, unlike a native app.</p>
 */
final class MacFileDialogMenus {
  private final Map<JMenuItem, KeyStroke> stripped = new IdentityHashMap<>();
  private final List<JMenu> disabled = new ArrayList<>();

  private MacFileDialogMenus() {
  }

  /**
   * Takes the shortcuts off every menu item on every frame's menu bar.
   *
   * @return the state to pass to {@link #restore()} once the dialog closes
   */
  static MacFileDialogMenus suspend() {
    final MacFileDialogMenus m = new MacFileDialogMenus();
    for (final JMenuBar bar : menuBars()) {
      for (int i = 0; i < bar.getMenuCount(); ++i) {
        m.strip(bar.getMenu(i));
        m.disable(bar.getMenu(i));
      }
    }
    return m;
  }

  /**
   * Puts back the shortcuts taken off by {@link #suspend()}.
   */
  void restore() {
    disabled.forEach(menu -> menu.setEnabled(true));
    disabled.clear();

    stripped.forEach((item, ks) -> {
      // Leave alone an item someone gave a new shortcut in the meantime
      if (item.getAccelerator() == null) {
        item.setAccelerator(ks);
      }
    });
    stripped.clear();
  }

  static List<JMenuBar> menuBars() {
    final List<JMenuBar> bars = new ArrayList<>();
    for (final Frame f : Frame.getFrames()) {
      if (f instanceof JFrame) {
        final JMenuBar bar = ((JFrame) f).getJMenuBar();
        if (bar != null) {
          bars.add(bar);
        }
      }
    }
    return bars;
  }

  /**
   * Greys out a menu for as long as the dialog is open. The JDK lets the
   * screen menu bar work behind a modal dialog, so without this the
   * module's menus could still be used, even to open a second file dialog.
   */
  private void disable(JMenu menu) {
    if (menu != null && menu.isEnabled()) {
      menu.setEnabled(false);
      disabled.add(menu);
    }
  }

  private void strip(JMenuItem item) {
    if (item == null) {
      return;
    }

    if (item instanceof JMenu) {
      for (final Component c : ((JMenu) item).getMenuComponents()) {
        if (c instanceof JMenuItem) {
          strip((JMenuItem) c);
        }
      }
    }
    else {
      final KeyStroke ks = item.getAccelerator();
      if (ks != null && !stripped.containsKey(item)) {
        stripped.put(item, ks);
        item.setAccelerator(null);
      }
    }
  }
}
