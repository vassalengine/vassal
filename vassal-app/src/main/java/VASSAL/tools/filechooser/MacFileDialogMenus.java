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
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import VASSAL.i18n.Resources;

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
 *
 * <p>That alone leaves Cmd+X, Cmd+C, Cmd+V and Cmd+A beeping, because a
 * Cocoa text field gets them only from Edit menu items, and VASSAL's menus
 * have none that Cocoa recognises. So while the dialog is open each menu
 * bar also gets an Edit menu of its own, in place of any Edit menu it
 * already has. {@link MacTextCommands} points its native items straight at
 * Cocoa's {@code cut:}, {@code copy:}, {@code paste:} and {@code selectAll:},
 * so they behave exactly like a native app's.</p>
 *
 * <p>An Edit menu already there is taken off the bar rather than hidden:
 * the JDK stops listening for a menu being hidden once its frame's menu
 * bar has been swapped out, as happens whenever the Player and Editor
 * windows take turns in front.</p>
 */
final class MacFileDialogMenus {
  private static final String[][] COMMANDS = {
    // label key, key, Cocoa action
    { "General.cut", "X", "cut:" },               //NON-NLS
    { "General.copy", "C", "copy:" },             //NON-NLS
    { "General.paste", "V", "paste:" },           //NON-NLS
    { "General.select_all", "A", "selectAll:" },  //NON-NLS
  };

  private final Map<JMenuItem, KeyStroke> stripped = new IdentityHashMap<>();
  private final Map<JMenuBar, JMenu> added = new IdentityHashMap<>();
  private final List<JMenu> disabled = new ArrayList<>();
  private final List<JMenuBar> removedFrom = new ArrayList<>();
  private final List<JMenu> removed = new ArrayList<>();
  private final List<Integer> removedAt = new ArrayList<>();

  private MacFileDialogMenus() {
  }

  /**
   * Takes the shortcuts off every menu item on every frame's menu bar.
   *
   * @return the state to pass to {@link #restore()} once the dialog closes
   */
  static MacFileDialogMenus suspend() {
    final MacFileDialogMenus m = new MacFileDialogMenus();
    final boolean edit = MacTextCommands.isAvailable();
    for (final JMenuBar bar : menuBars()) {
      for (int i = 0; i < bar.getMenuCount(); ++i) {
        m.strip(bar.getMenu(i));
        m.disable(bar.getMenu(i));
      }
      if (edit) {
        m.addEditMenu(bar);
      }
    }
    if (edit) {
      // If this fails, the items' Swing actions still pass the commands on
      wireEditMenu();
    }
    return m;
  }

  /**
   * Puts back the shortcuts taken off by {@link #suspend()}.
   */
  void restore() {
    added.forEach((bar, menu) -> {
      bar.remove(menu);
      bar.revalidate();
    });
    added.clear();
    disabled.forEach(menu -> menu.setEnabled(true));
    disabled.clear();

    // Put back in the reverse order of removal, so the indices hold
    for (int i = removed.size() - 1; i >= 0; --i) {
      final JMenuBar bar = removedFrom.get(i);
      bar.add(removed.get(i), (int) removedAt.get(i));
      bar.revalidate();
    }
    removedFrom.clear();
    removed.clear();
    removedAt.clear();

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

  private void addEditMenu(JMenuBar bar) {
    final String title = Resources.getString("General.edit"); //NON-NLS

    // Stand in for the Editor's Edit menu, if there is one
    int index = -1;
    for (int i = bar.getMenuCount() - 1; i >= 0; --i) {
      final JMenu menu = bar.getMenu(i);
      if (menu != null && title.equals(menu.getText())) {
        bar.remove(i);
        removedFrom.add(bar);
        removed.add(menu);
        removedAt.add(i);
        index = i;
      }
    }
    if (index < 0) {
      // After File, as on every Mac
      index = Math.min(1, bar.getMenuCount());
    }

    final JMenu menu = new JMenu(title);
    final int mask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx();
    for (final String[] c : COMMANDS) {
      final Action a = new AbstractAction(Resources.getString(c[0])) {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          MacTextCommands.send(c[2]);
        }
      };
      a.putValue(Action.ACCELERATOR_KEY,
        KeyStroke.getKeyStroke(KeyEvent.getExtendedKeyCodeForChar(c[1].charAt(0)), mask));
      menu.add(a);
    }

    bar.add(menu, index);
    bar.revalidate();
    added.put(bar, menu);
  }

  /**
   * Points the native items of the Edit menu now on the screen menu bar at
   * Cocoa's own editing actions.
   */
  private static void wireEditMenu() {
    final String[] titles = new String[COMMANDS.length];
    final String[] actions = new String[COMMANDS.length];
    for (int i = 0; i < COMMANDS.length; ++i) {
      titles[i] = Resources.getString(COMMANDS[i][0]);
      actions[i] = COMMANDS[i][2];
    }
    MacTextCommands.wireToFirstResponder(titles, actions);
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
