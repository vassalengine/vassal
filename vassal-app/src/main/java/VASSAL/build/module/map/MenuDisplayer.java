/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.map;

import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.UIManager;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import VASSAL.counters.ActionButton;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.Deck;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.KeyCommandSubMenu;
import VASSAL.counters.MenuSeparator;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.Properties;

public class MenuDisplayer extends MouseAdapter implements Buildable {
  public static final Font POPUP_MENU_FONT = new Font(Font.DIALOG, Font.PLAIN, 10);

  protected Map map;
  protected PieceFinder targetSelector;

  @Override
  public void addTo(Buildable b) {
    targetSelector = createTargetSelector();
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  /**
   * Return a {@link PieceFinder} instance that will select a
   * {@link GamePiece} whose menu will be displayed when the
   * user clicks on the map
   * @return
   */
  protected PieceFinder createTargetSelector() {
    return new PieceFinder.PieceInStack() {
      @Override
      public Object visitDeck(Deck d) {
        final Point pos = d.getPosition();
        final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
        if (d.getShape().contains(p)) {
          return d;
        }
        else {
          return null;
        }
      }
    };
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  @Override
  public void build(Element e) {
  }


  // For those who wish to override text behavior of both menus and submenus
  protected static String getMenuText(KeyCommand keyCommand) {
    return keyCommand.getLocalizedMenuText();
  }

  // This both eliminates duplicate code AND makes this critical menu-building functionality able to "play well with others".
  // Menu text & behavior can now be custom-classed without needing to override the monster that is MenuDisplayer#createPopup.
  protected static JMenuItem makeMenuItem(KeyCommand keyCommand) {
    // We remember the oldAcceleratorFont so that we can set it back after creating the JMenuItem
    final Object oldAcceleratorFont = UIManager.get("MenuItem.acceleratorFont");
    UIManager.put("MenuItem.acceleratorFont", POPUP_MENU_FONT);
    final JMenuItem item = new JMenuItem(keyCommand.isMenuSeparator() ? MenuSeparator.SEPARATOR_NAME : getMenuText(keyCommand));
    item.setAccelerator(keyCommand.getKeyStroke());
    UIManager.put("MenuItem.acceleratorFont", oldAcceleratorFont);

    item.addActionListener(keyCommand);
    item.setFont(POPUP_MENU_FONT);
    item.setEnabled(keyCommand.isEnabled());

    return item;
  }

  public static JPopupMenu createPopup(GamePiece target) {
    return createPopup(target, false);
  }

  /**
   *
   * @param target
   * @param global If true, then apply the KeyCommands globally,
   * i.e. to all selected pieces
   * @return
   */
  public static JPopupMenu createPopup(GamePiece target, boolean global) {
    final JPopupMenu popup = new JPopupMenu();
    final KeyCommand[] c = (KeyCommand[]) target.getProperty(Properties.KEY_COMMANDS);
    if (c != null) {
      final ArrayList<JMenuItem> commands = new ArrayList<>();
      final ArrayList<KeyStroke> strokes = new ArrayList<>();

      // Maps instances of KeyCommandSubMenu to corresponding JMenu
      final HashMap<KeyCommandSubMenu, JMenu> subMenus = new HashMap<>();
      // Maps name to a list of commands with that name
      final HashMap<String, ArrayList<JMenuItem>> commandNames = new HashMap<>();

      // If a deck has only one menu item, and it's one of the ones that just fires off a choose-from-here dialog, then skip directly to the dialog
      if (target instanceof Deck) {
        if (c.length == 1) {
          final String menu_item = c[0].getName();
          if (menu_item.equals(((Deck)target).getDrawMultipleMessage()) || menu_item.equals(((Deck)target).getDrawSpecificMessage())) {
            c[0].actionPerformed(new ActionEvent(popup, 0, ""));
            return null;
          }
        }
      }

      for (final KeyCommand keyCommand : c) {
        keyCommand.setGlobal(global);
        final KeyStroke stroke = keyCommand.getKeyStroke();
        JMenuItem item = null;
        if (keyCommand instanceof KeyCommandSubMenu) {
          final JMenu subMenu = new JMenu(getMenuText(keyCommand));
          subMenu.setFont(POPUP_MENU_FONT);
          subMenus.put((KeyCommandSubMenu) keyCommand, subMenu);
          item = subMenu;
          commands.add(item);
          strokes.add(KeyStroke.getKeyStroke('\0'));
        }
        else {
          if (strokes.contains(stroke) && !keyCommand.isMenuSeparator()) {
            final JMenuItem command = commands.get(strokes.indexOf(stroke));
            final Action action = command.getAction();
            if (action != null) {
              final String commandName =
                (String) command.getAction().getValue(Action.NAME);
              if (commandName == null ||
                      commandName.length() < keyCommand.getName().length()) {
                item = makeMenuItem(keyCommand);
                commands.set(strokes.indexOf(stroke), item);
              }
            }
          }
          else {
            strokes.add((stroke != null && !keyCommand.isMenuSeparator()) ? stroke : KeyStroke.getKeyStroke('\0'));
            item = makeMenuItem(keyCommand);
            commands.add(item);
          }
        }
        if (keyCommand.getName() != null &&
            keyCommand.getName().length() > 0 &&
            item != null) {
          final ArrayList<JMenuItem> l = commandNames.computeIfAbsent(keyCommand.getName(), k -> new ArrayList<>());
          l.add(item);
        }
      }

      // Move commands from main menu into submenus
      for (final java.util.Map.Entry<KeyCommandSubMenu, JMenu> e :
                                                        subMenus.entrySet()) {
        final KeyCommandSubMenu menuCommand = e.getKey();
        final JMenu subMenu = e.getValue();

        for (final Iterator<String> it2 = menuCommand.getCommands(); it2.hasNext();) {
          final ArrayList<JMenuItem> matchingCommands =
            commandNames.get(it2.next());
          if (matchingCommands != null) {
            for (final JMenuItem item : matchingCommands) {
              subMenu.add(item);
              commands.remove(item);
            }
          }
        }
      }

      // Promote contents of a single submenu [Removed as per Bug 4775]
      // if (commands.size() == 1) {
      //   if (commands.get(0) instanceof JMenu) {
      //     JMenu submenu = (JMenu) commands.get(0);
      //     commands.remove(submenu);
      //     for (int i = 0; i < submenu.getItemCount(); i++) {
      //       commands.add(submenu.getItem(i));
      //     }
      //   }
      // }

      for (final JMenuItem item : commands) {
        final String text = item.getText();
        if (MenuSeparator.SEPARATOR_NAME.equals(text)) {
          popup.addSeparator();
        } 
        else if (text != null && !text.isBlank()) {
          popup.add(item);
        }
      }
    }

    return popup;
  }

  @Override
  public void mousePressed(MouseEvent e) {
    maybePopup(e, false);
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    maybePopup(e, true);
  }

  protected void maybePopup(MouseEvent e) {
    maybePopup(e, false);
  }

  protected void maybePopup(MouseEvent e, boolean specialLaunchAllowed) {
    final GamePiece p = map.findPiece(e.getPoint(), targetSelector);
    if (p == null) {
      return;
    }

    if (!e.isPopupTrigger()) {
      if (e.isAltDown() || e.isShiftDown() || !specialLaunchAllowed) {
        return;
      }
      if (map.getPieceMover().getBreachedThreshold()) { // If we're finishing a legit drag
        return;
      }
      if (map.getKeyBufferer().isLasso()) { // If we dragged a selection box
        return;
      }
      final String launchPopup = (String) p.getProperty(ActionButton.LAUNCH_POPUP_MENU);
      if (!"true".equals(launchPopup)) { //NON-NLS
        return;
      }
    }

    final EventFilter filter = (EventFilter) p.getProperty(Properties.SELECT_EVENT_FILTER);
    if (filter != null && filter.rejectEvent(e)) {
      return;
    }

    final JPopupMenu popup = createPopup(p, true);
    if (popup != null) {
      popup.addPopupMenuListener(new PopupMenuListener() {
        @Override
        public void popupMenuCanceled(PopupMenuEvent evt) {
          map.repaint();
        }

        @Override
        public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) {
          map.repaint();
        }

        @Override
        public void popupMenuWillBecomeVisible(PopupMenuEvent evt) {
        }
      });

      // NB: The conversion back to component coordinates is correct. The
      // master mouse event listener on the map translates all coordinates to
      // map coordinates before passing them on.
      final Point pt = map.mapToComponent(e.getPoint());

      // It is possible for the map to close before the menu is displayed
      if (map.getView().isShowing()) {

        // Inform the piece where player clicked, if it wants to know.
        KeyBuffer.getBuffer().setClickPoint(e.getPoint());

        popup.show(map.getView(), pt.x, pt.y);
      }
    }

    e.consume();
  }
}
