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

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.ActionButton;
import VASSAL.counters.Deck;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.KeyCommandSubMenu;
import VASSAL.counters.MultiLocationCommand;
import VASSAL.counters.MenuSeparator;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.Properties;
import VASSAL.tools.NamedKeyManager;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.*;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

public class MenuDisplayer extends MouseAdapter implements Buildable {
  @Deprecated(since = "2022-08-08", forRemoval = true)
  public static final Font POPUP_MENU_FONT = new Font(Font.DIALOG, Font.PLAIN, 10);
  private static final java.util.Map<String, Boolean> keyCommandVisibilityMap = new ConcurrentHashMap<>();

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
    final CustomMenuItem item = new CustomMenuItem(keyCommand.isMenuSeparator() ? MenuSeparator.SEPARATOR_NAME : getMenuText(keyCommand), keyCommand);
    if (!NamedKeyManager.isNamed(keyCommand.getKeyStroke())) { // If the KeyStroke is named, then there is no accelerator
      item.setAccelerator(keyCommand.getKeyStroke());
    }
    item.addActionListener(keyCommand);

    item.setEnabled(keyCommand.isEnabled());

    return item;
  }

  protected static class CustomMenuItem extends JMenuItem {
    private final KeyCommand keyCommand;

    public CustomMenuItem(String text, KeyCommand keyCommand) {
      super(text);
      this.keyCommand = keyCommand;
    }

    @Override
    protected void processMouseEvent(MouseEvent e) {
      if (e.getID() == MouseEvent.MOUSE_PRESSED && SwingUtilities.isRightMouseButton(e)) {
        String commandId = keyCommand.getName();
        boolean currentVisibility = keyCommandVisibilityMap.getOrDefault(commandId, true);
        keyCommandVisibilityMap.put(commandId, !currentVisibility);

        // Get the current popup menu and its invoker
        JPopupMenu popupMenu = (JPopupMenu) this.getParent();
        Component invoker = popupMenu.getInvoker();
        if (invoker instanceof JComponent) {
          // Find the "Unwanted" submenu in the current popup menu
          JMenu unwantedMenu = null;
          for (Component component : popupMenu.getComponents()) {
            if (component instanceof JMenu && "Unwanted".equals(((JMenu) component).getText())) {
              unwantedMenu = (JMenu) component;
              break;
            }
          }

          // check what the parent to this component is and print it to console
          popupMenu.remove(this);
          unwantedMenu.add(this);

          popupMenu.revalidate();
          popupMenu.repaint();

          // Hide and show the popup menu to force it to resize
          popupMenu.setVisible(false);
          popupMenu.setVisible(true);
        }
      } else {
        super.processMouseEvent(e);
      }
    }
  }

  public static JPopupMenu createPopup(GamePiece target) {
      return createPopup(target, false);
    }



    /**
     * @param target
     * @param global If true, then apply the KeyCommands globally,
     *               i.e. to all selected pieces
     * @return
     */
    public static JPopupMenu createPopup(GamePiece target, boolean global) {
      final JPopupMenu popup = new JPopupMenu();
      popup.putClientProperty("gamePiece", target);
      final KeyCommand[] c = (KeyCommand[]) target.getProperty(Properties.KEY_COMMANDS);
      if (c != null) {
        final List<JMenuItem> commands = new ArrayList<>();
        final List<KeyStroke> strokes = new ArrayList<>();
        final java.util.Map<KeyCommandSubMenu, JMenu> subMenus = new java.util.HashMap<>();
        final java.util.Map<String, List<JMenuItem>> commandNames = new java.util.HashMap<>();
        final JMenu unwantedMenu = new JMenu("Unwanted");
        boolean hasUnwantedCommands = true;

        for (final KeyCommand keyCommand : c) {
          keyCommand.setGlobal(global);
          JMenuItem item = null;

          if (keyCommandVisibilityMap.getOrDefault(keyCommand.getName(), true)) {
            if (keyCommand instanceof KeyCommandSubMenu) {
              final JMenu subMenu = new JMenu(getMenuText(keyCommand));
              subMenus.put((KeyCommandSubMenu) keyCommand, subMenu);
              item = subMenu;
              commands.add(item);
              strokes.add(KeyStroke.getKeyStroke('\0'));
            } else {
              final KeyStroke stroke = keyCommand.getKeyStroke();
              if (strokes.contains(stroke) && !keyCommand.isMenuSeparator() && !(keyCommand instanceof MultiLocationCommand.MultiLocationKeyCommand)) {
                final JMenuItem command = commands.get(strokes.indexOf(stroke));
                final Action action = command.getAction();
                if (action != null) {
                  final String commandName = (String) command.getAction().getValue(Action.NAME);
                  if (commandName == null || commandName.length() < keyCommand.getName().length()) {
                    item = makeMenuItem(keyCommand);
                    commands.set(strokes.indexOf(stroke), item);
                  }
                }
              } else {
                strokes.add((stroke != null && !keyCommand.isMenuSeparator()) ? stroke : KeyStroke.getKeyStroke('\0'));
                item = makeMenuItem(keyCommand);
                commands.add(item);
              }
            }
          } else {
            unwantedMenu.add(makeMenuItem(keyCommand));
            hasUnwantedCommands = true;
          }

          if (keyCommand.getName() != null && keyCommand.getName().length() > 0 && item != null) {
            final List<JMenuItem> l = commandNames.computeIfAbsent(keyCommand.getName(), k -> new ArrayList<>());
            l.add(item);
          }
        }

        for (final java.util.Map.Entry<KeyCommandSubMenu, JMenu> e : subMenus.entrySet()) {
          final KeyCommandSubMenu menuCommand = e.getKey();
          final JMenu subMenu = e.getValue();

          for (final Iterator<String> it2 = menuCommand.getCommands(); it2.hasNext(); ) {
            final List<JMenuItem> matchingCommands = commandNames.get(it2.next());
            if (matchingCommands != null) {
              for (final JMenuItem item : matchingCommands) {
                subMenu.add(item);
                commands.remove(item);
              }
            }
          }
        }

        for (final JMenuItem item : commands) {
          final String text = item.getText();
          if (MenuSeparator.SEPARATOR_NAME.equals(text)) {
            popup.addSeparator();
          } else if (text != null && !text.isBlank()) {
            popup.add(item);
          }
        }

        if (hasUnwantedCommands) {
          popup.addSeparator();
          popup.add(unwantedMenu);
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

    // This block detects ActionButton traits
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

      final Point epos = e.getPoint();
      final Point rel = map.positionOf(p);
      epos.translate(-rel.x, -rel.y);
      final Shape s = p.getShape();
      if (!s.contains(epos)) {
        return;
      }

      // Get a list of the ActionButton traits in this piece that overlap this mouseclick
      final List<GamePiece> actionButtons = ActionButton.matchingTraits(p, epos);

      boolean anyMenu = false;
      // Check if any of the overlapping action buttons have the launch-menu flag set
      for (final GamePiece trait : actionButtons) {
        final ActionButton action = (ActionButton) trait;

        if (!action.isLaunchPopupMenu()) continue;
        anyMenu = true;
        break;
      }

      if (!anyMenu) {
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

  @Override
  public boolean isMandatory() {
    return true;
  }

  @Override
  public boolean isUnique() {
    return true;
  }
}
