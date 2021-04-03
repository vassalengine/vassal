/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import java.awt.Point;
import java.awt.Rectangle;
import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitor;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;

/** Adds a button to a Maps toolbar that adjusts the positions of all pieces
 * so that their centroid is at the center of the map
 */
public class PieceRecenterer extends AbstractToolbarItem implements DeckVisitor {
  // These 4 identical to AbstractToolbarItem and exist for "clirr purposes"
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //NON-NLS
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //NON-NLS
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String HOTKEY = "hotkey"; //NON-NLS
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //NON-NLS

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected Map map;
  protected DeckVisitorDispatcher dispatcher;

  public PieceRecenterer() {
    setNameKey("");

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.PieceRecenterer.recenter"),
      Resources.getString("Editor.PieceRecenterer.recenter"),
      "/images/recenter.gif", //NON-NLS
      e -> GameModule.getGameModule().sendAndLog(recenter(map))
    ));
    launch = getLaunchButton(); // for compatibility

    dispatcher = new DeckVisitorDispatcher(this);
  }

  /**
   * Returns a Command that moves all pieces so that their centroid is
   * centered on the map.
   */
  public Command recenter(Map map) {
    final Command c = new NullCommand();
    final GamePiece[] pieces = map.getPieces();
    final Rectangle r = new Rectangle(0, 0, -1, -1);

    for (final GamePiece p : pieces) {
      if (Boolean.TRUE.equals(dispatcher.accept(p))) {
        final Point pt = p.getPosition();
        final Rectangle pRect = p.getShape().getBounds();
        pRect.translate(pt.x, pt.y);
        r.add(pRect);
      }
    }

    if (r.height >= 0 && r.width >= 0) {
      final int dx = map.mapSize().width / 2 - (r.x + r.width / 2);
      final int dy = map.mapSize().height / 2 - (r.y + r.height / 2);
      for (final GamePiece p : pieces) {
        if (Boolean.TRUE.equals(dispatcher.accept(p))) {
          final ChangeTracker tracker = new ChangeTracker(p);
          final Point pt = p.getPosition();
          pt.translate(dx, dy);
          p.setPosition(pt);
          c.append(tracker.getChangeCommand());
        }
      }
    }

    map.repaint();
    return c;
  }

  /** Implements {@link DeckVisitor}.  Returns Boolean.TRUE if the piece should be moved */
  @Override
  public Object visitDeck(Deck d) {
    return Boolean.TRUE;
  }

  /** Implements {@link DeckVisitor}.  Returns Boolean.TRUE if the piece should be moved */
  @Override
  public Object visitDefault(GamePiece p) {
    return Boolean.TRUE;
  }

  /** Implements {@link DeckVisitor}.  Returns Boolean.TRUE if the piece should be moved */
  @Override
  public Object visitStack(Stack s) {
    return s.getPieceCount() > 0 ? Boolean.TRUE : Boolean.FALSE;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PieceRecenterer.component_type"); //$NON-NLS-1$
  }

  @Override
  public void addTo(Buildable parent) {
    map = (Map)parent;
    map.getToolBar().add(getLaunchButton());
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/recenter.gif"); //NON-NLS
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "PieceRecenterer"); //NON-NLS
  }

  @Override
  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(getLaunchButton());
  }
}
