/*
 * $Id$
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import VASSAL.build.AbstractConfigurable;
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
import VASSAL.tools.NamedKeyStroke;

/** Adds a button to a Maps toolbar that adjusts the positions of all pieces
 * so that their centroid is at the center of the map
 */
public class PieceRecenterer extends AbstractConfigurable implements DeckVisitor {
  public static final String BUTTON_TEXT="text";
  public static final String ICON="icon";
  public static final String HOTKEY="hotkey";
  public static final String TOOLTIP = "tooltip";

  protected LaunchButton launch;
  protected Map map;
  protected DeckVisitorDispatcher dispatcher;

  public PieceRecenterer() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        GameModule.getGameModule().sendAndLog(recenter(map));
      }
    };
    launch = new LaunchButton("Recenter",TOOLTIP,BUTTON_TEXT,HOTKEY,ICON,al);
    dispatcher = new DeckVisitorDispatcher(this);
  }

  /**
   * Returns a Command that moves all pieces so that their centroid is
   * centered on the map.
   */
  public Command recenter(Map map) {
    final Command c = new NullCommand();
    final GamePiece[] pieces = map.getPieces();
    final Rectangle r = new Rectangle(0,0,-1,-1);

    for (GamePiece p : pieces) {
      if (Boolean.TRUE.equals(dispatcher.accept(p))) {
        final Point pt = p.getPosition();
        final Rectangle pRect = p.getShape().getBounds();
        pRect.translate(pt.x,pt.y);
        r.add(pRect);
      }
    }

    if (r.height >= 0 && r.width >= 0) {
      final int dx = map.mapSize().width/2-(r.x+r.width/2);
      final int dy = map.mapSize().height/2-(r.y+r.height/2);
      for (GamePiece p : pieces) {
        if (Boolean.TRUE.equals(dispatcher.accept(p))) {
          final ChangeTracker tracker = new ChangeTracker(p);
          final Point pt = p.getPosition();
          pt.translate(dx,dy);
          p.setPosition(pt);
          c.append(tracker.getChangeCommand());
        }
      }
    }

    map.repaint();
    return c;
  }

  /** Implements {@link DeckVisitor}.  Returns Boolean.TRUE if the piece should be moved */
  public Object visitDeck(Deck d) {
    return Boolean.TRUE;
  }

  /** Implements {@link DeckVisitor}.  Returns Boolean.TRUE if the piece should be moved */
  public Object visitDefault(GamePiece p) {
    return Boolean.TRUE;
  }

  /** Implements {@link DeckVisitor}.  Returns Boolean.TRUE if the piece should be moved */
  public Object visitStack(Stack s) {
    return s.getPieceCount() > 0 ? Boolean.TRUE : Boolean.FALSE;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PieceRecenter.component_type"); //$NON-NLS-1$
  }

  public void addTo(Buildable parent) {
    map = (Map)parent;
    map.getToolBar().add(launch);
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.BUTTON_ICON),
      Resources.getString(Resources.HOTKEY_LABEL),
    };
  }

  public String[] getAttributeNames() {
    return new String[]{
      BUTTON_TEXT,
      TOOLTIP,
      ICON,HOTKEY
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/recenter.gif");
    }
  }

  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "PieceRecenterer");
  }

  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(launch);
  }

  public void setAttribute(String key, Object value) {
    launch.setAttribute(key,value);
  }
}
