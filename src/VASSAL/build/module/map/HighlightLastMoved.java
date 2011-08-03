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
package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.HashMap;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;

public class HighlightLastMoved extends AbstractConfigurable implements Drawable, MouseListener, GameComponent {
  public static final String ENABLED = "enabled";
  public static final String COLOR = "color";
  public static final String THICKNESS = "thickness";

  protected ColoredBorder highlighter;
  protected GamePiece lastMoved;
  protected static java.util.Map<Map,HighlightLastMoved> instances =
    new HashMap<Map,HighlightLastMoved>();

  protected boolean enabled;
  protected boolean currentlyEnabled;

  public HighlightLastMoved() {
    highlighter = new ColoredBorder(Color.RED, 2);
    enabled = true;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString("Editor.HighlightLastMoved.enabled"), //$NON-NLS-1$
        Resources.getString(Resources.COLOR_LABEL),
        Resources.getString("Editor.HighlightLastMoved.thickness"), //$NON-NLS-1$
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Boolean.class,
      Color.class,
      Integer.class
    };
  }

  public String[] getAttributeNames() {
    return new String[]{
      ENABLED,
      COLOR,
      THICKNESS
    };
  }

  public void setAttribute(String key, Object value) {
    if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      highlighter.setColor((Color) value);
    }
    else if (THICKNESS.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      highlighter.setThickness(((Integer) value).intValue());
    }
    else if (ENABLED.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      enabled = ((Boolean) value).booleanValue();
    }
  }

  public String getAttributeValueString(String key) {
    if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(highlighter.getColor());
    }
    else if (THICKNESS.equals(key)) {
      return String.valueOf(highlighter.getThickness());
    }
    else if (ENABLED.equals(key)) {
      return String.valueOf(enabled);
    }
    else {
      return null;
    }
  }

  public void addTo(Buildable parent) {
    Map map = (Map) parent;
    map.addDrawComponent(this);
    map.addLocalMouseListener(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    instances.put(map, this);
    validator = new SingleChildInstance(map, getClass());
  }

  public void removeFrom(Buildable parent) {
    Map map = (Map) parent;
    map.removeDrawComponent(this);
    map.removeLocalMouseListener(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    instances.remove(map);
  }

  public void draw(Graphics g, Map map) {
    if (lastMoved != null && enabled) {
      if (lastMoved.getMap() == map) {
        highlighter.draw(lastMoved, g, (int) (lastMoved.getPosition().x * map.getZoom()),
            (int) (lastMoved.getPosition().y * map.getZoom()), map.getView(), map.getZoom());
      }
      else {
        lastMoved = null;
      }
    }
  }

  public void setup(boolean gameStarting) {
    currentlyEnabled = gameStarting && enabled;
    lastMoved = null;
  }

  public Command getRestoreCommand() {
    return null;
  }

  public static void setLastMoved(GamePiece p) {
    HighlightLastMoved h = instances.get(p.getMap());
    if (h != null) {
      h.setLastMovedPiece(p);
    }
  }

  public void setLastMovedPiece(GamePiece p) {
    if (currentlyEnabled) {
      if (p.getParent() instanceof Stack) {
        lastMoved = p.getParent();
      }
      else {
        lastMoved = p;
      }
      if (lastMoved.getMap() != null) {
        lastMoved.getMap().getPieceCollection().moveToFront(lastMoved);
      }
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
    lastMoved = null;
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "LastMoveHighlighter");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.HighlightLastMoved.component_type"); //$NON-NLS-1$
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }
}
