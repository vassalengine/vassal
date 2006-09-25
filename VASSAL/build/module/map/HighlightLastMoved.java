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
import java.io.File;
import java.net.MalformedURLException;
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

public class HighlightLastMoved extends AbstractConfigurable implements Drawable, MouseListener, GameComponent {
  public static final String COLOR = "color";
  public static final String THICKNESS = "thickness";

  protected ColoredBorder highlighter = new ColoredBorder(Color.RED, 2);
  protected GamePiece lastMoved;
  protected static java.util.Map instances = new HashMap();
  private boolean enabled;

  public String[] getAttributeDescriptions() {
    return new String[]{"Color", "Thickness"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Color.class, Integer.class};
  }

  public String[] getAttributeNames() {
    return new String[]{COLOR, THICKNESS};
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
  }

  public String getAttributeValueString(String key) {
    if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(highlighter.getColor());
    }
    else if (THICKNESS.equals(key)) {
      return String.valueOf(highlighter.getThickness());
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
    if (lastMoved != null) {
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
    enabled = gameStarting;
    lastMoved = null;
  }

  public Command getRestoreCommand() {
    return null;
  }

  public static void setLastMoved(GamePiece p) {
    HighlightLastMoved h = (HighlightLastMoved) instances.get(p.getMap());
    if (h != null) {
      h.setLastMovedPiece(p);
    }
  }

  public void setLastMovedPiece(GamePiece p) {
    if (enabled) {
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
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "Map.htm"), "#LastMoveHighlighter");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public static String getConfigureTypeName() {
    return "Last Move Highlighter";
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
}
