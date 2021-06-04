/*
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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.HashMap;

public class HighlightLastMoved extends AbstractConfigurable implements Drawable, MouseListener, GameComponent {
  public static final String ENABLED = "enabled"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS
  public static final String THICKNESS = "thickness"; //NON-NLS

  protected ColoredBorder highlighter;
  protected GamePiece lastMoved;
  protected static final java.util.Map<Map, HighlightLastMoved> instances = new HashMap<>();

  protected boolean enabled;
  protected boolean currentlyEnabled;

  public HighlightLastMoved() {
    highlighter = new ColoredBorder(Color.RED, 2);
    enabled = true;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString("Editor.HighlightLastMoved.enabled"), //$NON-NLS-1$
        Resources.getString(Resources.COLOR_LABEL),
        Resources.getString("Editor.HighlightLastMoved.thickness"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Boolean.class,
      Color.class,
      Integer.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      ENABLED,
      COLOR,
      THICKNESS
    };
  }

  @Override
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
      highlighter.setThickness((Integer) value);
    }
    else if (ENABLED.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      enabled = (Boolean) value;
    }
  }

  @Override
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

  @Override
  public void addTo(Buildable parent) {
    final Map map = (Map) parent;
    map.addDrawComponent(this);
    map.addLocalMouseListener(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    instances.put(map, this);
  }

  @Override
  public void removeFrom(Buildable parent) {
    final Map map = (Map) parent;
    map.removeDrawComponent(this);
    map.removeLocalMouseListener(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    instances.remove(map);
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (lastMoved == null || !enabled) {
      return;
    }

    if (lastMoved.getMap() == map) {
      final Graphics2D g2d = (Graphics2D) g;
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final double zoom = map.getZoom() * os_scale;

      highlighter.draw(
        lastMoved,
        g,
        (int) (lastMoved.getPosition().x * zoom),
        (int) (lastMoved.getPosition().y * zoom),
        map.getView(),
        zoom
      );
    }
    else {
      lastMoved = null;
    }
  }

  @Override
  public void setup(boolean gameStarting) {
    currentlyEnabled = gameStarting && enabled;
    lastMoved = null;
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  public static void setLastMoved(GamePiece p) {
    final HighlightLastMoved h = instances.get(p.getMap());
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

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mousePressed(MouseEvent e) {
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    lastMoved = null;
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "LastMoveHighlighter"); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.HighlightLastMoved.component_type"); //$NON-NLS-1$
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }
}
