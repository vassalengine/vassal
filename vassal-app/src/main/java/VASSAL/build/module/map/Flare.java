/*
 * Copyright (c) 2020 Vassalengine.org   Brian Reynolds
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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.FlareCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.SwingUtils;

public class Flare extends AbstractConfigurable
        implements CommandEncoder, GameComponent, Drawable, MouseListener {
  public static final String COMMAND_PREFIX = "FLARE:";
  private Map map;
  private int circleSize;
  private boolean circleScale;
  private int pulses;
  private int pulsesPerSec;
  private boolean animate;
  private String flareKey;
  private Color color;
  private Point clickPoint;
  private volatile boolean active;
  public static final String CIRCLE_SIZE  = "circleSize";
  public static final String CIRCLE_SCALE = "circleScale";
  public static final String CIRCLE_COLOR = "circleColor";
  public static final String FLARE_KEY    = "flareKey";
  public static final String PULSES       = "flarePulses";
  public static final String PULSES_PER_SEC = "flarePulsesPerSec";

  public static final String FLARE_ALT_LOCAL       = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt"));
  public static final String FLARE_CTRL_LOCAL      = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl"));
  public static final String FLARE_COMMAND_LOCAL   = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.meta"));
  public static final String FLARE_ALT_SHIFT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt_shift"));
  public static final String FLARE_SHIFT_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.shift_command"));
  public static final String FLARE_CTRL_SHIFT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl_shift"));
  public static final String FLARE_ALT_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt_command"));
  public static final String FLARE_CTRL_ALT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl_alt"));
  public static final String FLARE_ALT_SHIFT_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt_shift_command"));
  public static final String FLARE_CTRL_ALT_SHIFT_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl_alt_shift"));

  public static final String FLARE_ALT            = "keyAlt";
  public static final String FLARE_CTRL           = "keyCtrl";
  public static final String FLARE_ALT_SHIFT      = "keyAltShift";
  public static final String FLARE_CTRL_SHIFT     = "keyCtrlShift";
  public static final String FLARE_CTRL_ALT       = "keyCtrlAlt";
  public static final String FLARE_CTRL_ALT_SHIFT = "keyCtrlAltShift";

  private static final int STROKE = 3;

  public Flare() {
    circleSize   = 100;
    circleScale  = true;
    color        = Color.RED;
    active       = false;
    flareKey     = FLARE_ALT;
    pulses       = 6;
    pulsesPerSec = 3;
  }

  public String getDescription() {
    return Resources.getString("Editor.Flare.desc");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Flare.configure");
  }

  public Class<?>[] getAttributeTypes() {
    return new Class[] { FlareKeyConfig.class, Integer.class, Color.class, Boolean.class, Integer.class, Integer.class };
  }

  public String[] getAttributeNames() {
    return new String[] { FLARE_KEY, CIRCLE_SIZE, CIRCLE_COLOR, CIRCLE_SCALE, PULSES, PULSES_PER_SEC };
  }

  public String[] getAttributeDescriptions() {
    return new String[] { Resources.getString("Editor.Flare.flare_key"),
            Resources.getString("Editor.Flare.circle_size"),
            Resources.getString("Editor.Flare.circle_color"),
            Resources.getString("Editor.Flare.circle_scale"),
            Resources.getString("Editor.Flare.pulses"),
            Resources.getString("Editor.Flare.pulses_per_sec") };
  }

  public String getAttributeValueString(final String key) {
    if (FLARE_KEY.equals(key)) {
      return flareKey;
    }
    else if (CIRCLE_SIZE.equals(key)) {
      return String.valueOf(circleSize);
    }
    else if (CIRCLE_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (CIRCLE_SCALE.equals(key)) {
      return String.valueOf(circleScale);
    }
    else if (PULSES.equals(key)) {
      return String.valueOf(pulses);
    }
    else if (PULSES_PER_SEC.equals(key)) {
      return String.valueOf(pulsesPerSec);
    }
    return null;
  }


  public void setAttribute(String key, Object value) {
    if (FLARE_KEY.equals(key)) {
      if (FLARE_ALT_LOCAL.equals(value)) {
        flareKey = FLARE_ALT;
      }
      else if (FLARE_COMMAND_LOCAL.equals(value) || FLARE_CTRL_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL;
      }
      else if (FLARE_ALT_SHIFT_LOCAL.equals(value)) {
        flareKey = FLARE_ALT_SHIFT;
      }
      else if (FLARE_SHIFT_COMMAND_LOCAL.equals(value) || FLARE_CTRL_SHIFT_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL_SHIFT;
      }
      else if (FLARE_ALT_COMMAND_LOCAL.equals(value) || FLARE_CTRL_ALT_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL_ALT;
      }
      else if (FLARE_ALT_SHIFT_COMMAND_LOCAL.equals(value) || FLARE_CTRL_ALT_SHIFT_LOCAL.equals(value)) {
        flareKey = FLARE_CTRL_ALT_SHIFT;
      }
      else {
        flareKey = (String) value;
      }
    }
    else if (CIRCLE_SIZE.equals(key)) {
      if (value instanceof String) {
        circleSize = Integer.parseInt((String) value);
      }
      else if (value instanceof Integer) {
        circleSize = (Integer) value;
      }
    }
    else if (CIRCLE_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color)value;
    }
    else if (CIRCLE_SCALE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      circleScale = (Boolean)value;
    }
    else if (PULSES.equals(key)) {
      if (value instanceof String) {
        pulses = Integer.parseInt((String) value);
      }
      else {
        pulses = (Integer) value;
      }
    }
    else if (PULSES_PER_SEC.equals(key)) {
      if (value instanceof String) {
        pulsesPerSec = Integer.parseInt((String) value);
      }
      else {
        pulsesPerSec = (Integer) value;
      }
    }
  }


  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Flare.htm");  //$NON-NLS-1$
  }


  public void addTo(final Buildable parent) {
    if (parent instanceof Map) {
      map = (Map) parent;
      GameModule.getGameModule().addCommandEncoder(this);
      map.addDrawComponent(this);
      map.addLocalMouseListener(this);
    }
  }

  public void removeFrom(final Buildable parent) {
    if (parent instanceof Map) {
      GameModule.getGameModule().removeCommandEncoder(this);
    }
  }

  private double os_scale = 1.0;

  private volatile float animfrac;

  private void repaintArea() {
    map.repaint(new Rectangle(
            (int)(clickPoint.x - circleSize / 2.0 - 2 * STROKE * os_scale),
            (int)(clickPoint.y - circleSize / 2.0 - 2 * STROKE * os_scale),
            (int)(circleSize + 4 * STROKE * os_scale + 0.5),
            (int)(circleSize + 4 * STROKE * os_scale + 0.5)
    ));
  }

  private final Animator animator = new Animator(0, 1, Animator.RepeatBehavior.LOOP, new TimingTargetAdapter() {
    @Override
    public void begin() {
      active = true;
      animfrac = 0.0f;
      repaintArea();
    }

    @Override
    public void timingEvent(float fraction) {
      animfrac = fraction;
      repaintArea();
    }

    @Override
    public void end() {
      active = false;
      repaintArea();
    }
  });

  public void startAnimation(final boolean isLocal) {
    if (!isLocal) {
      if (GlobalOptions.getInstance().centerOnOpponentsMove()) {
        map.centerAt(clickPoint);
      }
    }

    animator.stop();
    animate = pulses > 0 && pulsesPerSec > 0;
    animator.setRepeatCount(Math.max(pulses, 1));
    animator.setDuration(1000 / Math.max(pulsesPerSec, 1));
    animator.start();
  }

  public void draw(final Graphics g, final Map map) {
    if (active && clickPoint != null) {
      final Graphics2D g2d = (Graphics2D) g;
      os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

      double diameter = (circleScale ? map.getZoom() : 1.0) * os_scale * circleSize;
      if (animate) {
        diameter *= (1.0 - animfrac);
      }

      if (diameter <= 0.0) {
        return;
      }

      // translate the click location for current zoom
      final Point p = map.mapToDrawing(clickPoint, os_scale);
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
              RenderingHints.VALUE_ANTIALIAS_ON);

      // draw a circle around the selected point
      g2d.setColor(color);
      g2d.setStroke(new BasicStroke((float)(STROKE * os_scale)));
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
              RenderingHints.VALUE_ANTIALIAS_ON);
      g2d.drawOval(
              (int)(p.x - diameter / 2.0),
              (int)(p.y - diameter / 2.0),
              (int)(diameter + 0.5),
              (int)(diameter + 0.5)
      );
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  public String encode(final Command c) {
    if (c instanceof FlareCommand) {
      return COMMAND_PREFIX + ((FlareCommand) c).getClickPoint().x + "," + ((FlareCommand) c).getClickPoint().y;
    }
    return null;
  }

  public Command decode(final String s) {
    if (s.startsWith(COMMAND_PREFIX)) {
      final int x = Integer.parseInt(s.substring(s.indexOf(":") + 1, s.indexOf(",")));
      final int y = Integer.parseInt(s.substring(s.indexOf(",") + 1));
      clickPoint = new Point(x, y);
      return new FlareCommand(this);
    }
    return null;
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void mouseClicked(final MouseEvent e) {
  }

  public void mousePressed(final MouseEvent e) {
    if (!SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }
    if (FLARE_CTRL.equals(flareKey)) {
      if (!SwingUtils.isSelectionToggle(e) || e.isAltDown() || e.isShiftDown()) {
        return;
      }
    }
    else if (FLARE_ALT.equals(flareKey)) {
      if (!e.isAltDown() || e.isShiftDown() || SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    else if (FLARE_ALT_SHIFT.equals(flareKey)) {
      if (!e.isAltDown() || !e.isShiftDown() || SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    else if (FLARE_CTRL_SHIFT.equals(flareKey)) {
      if (!e.isShiftDown() || !SwingUtils.isSelectionToggle(e) || e.isAltDown()) {
        return;
      }
    }
    else if (FLARE_CTRL_ALT.equals(flareKey)) {
      if (!e.isAltDown() || !SwingUtils.isSelectionToggle(e) || e.isShiftDown()) {
        return;
      }
    }
    else if (FLARE_CTRL_ALT_SHIFT.equals(flareKey)) {
      if (!e.isAltDown() || !e.isShiftDown() || !SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    clickPoint = new Point(e.getX(), e.getY());
    final GameModule mod = GameModule.getGameModule();
    final Command c = new FlareCommand(this);
    mod.sendAndLog(c);
    startAnimation(true);
  }

  public void mouseReleased(final MouseEvent e) {
  }

  public void mouseEntered(final MouseEvent e) {
  }

  public void mouseExited(final MouseEvent e) {
  }

  public void setup(final boolean gameStarting) {
  }

  public Command getRestoreCommand() {
    return null;
  }

  public void setClickPoint(final Point p) {
    clickPoint = p;
  }

  public Point getClickPoint() {
    return clickPoint;
  }


  public static class FlareKeyConfig extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] {
        FLARE_ALT_LOCAL,
        SystemUtils.IS_OS_MAC_OSX && !GlobalOptions.getInstance().getPrefMacLegacy() ? FLARE_COMMAND_LOCAL : FLARE_CTRL_LOCAL,
        FLARE_ALT_SHIFT_LOCAL,
        SystemUtils.IS_OS_MAC_OSX && !GlobalOptions.getInstance().getPrefMacLegacy() ? FLARE_SHIFT_COMMAND_LOCAL : FLARE_CTRL_SHIFT_LOCAL,
        SystemUtils.IS_OS_MAC_OSX && !GlobalOptions.getInstance().getPrefMacLegacy() ? FLARE_ALT_COMMAND_LOCAL : FLARE_CTRL_ALT_LOCAL,
        SystemUtils.IS_OS_MAC_OSX && !GlobalOptions.getInstance().getPrefMacLegacy() ? FLARE_ALT_SHIFT_COMMAND_LOCAL : FLARE_CTRL_ALT_SHIFT_LOCAL,
      };
    }
  }

}