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

import java.awt.*;
import java.awt.event.MouseListener;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import javax.swing.Timer;

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
    implements CommandEncoder, GameComponent, Drawable, ActionListener, MouseListener {
  public static final String COMMAND_PREFIX = "FLARE:";
  private Map map;
  private int circleSize;
  private int pulses;
  private int pulsesPerSec;
  private int frame;
  private boolean animate;
  private int frames;
  private int framesPerPulse;
  private String flareKey;
  private Color color;
  private static final int CIRCLE_RATE    = 33; // 33ms for approx 30 frames/sec
  private static final int DEFAULT_FRAMES = 60; // 2 sec for basic "no animation" pulse
  private Point clickPoint;
  private Boolean active;
  private Timer timer;
  public static final String CIRCLE_SIZE  = "circleSize";
  public static final String CIRCLE_COLOR = "circleColor";
  public static final String FLARE_KEY    = "flareKey";
  public static final String PULSES       = "flarePulses";
  public static final String PULSES_PER_SEC = "flarePulsesPerSec";
  
  public static final String FLARE_ALT_LOCAL     = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.alt"));
  public static final String FLARE_CTRL_LOCAL    = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.ctrl"));
  public static final String FLARE_COMMAND_LOCAL = Resources.getString("Editor.Flare.flare_key_desc", Resources.getString("Keys.meta"));
  
  public static final String FLARE_ALT = "keyAlt";
  public static final String FLARE_CTRL = "keyCtrl";
  


  public Flare() {
    circleSize   = 100;
    color        = Color.RED;
    active       = false;
    flareKey     = FLARE_ALT;
    pulses       = 6;
    pulsesPerSec = 3;
  }

  public String getDescription() {
    return Resources.getString("Map Flare");
  }
  
  public static String getConfigureTypeName() {
    return Resources.getString("Map Flare");
  }

  public Class<?>[] getAttributeTypes() {
    return new Class[] { FlareKeyConfig.class, Integer.class, Color.class, Integer.class, Integer.class };
  }

  public String[] getAttributeNames() {
    return new String[] { FLARE_KEY, CIRCLE_SIZE, CIRCLE_COLOR, PULSES, PULSES_PER_SEC };
  }

  public String[] getAttributeDescriptions() {
    return new String[] { Resources.getString("Editor.Flare.flare_key"), 
                          Resources.getString("Editor.Flare.circle_size"), 
                          Resources.getString("Editor.Flare.circle_color"),
                          Resources.getString("Editor.Flare.pulses"),
                          Resources.getString("Editor.Flare.pulses_per_sec") };
  }

  public String getAttributeValueString(final String key) {
    if (FLARE_KEY.equals(key)) {
      return flareKey;
    }
    else if (CIRCLE_SIZE.equals(key)) {
      return String.valueOf(this.circleSize);
    }
    else if (CIRCLE_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(this.color);
    }
    else if (PULSES.equals(key)) {
      return String.valueOf(this.pulses);
    }
    else if (PULSES_PER_SEC.equals(key)) {
      return String.valueOf(this.pulsesPerSec);
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
      timer  = new Timer(CIRCLE_RATE, this);
      frame  = 0;
    }
  }
  
  public void removeFrom(final Buildable parent) {
    if (parent instanceof Map) {
      GameModule.getGameModule().removeCommandEncoder(this);
    }
  }


  public void startAnimation(final boolean isLocal) {
    if (!isLocal) {
      map.centerAt(this.clickPoint);
    }
    active = true;
    timer.restart();
    frame = 0;
    
    animate = ((pulses > 0) && (pulsesPerSec > 0));
    if (animate) {
      framesPerPulse = 30 / pulsesPerSec;
      if (framesPerPulse < 1) framesPerPulse = 1;
      frames = pulses * framesPerPulse;
    } 
    else {
      frames = DEFAULT_FRAMES;
    }
    
    map.getView().repaint();
  }

  public void draw(final Graphics g, final Map map) {
    if (active && clickPoint != null) {
      final Graphics2D g2d = (Graphics2D) g;
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
      
      int diameter = (int)(map.getZoom() * os_scale * circleSize);
      if (animate) {
        diameter = diameter * (framesPerPulse - (frame % framesPerPulse)) / framesPerPulse;
      }
      if (diameter <= 0) {
        return;
      }

      // translate the piece center for current zoom
      final Point p = map.mapToDrawing(clickPoint, os_scale);
      
      // draw a circle around the selected point
      g2d.setColor(color);
      g2d.setStroke(new BasicStroke((float)(3 * os_scale)));
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON);
      g2d.drawOval(p.x - diameter / 2, p.y - diameter / 2, diameter, diameter); 
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

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void actionPerformed(final ActionEvent e) {
    frame++;
    if (frame >= frames) {
      active = false;
      timer.stop();
    }
    if (!active || animate) {
      map.repaint();
    }    
  }

  public void mouseClicked(final MouseEvent e) {
  }

  public void mousePressed(final MouseEvent e) {
    if (!SwingUtils.isMainMouseButtonDown(e)) {
      return;
    }
    if (FLARE_CTRL.equals(flareKey)) {
      if (!SwingUtils.isSelectionToggle(e)) {
        return;
      }
    }
    else {
      if (!e.isAltDown()) {
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
      String[] values = new String[2];
      values[0] = FLARE_ALT_LOCAL;
      values[1] = (SystemUtils.IS_OS_MAC_OSX && !GlobalOptions.getInstance().getPrefMacLegacy()) ? FLARE_COMMAND_LOCAL : FLARE_CTRL_LOCAL; // Show proper Mac "Ctrl" key
      return values;
    }
  }

}
