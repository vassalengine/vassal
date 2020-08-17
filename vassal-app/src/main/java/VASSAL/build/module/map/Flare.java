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

import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Graphics;
import VASSAL.build.GameModule;
import VASSAL.build.Buildable;
import javax.swing.Timer;
import java.awt.Point;
import VASSAL.build.module.Map;
import java.awt.event.MouseListener;
import java.awt.event.ActionListener;
import VASSAL.build.module.GameComponent;
import VASSAL.command.CommandEncoder;
import VASSAL.build.AbstractConfigurable;
import VASSAL.command.FlareCommand;
import VASSAL.i18n.Resources;

public class Flare extends AbstractConfigurable
    implements CommandEncoder, GameComponent, Drawable, ActionListener, MouseListener {
  public static final String COMMAND_PREFIX = "FLARE:";
  private Map map;
  private int circleSize;
  private int colorRed;
  private int colorGreen;
  private int colorBlue;
  private static final int CIRCLE_DURATION = 2000;
  private Point clickPoint;
  private Boolean active;
  private Timer timer;
  public static final String CIRCLESIZE = "circlesize";
  public static final String COLORRED = "colorred";
  public static final String COLORGREEN = "colorgreen";
  public static final String COLORBLUE = "colorblue";

  public Flare() {
    this.circleSize = 100;
    this.colorRed = 255;
    this.colorGreen = 0;
    this.colorBlue = 0;
    this.active = false;
  }

  public String getDescription() {
    return Resources.getString("Map Flare");
  }
  
  public static String getConfigureTypeName() {
    return Resources.getString("Map Flare");
  }

  public Class<?>[] getAttributeTypes() {
    return (Class<?>[]) new Class[] { Integer.class, Integer.class, Integer.class, Integer.class };
  }

  public String[] getAttributeNames() {
    return new String[] { "circlesize", "colorred", "colorgreen", "colorblue" };
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Circle Size", "Red", "Green", "Blue" };
  }

  public String getAttributeValueString(final String key) {
    if ("circlesize".equals(key)) {
      return new StringBuilder().append(this.circleSize).toString();
    }
    if ("colorred".equals(key)) {
      return new StringBuilder().append(this.colorRed).toString();
    }
    if ("colorgreen".equals(key)) {
      return new StringBuilder().append(this.colorGreen).toString();
    }
    if ("colorblue".equals(key)) {
      return new StringBuilder().append(this.colorBlue).toString();
    }
    return null;
  }

  public void setAttribute(final String key, final Object value) {
    if ("circlesize".equals(key)) {
      if (value instanceof String) {
        this.circleSize = Integer.parseInt((String) value); 
      } 
      else if (value instanceof Integer) {
        this.circleSize = (Integer) value;
      }
    } 
    else if ("colorred".equals(key)) {
      if (value instanceof String) {
        this.colorRed = Integer.parseInt((String) value);
      } 
      else if (value instanceof Integer) {
        this.colorRed = (Integer) value;
      }
    } 
    else if ("colorgreen".equals(key)) {
      if (value instanceof String) {
        this.colorGreen = Integer.parseInt((String) value);
      } 
      else if (value instanceof Integer) {
        this.colorGreen = (Integer) value;
      }
    } 
    else if ("colorblue".equals(key)) {
      if (value instanceof String) {
        this.colorBlue = Integer.parseInt((String) value);
      } 
      else if (value instanceof Integer) {
        this.colorBlue = (Integer) value;
      }
    }
  }

  public void addTo(final Buildable parent) {
    if (parent instanceof Map) {
      this.map = (Map) parent;
      GameModule.getGameModule().addCommandEncoder((CommandEncoder) this);
      this.map.addDrawComponent((Drawable) this);
      this.map.addLocalMouseListener((MouseListener) this);
      this.timer = new Timer(CIRCLE_DURATION, this);
    }
  }
  
  public void removeFrom(final Buildable parent) {
    if (parent instanceof Map) {
      GameModule.getGameModule().removeCommandEncoder((CommandEncoder) this);
    }
  }


  public void startAnimation(final boolean isLocal) {
    if (!isLocal) {
      this.map.centerAt(this.clickPoint);
    }
    this.active = true;
    this.timer.restart();
    this.map.getView().repaint();
  }

  public void draw(final Graphics g, final Map map) {
    if (this.active && this.clickPoint != null) {                 
      final Graphics2D g2d = (Graphics2D) g;
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final int diameter = (int)(map.getZoom() * os_scale * this.circleSize);

      // translate the piece center for current zoom
      
      final Point p = map.mapToDrawing(clickPoint, os_scale);                             
      
      // draw a circle around the selected point
      g2d.setColor(Color.RED);
      final Color drawColor = new Color(this.colorRed, this.colorGreen, this.colorBlue);
      g.setColor(drawColor);
      g2d.setStroke(new BasicStroke((float)(3 * os_scale)));
      
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
      this.clickPoint = new Point(x, y);
      return (Command) new FlareCommand(this);
    }
    return null;
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void actionPerformed(final ActionEvent e) {
    this.active = false;
    this.timer.stop();
    this.map.repaint();
  }

  public void mouseClicked(final MouseEvent e) {
  }

  public void mousePressed(final MouseEvent e) {
    if (e.isAltDown()) {
      this.clickPoint = new Point(e.getX(), e.getY());
      final GameModule mod = GameModule.getGameModule();
      final Command c = (Command) new FlareCommand(this);
      mod.sendAndLog(c);
      this.startAnimation(true);
    }
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
    this.clickPoint = p;
  }

  public Point getClickPoint() {
    return this.clickPoint;
  }
}
