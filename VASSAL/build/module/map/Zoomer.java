/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;
import java.util.Vector;
import javax.swing.JLabel;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.border.BevelBorder;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.i18n.Resources;
import VASSAL.tools.BackgroundTask;
import VASSAL.tools.LaunchButton;

/**
 * Controls the zooming in/out of a Map Window
 */
public class Zoomer extends AbstractConfigurable implements GameComponent {
  protected Map map;
  protected double zoomStep = 1.5;
  protected int zoomLevel = 0;
  protected int zoomStart = 1;
  protected double[] zoomFactor;
  protected int maxZoom = 3;
  protected LaunchButton zoomInButton;
  protected LaunchButton zoomOutButton;

  public Zoomer() {
    ActionListener zoomIn = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        zoomIn();
      }
    };
    ActionListener zoomOut = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (zoomLevel < zoomFactor.length - 1) {
          final JWindow w = new JWindow(SwingUtilities.getWindowAncestor(map.getView()));
          w.getContentPane().setBackground(Color.white);
          JLabel l = new JLabel(Resources.getString("Zoomer.scaling_map")); //$NON-NLS-1$
          l.setFont(new Font("Dialog",Font.PLAIN,48)); //$NON-NLS-1$
          l.setBackground(Color.white);
          l.setForeground(Color.black);
          l.setBorder(new BevelBorder(BevelBorder.RAISED,Color.lightGray,Color.darkGray));
          w.getContentPane().add(l);
          w.pack();
          Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
          w.setLocation(d.width/2-w.getSize().width/2,d.height/2-w.getSize().height/2);
          final Vector finished = new Vector();
          Runnable runnable = new Runnable() {
            public void run() {
              try {
                Thread.sleep(100);
                if (!finished.contains(w)) {
                  w.setVisible(true);
                }
              }
              catch (InterruptedException e1) {
              }
            }
          };
          new Thread(runnable).start();
          BackgroundTask task = new BackgroundTask() {
            public void doFirst() {
              scaleBoards(zoomFactor[zoomLevel + 1]);
            }

            public void doLater() {
              zoomOut();
              finished.add(w);
              w.dispose();
            }
          };
          task.start();
        }
      }
    };

    zoomInButton = new LaunchButton(null, IN_TOOLTIP, IN_BUTTON_TEXT, ZOOM_IN, IN_ICON_NAME, zoomIn);
    zoomInButton.setAttribute(IN_TOOLTIP, Resources.getString("Zoomer.zoom_in")); //$NON-NLS-1$
    zoomInButton.setAttribute(IN_ICON_NAME, IN_DEFAULT_ICON);
    zoomOutButton = new LaunchButton(null, OUT_TOOLTIP, OUT_BUTTON_TEXT, ZOOM_OUT, OUT_ICON_NAME, zoomOut);
    zoomOutButton.setAttribute(OUT_TOOLTIP, Resources.getString("Zoomer.zoom_out")); //$NON-NLS-1$
    zoomOutButton.setAttribute(OUT_ICON_NAME, OUT_DEFAULT_ICON);

    setConfigureName(null);
  }

  public static String getConfigureTypeName() {
    return "Zoom capability";
  }

  public String[] getAttributeNames() {
    return new String[]{FACTOR, MAX, ZOOM_START, 
        IN_TOOLTIP, IN_BUTTON_TEXT, IN_ICON_NAME, ZOOM_IN, 
        OUT_TOOLTIP, OUT_BUTTON_TEXT, OUT_ICON_NAME, ZOOM_OUT};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Magnification factor:  ",
                        "Number of zoom levels:  ",
                        "Starting zoom level:  ",
                        "Zoom in tooltip text:  ",
                        "Zoom in button text:  ",
                        "Zoom in Icon:  ",
                        "Zoom in hotkey:  ",
                        "Zoom out tooltip text:  ",
                        "Zoom out button text:  ",
                        "Zoom out Icon:  ",
                        "Zoom out hotkey:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Double.class,
                       Integer.class,
                       Integer.class,
                       String.class,
                       String.class,
                       InIconConfig.class,
                       KeyStroke.class,
                       String.class,
                       String.class,
                       OutIconConfig.class,
                       KeyStroke.class};
  }

  public static class InIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, IN_DEFAULT_ICON);
    }
  }
  
  public static class OutIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, OUT_DEFAULT_ICON);
    }
  }
  
  protected static final String FACTOR = "factor"; //$NON-NLS-1$
  protected static final String MAX = "max"; //$NON-NLS-1$
  protected static final String ZOOM_START = "zoomStart"; //$NON-NLS-1$
  
  protected static final String ZOOM_IN = "zoomInKey"; //$NON-NLS-1$
  protected static final String IN_TOOLTIP = "inTooltip"; //$NON-NLS-1$
  protected static final String IN_BUTTON_TEXT = "inButtonText"; //$NON-NLS-1$
  protected static final String IN_ICON_NAME = "inIconName"; //$NON-NLS-1$
  protected static final String IN_DEFAULT_ICON = "/images/zoomIn.gif"; //$NON-NLS-1$
  
  protected static final String ZOOM_OUT = "zoomOutKey"; //$NON-NLS-1$
  protected static final String OUT_TOOLTIP = "outTooltip"; //$NON-NLS-1$
  protected static final String OUT_BUTTON_TEXT = "outButtonText"; //$NON-NLS-1$
  protected static final String OUT_ICON_NAME = "outIconName"; //$NON-NLS-1$
  protected static final String OUT_DEFAULT_ICON = "/images/zoomOut.gif"; //$NON-NLS-1$
  
  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);

    map = (Map) b;

    validator = new SingleChildInstance(map,getClass());

    map.setZoomer(this);
    map.getToolBar().add(zoomInButton);
    map.getToolBar().add(zoomOutButton);
  }

  public String getAttributeValueString(String key) {
    if (MAX.equals(key)) {
      return "" + maxZoom; //$NON-NLS-1$
    } 
    else if (ZOOM_START.equals(key)) {
	  return "" + zoomStart; //$NON-NLS-1$
    }
    else if (FACTOR.equals(key)) {
      return "" + zoomStep; //$NON-NLS-1$
    }
    else if (zoomInButton.getAttributeValueString(key) != null) {
      return zoomInButton.getAttributeValueString(key);
    }
    else {
      return zoomOutButton.getAttributeValueString(key);
    }
  }

  public void setAttribute(String key, Object val) {
    if (MAX.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      if (val != null) {
        maxZoom = ((Integer) val).intValue();
      }
      initZoomFactors();
    }
	else if (ZOOM_START.equals(key)) {
	  if (val instanceof String) {
		val = new Integer((String) val);
	  }
	  if (val != null) {
		zoomStart = ((Integer) val).intValue();
	  }
	  if (zoomStart < 1) {
	  	 zoomStart = 1;
	  }
	  if (zoomStart > maxZoom) {
	  	 zoomStart = maxZoom;
	  }
	  initZoomFactors();
	}    
    else if (FACTOR.equals(key)) {
      if (val instanceof String) {
        val = new Double((String) val);
      }
      if (val != null) {
        zoomStep = ((Double) val).doubleValue();
      }
      initZoomFactors();
    }
    else {
      zoomInButton.setAttribute(key, val);
      zoomOutButton.setAttribute(key, val);
    }
  }

  private void initZoomFactors() {
    zoomFactor = new double[maxZoom];
    zoomFactor[0] = 1.0;
    for (int i = 1; i < zoomFactor.length; ++i) {
      zoomFactor[i] = zoomFactor[i-1]/zoomStep;
    }
    if (zoomStart < 1) {
    	zoomLevel = 0;
    }
    else if (zoomStart > maxZoom) {
    	zoomLevel = maxZoom-1;
    }
    else {
    	zoomLevel = zoomStart-1;
    }
	zoomInButton.setEnabled(zoomLevel > 0);
	zoomOutButton.setEnabled(zoomLevel < maxZoom - 1);
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.setZoomer(null);
    map.getToolBar().remove(zoomInButton);
    map.getToolBar().remove(zoomOutButton);
  }

  public double getZoomFactor() {
    return zoomFactor[zoomLevel];
  }

  private void scaleBoards(double zoom) {
    for (Enumeration e = map.getAllBoards(); e.hasMoreElements();) {
      Board b = (Board) e.nextElement();
      b.getScaledImage(zoom, map.getView());
    }
  }

  public void zoomIn() {
    if (zoomInButton.isEnabled()) {
      Rectangle r = map.getView().getVisibleRect();
      Point center = new Point(r.x + r.width / 2, r.y + r.height / 2);
      center = map.mapCoordinates(center);

      zoomLevel--;
      zoomInButton.setEnabled(zoomLevel > 0);
      zoomOutButton.setEnabled(zoomLevel < maxZoom - 1);

      map.centerAt(center);

      map.repaint(true);
      map.getView().revalidate();
    }
  }

  public void zoomOut() {
    if (zoomOutButton.isEnabled()) {
      Rectangle r = map.getView().getVisibleRect();
      Point center = new Point(r.x + r.width / 2, r.y + r.height / 2);
      center = map.mapCoordinates(center);

      zoomLevel++;
      zoomInButton.setEnabled(zoomLevel > 0);
      zoomOutButton.setEnabled(zoomLevel < maxZoom - 1);

      map.centerAt(center);

      map.repaint(true);
      map.getView().revalidate();
    }
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "Zoom"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      zoomLevel = zoomStart-1;
	  zoomInButton.setEnabled(zoomLevel > 0);
	  zoomOutButton.setEnabled(zoomLevel < maxZoom - 1);
    }
  }

  public VASSAL.command.Command getRestoreCommand() {
    return null;
  }

}
