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
package VASSAL.build.module;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.BoxWidget;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.MapWidget;
import VASSAL.build.widget.PanelWidget;
import VASSAL.build.widget.TabWidget;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;

/**
 * A top-level Widget for displaying Charts
 */
public class ChartWindow extends Widget {

  public static final String DEPRECATED_NAME = "label";
  public static final String NAME = "name";
  public static final String BUTTON_TEXT = "text";
  public static final String ICON = "icon";
  public static final String HOTKEY = "hotkey";

  private LaunchButton launch;
  private JDialog frame;
  private Container root;

  private String id;

  public ChartWindow() {
    root = new JPanel();
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        frame.setVisible(!frame.isVisible());
      }
    };
    launch = new LaunchButton(null, BUTTON_TEXT, HOTKEY, ICON, al);

    setAttribute(NAME, "Charts");
    setAttribute(BUTTON_TEXT, "Charts");
  }

  /**
   * Expects to be added to a GameModule.  Adds a JButton to the
   * control window's toolbar.  Pushing the button displays the window
   */
  public void addTo(Buildable b) {
    rebuild();
    int count = 0;
    for (java.util.Enumeration e =
        GameModule.getGameModule().getComponents(ChartWindow.class);
         e.hasMoreElements();) {
      count++;
      e.nextElement();
    }

    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(launch);

    frame = new JDialog(GameModule.getGameModule().getFrame());
    GameModule.getGameModule().addKeyStrokeSource
        (new KeyStrokeSource(frame.getRootPane(), JComponent.WHEN_IN_FOCUSED_WINDOW));

    while (root.getComponentCount() > 0) {
      frame.getContentPane().add(root.getComponent(0));
    }
    root = frame.getContentPane();
    frame.setTitle(launch.getAttributeValueString(DEPRECATED_NAME));
    id = "ChartWindow" + count;
    String key = PositionOption.key + id;
    GameModule.getGameModule().getPrefs().addOption
        (new PositionOption(key, frame));
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
  }

  public void setAttribute(String key, Object val) {
    if (DEPRECATED_NAME.equals(key)) {
      setAttribute(NAME, val);
      setAttribute(BUTTON_TEXT, val);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) val);
      launch.setToolTipText((String) val);
      if (frame != null) {
        frame.setTitle((String) val);
      }
    }
    else {
      launch.setAttribute(key, val);
    }
  }

  /**
   * The attributes of a ChartWindow are:
   * <code>NAME</code> Appears as the name of the button in the toolbar and the window itself
   * <code>HOTKEY</code> for the hotkey equivalent for the button
   */
  public String[] getAttributeNames() {
    String[] s = {NAME, BUTTON_TEXT, ICON, HOTKEY};
    return s;
  }

  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else {
      return launch.getAttributeValueString(name);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{VASSAL.build.widget.Chart.class,
                       VASSAL.build.widget.HtmlChart.class,
                       TabWidget.class,
                       PanelWidget.class,
                       BoxWidget.class,
                       ListWidget.class,
                       MapWidget.class};
  }

  public void add(Buildable b) {
    if (b instanceof Widget) {
      root.add(((Widget) b).getComponent());
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof Widget) {
      root.remove(((Widget) b).getComponent());
    }
    super.remove(b);
  }

  public java.awt.Component getComponent() {
    return root;
  }

  public static String getConfigureTypeName() {
    return "Charts";
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name", "Button text", "Button icon", "Hotkey"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/chart.gif");
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.htm");
  }
}

