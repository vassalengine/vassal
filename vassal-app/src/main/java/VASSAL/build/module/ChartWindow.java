/*
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

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.BoxWidget;
import VASSAL.build.widget.Chart;
import VASSAL.build.widget.HtmlChart;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.MapWidget;
import VASSAL.build.widget.PanelWidget;
import VASSAL.build.widget.TabWidget;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;

/**
 * A top-level Widget for displaying Charts
 */
public class ChartWindow extends Widget {
  public static final String DEPRECATED_NAME = "label"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  protected LaunchButton launch;
  protected JDialog frame;
  protected Container root;
  protected String tooltip = ""; //$NON-NLS-1$
  protected String id;

  public ChartWindow() {
    root = new JPanel();
    ActionListener al = new ActionListener() {
      boolean initialized;

      @Override
      public void actionPerformed(ActionEvent e) {
        if (!initialized) {
          String key = PositionOption.key + id;
          GameModule.getGameModule().getPrefs().addOption(new PositionOption(key, frame));
          initialized = true;
        }
        frame.setVisible(!frame.isVisible());
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, al);
    setAttribute(NAME, Resources.getString("Editor.ChartWindow.component_type"));
    setAttribute(BUTTON_TEXT, Resources.getString("Editor.ChartWindow.component_type"));
    launch.setAttribute(TOOLTIP, Resources.getString("Editor.ChartWindow.component_type"));
  }

  /**
   * Expects to be added to a GameModule. Adds a JButton to the control window's toolbar. Pushing the button displays
   * the window
   */
  @Override
  public void addTo(Buildable b) {
    rebuild();
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getToolBar().add(launch);
    frame = new JDialog(GameModule.getGameModule().getFrame());
    GameModule.getGameModule().addKeyStrokeSource(new KeyStrokeSource(frame.getRootPane(), JComponent.WHEN_IN_FOCUSED_WINDOW));
    while (root.getComponentCount() > 0) {
      frame.add(root.getComponent(0));
    }
    root = frame.getContentPane();
    frame.setTitle(launch.getAttributeValueString(DEPRECATED_NAME));
    int count = GameModule.getGameModule().getComponentsOf(ChartWindow.class).size();
    id = "ChartWindow" + count; //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (DEPRECATED_NAME.equals(key)) {
      setAttribute(NAME, val);
      setAttribute(BUTTON_TEXT, val);
    }
    else if (NAME.equals(key)) {
      setConfigureName((String) val);
      if (tooltip.length() == 0) {
        launch.setToolTipText((String) val);
      }
      if (frame != null) {
        frame.setTitle((String) val);
      }
    }
    else if (TOOLTIP.equals(key)) {
      tooltip = (String) val;
      launch.setAttribute(key, val);
    }
    else {
      launch.setAttribute(key, val);
    }
  }

  /**
   * The attributes of a ChartWindow are: <code>NAME</code> Appears as the name of the button in the toolbar and the
   * window itself <code>HOTKEY</code> for the hotkey equivalent for the button
   */
  @Override
  public String[] getAttributeNames() {
    return new String[] {NAME, BUTTON_TEXT, TOOLTIP, ICON, HOTKEY};
  }

  @Override
  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (TOOLTIP.equals(name)) {
      return tooltip.length() == 0 ? launch.getAttributeValueString(name) : tooltip;
    }
    else {
      return launch.getAttributeValueString(name);
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{
      Chart.class,
      HtmlChart.class,
      TabWidget.class,
      PanelWidget.class,
      BoxWidget.class,
      ListWidget.class,
      MapWidget.class
    };
  }

  @Override
  public void add(Buildable b) {
    if (b instanceof Widget) {
      root.add(((Widget) b).getComponent());
    }
    super.add(b);
  }

  @Override
  public void remove(Buildable b) {
    if (b instanceof Widget) {
      root.remove(((Widget) b).getComponent());
    }
    super.remove(b);
  }

  @Override
  public java.awt.Component getComponent() {
    return root;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ChartWindow.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString(Resources.NAME_LABEL),
          Resources.getString(Resources.BUTTON_TEXT),
          Resources.getString(Resources.TOOLTIP_TEXT),
          Resources.getString(Resources.BUTTON_ICON),
          Resources.getString(Resources.HOTKEY_LABEL)
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/chart.gif"); //$NON-NLS-1$
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.htm"); //$NON-NLS-1$
  }
}
