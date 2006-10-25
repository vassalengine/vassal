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

import java.awt.BorderLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.BoxWidget;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.PanelWidget;
import VASSAL.build.widget.PieceSlot;
import VASSAL.build.widget.TabWidget;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.UniqueIdManager;

/**
 * A window from which players can create new {@link GamePiece}s by
 * clicking and dragging from the PieceWindow.  The actual GamePieces
 * are contained in {@link PieceSlot} components.  PieceWindow extends
 * {@link Widget}, so it may be composed of various tabs, lists, etc.  */
public class PieceWindow extends Widget implements UniqueIdManager.Identifyable {
  private String id;
  private LaunchButton launch;
  private boolean hidden;
  public static final String DEPRECATED_NAME = "entryName";
  public static final String NAME = "name";
  public static final String BUTTON_TEXT = "text";
  public static final String ICON = "icon";
  public static final String HOTKEY = "hotkey";
  public static final String HIDDEN = "hidden";
  private static UniqueIdManager idMgr = new UniqueIdManager("PieceWindow");
  private JComponent root;
  private ComponentSplitter.SplitPane mainWindowDock;

  public PieceWindow() {
    root = new JPanel(new BorderLayout());
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launchButtonPressed();
      }
    };
    launch = new LaunchButton("Pieces", BUTTON_TEXT, HOTKEY, ICON, al);
    launch.setToolTipText("Show/Hide the Pieces window");
  }

  private Window initFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      final JDialog d = new JDialog(GameModule.getGameModule().getFrame());
      d.getContentPane().add(root);
      d.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      d.setTitle(getConfigureName());
      addPropertyChangeListener(new java.beans.PropertyChangeListener() {
        public void propertyChange(java.beans.PropertyChangeEvent e) {
          if (Configurable.NAME_PROPERTY
              .equals(e.getPropertyName())) {
            d.setTitle((String) e.getNewValue());
          }
        }
      });
      return d;
    }
    else {
      final JFrame d = new JFrame();
      d.getContentPane().add(root);
      d.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      d.setTitle(getConfigureName());
      addPropertyChangeListener(new java.beans.PropertyChangeListener() {
        public void propertyChange(java.beans.PropertyChangeEvent e) {
          if (Configurable.NAME_PROPERTY
              .equals(e.getPropertyName())) {
            d.setTitle((String) e.getNewValue());
          }
        }
      });
      return d;
    }
  }

  public void launchButtonPressed() {
    if (mainWindowDock != null) {
      mainWindowDock.toggleVisibility();
    }
    else {
      root.getTopLevelAncestor().setVisible(!root.getTopLevelAncestor().isVisible());
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PieceWindow.htm");
  }

  public void build(org.w3c.dom.Element e) {
    super.build(e);
    rebuild();
  }

  public boolean shouldDockIntoMainWindow() {
    return "PieceWindow0".equals(id);
  }

  public java.awt.Component getComponent() {
    return root;
  }

  public static String getConfigureTypeName() {
    return "Game Piece Palette";
  }

  /**
   * A PieceWindow may contain a {@link TabWidget}, a {@link
   * PanelWidget}, a {@link BoxWidget}, a {@link ListWidget}, or a
   * {@link PieceSlot} */
  public Class[] getAllowableConfigureComponents() {
    return new Class[]{TabWidget.class, PanelWidget.class, BoxWidget.class, ListWidget.class, PieceSlot.class};
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

  /**
   * Each instanceof PieceWindow has a unique String identifier
   *
   * @return the identifier for this PieceWindow
   */
  public String getId() {
    return id;
  }

  /**
   * Each instanceof PieceWindow has a unique String identifier
   */
  public void setId(String s) {
    id = s;
  }

  /**
   * Expects to be added to a {@link GameModule}.  When added, sets
   * the containing window to visible */
  public void addTo(Buildable parent) {
    idMgr.add(this);

    if (!hidden) {
      String key = PositionOption.key + getConfigureName();
      if ("PieceWindow0".equals(id) && GlobalOptions.getInstance().isUseSingleWindow()) {
        mainWindowDock = new ComponentSplitter().splitLeft(GameModule.getGameModule().getControlPanel(), root, false);
      }
      else {
        Window w = initFrame();
        final PositionOption pos = new VisibilityOption(key, w);
        GameModule.getGameModule().getPrefs().addOption(pos);
      }
      GameModule.getGameModule().getToolBar().add(launch);
    }
  }

  public void removeFrom(Buildable parent) {
    if (mainWindowDock == null && root != null && root.getTopLevelAncestor() != null) {
      root.getTopLevelAncestor().setVisible(false);
    }
    GameModule.getGameModule().getToolBar().remove(launch);
    idMgr.remove(this);
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name", "Hidden (requires restart)", "Button text", "Button icon", "Hotkey to show/hide"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, Boolean.class, String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/counter.gif");
    }
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, HIDDEN, BUTTON_TEXT, ICON, HOTKEY};
  }

  public void setAttribute(String name, Object value) {
    if (DEPRECATED_NAME.equals(name)) {
      setAttribute(NAME, value);
      setAttribute(BUTTON_TEXT, value);
    }
    else if (NAME.equals(name)) {
      String s = (String) value;
      setConfigureName(s);
      launch.setToolTipText("Show/Hide the " + s + " window");
    }
    else if (HIDDEN.equals(name)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String)value);
      }
      hidden = ((Boolean)value).booleanValue();
    }
    else {
      launch.setAttribute(name, value);
    }
  }

  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (HIDDEN.equals(name)) {
      return String.valueOf(hidden);
    }
    else {
      return launch.getAttributeValueString(name);
    }
  }
}
