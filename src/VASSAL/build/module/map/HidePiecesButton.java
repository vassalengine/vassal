/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Ben smith
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

import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JPanel;

import org.w3c.dom.Element;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.Translatable;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;

/**
 * This removes all game pieces from the (@link Map)
 * therefore providing an un-cluttered view.
 */
public class HidePiecesButton extends JPanel implements MouseListener,
    AutoConfigurable, GameComponent, Drawable {
  private static final long serialVersionUID = 1L;

  protected boolean piecesVisible = false;
  protected Map map;
  protected LaunchButton launch;
  protected String showingIcon;
  protected String hiddenIcon;
  protected ComponentI18nData myI18nData;
  public static final String DEFAULT_SHOWING_ICON = "/images/globe_unselected.gif";
  public static final String DEFAULT_HIDDEN_ICON = "/images/globe_selected.gif";

  public static final String HOTKEY = "hotkey";
  public static final String HIDDEN_ICON = "hiddenIcon";
  public static final String SHOWING_ICON = "showingIcon";
  public static final String LAUNCH_ICON = "icon";
  public static final String TOOLTIP = "tooltip";
  public static final String BUTTON_TEXT = "buttonText";


  public HidePiecesButton() {
    ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setPiecesVisible(!piecesVisible);
      }
    };
    launch = new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, LAUNCH_ICON, al);
    launch.setAttribute(TOOLTIP, "Hide all pieces on this map");
    addMouseListener(this);
  }

  /**
   * Expects to be added to a {@link Map}.  Adds itself as a {@link
   * GameComponent} and a {@link Drawable} component */
  public void addTo(Buildable b) {
    map = (Map) b;

    GameModule.getGameModule().getGameState().addGameComponent(this);

    map.addDrawComponent(this);

    map.getToolBar().add(launch);

    if (b instanceof Translatable) {
      getI18nData().setOwningComponent((Translatable) b);
    }
  }

  protected void setPiecesVisible(boolean visible) {
    map.setPiecesVisible(visible);
    launch.setAttribute(LAUNCH_ICON, visible ? showingIcon : hiddenIcon);
    piecesVisible = visible;
    map.repaint();
  }

  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.removeDrawComponent(this);
    map.getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
  }

  public void setAttribute(String key, Object value) {
    if (SHOWING_ICON.equals(key)) {
      showingIcon = (String) value;
    }
    else if (HIDDEN_ICON.equals(key)) {
      hiddenIcon = (String) value;
    }
    else {
      launch.setAttribute(key,value);
    }
  }

  public void build(Element e) {
    AutoConfigurable.Util.buildAttributes(e, this);
  }

  public String[] getAttributeNames() {
    return new String[]{BUTTON_TEXT, TOOLTIP, HOTKEY, SHOWING_ICON, HIDDEN_ICON};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    return null;
  }

  public String getAttributeValueString(String key) {
    String s = null;
    if (HIDDEN_ICON.equals(key)) {
      s = hiddenIcon;
    }
    else if (SHOWING_ICON.equals(key)) {
      s = showingIcon;
    }
    else {
      s = launch.getAttributeValueString(key);
    }
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.HOTKEY_LABEL),
      Resources.getString("Editor.HidePieceButton.show_icon"), //$NON-NLS-1$
      Resources.getString("Editor.HidePieceButton.hide_icon"), //$NON-NLS-1$
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      NamedKeyStroke.class,
      ShowingIconConfig.class,
      HiddenIconConfig.class
    };
  }

  public static class ShowingIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key,name,DEFAULT_SHOWING_ICON);
    }
  }

  public static class HiddenIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key,name,DEFAULT_HIDDEN_ICON);
    }
  }

  public void draw(Graphics g, Map m) {
    repaint();
  }

  public boolean drawAboveCounters() {
    return false;
  }

  public void paint(Graphics g) {
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
  }

  public String getToolTipText(MouseEvent e) {
    return null;
  }

  public Command getRestoreCommand() {
    return null;
  }

  public void setup(boolean show) {
    if (show) {
      setPiecesVisible(true);
    }
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.HidePieceButton.component_type"); //$NON-NLS-1$
  }

  public String getConfigureName() {
    return null;
  }

  public Configurer getConfigurer() {
    return new AutoConfigurer(this);
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public void addPropertyChangeListener(java.beans.PropertyChangeListener l) {
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "HidePieces");
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return AutoConfigurable.Util.getBuildElement(doc, this);
  }

  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, "HidePieces");
    }
    return myI18nData;
  }
}


