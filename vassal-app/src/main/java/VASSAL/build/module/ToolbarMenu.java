/*
 *
 * Copyright (c) 2006 Rodney Kinney
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

import java.awt.Component;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.ToolBarComponent;
import org.apache.commons.lang3.ArrayUtils;

/**
 * Takes buttons from the toolbar of a Map or the main module and places
 * them into a popup menu
 *
 * @author rkinney
 *
 */
public class ToolbarMenu extends AbstractToolbarItem
                         implements ContainerListener,
                                    PropertyChangeListener,
                                    GameComponent {
  public static final String DESCRIPTION = "description"; //NON-NLS //non-standard legacy name key different from AbstractToolbarItem

  // These four items here for clirr purposes
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_ICON = "icon"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String BUTTON_HOTKEY = "hotkey"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$

  public static final String MENU_ITEMS = "menuItems"; //$NON-NLS-1$
  /** Buttons where this property contains a JPopupMenu will turn into sub-menus */
  public static final String MENU_PROPERTY = "ToolbarMenu.popup"; //$NON-NLS-1$
  public static final String HIDDEN_BY_TOOLBAR = "hidden"; //$NON-NLS-1$
  protected List<String> menuItems = new ArrayList<>();
  protected Map<AbstractButton, JMenuItem> buttonsToMenuMap =
    new HashMap<>();

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected JToolBar toolbar;
  protected JPopupMenu menu;
  protected Runnable menuBuilder;

  public ToolbarMenu() {
    setNameKey(DESCRIPTION); // We have a legacy name key that's different from the standard AbstractToolbarItem name key

    setLaunchButton(makeLaunchButton(
      Resources.getString("Editor.ToolbarMenu.tooltip_text"),
      Resources.getString(Resources.MENU),
      "",
      e -> launch()
    ));
    launch = getLaunchButton(); // for compatibility

    menu = new JPopupMenu();
    getLaunchButton().putClientProperty(MENU_PROPERTY, menu);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public void launch() {
    final LaunchButton lb = getLaunchButton();
    if (lb.isShowing()) {
      menu.show(lb, 0, 0);
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(super.getAttributeDescriptions(),
                             Resources.getString("Editor.ToolbarMenu.menu_entries")); //$NON-NLS-1$
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(super.getAttributeTypes(),
                             String[].class
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(super.getAttributeNames(),
      MENU_ITEMS
    );
  }

  @Override
  public String getAttributeValueString(String key) {
    if (MENU_ITEMS.equals(key)) {
      return StringArrayConfigurer.arrayToString(
        menuItems.toArray(new String[0]));
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (MENU_ITEMS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      menuItems = new ArrayList<>(Arrays.asList((String[]) value));
      if (toolbar != null) {
        scheduleBuildMenu();
      }
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof ToolBarComponent) {
      toolbar = ((ToolBarComponent) parent).getToolBar();
    }
    toolbar.add(getLaunchButton());
    toolbar.addContainerListener(this);
    scheduleBuildMenu();
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ToolbarMenu.html"); //$NON-NLS-1$
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ToolbarMenu.component_type"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
    toolbar.remove(getLaunchButton());
    toolbar.removeContainerListener(this);
  }

  protected void buildMenu() {
    for (final AbstractButton b : buttonsToMenuMap.keySet()) {
      b.removePropertyChangeListener(this);
      b.setVisible(true);
      b.putClientProperty(HIDDEN_BY_TOOLBAR, null);
    }
    buttonsToMenuMap.clear();
    menu.removeAll();
    final HashMap<String, JButton> nameToButton = new HashMap<>();
    if (toolbar != null) {
      final int n = toolbar.getComponentCount();
      for (int i = 0; i < n; ++i) {
        if (toolbar.getComponentAtIndex(i) instanceof JButton) {
          final JButton b = ((JButton) toolbar.getComponentAtIndex(i));
          String text =
            (String) b.getClientProperty(LaunchButton.UNTRANSLATED_TEXT);
          if (text == null) {
            text = b.getText();
          }
          nameToButton.put(text, b);
        }
      }
    }

    for (final String item : menuItems) {
      final JButton b = nameToButton.get(item);
      if (b != null) {
        final Object property = b.getClientProperty(MENU_PROPERTY);
        b.addPropertyChangeListener(this);
        b.setVisible(false);
        b.putClientProperty(HIDDEN_BY_TOOLBAR, Boolean.TRUE);

        if (property instanceof JPopupMenu) {
          // This button corresponds to another ToolbarMenu button.
          // Turn it into a submenu.
          final JPopupMenu toolbarMenu = (JPopupMenu) property;
          toolbarMenu.addContainerListener(this);
          final JMenu subMenu = new JMenu(b.getText());
          final Component[] items = toolbarMenu.getComponents();
          for (final Component component : items) {
            final JMenuItem otherItem = (JMenuItem) component;
            final JMenuItem myItem =
              new JMenuItem(otherItem.getText(), otherItem.getIcon());
            myItem.addActionListener(e -> otherItem.doClick());
            subMenu.add(myItem);
            buttonsToMenuMap.put(otherItem, myItem);
          }
          buttonsToMenuMap.put(b, subMenu);
          menu.add(subMenu);
        }
        else {
          final JMenuItem mi = new JMenuItem(b.getText(), b.getIcon());
          mi.setEnabled(b.isEnabled());
          mi.addActionListener(e -> b.doClick());
          buttonsToMenuMap.put(b, mi);
          menu.add(mi);
        }
      }
    }
  }

  protected void scheduleBuildMenu() {
    if (menuBuilder == null) {
      menuBuilder = () -> {
        buildMenu();
        menuBuilder = null;
      };
      SwingUtilities.invokeLater(menuBuilder);
    }
  }

  @Override
  public void componentAdded(ContainerEvent e) {
    scheduleBuildMenu();
  }

  @Override
  public void componentRemoved(ContainerEvent e) {
    scheduleBuildMenu();
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    final JButton b = (JButton) evt.getSource();
    final JMenuItem mi = buttonsToMenuMap.get(b);
    if (mi != null) {
      if (AbstractButton.TEXT_CHANGED_PROPERTY.equals(evt.getPropertyName())) {
        scheduleBuildMenu();
      }
      else if ("enabled".equals(evt.getPropertyName())) { //$NON-NLS-1$
        mi.setEnabled(b.isEnabled());
      }
      else if (AbstractButton.ICON_CHANGED_PROPERTY.equals(evt.getPropertyName())) {
        mi.setIcon(b.getIcon());
      }
    }
  }

  @Override
  public void setup(boolean gameStarting) {
    // Prevent our Toolbar buttons from becoming visible on Game close/reopen
    scheduleBuildMenu();
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    final List<String> l = new ArrayList<>(super.getMenuTextList());
    l.addAll(menuItems);
    return l;
  }
}
