/*
 * $Id$
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ToolBarComponent;

/**
 * Takes buttons from the toolbar of a Map or the main module and places
 * them into a popup menu
 *
 * @author rkinney
 *
 */
public class ToolbarMenu extends AbstractConfigurable
                         implements ContainerListener,
                                    PropertyChangeListener,
                                    GameComponent {
  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String BUTTON_ICON = "icon"; //$NON-NLS-1$
  public static final String BUTTON_HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String MENU_ITEMS = "menuItems"; //$NON-NLS-1$
  public static final String DESCRIPTION = "description"; //$NON-NLS-1$
  /** Buttons where this property contains a JPopupMenu will turn into sub-menus */
  public static final String MENU_PROPERTY = "ToolbarMenu.popup"; //$NON-NLS-1$
  public static final String HIDDEN_BY_TOOLBAR = "hidden"; //$NON-NLS-1$
  protected List<String> menuItems = new ArrayList<String>();
  protected Map<AbstractButton,JMenuItem> buttonsToMenuMap =
    new HashMap<AbstractButton,JMenuItem>();
  protected LaunchButton launch;
  protected JToolBar toolbar;
  protected JPopupMenu menu;
  protected Runnable menuBuilder;

  public ToolbarMenu() {
    launch = new LaunchButton(Resources.getString(Resources.MENU), TOOLTIP, BUTTON_TEXT, BUTTON_HOTKEY, BUTTON_ICON, new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        launch();
      }
    });
    menu = new JPopupMenu();
    launch.putClientProperty(MENU_PROPERTY, menu);
    launch.setToolTipText("Display Menu Options");
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public void launch() {
    menu.show(launch, 0, 0);
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
        Resources.getString(Resources.DESCRIPTION),
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.ToolbarMenu.menu_entries")}; //$NON-NLS-1$
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      String.class,
      Icon.class,
      NamedKeyStroke.class,
      String[].class
    };
  }

  public String[] getAttributeNames() {
    return new String[] {
      DESCRIPTION,
      BUTTON_TEXT,
      TOOLTIP,
      BUTTON_ICON,
      BUTTON_HOTKEY,
      MENU_ITEMS
    };
  }

  public String getAttributeValueString(String key) {
    if (MENU_ITEMS.equals(key)) {
      return StringArrayConfigurer.arrayToString(
        menuItems.toArray(new String[menuItems.size()]));
    }
    else if(DESCRIPTION.equals(key)) {
      return getConfigureName();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  public void setAttribute(String key, Object value) {
    if (MENU_ITEMS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      menuItems = new ArrayList<String>(Arrays.asList((String[]) value));
      if (toolbar != null) {
        scheduleBuildMenu();
      }
    }
    else if (BUTTON_TEXT.equals(key)) {
      launch.setAttribute(key, value);
    }
    else if(DESCRIPTION.equals(key)) {
      setConfigureName((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public void addTo(Buildable parent) {
    if (parent instanceof ToolBarComponent) {
      toolbar = ((ToolBarComponent) parent).getToolBar();
    }
    toolbar.add(launch);
    toolbar.addContainerListener(this);
    scheduleBuildMenu();
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ToolbarMenu.htm"); //$NON-NLS-1$
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ToolbarMenu.component_type"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
    toolbar.remove(launch);
    toolbar.removeContainerListener(this);
  }

  protected void buildMenu() {
    for (AbstractButton b : buttonsToMenuMap.keySet()) {
      b.removePropertyChangeListener(this);
      b.setVisible(true);
      b.putClientProperty(HIDDEN_BY_TOOLBAR, null);
    }
    buttonsToMenuMap.clear();
    menu.removeAll();
    HashMap<String,JButton> nameToButton = new HashMap<String,JButton>();
    if (toolbar != null) {
      for (int i = 0, n = toolbar.getComponentCount(); i < n; ++i) {
        if (toolbar.getComponentAtIndex(i) instanceof JButton) {
          JButton b = ((JButton) toolbar.getComponentAtIndex(i));
          String text =
            (String) b.getClientProperty(LaunchButton.UNTRANSLATED_TEXT);
          if (text == null) {
            text = b.getText();
          }
          nameToButton.put(text, b);
        }
      }
    }

    for (String item : menuItems) {
      final JButton b = nameToButton.get(item);
      if (b != null) {
        Object property = b.getClientProperty(MENU_PROPERTY);
        b.addPropertyChangeListener(this);
        b.setVisible(false);
        b.putClientProperty(HIDDEN_BY_TOOLBAR, new Boolean(true));

        if (property instanceof JPopupMenu) {
          // This button corresponds to another ToolbarMenu button.
          // Turn it into a submenu.
          JPopupMenu toolbarMenu = (JPopupMenu) property;
          toolbarMenu.addContainerListener(this);
          JMenu subMenu = new JMenu(b.getText());
          Component[] items = toolbarMenu.getComponents();
          for (int i = 0; i < items.length; i++) {
            final JMenuItem otherItem = (JMenuItem) items[i];
            JMenuItem myItem =
              new JMenuItem(otherItem.getText(), otherItem.getIcon());
            myItem.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                otherItem.doClick();
              }
            });
            subMenu.add(myItem);
            buttonsToMenuMap.put(otherItem, myItem);
          }
          buttonsToMenuMap.put(b, subMenu);
          menu.add(subMenu);
        }
        else {
          JMenuItem mi = new JMenuItem(b.getText(), b.getIcon());
          mi.setEnabled(b.isEnabled());
          mi.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              b.doClick();
            }
          });
          buttonsToMenuMap.put(b, mi);
          menu.add(mi);
        }
      }
    }
  }

  protected void scheduleBuildMenu() {
    if (menuBuilder == null) {
      menuBuilder = new Runnable() {
        public void run() {
          buildMenu();
          menuBuilder = null;
        }
      };
      SwingUtilities.invokeLater(menuBuilder);
    }
  }

  public void componentAdded(ContainerEvent e) {
    scheduleBuildMenu();
  }

  public void componentRemoved(ContainerEvent e) {
    scheduleBuildMenu();
  }

  public void propertyChange(PropertyChangeEvent evt) {
    JButton b = (JButton) evt.getSource();
    JMenuItem mi = buttonsToMenuMap.get(b);
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

  public void setup(boolean gameStarting) {
    // Prevent our Toolbar buttons from becoming visible on Game close/reopen
    scheduleBuildMenu();
  }

  public Command getRestoreCommand() {
    return null;
  }

}
