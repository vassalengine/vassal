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
package VASSAL.build;

import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Creates an item that is both configurable w/ an edit box {@link AbstractConfigurable} and buildable from the
 * XML buildFile {@link AbstractBuildable}, but which also has a Toolbar launch button.
 */
public abstract class AbstractToolbarItem extends AbstractConfigurable {
  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$

  protected LaunchButton launch; // Our toolbar "launch button"

  private Boolean configDesc; // True if we configure a name/description field

  /**
   * Create a standard toolbar launcher button for this item
   *
   * @param configDesc True if a description field will be configured
   * @param tooltip String tooltip for button
   * @param action  Action Listener when it is clicked
   */
  protected void makeLaunchButton(Boolean configDesc, String tooltip, String button_text, String iconFile, ActionListener action) {
    this.configDesc = configDesc;
    launch = new LaunchButton("", TOOLTIP, BUTTON_TEXT, HOTKEY, ICON, action);
    if (!tooltip.isEmpty()) {
      setAttribute(TOOLTIP, tooltip);
    }
    if (!button_text.isEmpty()) {
      setAttribute(NAME, button_text);
      launch.setAttribute(BUTTON_TEXT, button_text);
    }
    if (!iconFile.isEmpty()) {
      setAttribute(ICON, iconFile);
    }
  }


  /**
   * @return Launch button for this Toolbar item.
   */
  public LaunchButton getLaunchButton () {
    return launch;
  }


  /**
   * This getAttributeNames() will return the items specific to the Toolbar Button - classes extending this should
   * add their own items as well.
   * <p>
   * Lists all the buildFile (XML) attribute names for this component.
   * If this component is ALSO an {@link AbstractConfigurable}, then this list of attributes determines the appropriate
   * attribute order for {@link AbstractConfigurable#getAttributeDescriptions()} and {@link AbstractConfigurable#getAttributeTypes()}.
   *
   * @return a list of all buildFile (XML) attribute names for this component
   */
  @Override
  public String[] getAttributeNames() {
    if (configDesc) {
      return new String[]{NAME, BUTTON_TEXT, TOOLTIP, ICON, HOTKEY};
    }
    else {
      return new String[]{BUTTON_TEXT, TOOLTIP, ICON, HOTKEY};
    }
  }

  /**
   * This getAttributeDescriptions() will return the items specific to the Toolbar Button - classes extending this should
   * add their own items as well.
   *
   * @return an array of Strings describing the buildFile (XML) attributes of this component. These strings are used as prompts in the
   * Properties window for this object, when the component is configured in the Editor. The order of descriptions should
   * be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   */
  @Override
  public String[] getAttributeDescriptions() {
    if (configDesc) {
      return new String[]{
        Resources.getString(Resources.DESCRIPTION),
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL)
      };
    }
    else {
      return new String[]{
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL)
      };
    }
  }

  /**
   * This getAttributeTypes() will return the items specific to the Toolbar Button - classes extending this should
   * add their own items as well.
   *
   * @return the Class for the buildFile (XML) attributes of this component. Valid classes include: String, Integer, Double, Boolean, Image,
   * Color, and KeyStroke, along with any class for which a Configurer exists in VASSAL.configure. The class determines, among other things,
   * which type of {@link AutoConfigurer} will be used to configure the attribute when the object is configured in the Editor.
   * <p>
   * The order of classes should be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    if (configDesc) {
      return new Class<?>[]{
        String.class,
        String.class,
        String.class,
        IconConfig.class,
        NamedKeyStroke.class,
      };
    }
    else {
      return new Class<?>[]{
        String.class,
        String.class,
        IconConfig.class,
        NamedKeyStroke.class,
      };
    }
  }

  /**
   * Configures the toolbar's button icon
   */
  public static class IconConfig implements ConfigurerFactory {
    /**
     * @param c    AutoConfigurable
     * @param key  Key
     * @param name Name
     * @return Configurer for the icon
     */
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((AbstractToolbarItem) c).getLaunchButton().getAttributeValueString(AbstractToolbarItem.ICON));
    }
  }

   /**
   * Classes extending AbstractToolbarItem should call this as a super() method after checking for their own keys.
   *
   * Sets a buildFile (XML) attribute value for this component. The <code>key</code> parameter will be one of those listed in {@link #getAttributeNames}.
   * If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   * <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> will be an instance of
   * the corresponding Class listed in {@link AbstractConfigurable#getAttributeTypes}
   *
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   *              <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> can also be an instance of
   *              the corresponding Class listed in {@link AbstractConfigurable#getAttributeTypes}
   */
  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  /**
   * Classes extending AbstractToolbarItem should call this as a super() method after checking for their own keys.
   *
   * @return a String representation of the XML buildFile attribute with the given name. When initializing a module,
   * this String value will loaded from the XML and passed to {@link #setAttribute}. It is also frequently used for
   * checking the current value of an attribute.
   *
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   */
  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }


  /**
   * The component to be added to the control window toolbar
   */
  protected Component getComponent() {
    return launch;
  }

  @Override
  public void addTo(Buildable parent) {
    GameModule.getGameModule().getToolBar().add(getComponent());
  }

  @Override
  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(getComponent());
    GameModule.getGameModule().getToolBar().revalidate();
  }

  /**
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(getAttributeValueString(BUTTON_TEXT), getAttributeValueString(TOOLTIP));
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOTKEY)));
  }

  /**
   * Classes extending {@link VASSAL.build.AbstractBuildable} should override this method in order to add
   * the names of any image files they use to the collection. For "find unused images" and "search".
   *
   * @param s Collection to add image names to
   */
  @Override
  protected void addLocalImageNames(Collection<String> s) {
    s.add(launch.getIconAttribute());
  }
}