/*
 *
 * Copyright (c) 2020 by vassalengine.org, Brian Reynolds
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

import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Command;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedExpressionConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ToolBarComponent;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Creates an item that is both configurable w/ an edit box {@link AbstractConfigurable} and buildable from the
 * XML buildFile {@link AbstractBuildable}, but which also has a Toolbar launch button.
 */
public abstract class AbstractToolbarItem extends AbstractConfigurable implements GameComponent, PropertyChangeListener {

  // These are the "standard keys" - recommended for all new classes extending AbstractToolbarItem
  public static final String NAME          = "name";    //$NON-NLS-1$
  public static final String TOOLTIP       = "tooltip"; //$NON-NLS-1$
  public static final String BUTTON_TEXT   = "text";    //$NON-NLS-1$
  public static final String HOTKEY        = "hotkey";  //$NON-NLS-1$
  public static final String ICON          = "icon";    //$NON-NLS-1$
  public static final String CAN_DISABLE   = "canDisable";   //NON-NLS
  public static final String PROPERTY_GATE = "propertyGate"; //NON-NLS
  public static final String DISABLED_ICON = "disabledIcon"; //NON-NLS
  public static final String HIDE_WHEN_DISABLED = "hideWhenDisabled"; //NON-NLS

  protected LaunchButton launch;              // Our toolbar "launch button"

  protected IconConfigurer disabledIconConfig = new IconConfigurer(DISABLED_ICON, null, null);

  protected boolean showDisabledOptions = true; // True if our configurers are allowed to show the disable-this-button properties
  protected boolean canDisable = false;         // True if we have a disable-this-button property
  protected boolean hideWhenDisabled = false;   // True if we hide the button when disabled
  protected String propertyGate = "";           // Name of our gating property (button is disabled if this global property is "true")
  protected MutableProperty.Impl property;

  private String nameKey       = NAME;        // Some legacy objects will want to use a non-standard key (or none)
  private String tooltipKey    = TOOLTIP;     // Some legacy objects will want to use a non-standard key
  private String buttonTextKey = BUTTON_TEXT; // Some legacy objects will want to use a non-standard key
  private String hotKeyKey     = HOTKEY;      // Some legacy objects will want to use a non-standard key
  private String iconKey       = ICON;        // Some legacy objects will want to use a non-standard key

  private String namePrompt = ""; // Overrides the default value returned for the name key by getAttributeDescriptions.

  protected void setNamePrompt(String namePrompt) {
    this.namePrompt = namePrompt;
  }

  protected void setNameKey(String nameKey) {
    this.nameKey = nameKey;
  }

  protected String getNameKey() {
    return nameKey;
  }

  protected void setTooltipKey(String tooltipKey) {
    this.tooltipKey = tooltipKey;
  }

  protected void setButtonTextKey(String buttonTextKey) {
    this.buttonTextKey = buttonTextKey;
  }

  protected void setHotKeyKey(String hotKeyKey) {
    this.hotKeyKey = hotKeyKey;
  }

  protected void setIconKey(String iconKey) {
    this.iconKey = iconKey;
  }

  protected void setShowDisabledOptions(boolean show) {
    showDisabledOptions = show;
  }

  public boolean isShowDisabledOptions() {
    return showDisabledOptions;
  }

  /**
   * Create a standard toolbar launcher button for this item
   *
   * @param tooltip String tooltip for button
   * @param button_text Text for button
   * @param iconFile filename for icon default
   * @param action  Action Listener when launch button is clicked
   * @return launch button
   */
  protected LaunchButton makeLaunchButton(String tooltip, String button_text, String iconFile, ActionListener action) {
    launch = new LaunchButton(button_text, tooltipKey, buttonTextKey, hotKeyKey, iconKey, action, true);
    //launch = new LaunchButton(button_text, tooltipKey, buttonTextKey, hotKeyKey, iconKey, action);
    if (!tooltip.isEmpty()) {
      setAttribute(tooltipKey, tooltip);
    }
    if (!button_text.isEmpty()) {
      if (!nameKey.isEmpty()) {
        setAttribute(nameKey, button_text);
      }
      launch.setAttribute(buttonTextKey, button_text);
    }
    if (!iconFile.isEmpty()) {
      setAttribute(iconKey, iconFile);
    }

    launch.setDisabledIcon(disabledIconConfig.getIconValue());

    checkDisabled();

    return launch;
  }

  /**
   * @return Launch button for this Toolbar item.
   */
  public LaunchButton getLaunchButton() {
    return launch;
  }

  /**
   * Sets launch button for this toolbar item
   * @param launch - launch button
   */
  protected void setLaunchButton(LaunchButton launch) {
    this.launch = launch;
  }

  protected void addPropertyGateListener() {
    addPropertyGateListener(false);
  }

  /**
   * If we have a disable-this-button property, set a listener on it
   */
  protected void addPropertyGateListener(boolean dontCheck) {
    if (property != null) {
      removePropertyGateListener();
    }

    if (isShowDisabledOptions() && canDisable) {
      if (!propertyGate.isEmpty()) {
        final Buildable ancestor = getNonFolderAncestor();
        property = (ancestor instanceof Map) ? (MutableProperty.Impl) ((Map)ancestor).getMutableProperty(propertyGate) : null;
        if (property == null) {
          property = (MutableProperty.Impl) GameModule.getGameModule().getMutableProperty(propertyGate);
        }
        if (property != null) {
          property.addMutablePropertyChangeListener(this);
        }
      }
    }

    if (!dontCheck) {
      checkDisabled();
    }
  }

  /**
   * Remove any existing disable-this-button property
   */
  protected void removePropertyGateListener() {
    if (property != null) {
      property.removeMutablePropertyChangeListener(this);
      property = null;
    }
  }

  /**
   * Listens to our disable-this-button property; enables/disables our button as appropriate when it changes
   * @param evt property change event
   */
  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    final String name = evt.getPropertyName();
    if (name.equals(propertyGate)) {
      final String value = String.valueOf(evt.getNewValue());
      disableIfTrue("true".equals(value)); //NON-NLS
    }
  }

  /**
   * Check our disable-this-button property and enable/disable our button as appropriate based on its value
   */
  public void checkDisabled() {
    if ((!isShowDisabledOptions() || !canDisable) && (launch != null)) {
      launch.setEnabled(true);

      // Disabling hide-when-disabled for now, it seems to be creating problems
      //launch.setForceInvisible(false);
      //launch.checkVisibility();

      return;
    }
    if ((property == null) && !propertyGate.isEmpty()) {
      addPropertyGateListener(true);
    }
    if (property != null) {
      disableIfTrue("true".equals(property.getPropertyValue())); //NON-NLS
    }
  }

  /**
   * @param disable true to disable our launch button, false to enable it
   */
  public void disableIfTrue(boolean disable) {
    if (launch != null) {
      launch.setEnabled(!isShowDisabledOptions() || !canDisable || !disable);

      // Disabling hide-when-disabled for now, it seems to be creating problems
      //launch.setForceInvisible(disable && canDisable && hideWhenDisabled);
      //launch.checkVisibility();
    }
  }

  @Override
  public void setup(boolean gameStarting) {
    addPropertyGateListener(); // Always ensure our property gate listener is in place
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * This getAttributeNames() will return the items specific to the Toolbar Button - classes extending this should
   * add their own items as well. If the "nameKey" is blank, then no "name" configure entry will be generated.
   * Extending classes can use ArrayUtils.addAll(super.getAttributeNames(), key1, ..., keyN), or supply their own
   * order from scratch.
   * <p>
   * Lists all the buildFile (XML) attribute names for this component.
   * If this component is ALSO an {@link AbstractConfigurable}, then this list of attributes determines the appropriate
   * attribute order for {@link AbstractConfigurable#getAttributeDescriptions()} and {@link AbstractConfigurable#getAttributeTypes()}.
   *
   * @return an array of all buildFile (XML) attribute names for this component
   */
  @Override
  public String[] getAttributeNames() {
    if (!nameKey.isEmpty()) {
      return new String[]{nameKey, buttonTextKey, tooltipKey, iconKey, hotKeyKey, CAN_DISABLE, PROPERTY_GATE, HIDE_WHEN_DISABLED, DISABLED_ICON};
    }
    else {
      return new String[]{buttonTextKey, tooltipKey, iconKey, hotKeyKey, CAN_DISABLE, PROPERTY_GATE, HIDE_WHEN_DISABLED, DISABLED_ICON};
    }
  }

  /**
   * This getAttributeDescriptions() will return the items specific to the Toolbar Button - classes extending this should
   * add their own items as well. If the "nameKey" is blank, then no "name" configure entry will be generated.
   * Extending classes can use ArrayUtils.addAll(super.getAttributeDescriptions(), key1, ..., keyN), or supply their own
   * order from scratch.
   *
   * @return an array of Strings describing the buildFile (XML) attributes of this component. These strings are used as prompts in the
   * Properties window for this object, when the component is configured in the Editor. The order of descriptions should
   * be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   */
  @Override
  public String[] getAttributeDescriptions() {
    if (!nameKey.isEmpty()) {
      return new String[]{
        namePrompt.isEmpty() ? Resources.getString(Resources.DESCRIPTION) : namePrompt,
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.AbstractToolbarItem.can_disable"),
        Resources.getString("Editor.AbstractToolbarItem.property_gate"),
        Resources.getString("Editor.AbstractToolbarItem.hide_when_disabled"),
        Resources.getString("Editor.AbstractToolbarItem.disabled_icon"),
      };
    }
    else {
      return new String[]{
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.AbstractToolbarItem.can_disable"),
        Resources.getString("Editor.AbstractToolbarItem.property_gate"),
        Resources.getString("Editor.AbstractToolbarItem.hide_when_disabled"),
        Resources.getString("Editor.AbstractToolbarItem.disabled_icon"),
      };
    }
  }

  /**
   * This getAttributeTypes() will return the items specific to the Toolbar Button - classes extending this should
   * add their own items as well. If the "nameKey" is blank, then no "name" configure entry will be generated.
   * Extending classes can use ArrayUtils.addAll(super.getAttributeTypes(), key1, ..., keyN), or supply their own
   * order from scratch.
   *
   * @return the Class for the buildFile (XML) attributes of this component. Valid classes include: String, Integer, Double, Boolean, Image,
   * Color, and KeyStroke, along with any class for which a Configurer exists in VASSAL.configure. The class determines, among other things,
   * which type of {@link AutoConfigurer} will be used to configure the attribute when the object is configured in the Editor.
   * <p>
   * The order of classes should be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    if (!nameKey.isEmpty()) {
      return new Class<?>[]{
        String.class,
        FormattedStringConfig.class,
        String.class,
        IconConfig.class,
        NamedKeyStroke.class,
        Boolean.class,
        String.class,
        Boolean.class,
        IconConfig.class,
      };
    }
    else {
      return new Class<?>[]{
        FormattedStringConfig.class,
        String.class,
        IconConfig.class,
        NamedKeyStroke.class,
        Boolean.class,
        String.class,
        Boolean.class,
        IconConfig.class
      };
    }
  }


  public static class FormattedStringConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedExpressionConfigurer(key, name, new String[]{});
    }
  }

  /**
   * Configures the toolbar's button icon. Classes extending AbstractToolbarItem no longer need their own IconConfig
   * for the toolbar button, though some must presently keep it for binary compatibility.
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
   * Classes extending AbstractToolbarItem can call this as a super() method after checking for their own keys, to
   * avoid needing to deal with the nitty gritty of the toolbar button.
   * <p>
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
    if (!nameKey.isEmpty() && nameKey.equals(key)) {
      setConfigureName((String) value);
    }
    else if (CAN_DISABLE.equals(key)) {
      if (value instanceof String) {
        canDisable = "true".equals(value); //NON-NLS
      }
      else if (value instanceof Boolean) {
        canDisable = (Boolean)value;
      }
      checkDisabled();
    }
    else if (PROPERTY_GATE.equals(key)) {
      propertyGate = (String)value;
      addPropertyGateListener();
    }
    else if (DISABLED_ICON.equals(key)) {
      if (value instanceof String) {
        disabledIconConfig.setValue((String) value);
        if (launch != null) {
          launch.setDisabledIcon(disabledIconConfig.getIconValue());
        }
      }
    }
    else if (HIDE_WHEN_DISABLED.equals(key)) {
      if (value instanceof String) {
        hideWhenDisabled = "true".equals(value); //NON-NLS
      }
      else if (value instanceof Boolean) {
        hideWhenDisabled = (Boolean)value;
      }
      checkDisabled();
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  /**
   * Classes extending AbstractToolbarItem can call this as a super() method after checking for their own keys, to
   * avoid needing to deal with the nitty gritty of the toolbar button.
   *
   * @return a String representation of the XML buildFile attribute with the given name. When initializing a module,
   * this String value will loaded from the XML and passed to {@link #setAttribute}. It is also frequently used for
   * checking the current value of an attribute.
   *
   * @param key the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   */
  @Override
  public String getAttributeValueString(String key) {
    if (!nameKey.isEmpty() && nameKey.equals(key)) {
      return getConfigureName();
    }
    else if (CAN_DISABLE.equals(key)) {
      return String.valueOf(canDisable);
    }
    else if (PROPERTY_GATE.equals(key)) {
      return propertyGate;
    }
    else if (DISABLED_ICON.equals(key)) {
      return disabledIconConfig.getValueString();
    }
    else if (HIDE_WHEN_DISABLED.equals(key)) {
      return String.valueOf(hideWhenDisabled);
    }
    else {
      return launch.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (List.of(PROPERTY_GATE).contains(key)) {
      return () -> isShowDisabledOptions() && canDisable;
    }
    if (HIDE_WHEN_DISABLED.equals(key)) {
      return () -> false;  // Disabling hide-when-disabled for now, it seems to be creating problems
    }
    if (DISABLED_ICON.equals(key)) {
      return () -> isShowDisabledOptions() && canDisable; // && !hideWhenDisabled; // removed hide-when-disabled feature for now
    }
    else if (CAN_DISABLE.equals(key)) {
      return this::isShowDisabledOptions;
    }
    else {
      return null;
    }
  }

  /**
   * The component to be added to the control window toolbar
   */
  protected Component getComponent() {
    return launch;
  }

  /**
   * Default behavior adds the button to the parent component
   * @param parent parent Buildable to add this component to as a subcomponent.
   */
  @Override
  public void addTo(Buildable parent) {
    final GameModule gm = GameModule.getGameModule();
    gm.getGameState().addGameComponent(this);

    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder)parent).getNonFolderAncestor();
    }

    if (parent instanceof ToolBarComponent) {
      ((ToolBarComponent)parent).getToolBar().add(getComponent());
    }
  }

  /**
   * Remove from our parent
   * @param parent parent
   */
  @Override
  public void removeFrom(Buildable parent) {
    final GameModule gm = GameModule.getGameModule();
    gm.getGameState().removeGameComponent(this);

    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder)parent).getNonFolderAncestor();
    }

    if (parent instanceof ToolBarComponent) {
      ((ToolBarComponent)parent).getToolBar().remove(getComponent());
      ((ToolBarComponent)parent).getToolBar().revalidate();
    }
  }

  @Override
  public List<String> getPropertyList() {
    final ArrayList<String> l = new ArrayList<>();

    if (!isShowDisabledOptions() || !canDisable) {
      return l;
    }

    final String prop = getAttributeValueString(PROPERTY_GATE);
    if ((prop != null) && !prop.isEmpty()) {
      l.add(prop);
    }

    return l;
  }

  /**
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return Arrays.asList(getAttributeValueString(buttonTextKey), getAttributeValueString(tooltipKey));
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(hotKeyKey)));
  }

  /**
   * Classes extending {@link VASSAL.build.AbstractBuildable} should override this method in order to add
   * the names of any image files they use to the collection. For "find unused images" and "search".
   *
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final String string = launch.getAttributeValueString(launch.getIconAttribute());
    if (string != null && !string.isEmpty()) { // Launch buttons sometimes have null icon attributes - yay
      s.add(string);
    }
    final String disabledIconName = disabledIconConfig.getValueString();
    if (disabledIconName != null && !disabledIconName.isEmpty()) {
      s.add(disabledIconName);
    }

  }
}
