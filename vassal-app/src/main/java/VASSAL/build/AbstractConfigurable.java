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
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Translatable;
import VASSAL.search.SearchTarget;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * An abstract implementation of the {@link Configurable} interface. To make a component which is both buildable from the buildFile (XML)
 * and whose XML attributes are then editable/configurable with a dialog in the Editor, extend this class. Takes care of most of
 * the Configurable functionality. Provides the basis for a component to have a configuration dialog in the Editor, allowing various
 * attributes to be edited, retained, and saved/loaded from the module's buildFile (XML).
 */
public abstract class AbstractConfigurable extends AbstractBuildable implements AutoConfigurable, SearchTarget {
  protected PropertyChangeSupport changeSupport;
  protected String name; // Language-independent name used for programmatic identification (including within Modules by e.g. Traits and module components)
  protected String localizedName; // Locale-sensitive name for on-screen display
  protected Configurer config;
  protected ComponentI18nData myI18nData;

  /**
   * Remove a Buildable object from this object
   */
  @Override
  public void remove(Buildable b) {
    buildComponents.remove(b);

    if (b instanceof AbstractBuildable) {
      ((AbstractBuildable) b).setAncestor(null);
    }
  }

  /**
   * ConfigureName is used, in particular, by the Editor to track the names of components, and is preferred by {@link VASSAL.tools.UniqueIdManager}.
   * @return The language-independent name of this component used for programmatic identification (including within Modules by e.g. Traits and module components)
   */
  @Override
  public String getConfigureName() {
    return name;
  }

  /**
   * @return The localized name for on-screen display
   */
  public String getLocalizedConfigureName() {
    return localizedName;
  }

  /**
   * Sets the name and fires a PropertyChangeEvent
   */
  public void setConfigureName(String s) {
    final String oldName = name;
    if (changeSupport != null) {
      changeSupport.firePropertyChange(NAME_PROPERTY, oldName, s);
    }
    if (Localization.getInstance().isTranslationInProgress()) {
      localizedName = s;
    }
    else if (Localization.getInstance().isTranslationComplete()) {
      name = s;
    }
    else {
      name = s;
      localizedName = s;
    }
  }

  /**
   * @return an array of Strings describing the buildFile (XML) attributes of this component. These strings are used as prompts in the
   * Properties window for this object, when the component is configured in the Editor. The order of descriptions should
   * be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   */
  @Override
  public abstract String[] getAttributeDescriptions();

  /**
   * @return the Class for the buildFile (XML) attributes of this component. Valid classes include: String, Integer, Double, Boolean, Image,
   * Color, and KeyStroke, along with any class for which a Configurer exists in VASSAL.configure. The class determines, among other things,
   * which type of {@link AutoConfigurer} will be used to configure the attribute when the object is configured in the Editor.
   *
   * The order of classes should be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   */
  @Override
  public abstract Class<?>[] getAttributeTypes();

  /**
   * By default, all attributes are visible
   *
   * @param name Name (key) of one of this component's attributes
   * @return a {@link VisibilityCondition} for this attribute, or null if attribute should always be visible.
   */
  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    return null;
  }

  /**
   * @return the i18n data for this component
   */
  @Override
  public ComponentI18nData getI18nData() {
    if (myI18nData == null) {
      myI18nData = new ComponentI18nData(this, getI18nPrefix());
    }
    return myI18nData;
  }

  /**
   * @return Generate a standard prefix for i18n keys for attributes of this component - Classname.attributeName
   */
  protected String getI18nPrefix() {
    String key = getClass().getSimpleName();
    if (getConfigureName() != null && getConfigureName().length() > 0) {
      key += "." + getConfigureName();
    }
    return key + ".";
  }

  /**
   * Over-ride the default attribute translatability. This is called by individual components to force specific
   * attributes to be translatable or not translatable
   * @param attr Attribute name/key
   * @param b true if translatable, false if not
   */
  protected void setAttributeTranslatable(String attr, boolean b) {
    getI18nData().setAttributeTranslatable(attr, b);
  }

  /**
   * Sets all attributes untranslatable.
   */
  protected void setAllAttributesUntranslatable() {
    getI18nData().setAllAttributesUntranslatable();
  }

  /**
   * Set the owning translatable of this component
   */
  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof Translatable) {
      ((Translatable) b).getI18nData().setOwningComponent(this);
    }
  }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    if (changeSupport == null) {
      changeSupport = new PropertyChangeSupport(this);
    }
    changeSupport.addPropertyChangeListener(l);
  }

  public void removePropertyChangeListener(PropertyChangeListener l) {
    if (changeSupport != null) {
      changeSupport.removePropertyChangeListener(l);
    }
  }

  /**
   * @return A list of all child components (i.e. subcomponents) of this component that are configurable.
   */
  @Override
  public Configurable[] getConfigureComponents() {
    final ArrayList<Configurable> l = new ArrayList<>();
    for (final Buildable b : getBuildables()) {
      if (b instanceof Configurable) {
        l.add((Configurable) b);
      }
    }
    return l.toArray(new Configurable[0]);
  }

  /**
   * The default {@link Configurer} of an {@link AbstractConfigurable} class is an instance of {@link AutoConfigurer}
   * @return Configurer for this component
   */
  @Override
  public Configurer getConfigurer() {
    if (config == null) {
      config = new AutoConfigurer(this);
    }
    else {
      ((AutoConfigurer) config).reset();
    }
    return config;
  }

  /**
   * {@link SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.emptyList();
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Collections.emptyList();
  }

  /**
   * Returns the name of the configurable type for display purposes. Reflection is
   * used to call <code>getConfigureTypeName()</code>, which should be
   * a static method if it exists in the given class. (This is necessary
   * because static methods are not permitted in interfaces.)
   *
   * @return the configure name of the class
   */
  public String getTypeName() {
    try {
      return (String) this.getClass().getMethod("getConfigureTypeName").invoke(null);
    }
    catch (NoSuchMethodException e) {
      // Ignore. This is normal, since some classes won't have this method.
    }
    catch (IllegalAccessException | ExceptionInInitializerError
      | NullPointerException | InvocationTargetException | IllegalArgumentException e) {
      ErrorDialog.bug(e);
    }

    return this.getClass().getSimpleName();
  }
}
