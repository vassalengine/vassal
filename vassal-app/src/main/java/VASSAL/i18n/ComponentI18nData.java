/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney, Brent Easton
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
package VASSAL.i18n;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Configurable;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PlaceMarker;

/**
 * Object encapsulating the internationalization information for a component.
 * The majority of translatable components subclass AbstractConfigurable,
 * but some extend JFrame or JDialog and implement Configurable or
 * AutoConfigurable.
 *
 * AbstractConfigurable components are almost completely handled within the
 * AbstractConfigurable base class. AutoConfigurable/Configurable components
 * must call a different constructor and supply additional information.
 *
 * @author Brent Easton
 */
public class ComponentI18nData {
  protected String prefix;
  protected Translatable parent;
  protected Configurable myComponent;
  protected Map<String, Property> translatableProperties = new TreeMap<>();
  protected Map<String, Property> allProperties = new TreeMap<>();
  protected List<Translatable> children = new ArrayList<>();

  /**
   * Build from an AbstractConfigurable. The parent will be set from
   * AbstractConfigurable.add(). untranslatedValues will be filled in as
   * attributes are translated.
   *
   * @param c
   *          AbstractConfigurable component
   * @param prefix
   *          I18n Prefix
   */
  public ComponentI18nData(AbstractConfigurable c, String prefix) {
    init(c, prefix, c.getAttributeNames(), c.getAttributeTypes(), c.getAttributeDescriptions());
  }

  @Deprecated(since = "2020-10-26", forRemoval = true)
  public ComponentI18nData(AbstractConfigurable c, String prefix, ArrayList<String> names, ArrayList<Class<?>> types, ArrayList<String> descriptions) { //NOPMD
    init(c, prefix, names.toArray(new String[0]), types.toArray(new Class<?>[0]), descriptions.toArray(new String[0]));
  }

  public ComponentI18nData(AbstractConfigurable c, String prefix, List<String> names, List<Class<?>> types, List<String> descriptions) {
    init(c, prefix, names.toArray(new String[0]), types.toArray(new Class<?>[0]), descriptions.toArray(new String[0]));
  }

    /**
     * Build from an AutoConfigurable
     *
     * @param c
     *          AutoConfigurable component
     * @param prefix
     *          I18n prefix
     */
  public ComponentI18nData(AutoConfigurable c, String prefix) {
    parent = null;
    init(c, prefix, c.getAttributeNames(),
         c.getAttributeTypes(), c.getAttributeDescriptions());
  }

  protected void init(Configurable c, String pfx, String[] names,
                      Class<?>[] types, String[] descriptions) {
    final boolean[] translatable = new boolean[types.length];
    for (int i = 0; i < types.length; i++) {
      translatable[i] = types[i] != null &&
        (types[i].equals(String.class) ||
         TranslatableConfigurerFactory.class.isAssignableFrom(types[i]));
    }
    init(c, pfx, names, descriptions, translatable);
  }

  protected void init(Configurable c, String pfx, String[] names, String[] descriptions, boolean[] translatable) {
    setPrefix(pfx);
    myComponent = c;
    children.addAll(Arrays.asList(myComponent.getConfigureComponents()));
    for (int i = 0; i < translatable.length; i++) {
      final Property p = new Property(names[i], descriptions[i]);
      allProperties.put(names[i], p);
      if (translatable[i]) {
        translatableProperties.put(names[i], p);
      }
    }
  }

  /**
   * Build from a Configurable. Configurable does not support
   * getAttributeNames() getAttributeTypes() or getAttributeValueString(),
   * so more information must be supplied.
   *
   * @param c
   *          Component
   * @param prefix
   *          I18n prefix
   * @param parent
   *          parent translatable
   * @param names
   *          Array of attribute names
   * @param translatable
   *          Array of Attribute translatable status
   */
  public ComponentI18nData(Configurable c, String prefix, Translatable parent, String[] names, boolean[] translatable, String[] descriptions) {
    myComponent = c;
    this.parent = parent;
    init(c, prefix, names, descriptions, translatable);
  }

  public ComponentI18nData(Configurable c, String prefix, Translatable parent) {
    this(c, prefix, parent, new String[0], new boolean[0], new String[0]);
  }

  public ComponentI18nData(Configurable c, String prefix) {
    this(c, prefix, null);
  }

  /**
   * Special build for PrototypeDefinition and PieceSlot
   */
  public ComponentI18nData(Configurable c, GamePiece piece) {
    myComponent = c;
    setPrefix(TranslatablePiece.PREFIX);
    parent = null;
    for (GamePiece p = piece; p != null;) {
      if (p instanceof TranslatablePiece) {
        final PieceI18nData pieceData = ((TranslatablePiece) p).getI18nData();
        for (final PieceI18nData.Property prop : pieceData.getProperties()) {
          final Property property = new Property(prop.getName(), prop.getDescription());
          translatableProperties.put(prop.getName(), property);
          allProperties.put(prop.getName(), property);
        }
      }
      if (p instanceof PlaceMarker) {
        if (((PlaceMarker)p).isMarkerStandalone()) {
          children.add(new TranslatableMarker((PlaceMarker) p));
        }
      }
      if (p instanceof BasicPiece) {
        p = null;
      }
      else {
        p = ((Decorator) p).getInner();
      }
    }
  }

  /**
   * Return a unique Key prefix identifying this component
   */
  public String getPrefix() {
    return prefix;
  }

  public void setPrefix(String p) {
    prefix = p.intern();
  }

  /**
   * Return a unique key prefix including a full path of parent prefixes. All Translatable Pieces share a common prefix.
   *
   * @return Full Prefix
   */
  public String getFullPrefix() {
    if (TranslatablePiece.PREFIX.equals(prefix)) {
      return prefix;
    }
    String fullPrefix = getOwningComponent() == null ? "" : getOwningComponent().getI18nData() //$NON-NLS-1$
        .getFullPrefix();
    if (fullPrefix.length() > 0 && prefix.length() > 0) {
      fullPrefix += "."; //$NON-NLS-1$
    }
    return fullPrefix + prefix;
  }

  /**
   * Return a list of all of the translatable Keys for attributes of this Translatable item.
   */
  public Collection<String> getAttributeKeys() {
    return translatableProperties.keySet();
  }

  /**
   * Set the owning Translatable of this component
   */
  public void setOwningComponent(Translatable t) {
    parent = t;
  }

  /**
   * Return the owning Translatable of this component
   */
  public Translatable getOwningComponent() {
    return parent;
  }

  /**
   * Is the specified attribute allowed to be translated?
   *
   * @param attr
   *          Attribute name
   * @return is translatable
   */
  public boolean isAttributeTranslatable(String attr) {
    return translatableProperties.containsKey(attr);
  }

  /**
   * Return true if this component has any translatable attributes, or if any of its children are translatable
   *
   * @return component translatable status
   */
  public boolean isTranslatable() {
    if (!translatableProperties.isEmpty()) {
      return true;
    }
    for (final Translatable child : children) {
      if (child.getI18nData().isTranslatable()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Force a specified attribute to be translatable/not translatable
   *
   * @param attribute
   *          Attribute name
   * @param set
   *          translatable status
   */
  public void setAttributeTranslatable(String attribute, boolean set) {
    if (set) {
      translatableProperties.put(attribute, allProperties.get(attribute));
    }
    else {
      translatableProperties.remove(attribute);
    }
  }

  /**
   * Convenience method to force all attributes to be not translatable
   */
  public void setAllAttributesUntranslatable() {
    translatableProperties.clear();
  }

  /**
   * Apply a translation to the specified attribute. Record the untranslated value in the untranslatedValues array and
   * set the new value into the real attribute
   *
   * @param attr
   *          Attribute name
   * @param value
   *          Translated value
   */
  public void applyTranslation(String attr, String value) {
    final Property p = translatableProperties.get(attr);
    if (attr != null) {
      p.setUntranslatedValue(myComponent.getAttributeValueString(attr));
      myComponent.setAttribute(attr, value);
    }
  }

  /**
   * Return description for named Attribute
   *
   * @param attr Attribute
   * @return description
   */
  public String getAttributeDescription(String attr) {
    return allProperties.get(attr).getDescription();
  }

  /**
   * Return the pre-translation value stored in this Object.
   *
   * @param attr
   *          Attribute Name
   * @return untranslated value
   */
  public String getLocalUntranslatedValue(String attr) {
    final String val;
    final Property p = allProperties.get(attr);
    if (p == null || p.getUntranslatedValue() == null) {
      val = myComponent.getAttributeValueString(attr);
    }
    else {
      val = p.getUntranslatedValue();
    }
    return val;
  }

  /**
   * Set an untranslatedValue for the specified attribute. Used by components that do not subclass AbstractConfigurable
   *
   * @param attr
   *          Attribute name
   * @param value
   *          untranslated value
   */
  public void setUntranslatedValue(String attr, String value) {
    allProperties.get(attr).setUntranslatedValue(value);
  }

  /*
   * Return a translation of an attribute
   */
  public String getTranslatedValue(String attr, Translation translation) {
    final String fullKey = getFullPrefix() + attr;
    return translation.translate(fullKey);
  }

  /**
   * Return all child Translatable components of this component
   *
   * @return Child translatables
   */
  public List<Translatable> getChildren() {
    return Collections.unmodifiableList(children);
  }

  /**
   * Return true if this component or any of its children have at least one translatable attribute with a non-null value
   * that does not have a translation in the supplied translation.
   *
   * @param t
   *          Translation
   * @return true if translation of this component is not complete
   */
  public boolean hasUntranslatedAttributes(Translation t) {
    if (t == null) {
      return false;
    }
    /*
     * Check attributes of this component first
     */
    for (final Property p : translatableProperties.values()) {
      final String currentValue = myComponent.getAttributeValueString(p.getName());
      if (currentValue != null && currentValue.length() > 0) {
        final String translation = getTranslatedValue(p.getName(), t);
        if (translation == null || translation.length() == 0) {
          return true;
        }
      }
    }
    /*
     * Check Children
     */
    for (final Translatable child : children) {
      if (child.getI18nData().hasUntranslatedAttributes(t)) {
        return true;
      }
    }
    /*
     * Nothing left untranslated!
     */
    return false;
  }
  /** An attribute of a Configurable component that can be translated into another language */
  public static class Property {
    private final String name;
    private final String description;
    private String untranslatedValue;

    public Property(String name, String description) {
      super();
      this.name = name;
      this.description = description;
      this.untranslatedValue = name;
    }

    public String getDescription() {
      return description;
    }

    public String getName() {
      return name;
    }

    public String getUntranslatedValue() {
      return untranslatedValue;
    }

    public void setUntranslatedValue(String untranslatedValue) {
      this.untranslatedValue = untranslatedValue;
    }
  }
}
