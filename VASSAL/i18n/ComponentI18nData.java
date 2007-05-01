/*
 * $Id: ComponentI18nData.java 1417 2006-11-03 14:57:33 +0000 (Fri, 03 Nov 2006) rodneykinney $
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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/**
 * Object encapsulating the internationalization information for a component.
 * The majority of translatable components subclass AbstractConfigurable, but
 * some extend JFrame or JDialog and implement Configurable or AutoConfigurable.
 * 
 * AbstractConfigurable components are almost completely handled within the 
 * AbstractConfigurable base class. AutoConfigurable/Configurable components
 * must call a different constructor and supply additional information
 *
 * @author Brent Easton
 *
 */
public class ComponentI18nData {

  protected String prefix;
  protected Translatable parent;
  protected Configurable myComponent;
  protected String[] attributeNames;
  protected String[] attributeDescriptions;
  protected boolean[] translatable;
  protected String[] untranslatedValues = null;
  protected String untranslatedConfigureName = null;

  /**
   * Build from an AbstractConfigurable. The parent will be set from
   * AbstractConfigurable.add(). untranslatedValues will be filled in
   * as attributes are translated.
   *
   * @param c AbstractConfigurable component
   * @param prefix I18n Prefix
   */
  public ComponentI18nData(AbstractConfigurable c, String prefix) {
    init(c, prefix, c.getAttributeNames(), c.getAttributeTypes(), c.getAttributeDescriptions());
  }

  /**
   * Build from an AutoConfigurable
   *
   * @param c AutoConfgurable component
   * @param prefix I18n prefix
   */
  public ComponentI18nData(AutoConfigurable c, String prefix) {
    this.prefix = prefix;
    parent = null;
    init(c, prefix, c.getAttributeNames(), c.getAttributeTypes(), c.getAttributeDescriptions());
  }

  protected void init(Configurable c, String pfx, String[] names, Class[] types, String[] descriptions) {
    prefix = pfx;
    myComponent = c;
    attributeNames = names;
    attributeDescriptions = descriptions;
    
    translatable = new boolean[attributeNames.length];
    for (int i = 0; i < types.length && i < attributeNames.length; i++) {
      if (types[i] != null) {        
        translatable[i] = (types[i].equals(String.class) || isImplementing(types[i], TranslatableConfigurerFactory.class));
      }
    }
  }
  
  /**
   * Test if a class implements a specified interface
   * @param test Class to test
   * @param iface Interface to test
   * @return true if test implements iface
   */
  protected boolean isImplementing (Class test, Class iface) {
    Class[] interfaces = test.getInterfaces();
    for (int i = 0; i < interfaces.length; i++) {
      if (interfaces[i].equals(iface)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Build from a Configurable. Configurable does not support
   * getAttributeNames() getAttributeTypes() or getAttributeValueString(), so
   * more information must be supplied.
   *
   * @param c Component
   * @param prefix I18n prefix
   * @param parent parent translatable
   * @param names Array of attribute names
   * @param translatable Array of Attribute translatable status
   */
  public ComponentI18nData(Configurable c, String prefix, Translatable parent, String[] names,
      boolean[] translatable, String[] descriptions) {
    myComponent = c;
    this.prefix = prefix;
    this.parent = parent;
    this.attributeNames = names;
    this.translatable = translatable;
    this.attributeDescriptions = descriptions;
  }

  public ComponentI18nData(Configurable c, String prefix, Translatable parent) {
    this(c, prefix, parent, new String[0], new boolean[0], new String[0]);
  }

  public ComponentI18nData(Configurable c, String prefix) {
    this(c, prefix, (Translatable) null);
  }
  
  /**
   * Special build for PrototypeDefinition and PieceSlot
   */
  
  public ComponentI18nData(Configurable c, GamePiece piece) {
    myComponent = c;
    this.prefix = TranslatablePiece.PREFIX;
    parent = null;
    
    ArrayList<String> values = new ArrayList<String>();
    ArrayList<String> descriptions = new ArrayList<String>();
    
    for (GamePiece p = piece; p != null; ) {
      if (p instanceof TranslatablePiece) {
        PieceI18nData pieceData = ((TranslatablePiece) p).getI18nData();
        String[] val = pieceData.getValues();
        String[] desc = pieceData.getDescriptions();
        for (int i = 0; i < val.length; i++) {
          values.add(val[i]);
          descriptions.add(desc[i]);
        }
      }
      if (p instanceof BasicPiece) {
        p = null;
      }
      else {
        p = ((Decorator) p).getInner();
      }
    }
    
    attributeNames = values.toArray(new String[0]);
    untranslatedValues = attributeNames;
    attributeDescriptions = descriptions.toArray(new String[0]);
    
    translatable = new boolean[attributeNames.length];
    for (int i = 0; i <attributeNames.length; translatable[i++] = true);
  }
  
  /**
   * Return a unique Key prefix identifying this component
   */
  public String getPrefix() {
    return prefix;
  }

  public void setPrefix(String p) {
    prefix = p;
  }
  /**
   * Return a unique key prefix including a full path of parent prefixes. 
   * All Translatable Pieces share a common prefix.
   * 
   * @return
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
   * Return a list of all of the translatable Keys for attributes of this
   * Translatable item.
   */
  public String[] getAttributeKeys() {
    int count = 0;
    for (int i = 0; i < translatable.length; i++) {
      if (translatable[i]) {
        count++;
      }
    }
    String[] keys = new String[count];
    int idx = 0;
    for (int i = 0; i < translatable.length; i++) {
      if (translatable[i]) {
        keys[idx++] = attributeNames[i];
      }
    }
    return keys;
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
   * @param attr Attribute name
   * @return is translatable
   */
  public boolean isAttributeTranslatable(String attr) {
    for (int i = 0; i < attributeNames.length; i++) {
      if (attr.equals(attributeNames[i])) {
        return translatable[i];
      }
    }
    return false;
  }

  /**
   * Return true if this component has any translatable attributes,
   * or if any of its children are translatable
   *
   * @return component translatable status
   */
  public boolean isTranslatable() {
    for (int i = 0; i < translatable.length; i++) {
      if (translatable[i]) {
        return true;
      }
    }
    
    Translatable[] children = getChildren();
    for (int i = 0; i < children.length; i++) {
      if (children[i].getI18nData().isTranslatable()) {
        return true;
      }
    }
    return false;
  }
  
  /**
   * Replace the full set of translatable attribute names
   * @param names Array of attribute names
   */
  public void setTranslatableAttributes(String[] names) {
    attributeNames = names;
    translatable = new boolean[attributeNames.length];
    for (int i = 0; i < attributeNames.length; i++) {
      translatable[i] = true;
    }
  }
  
  /**
   * Force a specified attribute to be translatable/not translatable
   * @param attribute Attribute name
   * @param set translatable status
   */
  public void setAttributeTranslatable(String attribute, boolean set) {
    for (int i = 0; i < attributeNames.length; i++) {
      if (attributeNames[i].equals(attribute)) {
        translatable[i] = set;
      }
    }
  }

  /**
   * Convenience method to force all attributes to be not translatable
   */
  public void setAllAttributesUntranslatable() {
    for (int i = 0; i < translatable.length; i++) {
      translatable[i] = false;
    }
  }
  
  /**
   * Apply a translatation to the specified attribute. Record the 
   * untranslated value in the untranslatedValues array and set the
   * new value into the real attribute
   * 
   * @param attr Attribute name
   * @param value Translated value
   */
  public void applyTranslation(String attr, String value) {
    if (untranslatedValues == null) {
      untranslatedValues = new String[attributeNames.length];
    }
    String currentValue = myComponent.getAttributeValueString(attr);
    for (int i = 0; i < attributeNames.length; i++) {
      if (attributeNames[i].equals(attr)) {
        untranslatedValues[i] = currentValue;
      }
    }
    myComponent.setAttribute(attr, value);
  }
  
  /**
   * Return description for named Attribute
   * 
   * @param attr
   * @return description
   */
  public String getAttributeDescription(String attr) {
    for (int i = 0; i < attributeNames.length; i++) {
      if (attributeNames[i].equals(attr)) {
        return attributeDescriptions[i];
      }
    }
    return "";
  }
  
  /**
   * Return the pre-translation value of the specified attribute
   *
   * @param attr Attribute name
   * @return untranslated value
   */
  public String getUntranslatedValue(String attr) {
    if (GameModule.getGameModule().isLocalizationEnabled()) {
      return getLocalUntranslatedValue(attr);
    }
    else if (myComponent instanceof AbstractConfigurable) {
       return ((AbstractConfigurable) myComponent).getAttributeValueString(attr); 
    }
    else if (myComponent instanceof AutoConfigurable) {
       return ((AutoConfigurable) myComponent).getAttributeValueString(attr);
    }  
    return getLocalUntranslatedValue(attr);
  }
  
  /**
   * Return the pre-translation value stored in this Object.
   * 
   * @param attr Attribute Name
   * @returnun translated value
   */
  public String getLocalUntranslatedValue(String attr) {
    for (int i = 0; i < attributeNames.length; i++) {
      if (attr.equals(attributeNames[i]) && untranslatedValues != null) {
        return untranslatedValues[i] == null ? myComponent.getAttributeValueString(attr) : untranslatedValues[i]; //$NON-NLS-1$
      }
    } 
    return myComponent.getAttributeValueString(attr); 
  }
  
  /**
   * Set an untranslatedValue for the specified attribute. Used by components
   * that do not subclass AbstractConfigurable
   * 
   * @param attr Attribute name
   * @param value untranslated value
   */
  public void setUntranslatedValue(String attr, String value) {
    if (untranslatedValues == null) {
      untranslatedValues = new String[attributeNames.length];
    }
    for (int i = 0; i < attributeNames.length && i < untranslatedValues.length; i++) {
      if (attr.equals(attributeNames[i])) {
        untranslatedValues[i] = value;
      }
    }
  }
  
  /**
   * Save the untranslated ConfigureName for this component
   * 
   * @param name untranslated configure name
   */
  public void setUntranslatedConfigureName(String name) {
    untranslatedConfigureName = name;
  }
  
  /**
   * Return the untranslated Configure name for this component
   * 
   * @return untranslated configure name
   */
  public String getUntranslatedConfigureName() {
    return untranslatedConfigureName;
  }
  /*
   * Return a translation of an attribute
   */
  public String getTranslatedValue(String attr, Translation translation) {
    String fullKey = getFullPrefix();
    if (fullKey.length() > 0) {
      fullKey += "."; //$NON-NLS-1$
    }
    fullKey += attr;
    return translation.translate(fullKey);
  }

  /**
   * Return all child Translatable components of this component
   * @return Child translatables
   */
  public Translatable[] getChildren() {
    return myComponent.getConfigureComponents();
  }

  /**
   * Return true if this component or any of its children have at least
   * one translatable attribute with a non-null value that does not have
   * a translation in the supplied translation.
   *
   * @param t Translation
   * @return true if translation of this component is not complete
   */
  public boolean hasUntranslatedAttributes(Translation t) {
    
    if (t == null) {
      return false;
    }

    /*
     * Check attributes of this component first
     */
    for (int i = 0; i < attributeNames.length; i++) {
      if (translatable[i]) {
        String currentValue = myComponent.getAttributeValueString(attributeNames[i]);
        if (currentValue != null && currentValue.length() > 0) {
          String translation = getTranslatedValue(attributeNames[i], t);
          if (translation == null || translation.length() == 0) {
            return true;
          }
        }
      }
    }
    
    /*
     * Check Children
     */
    Translatable[] children = getChildren();
    for (int i = 0; i < children.length; i++) {
      if (children[i].getI18nData().hasUntranslatedAttributes(t)) {
        return true;
      }
    }
    
    /* 
     * Nothing left untranslated!
     */
    return false;
  }

}
