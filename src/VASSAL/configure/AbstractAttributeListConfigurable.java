/*
 * $Id$
 *
 * Copyright (c) 2011 by Pieter Geerkens
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
package VASSAL.configure;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.AbstractConfigurable;
import VASSAL.configure.Attribute.AbstractAttribute;
import VASSAL.configure.Attribute.AttributeParser;
/**
 * LinkedHashMap implementation of <code>AttributeList</code>, which can be 
 * substituted freely for <code>AbstractConfigurable</code>.
 * @author Pieter Geerkens
 *
 */
public abstract class AbstractAttributeListConfigurable
extends AbstractConfigurable  implements AttributeList {
    private static final Logger logger =
          LoggerFactory.getLogger(AbstractAttributeListConfigurable.class);

  private final Map<String, Attribute.AbstractAttribute<?>> attributes; 
  Boolean b;
  // ------ Constructors------
  AbstractAttributeListConfigurable() { this(8);  }
  protected AbstractAttributeListConfigurable(int capacity) {
    attributes = new LinkedHashMap<String,Attribute.AbstractAttribute<?>>(capacity);
  }
  
  // ------ AttributeList implementation ------
  @Override
  public void addAttribute (Attribute.AbstractAttribute<?> attribute) {
    final String key = attribute.name();
    attributes.put(attribute.name(), attribute);
    try {
      getDeclaredField(key).setAccessible(true);;
    }
    catch (NoSuchFieldException e) {
      logger.error(String.format("Field %1$s not found in module %2$s", key, e.getStackTrace()[0].getClassName()));
    }
  }

  public void addAttribute (String key, String description) {
    this.addAttribute(key, description, AbstractAttribute.visible);
  }

  public void addAttribute (String key, String description, VisibilityCondition visibility) {
    try {
      Field field = getDeclaredField(key);
      addAttribute(field.getType(), key, description, visibility);
    }
    catch (NoSuchFieldException e) {
      logger.error(String.format("Field %1$s not found in module %2$s", key, e.getStackTrace()[0].getClassName()));
    }
  }

  public <T> void addAttribute (Class<T> clazz, String key, String description, 
      VisibilityCondition visibility) {
    this.addAttribute(new AbstractAttribute<T>(clazz,null,key,description,visibility){});
  }
  
  @Override
  public AbstractAttribute<?> attribute(String key) {
    return attributes.get(key);
  }
  
  // partial AbstractConfigurable implementation
  @Override
  public Class<?>[] getAttributeTypes() {
    Class<?>[] ac = new Class<?>[attributes.size()];
    Iterator<Entry<String,Attribute.AbstractAttribute<?>>> it = attributes.entrySet().iterator();
    int j = 0;
    while (it.hasNext()) {
      ac[j++] = it.next().getValue().typeAdvertised;
    }
    return ac;
  }
  @Override
  public String[] getAttributeDescriptions() {
    String[] as = new String[attributes.size()];
    Iterator<Entry<String,Attribute.AbstractAttribute<?>>> it = attributes.entrySet().iterator();
    int j = 0;
    while (it.hasNext()) {
      as[j++] = it.next().getValue().description;
    }
    return as;
  }
  @Override
  public String[] getAttributeNames() {
    return attributes.keySet().toArray(new String[1]);
  }

//  vvv in progress vvv
  /** Helper for public getAttributeValue that checks casts to <code>T</code>. */
  private <T> T getAttributeValue(Attribute.AbstractAttribute<T> attribute, Object value) {
    return attribute.typeStorage().cast(value);
  }
  @SuppressWarnings("unchecked")  // attribute
  @Override
  public <T> T getAttributeValue(Class<T> clazz, String key) {
    Attribute.AbstractAttribute<T> attribute;
    if (clazz.isAssignableFrom(Attribute.AbstractAttribute.class)) {
      attribute = (AbstractAttribute<T>) get(key);
      try {
        final Field field = getDeclaredField(key);
        field.setAccessible(true);
        return getAttributeValue(attribute, field.get(this));
        // *** TODO ** use proper exceptions
      }
      catch (NoSuchFieldException e) {
        throw new IllegalArgumentException(String.format(
            "Attribute '%1$s' not defined for class '%2$s\n",key,this.getClass().getName()),e);
      }
      catch (IllegalAccessException e) {
        throw new IllegalArgumentException(String.format(
            "Attribute '%1$s' is inaccessible for class '%2$s\n",key,this.getClass().getName()),e);
      }
    }
    else {
      throw new IllegalArgumentException(String.format(
          "Attribute '%1$s' is not assignable to class '%2$s\n",
          key,Attribute.AbstractAttribute.class.getName()));
    }
  }
  <T> T test(String key) throws ClassNotFoundException {
    @SuppressWarnings("unchecked")
    Class<T> clazz = (Class<T>) Class.forName(get(key).typeStorage.getName());
    return getAttributeValue(clazz, key);
  }
// ^^^ In Progress ^^^
  
  /** Helper for public getAttributeValueString that checks casts to <code>T</code>. */
  private <T> String getAttributeValueString(AttributeParser<T> attributeParser, Object value) {
    return attributeParser.toString(attributeParser.typeStorage().cast(value));
  }
  @Override
  public String getAttributeValueString(String key) {
    String sReturn = "";
    Attribute.AbstractAttribute<?> attribute = get(key);
    try {
      final Field field = getDeclaredField(key);
      field.setAccessible(true);
      if (attribute instanceof AttributeParser<?>) {
        sReturn = getAttributeValueString((AttributeParser<?>) attribute, field.get(this));
      }
      else {
        sReturn = field.get(this).toString();
      }
      return sReturn;
//    } catch (SecurityException e) {
//    } catch (IllegalArgumentException e) {
      // *** TODO ** use proper exceptions
    }
    catch (NoSuchFieldException e) {
      throw new IllegalArgumentException(String.format(
          "Attribute '%1$s' not defined for class '%2$s\n",key,this.getClass().getName()),e);
    }
    catch (IllegalAccessException e) {
      throw new IllegalArgumentException(String.format(
          "Attribute '%1$s' is inaccessible for class '%2$s\n",key,this.getClass().getName()),e);
    }
  }
  
  @Override
  public void setAttribute(String key, Object value) {
    Attribute.AbstractAttribute<?> attribute = get(key);
    try {
      final Field field = getDeclaredField(key);
      field.setAccessible(true);
      final Class<?> type = field.getType();
      if (getBoxingClass(attribute.typeStorage).isAssignableFrom(value.getClass())) {
          field.set(this, value);
      }
      else if (value instanceof String) {
        if (AttributeParser.class.isAssignableFrom(attribute.getClass().getSuperclass()) ) {
          field.set(this, ((AttributeParser<?>)get(key)).valueOf((String) value));
        }
        else {
          Class<?> boxingType = getBoxingClass(type);
          try {
            field.set(this,
                boxingType.cast(boxingType.getDeclaredMethod("valueOf", String.class)
                    .invoke(this,value)));
          }
          catch (InvocationTargetException e) { 
            if ( (e.getTargetException() instanceof NumberFormatException)) {
              ; // ignore these
            }
            else {
              throw e;
            }
          }
        }
      }
      else {
        throw new IllegalArgumentException("value of type " + 
            value.getClass().getSimpleName() + " not assignable to attribute " +
            key + " of type " + attribute.type().getSimpleName());
      }
      // *** TODO ** use proper exceptions
    }
    catch (NoSuchMethodException e) {  // very fatal error
      e.printStackTrace();
      System.exit(0);
//    } catch (NoSuchFieldException e) {
//    } catch (IllegalAccessException e) {
//    } catch (InvocationTargetException e) {
    }
    catch (Exception e) {
        throw new IllegalArgumentException("value of type " + 
            value.getClass().getSimpleName() + " not assignable to attribute " +
            key + " of type " + attribute.type().getSimpleName(), e);
    }

    if (changeSupport != null) {
      changeSupport.firePropertyChange(key,null,value);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    return get(key).getVisibility();
  }

  // Iterable implementation
  @Override
  public Iterator<AbstractAttribute<?>> iterator() {
    return   attributes.values().iterator();
  }
  
  // ------------------- Utility methods -----------------------------------
  /**
   * @param key name of the attribute to be returned
   * @return the attribute identified by <code>key</code>
   * @throws IllegalArgumentException if attribute <code>key</code> not found
   */
  private Attribute.AbstractAttribute<?> get(String key) {
    Attribute.AbstractAttribute<?> attribute = attributes.get(key);
    if (attribute == null) {
      throw new IllegalArgumentException("unknown attribute '" + 
          key + "' for " + this.getClass().getSimpleName());
    }
    return attribute;
  }
  
  private Field getDeclaredField(@SuppressWarnings("hiding") String name) throws NoSuchFieldException {
    final String key = name;
    final Class<?> thisClass = this.getClass();
    try {
      return AccessController.doPrivileged(
        new PrivilegedExceptionAction<Field>() {
          @Override
          public Field run() throws NoSuchFieldException {
            Class<?> clazz = thisClass;
            while (true) {
              try {
                return clazz.getDeclaredField(key);
              } 
              catch (NoSuchFieldException e) { // look up-stream
                if (clazz == AbstractAttributeListConfigurable.class) throw e;
                clazz = clazz.getSuperclass();
              }
            }
          }
        });
    } 
    catch (PrivilegedActionException e) {
      throw (NoSuchFieldException) e.getException();
    }
  }
  private static Class <?> getBoxingClass (Class<?> clazz) {
    if (long.class.isAssignableFrom(clazz))   { return Long.class;      } else
    if (char.class.isAssignableFrom(clazz))  { return Character.class;  } else
    if (byte.class.isAssignableFrom(clazz))  { return Byte.class;      } else
    if (int.class.isAssignableFrom(clazz))   { return Integer.class;    } else
    if (short.class.isAssignableFrom(clazz))   { return Short.class;    } else
    if (boolean.class.isAssignableFrom(clazz)){ return Boolean.class;    } else
    if (float.class.isAssignableFrom(clazz))   { return Float.class;    } else
    if (double.class.isAssignableFrom(clazz)) { return Double.class;    } else
                                { return clazz;        } 
  }
}
