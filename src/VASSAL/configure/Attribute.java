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

import java.awt.Color;
import java.util.LinkedHashMap;
import java.util.Map;

import VASSAL.build.AutoConfigurable;

/**
 * The contracting interface for property objects desiring to be automatically 
 * managed for <code>Configurer</code> and kin by {@link AbstractAttributeListConfigurable} 
 * for an object inheriting from {@link AbstractConfigurable}.
 * @author Pieter Geerkens
 *
 * @param <T> The storage type (and usually the advertised type) of the 
 * <code>Attribute</code>
 */
public interface Attribute <T> { 
  public String description();
  public String name();
  /**
   * @return the storage type of the attribute, <code>Class&lt;T></code> (usually); 
   * but may be an alias in some cases to work-around the inability to extend 
   * the boxed primitive types.
   * 
   * @see   Attribute.FormattedStringAttribute
   */
  public Class<?> type();
  public VisibilityCondition getVisibility();
  public Class<T> typeStorage();
  
  public static interface AttributeParser <T> {
    /**
     * Parses the supplied <code>string</code> to type T.
     * @param string to be parsed to a value of type T.
     * @return the typed value of the supplied string
     */
    public T valueOf(String string);
    public String toString(T value);
    public Class<? extends T> typeStorage();
  }
  
  /**
   *
   * @param <T> the storage type for the attribute, and (usually) also the type 
   * declared to <code>Configurer</code> as the desired class of configurer.
   * 
   * @author Pieter Geerkens
   */
  public abstract class AbstractAttribute <T> implements Attribute<T> {
    public static final VisibilityCondition visible = new VisibilityCondition() {
      @Override public boolean shouldBeVisible() {return true;}
    };
    public static final VisibilityCondition invisible = new VisibilityCondition() {
      @Override public boolean shouldBeVisible() {return false;}
    };
    
    /** The actual storage type of the <code>attribute</code> */
    protected final Class<T> typeStorage;  
    /** The advertised ( to <code>Configurer</code> & kin) type of the <code>attribute</code> */
    protected final Class<?> typeAdvertised;   
    protected final String description;
    protected final String name;
    protected final VisibilityCondition visibility;
    
    AbstractAttribute(Class<T> typeStorage, Class<?> typeAdvertised, String name, String description) {
      this(typeStorage, typeAdvertised, name, description, visible);
    }
    AbstractAttribute(Class<T> typeStorage, Class<?> typeAdvertised, String name, String description, 
        VisibilityCondition visibility) {
      if (typeAdvertised == null) typeAdvertised = typeStorage;
      this.typeStorage     = typeStorage;
      this.typeAdvertised  = typeAdvertised;
      this.description     = description;
      this.name         = name;
      this.visibility     = visibility;
    }
    
    @Override
    public String description() { return this.description; }
    @Override
    public String name() { return name; }
    @Override
    public Class<?> type() { return typeAdvertised; }
    @Override
    public VisibilityCondition getVisibility() {return visibility;}
    @Override
    public Class<T> typeStorage() { return typeStorage; }
  }
  
  /**
   * @inheritDoc
   * <p>A utility class for {@link Attribute}s desiring a {@link FormattedStringConfigurer}. 
   * Although the storage type for this attribute class is <code>String</code>, it declares itself 
   * to {@link Configurer} as being of type <code>FormattedStringAttribute</code>.
   * @author Pieter Geerkens
   *
   */
  public abstract class FormattedStringAttribute extends AbstractAttribute<String> {
    private final String[] configOptions;
    public FormattedStringAttribute(String name, String description, String[] configOptions,
        VisibilityCondition visibility) {
      super(String.class,FormattedStringAttribute.class,name,description,visibility); 
      this.configOptions = configOptions;
    }
    public FormattedStringAttribute(String name, String description, String[] configOptions) {
      super(String.class,FormattedStringAttribute.class,name,description);
      this.configOptions = configOptions;
    }
    public Configurer getConfigurer(AutoConfigurable c) {
      return new FormattedStringConfigurer(name, description, configOptions);
    }
  }
  
//  public static class ColorAttributeParser implements AttributeParser <Color>{
//    public Color valueOf(String string) { return ColorConfigurer.stringToColor(string); } 
//    public String toString(Color color) { return ColorConfigurer.colorToString(color); }
//    public Class<Color> typeStorage() { return Color.class; }
//  }
  
  public abstract class ColorAttribute extends AbstractAttribute <Color> implements AttributeParser <Color>{
    public ColorAttribute(String name, String description, VisibilityCondition visibility) {
      super(Color.class,null,name,description,visibility); 
    }
    public ColorAttribute(String name, String description) {
      super(Color.class,null,name,description);
    }
    @Override
    public Color valueOf(String string) { return ColorConfigurer.stringToColor(string); } 
    @Override
    public String toString(Color color) { return ColorConfigurer.colorToString(color); }
  }

  /**
   * 
   * @author Pieter Geerkens
   *
   * @param <U>
   * @param <T>
   */
  public abstract class EnumAttribute <U, T extends Enum<T>> 
  extends AbstractAttribute <T>  {
    public EnumAttribute(Class<T> clazz, String name,String description, VisibilityCondition visibility) {
      super(clazz,null,name,description,visibility); 
    }
    public EnumAttribute(Class<T> clazz, String name, String description) {
      super(clazz,null,name,description);
    }
    public Enum<T>[] getValidValues() { return typeStorage.getEnumConstants(); }
    public String[] getValidValues(AutoConfigurable target) {
      final Enum<?>[] av = typeStorage.getEnumConstants(); 
      String[] as = new String[av.length];
      for (int i = 0; i < as.length; i++) {
        as[i] = av[i].toString();
      }
      return as;
    }
    public String toString(Enum<T> value) { return value.toString(); }
  }
  /** {@inheritDoc}
   * <p>This wrapper for <code>EnumAttribute</code> supports the deprecated 
   * paradigm of serializing to a Character value instead of to an Identifier
   *  string-value. 
   */
  @Deprecated
  public abstract class EnumCharAttribute <T extends Enum<T>> 
  extends EnumAttribute <Character, T> implements AttributeParser<Enum<T>> {
    private final Map<Character, Enum<T>> map;

    public EnumCharAttribute(Class<T> clazz, String name, String description) {
      this(clazz, name, description, visible);
    }
    public EnumCharAttribute(Class<T> clazz, String name,String description, VisibilityCondition visibility) {
      super(clazz,name,description,visibility); 
      map = initMap(new LinkedHashMap<Character, Enum<T>>(2));
    }
    
    private Map<Character, Enum<T>> initMap(Map<Character, Enum<T>> m){
      for (Enum<T> e: typeStorage.getEnumConstants()) {
        m.put(e.name().charAt(0),e);
      }
      return m;
    }
    /** {@inheritDoc}
     * <p>This implementation supports the deprecated paradigm of serializing 
     * to a single Character instead of to an enumeration identifier. 
     */
    @Override
    public Enum<T> valueOf (String string) {
      if (string.length() == 1) {
        return  map.get(string.charAt(0));
      }
      else {
        return Enum.valueOf(typeStorage, string);
      }
    }
  }
  /** {@inheritDoc}
   * <p>This wrapper for <code>EnumAttribute</code> supports the deprecated 
   * paradigm of serializing to an Integer value instead of to an Identifier
   * string-value. 
   */
  @Deprecated
  public abstract class EnumIntAttribute <T extends Enum<T>> 
  extends EnumAttribute <Integer, T> implements AttributeParser<Enum<T>> {
    private final Map<Integer, Enum<T>> map;

    public EnumIntAttribute(Class<T> clazz, String name, String description) {
      this(clazz, name, description, visible);
    }
    public EnumIntAttribute(Class<T> clazz, String name,String description, VisibilityCondition visibility) {
      super(clazz,name,description,visibility); 
      map = initMap(new LinkedHashMap<Integer, Enum<T>>(2));
    }
    
    private static Integer toInt(String string) {
      return Integer.valueOf(string.replace('_', ' ').trim());
    }
    private Map<Integer, Enum<T>> initMap(Map<Integer, Enum<T>> m){
      for (Enum<T> e: typeStorage.getEnumConstants()) {
        m.put(toInt(e.name()),e);
      }
      return m;
    }
    /** {@inheritDoc}
     * <p>This implementation supports the deprecated paradigm of serializing 
     * to an integer-valued string instead of to an enumeration identifier. 
     */
    @Override
    public Enum<T> valueOf (String string) {
      if (string.charAt(0) != '_') {
        return  map.get(toInt(string));
      }
      else {
        return Enum.valueOf(typeStorage, string);
      }
    }
  }
}
