/*
 * $Id: PropertyExpressionConfigurer.java,v 1.2 2006/09/28 04:59:19 swampwallaby Exp $
 *
 * Copyright (c) 2008 Brent Easton
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


/**
 * A Configurer for Property Expressions
 * This is a utility class to assist with the conversion and upgrade from
 * existing Property Match Expressions to use the new Bsh based expression
 * parser.
 */
public class PropertyExpressionConfigurer extends StringConfigurer {

  public PropertyExpressionConfigurer(String key, String name) {
    super(key, name);
  }

  public PropertyExpressionConfigurer(String key, String name, String val) {
    super(key, name, val);
  }
 
  public PropertyExpressionConfigurer(String key, String name, PropertyExpression val) {
    super(key, name, val.getExpression());
  }
  
}
