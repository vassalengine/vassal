/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney
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
package VASSAL.build.module.properties;

/**
 * Provides a new value for a global property. This class is an abstraction around the act of prompting a user for the
 * new value of a property during a game. Concrete implementation might be to increment a value, prompt the user to
 * select from an enum, etc.
 *
 * @author rkinney
 *
 */
public interface PropertyChanger {
  String getNewValue(String oldValue);
}
