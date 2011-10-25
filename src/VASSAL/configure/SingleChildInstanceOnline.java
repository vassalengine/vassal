/*
 * $Id: SingleChildInstance.java 7725 2011-07-31 18:51:43Z uckelman $
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;

/**
 * 
 * @author Pieter Geerkens
 *
 */
public interface SingleChildInstanceOnline {

  public boolean enabledToAdd(AbstractBuildable parent);
	  
  /**
   * Delegate for the SingleChildInstanceOnline interface
   * @author Pieter Geerkens
   *
   */
  public final class AbstractSingleChildInstanceOnline {
	  public static boolean enabledToAdd(AbstractBuildable parent, Class<? extends Buildable>clazz) {
		  return (parent.getComponentsOf(clazz).size() == 0);
	  }
  }
}
