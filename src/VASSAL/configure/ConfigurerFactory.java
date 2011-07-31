package VASSAL.configure;

import VASSAL.build.AutoConfigurable;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * General purpose type for defining property-sheet editors.
 * Classes that implement {@link AutoConfigurable} can return a class that implements
 * this interface as one of the classes returned in {@link AutoConfigurable#getAttributeTypes}
 * Implementing classes should have a no-arg constructor.
 * @see AutoConfigurer
 * @see AutoConfigurable
 */
public interface ConfigurerFactory {
  public Configurer getConfigurer(AutoConfigurable c, String key, String name);
}
