/*
 *
 * Copyright (c) 2020 by Brian Reynolds
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
package VASSAL.search;

import java.util.Collection;
import java.util.SortedSet;

/**
 * An interface for "things that contain images" to ship off convenient packages of filename.
 *
 * Used for finding what images can be removed from a module. Also for search.
 *
 * Implemented by {@link VASSAL.build.AbstractBuildable} (for most module components)
 */
public interface ImageTarget {
  /**
   * @return names of all images used by this item
   */
  SortedSet<String> getLocalImageNames();

  /**
   * Implementing classes should add
   * the names of any image files they use to the collection.
   *
   * @param s Collection to add image names to
   */
  void addLocalImageNames(Collection<String> s);
}
