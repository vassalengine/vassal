/*
 *
 * Copyright (c) 2020 by vassalengine.org, Brian Reynolds
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
import java.util.TreeSet;

/**
 * Abstract implementation of {@link ImageSearchTarget} interface. Used to find images contained by components/pieces/traits
 * for "Image Purge" tool,
 */
public abstract class AbstractImageFinder implements ImageSearchTarget {
  /**
   * @return names of all images used by the component and any subcomponents
   */
  @Override
  public SortedSet<String> getAllImageNames() {
    final SortedSet<String> s = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
    addImageNamesRecursively(s);
    return s;
  }

  /**
   * Adds all images used by this component AND any children (or inner decorators/pieces) to the collection.
   * @param s Collection to add image names to
   */
  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    addLocalImageNames(s); // Default implementation just adds ours
  }

  /**
   * @return names of all images used by this component
   */
  @Override
  public SortedSet<String> getLocalImageNames() {
    final SortedSet<String> s = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
    addLocalImageNames(s);
    return s;
  }

  /**
   * Classes extending AbstractImageFinder should override this method in order to add
   * the names of any image files they use to the collection.
   *
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    // Default implementation is that we don't HAVE any images to add.
  }
}
