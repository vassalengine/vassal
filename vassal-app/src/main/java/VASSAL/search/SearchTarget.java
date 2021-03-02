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

import VASSAL.tools.NamedKeyStroke;
import java.util.List;

/**
 * An interface for "things the editor might search for with its 'Search' function" to ship off convenient packages of
 * searchable strings in various categories.
 *
 * Implemented by things like {@link VASSAL.build.AbstractConfigurable} (for most module components) and {@link VASSAL.counters.Decorator} (for piece traits)
 */
public interface SearchTarget {
  /**
   * @return a list of the item's string/expression fields if any (for search)
   */
  List<String> getExpressionList();

  /**
   * @return a list of any Message Format strings referenced in the item, if any (for search)
   */
  List<String> getFormattedStringList();

  /**
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the item, if any (for search)
   */
  List<String> getMenuTextList();

  /**
   * @return a list of any Named KeyStrokes referenced in the item, if any (for search)
   */
  List<NamedKeyStroke> getNamedKeyStrokeList();

  /**
   * @return a list of any Property Names referenced in the item, if any (for search)
   */
  List<String> getPropertyList();
}
