/*
 *
 * Copyright (c) 2021 by Vassal Team.
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

package VASSAL.tools;

/*
 * This interface will be implemented by classes who support looking
   the right path for a given resource. Right now it only works
   for images  */
public interface ResourcePathFinder {
  String findImagePath(String name);

  String findHelpFileName(String name);
}
