/*
 * Copyright (c) 2026 by The VASSAL Development Team
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

package VASSAL.library;

import java.util.List;
import java.util.ArrayList;
import java.util.Date;

/**
 * A whole project.
 */
public class Project {
  /** Name of the project */
  protected final String name;
  /** Title of the game, articles in normal place */
  protected final String title;
  /** Sort title, articles at the end */
  protected final String sort;
  /** Modification date */
  protected final Date   date;
  /** Packages in project */
  protected final List<Package> packages = new ArrayList<>();

  /**
   * Constructor
   *
   * @param n Project name
   * @param t Game title
   * @param s Sort game title
   * @param d Date
   */
  Project(String n, String t, String s, Date d) {
    name  = n;
    title = t;
    date  = d;
    sort  = s;
  }

  public String getName() {
    return name;
  }
  
  public String getTitle() {
    return title;
  }

  public String getSort() {
    return sort;
  }

  public Date getDate() {
    return date;
  }
  
  /**
   * Get list of packages 
   */
  public List<Package> getPackages() {
    return packages;
  }

  /**
   * Add a package
   */
  public void addPackage(Package p) {
    packages.add(p);
  }


  @Override
  public String toString() {
    String s = " Project: " + getTitle();
    for (final Package packge : getPackages())
      s += "\n" + packge.toString();

    return s;
  }
}
//
// EOF
//
