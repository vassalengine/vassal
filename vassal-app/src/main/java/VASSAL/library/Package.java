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

/**
 * A package.  Packages are named and contain releases of files.
 */
public class Package {
  protected final String name;
  protected final List<Release> releases = new ArrayList<>();
  
  public Package(String name) {
    this.name = name;
  }
  
  /**
   * Get the name of the package
   */
  public String getName() {
    return name;
  }
  
  /**
   * Get list of contained files
   */
  public List<Release> getReleases() {
    return releases;
  }

  /**
   * Add a release
   */
  public void addRelease(Release rel) {
    releases.add(rel);
  }

  @Override
  public String toString() {
    String r = "  Package: " + getName();
    for (final Release release : getReleases())
      r += "\n" + release.toString();

    return r;
  }
}
//
// EOF
//

