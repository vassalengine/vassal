/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.properties;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

public class GlobalPropertyTest {
  @Test
  public void aNamedPropertyOffersItsNameToSearch() {
    final GlobalProperty gp = new GlobalProperty();
    gp.setAttribute(GlobalProperty.NAME, "Turn"); //NON-NLS

    assertThat(gp.getPropertyList(), contains("Turn")); //NON-NLS
  }

  /**
   * A Global Property without a name (not yet filled in, or missing from the
   * build file) used to make the Editor's search fail on List.of(null).
   */
  @Test
  public void anUnnamedPropertyOffersNothingToSearch() {
    final GlobalProperty gp = new GlobalProperty();
    gp.setAttribute(GlobalProperty.NAME, null);

    assertThat(gp.getPropertyList(), is(empty()));
  }
}
