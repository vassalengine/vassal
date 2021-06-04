/*
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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.i18n.Resources;

/**
 * Ensures that at most a single instance of a given type
 * belongs to a given parent
 */
public class SingleChildInstance implements ValidityChecker {
  private final AbstractConfigurable target;
  private final Class<?> childClass;

  public SingleChildInstance(AbstractConfigurable target,
                             Class<?> childClass) {
    this.childClass = childClass;
    this.target = target;
  }

  @Override
  public void validate(Buildable b, ValidationReport report) {
    if (b == target && target.getComponentsOf(childClass).size() > 1) {
      report.addWarning(
        Resources.getString("Editor.ValidityChecker.single_warning",
          ConfigureTree.getConfigureName(childClass),
          ConfigureTree.getConfigureName(target),
          ConfigureTree.getConfigureName(target.getClass())
        ));
    }
  }
}
