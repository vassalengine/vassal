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
 * Requires that at least one child of a given type
 * exist within a target component
 */
public class MandatoryComponent implements ValidityChecker {
  private final Class<?> requiredChildClass;
  private final AbstractConfigurable target;

  public MandatoryComponent(AbstractConfigurable target,
                            Class<?> requiredChildClass) {
    this.requiredChildClass = requiredChildClass;
    this.target = target;
  }

  @Override
  public void validate(Buildable b, ValidationReport report) {
    if (b == this.target &&
        target.getComponentsOf(requiredChildClass).isEmpty()) {
      report.addWarning(
        Resources.getString("Editor.ValidityChecker.mandatory_warning",
          ConfigureTree.getConfigureName(target),
          ConfigureTree.getConfigureName(target.getClass()),
          ConfigureTree.getConfigureName(requiredChildClass)
        )
      );
    }
  }
}
