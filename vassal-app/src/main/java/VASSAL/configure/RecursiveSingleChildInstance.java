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

import java.util.ArrayList;
import java.util.List;

/**
 * Ensures that at most a single instance of a given type
 * belongs to a given parent
 * Runs Recursively down whole component tree from target, using Buildable.isUnique to identify sub-components
 * that should be unique
 * Designed to be attached to the GameModule.
 */
public class RecursiveSingleChildInstance implements ValidityChecker {

  @Override
  public void validate(Buildable b, ValidationReport report) {
    if (b instanceof AbstractConfigurable) {
      final AbstractConfigurable parent = (AbstractConfigurable) b;
      // Keep track of what we have reported for this component so far
      final List<String> errorsSoFar = new ArrayList<>();
      parent.getBuildables().forEach(buildable -> {
        if (buildable.isUnique() && parent.getComponentsOf(buildable.getClass()).size() > 1) {
          final String childClass = ConfigureTree.getConfigureName(buildable.getClass());
          final String parentName = ConfigureTree.getConfigureName(parent);
          final String parentClass = ConfigureTree.getConfigureName(b.getClass());
          final String compare = childClass + parentName + parentClass;

          // Suppress duplicate reports
          if (!errorsSoFar.contains(compare)) {
            errorsSoFar.add(compare);
            report.addWarning(Resources.getString("Editor.ValidityChecker.single_warning", childClass, parentName, parentClass));
          }
        }
      });

      // Check the children
      parent.getBuildables().forEach(buildable -> validate(buildable, report));
    }
  }
}
