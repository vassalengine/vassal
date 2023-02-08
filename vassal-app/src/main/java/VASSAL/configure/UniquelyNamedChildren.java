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
import VASSAL.build.Configurable;
import VASSAL.i18n.Resources;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Ensures that any children of a given type have unique configure names
 */
public class UniquelyNamedChildren implements ValidityChecker {
  private final AbstractConfigurable target;
  private final Class<?> childClass;

  public UniquelyNamedChildren(AbstractConfigurable target,
                               Class<?> childClass) {
    this.childClass = childClass;
    this.target = target;
  }

  @Override
  public void validate(Buildable b, ValidationReport report) {
    if (b == target) {
      final Map<String, String> children = new HashMap<>();
      final List<String> duplicates = new ArrayList<>();

      for (final Object child : target.getAllDescendantComponentsOf(childClass)) {
        if (child instanceof Configurable) {
          final String name = ((Configurable) child).getConfigureName();
          final String exists = children.get(name);
          if (exists == null) {
            children.put(name, "Y");
          }
          else {
            if (!duplicates.contains(name)) {
              duplicates.add(name);
            }
          }
        }
      }

      if (!duplicates.isEmpty()) {
        for (final String name : duplicates) {
          report.addWarning(
            Resources.getString("Editor.ValidityChecker.duplicate_warning",
              ConfigureTree.getConfigureName(childClass),
              name,
              ConfigureTree.getConfigureName(target),
              ConfigureTree.getConfigureName(target.getClass())
            )
          );
        }
      }
    }
  }
}
