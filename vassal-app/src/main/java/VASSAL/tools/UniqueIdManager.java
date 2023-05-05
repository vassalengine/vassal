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
package VASSAL.tools;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.Buildable;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.i18n.Resources;

/**
 * A class for assigning unique identifiers to objects.  Identifiers will be
 * of the form prefix#, where prefix is specified at initialization and the #
 * is an increasing digit. Components will have the same ID provided they
 * are loaded in the same order.
 *
 * Unfortunately, this approach is flawed. If a module is edited, saved games
 * from previous versions can become broken. Worse, two players with different
 * extensions loaded could have incompatible behavior.
 *
 * The preferred way to have unique identifiers is to allow the user to provide
 * names and use a {@link VASSAL.configure.ValidityChecker} to ensure that the
 * names are unique.  This class provides some support for using this approach
 * while providing backward compatibility with old saved games and modules.
 *
 * Usage:  an {@link Identifyable} instance invokes {@link #add}, typically
 * during the {@link Buildable#build} method.  Classes can use the
 * {@link #getIdentifier} method to look up an identifier for that instance,
 * and can use {@link #findInstance} to look up a component by id.
 */
public class UniqueIdManager implements ValidityChecker {
  private final List<Identifyable> instances = new ArrayList<>();
  private final String prefix;

  public UniqueIdManager(String prefix) {
    this.prefix = prefix;
  }

  public void add(Identifyable i) {
    i.setId(prefix + instances.size());
    instances.add(i);
  }

  public void remove(Identifyable i) {
    final int index = instances.indexOf(i);
    if (index >= 0) {
      final int n = instances.size();
      for (int j = index + 1; j < n; ++j) {
        instances.get(j).setId(prefix + (j - 1));
      }
      instances.remove(index);
    }
  }

  /**
   * Make a best guess for a unique identifier for the target.
   * Use {@link Identifyable#getConfigureName} if non-null, otherwise
   * use {@link Identifyable#getId}
   * @param target identifiable target
   * @return identifier for target
   */
  public static String getIdentifier(Identifyable target) {
    String id = target.getConfigureName();
    if (id == null || id.length() == 0) {
      id = target.getId();
    }
    return id;
  }

  public Iterator<Identifyable> getAllInstances() {
    return instances.iterator();
  }

  /**
   * Return the first instance whose name or id matches the argument
   * @param id name or id
   * @return Return the first instance whose name or id matches the argument
   */
  public Identifyable findInstance(String id) {
    if (id != null) {
      for (final Identifyable i : instances) {
        if (id.equals(i.getConfigureName()) || id.equals(i.getId())) {
          return i;
        }
      }
    }
    return null;
  }

  /** Ensures that no other instance of the same class has the same name */
  @Override
  public void validate(Buildable target, ValidationReport report) {
    if (target instanceof Identifyable) {
      final Identifyable iTarget = (Identifyable) target;
      if (iTarget.getConfigureName() == null ||
          iTarget.getConfigureName().length() == 0) {
        report.addWarning(Resources.getString("Editor.UniqueIdManager.a_girl_has_no_name", ConfigureTree.getConfigureName(target.getClass())));
      }
      else if (instances.contains(iTarget)) {
        Identifyable compare = null;
        for (final Iterator<Identifyable> i = instances.iterator();
             i.hasNext() && compare != iTarget; ) {
          compare = i.next();
          if (compare != iTarget &&
              iTarget.getConfigureName().equals(compare.getConfigureName())) {
            report.addWarning(Resources.getString("Editor.UniqueIdManager.more_than_one",
                     ConfigureTree.getConfigureName(target.getClass()),
                              iTarget.getConfigureName()));
            break;
          }
        }
      }
    }
  }

  /**
   * An object with an identifier that can be manipulated by a
   * {@link UniqueIdManager}
   */
  public interface Identifyable {
    void setId(String id);

    String getId();

    String getConfigureName(); // User-assigned name
  }
}
