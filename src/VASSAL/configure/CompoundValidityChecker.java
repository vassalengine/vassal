/*
 * $Id$
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

import java.util.ArrayList;
import java.util.List;

import VASSAL.build.Buildable;

/**
 * Combines multiple instances of ValidityChecker
 */
public class CompoundValidityChecker implements ValidityChecker {
  private List<ValidityChecker> checkers = new ArrayList<ValidityChecker>();

  public CompoundValidityChecker(ValidityChecker checker1,
                                 ValidityChecker checker2) {
    append(checker1);
    append(checker2);
  }

  public CompoundValidityChecker append(ValidityChecker checker) {
    checkers.add(checker);
    return this;
  }

  public void validate(Buildable target, ValidationReport report) {
    for (ValidityChecker c : checkers) {
      c.validate(target, report);
    }
  }
}
