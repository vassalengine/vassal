/*
 *
 * Copyright (c) 2004 by VASSAL team
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

import java.util.Objects;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.i18n.Resources;
/**
 * Requires that the object does not have null configurable name
 */
public class NotNullConfigureName implements ValidityChecker {
  private final AbstractConfigurable target;

  public NotNullConfigureName(AbstractConfigurable target) {
    this.target = Objects.requireNonNull(target);
  }

  @Override
  public void validate(Buildable b, ValidationReport report) {
    if (b == target && ((target.getConfigureName() == null) || (target.getConfigureName().equals("")))) {
      report.addWarning(Resources.getString("Editor.ValidationReportDialog.blank_name", target.getClass().getName()));
    }
  }
}
