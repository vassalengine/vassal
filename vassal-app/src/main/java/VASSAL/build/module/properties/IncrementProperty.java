/*
 *
 * Copyright (c) 2000-2023 by Rodney Kinney, The VASSAL Development Team
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
package VASSAL.build.module.properties;

import VASSAL.build.BadDataReport;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;

/**
 * Increments a property by a given value.
 * The value can be specified as a FormattedString property and evaluated at runtime
 *
 * @author rkinney
 *
 */
public class IncrementProperty implements PropertyChanger {
  protected Constraints constraints;
  protected FormattedString format = new FormattedString();
  protected PropertyChangerConfigurer prop;

  public IncrementProperty(PropertyChangerConfigurer prop, String incr, Constraints constraints) {
    super();
    this.prop = prop;
    this.constraints = constraints;
    format.setFormat(incr);
  }

  public String getRawValue() {
    return format.getFormat();
  }

  public PropertyChangerConfigurer getProp() {
    return prop;
  }

  public Constraints getPropSource() {
    return constraints;
  }

  @Override
  public String getNewValue(String oldValue) {
    int value;
    try {
      if (oldValue == null || oldValue.length() == 0) {
        value = 0;
      }
      else {
        value = Integer.parseInt(oldValue);
      }
    }
    catch (final NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.non_number_error"), "Increment " + prop.getName() + ": oldValue " + "=" + oldValue, e)); //NON-NLS
      return oldValue;
    }

    final AuditTrail audit = AuditTrail.create((Auditable) constraints, format.getFormat());

    try {
      final String s = format.getText(getTargetPropertySource(), constraints, audit);
      final int incr = Integer.parseInt(s);
      if (!constraints.isNumeric()) { // Don't apply hidden constraints that have been "turned off" by module designer unchecking the numeric box
        value += incr;
      }
      else if (constraints.isWrap()) {
        final int min = constraints.getMinimumValue();
        final int max = constraints.getMaximumValue();
        final int range = max - min + 1;
        // NB: value - min + incr could be < 0 (but > -range), which is
        // why we add range and mod a second time to ensure that the
        // result is in [0,range).
        value = min + ((value - min + incr) % range + range) % range;
      }
      else {
        value += incr;
        value = Math.min(constraints.getMaximumValue(), value);
        value = Math.max(constraints.getMinimumValue(), value);
      }
      return String.valueOf(value);
    }
    catch (final NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.non_number_error"),
        "Increment " + prop.getName() + ": format=" + format.getFormat() + ", value=" + format.getText(constraints, constraints, audit), e, constraints, audit)); //NON-NLS
      return oldValue;
    }
  }

  public PropertySource getTargetPropertySource() {
    return constraints.getPropertySource();
  }

  public String getIncrement() {
    return format.getFormat();
  }

  public interface Constraints extends PropertySource {
    int getMinimumValue();
    int getMaximumValue();
    boolean isNumeric();
    boolean isWrap();
    PropertySource getPropertySource();
  }
}
