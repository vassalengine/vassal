/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney
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

  public String getNewValue(String oldValue) {
    int value = 0;
    try {
      if (oldValue == null || oldValue.length() == 0) {
        value = 0;
      }
      else {
        value = Integer.parseInt(oldValue);
      }
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataError(new BadDataReport(Resources.getString("Error.non_number_error"),"Increment "+prop.getName()+": oldValue "+"="+oldValue,e));
      return oldValue;
    }

    try {

      int incr = Integer.parseInt(format.getText(constraints.getPropertySource()));
      if (constraints.isWrap()) {
        if (value + incr > constraints.getMaximumValue()) {
          value = constraints.getMinimumValue() + (value + incr - constraints.getMaximumValue() - 1);
        }
        else if (value + incr < constraints.getMinimumValue()) {
          value = constraints.getMaximumValue() + (value + incr - constraints.getMinimumValue() + 1);
        }
        else {
          value += incr;
        }
      }
      else {
        value += incr;
        value = Math.min(constraints.getMaximumValue(), value);
        value = Math.max(constraints.getMinimumValue(), value);
      }
      return String.valueOf(value);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataError(new BadDataReport(Resources.getString("Error.non_number_error"),"Increment "+prop.getName()+": format="+format.getFormat()+", value="+format.getText(constraints),e));
      return oldValue;
    }
  }

  public String getIncrement() {
    return format.getFormat();
  }

  public static interface Constraints extends PropertySource {
    int getMinimumValue();
    int getMaximumValue();
    boolean isWrap();
    PropertySource getPropertySource();
  }
}
