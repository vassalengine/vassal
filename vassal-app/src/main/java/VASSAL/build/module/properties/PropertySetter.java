/*
 *
 * Copyright (c) 2006 by Rodney Kinney
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

import VASSAL.build.module.properties.PropertyChangerConfigurer.Constraints;
import VASSAL.script.expression.AuditTrail;
import VASSAL.tools.FormattedString;

/**
 * Provides a fixed value
 * The value can be specified as a FormattedString property and evaluated at runtime
 * @author rkinney
 */
public class PropertySetter implements PropertyChanger {
  private String newValue;
  private final Constraints propSource;
  private FormattedString format;

  public PropertySetter(String newValue, Constraints propSource) {
    this.newValue = newValue;
    this.propSource = propSource;
    if (propSource != null) {
      format = new FormattedString();
    }
  }

  public String getRawValue() {
    return newValue;
  }

  @Override
  public String getNewValue(String oldValue) {
    String s = newValue;
    if (format != null) {
      format.setFormat(s);
      s = format.getText(propSource.getPropertySource(), propSource, AuditTrail.create(propSource, s));
    }
    return s;
  }

  public void setNewValue(String newValue) {
    this.newValue = newValue;
  }


}
