/*
 *
 * Copyright (c) 2020 by Vassal developers
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

import java.util.HashMap;
import java.util.Map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;

/**
 * FormattedString.java
 *
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 *
 *
 */
public class FormattedString {

  // The actual string for display purposes
  protected String formatString;

  // An efficiently evaluable representation of the string
  protected Expression format;

  protected Map<String,String> props = new HashMap<>();
  protected PropertySource defaultProperties;

  public FormattedString() {
    this("");
  }

  public FormattedString(String s) {
    this(s,GameModule.getGameModule());
  }

  public FormattedString(PropertySource defaultProperties) {
    this("",defaultProperties);
  }

  public FormattedString(String formatString, PropertySource defaultProperties) {
    setFormat(formatString);
    this.defaultProperties = defaultProperties;
  }

  public void setFormat(String s) {
    formatString = s;
    format = Expression.createExpression(s);
  }

  public String getFormat() {
    return formatString;
  }

  public void setProperty(String name, String value) {
    props.put(name, value);
  }

  public void clearProperties() {
    props.clear();
  }

  /**
   * Return the resulting string after substituting properties
   * @return
   */
  public String getText() {
    return getText(defaultProperties, false);
  }

  public String getLocalizedText() {
    return getText(defaultProperties, true);
  }

  /**
   * Return the resulting string after substituting properties
   * Also, if any property keys match a property in the given GamePiece,
   * substitute the value of that property
   * @see GamePiece#getProperty
   * @param ps
   * @return
   */
  public String getText(PropertySource ps) {
    return getText(ps, false);
  }

  /**
   * Return the resulting string after substituting properties
   * Also, if any property keys match a property in the given GamePiece,
   * substitute the value of that property. If the resulting string is
   * empty, then the default is returned.
   * @see GamePiece#getProperty
   * @param ps
   * @param def the default if the result is otherwise empty
   * @return
   */
  public String getText(PropertySource ps, String def) {
    String s = getText(ps, false);
    if (s == null || s.length() == 0) {
      s = def;
    }
    return s;
  }

  public String getLocalizedText(PropertySource ps) {
    return getText(ps, true);
  }

  protected String getText(PropertySource ps, boolean localized){
    final PropertySource source = (ps==null) ? defaultProperties : ps;
    try {
      return format.evaluate(source, props, localized);
    }
    catch (ExpressionException e) {
      if (source instanceof EditablePiece) {
        ErrorDialog.dataError(new BadDataReport((EditablePiece) source, Resources.getString("Error.expression_error"), format.getExpression(), e));
      }
      else if (source instanceof AbstractConfigurable) {
        ErrorDialog.dataError(new BadDataReport((AbstractConfigurable) source, Resources.getString("Error.expression_error"), format.getExpression(), e));
      }
      else {
        ErrorDialog.dataError(new BadDataReport(Resources.getString("Error.expression_error"), format.getExpression(), e));
      }
      return "";
    }
  }

  /**
   * Expand a FormattedString using the supplied propertySource and parse it as
   * an integer. If the expanded string is not an integer, generate a Bad Data Report
   * with debugging information and return a value of 0
   *
   */
  public int getTextAsInt(PropertySource ps, String description, EditablePiece source) {
    int result = 0;
    final String value = getText(ps, "0");
    try {
      result = Integer.parseInt(value);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataError(new BadDataReport(source, Resources.getString("Error.non_number_error"), debugInfo(this, value, description), e));
    }
    return result;
  }

  public int getTextAsInt(PropertySource ps, String description, AbstractConfigurable source) {
    int result = 0;
    final String value = getText(ps, "0");
    try {
      result = Integer.parseInt(value);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataError(new BadDataReport(source, Resources.getString("Error.non_number_error"), debugInfo(this, value, description), e));
    }
    return result;
  }

  /**
   * Format a standard debug message for use in Decorator bad data reports.
   *
   *  description=value
   *  description[format]=value
   *
   * Use format 1 if the generated value is the same as the format
   * Use format 2 if the formated contains an expression that has been expanded.
   *
   * @param format Formatted String
   * @param description Description of the String
   * @param value Value generated by the formatted string
   * @return error message
   */
  public static String debugInfo(FormattedString format, String value, String description) {
    return description + (value.equals(format.getFormat()) ? "" : "[" + format.getFormat()+ "]") + "=" + value;
  }

  public String debugInfo(String value, String description) {
    return debugInfo(this, value, description);
  }

  public PropertySource getDefaultProperties() {
    return defaultProperties;
  }

  public void setDefaultProperties(PropertySource defaultProperties) {
    this.defaultProperties = defaultProperties;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result
      + ((formatString == null) ? 0 : formatString.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    FormattedString other = (FormattedString) obj;
    if (formatString == null) {
      if (other.formatString != null)
        return false;
    }
    else if (!formatString.equals(other.formatString))
      return false;
    return true;
  }

}
