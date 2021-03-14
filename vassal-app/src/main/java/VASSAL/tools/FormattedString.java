/*
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
import java.util.Objects;

import org.apache.commons.lang3.tuple.Pair;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.concurrent.ConcurrentSoftHashMap;

/**
 * FormattedString.java
 *
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 */
public class FormattedString implements Loopable {
  private static final Map<Pair<String, PropertySource>, FSData> CACHE = new ConcurrentSoftHashMap<>();

  private static class FSData {
    // The actual string for display purposes
    public final String formatString;

    // An efficiently evaluable representation of the string
    public final Expression format;

    public final PropertySource defaultProperties;

    public FSData(String fs, PropertySource dp) {
      formatString = fs;
      format = Expression.createExpression(fs);
      defaultProperties = dp;
    }
  }

  private static FSData dataOf(String fs, PropertySource dp) {
    return CACHE.computeIfAbsent(
      Pair.of(fs, dp),
      p -> new FSData(p.getLeft(), p.getRight())
    );
  }

  private FSData fsdata;

  protected Map<String, String> props;

  public FormattedString() {
    this("");
  }

  public FormattedString(String s) {
    this(s, GameModule.getGameModule());
  }

  public FormattedString(PropertySource defaultProperties) {
    this("", defaultProperties);
  }

  public FormattedString(String formatString, PropertySource defaultProperties) {
    fsdata = dataOf(formatString, defaultProperties);
  }

  public void setFormat(String fs) {
    fsdata = dataOf(fs, fsdata.defaultProperties);
  }

  public String getFormat() {
    return fsdata.formatString;
  }

  public void setProperty(String name, String value) {
    if (props == null) {
      props = new HashMap<>();
    }
    props.put(name.intern(), value != null ? value.intern() : null);
  }

  public void clearProperties() {
    if (props != null) {
      props.clear();
    }
  }

  public void setDefaultProperties(PropertySource defaultProperties) {
    fsdata = dataOf(fsdata.formatString, defaultProperties);
  }

  public PropertySource getDefaultProperties() {
    return fsdata.defaultProperties;
  }

  /**
   * @return the resulting string after substituting properties
   */
  public String getText() {
    return getText(fsdata.defaultProperties, false);
  }

  public String getLocalizedText() {
    return getText(fsdata.defaultProperties, true);
  }

  /**
   * Return the resulting string after substituting properties
   * Also, if any property keys match a property in the given GamePiece,
   * substitute the value of that property
   * @see GamePiece#getProperty
   * @param ps property source
   * @return Return the resulting string after substituting properties
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
   * @param ps Property source
   * @param def the default if the result is otherwise empty
   * @return Return the resulting string after substituting properties
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

  protected String getText(PropertySource ps, boolean localized) {
    final PropertySource source = ps == null ? fsdata.defaultProperties : ps;
    try {
      RecursionLimiter.startExecution(this);
      try {
        return fsdata.format.evaluate(source, props, localized);
      }
      catch (ExpressionException e) {
        BadDataReport bdr;
        final String msg = Resources.getString("Error.expression_error");
        final String exp = fsdata.format.getExpression();

        if (source instanceof EditablePiece) {
          bdr = new BadDataReport(
            (EditablePiece) source, msg, exp, e
          );
        }
        else if (source instanceof AbstractConfigurable) {
          bdr = new BadDataReport(
            (AbstractConfigurable) source, msg, exp, e
          );
        }
        else {
          bdr = new BadDataReport(msg, exp, e);
        }

        ErrorDialog.dataWarning(bdr);
        return "";
      }
    }
    catch (RecursionLimitException e) {
      ErrorDialog.dataWarning(new BadDataReport(
        Resources.getString("Error.possible_infinite_string_loop"),
        fsdata.format.getExpression(), e
      ));
      return "";
    }
    finally {
      RecursionLimiter.endExecution();
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
      ErrorDialog.dataWarning(new BadDataReport(
        source,
        Resources.getString("Error.non_number_error"),
        debugInfo(this, value, description), e
      ));
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
      ErrorDialog.dataWarning(new BadDataReport(
        source,
        Resources.getString("Error.non_number_error"),
        debugInfo(this, value, description), e
      ));
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
   * Use format 2 if the formatted contains an expression that has been expanded.
   *
   * @param format Formatted String
   * @param description Description of the String
   * @param value Value generated by the formatted string
   * @return error message
   */
  public static String debugInfo(FormattedString fs, String value, String description) {
    return description + (value.equals(fs.getFormat()) ? "" : "[" + fs.getFormat() + "]") + "=" + value;
  }

  public String debugInfo(String value, String description) {
    return debugInfo(this, value, description);
  }

  @Override
  public int hashCode() {
    return fsdata.formatString == null ? 0 : fsdata.formatString.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }

    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }

    final FormattedString other = (FormattedString) obj;
    return Objects.equals(fsdata.formatString, other.fsdata.formatString);
  }

  @Override
  public String getComponentTypeName() {
    return Resources.getString("Editor.FormattedString.component_type");
  }

  @Override
  public String getComponentName() {
    return Resources.getString("Editor.FormattedString.component_type");
  }
}
