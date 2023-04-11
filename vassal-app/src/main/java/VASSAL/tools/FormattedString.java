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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.AuditableException;
import VASSAL.script.expression.Expression;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.concurrent.ConcurrentSoftHashMap;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

/**
 * FormattedString.java
 *
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value.
 *
 * FormattedStrings are ultimately evaluated using the Beanshell Interpreter, so
 * a Formatted String can be specified as a valid BeanShell expression surrounded
 * by {}'s. For this reason, full support is included for generating Expression Audit
 * Trails to be parsed inward the the Expression interpreter.
 *
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

  /**
   * Return true if the supplied string contains $$ variables or is a Beanshell expression
   * @param text String to check
   * @return true if the string needs to be evaluated
   */
  public static boolean isDynamic(String text) {
    return text != null && (StringUtils.countMatches(text, "$") > 1  || text.trim().startsWith("{"));
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
   * Evaulate a formatted String and return unlocalized text
   * Use the default property source to find property values
   * 
   * @deprecated Use {@link #getText(Auditable, String)}
   * @return evaluated formatted String
   */
  @Deprecated(since = "2021-12-01")
  public String getText() {
    return getText(fsdata.defaultProperties, false, null, null);
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * Use the default property source to find property values
   * Create an AuditTrail object if expression auditing enabled
   * 
   * @param owner Owning component of this formatted string. 
   * @param fieldKey Message key describing the editor field holding this formmated string
   * @return evaluated formatted String
   */
  public String getText(Auditable owner, String fieldKey) {
    return getText(fsdata.defaultProperties, false, owner, AuditTrail.create(owner, getFormat(), Resources.getString(fieldKey)));
  }

  /**
   * Evaluate a Formatted String and return localized text   
   * Use the default property source to find property values
   * 
   * @deprecated Use {@link #getLocalizedText(Auditable, String)}
   * @return localized text
   */
  @Deprecated(since = "2021-12-01")
  public String getLocalizedText() {
    return getText(fsdata.defaultProperties, true, null, null);
  }

  /**
   * Evaulate a formatted String and return localized text
   * Use the default property source to find property values
   * Create an AuditTrail object if expression auditing enabled
   *
   * @param owner Owning component of this formatted string. 
   * @param fieldKey Message key describing the editor field holding this formmated string
   * @return evaluated formatted String
   */
  public String getLocalizedText(Auditable owner, String fieldKey) {
    return getText(fsdata.defaultProperties, true, owner, AuditTrail.create(owner, getFormat(), Resources.getString(fieldKey)));
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * Also, if any property keys match a property in the given GamePiece,
   * substitute the value of that property
   *
   * @param ps property source
   * @return Return the resulting string after substituting properties
   * @deprecated Use {@link #getText(PropertySource, Auditable, String)}
   */
  @Deprecated(since = "2021-12-01")
  public String getText(PropertySource ps) {
    return getText(ps, false, null, null);
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * Use the supplied property source to find property values
   * Create an AuditTrail object if expression auditing enabled
   *
   * @param owner Owning component of this formatted string.
   * @param fieldKey Message key describing the editor field holding this formatted string
   * @return evaluated formatted String
   */
  public String getText(PropertySource ps, Auditable owner, String fieldKey) {
    return getText(ps, false, owner, AuditTrail.create(owner, fsdata.formatString, Resources.getString(fieldKey)));
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * Use the supplied property source to find property values
   *
   * @param ps Property source to supply property values
   * @param owner Owning component of this formatted string.
   * @param audit AuditTrail to use for expression auditing
   * @return evaluated formatted String
   */
  public String getText(PropertySource ps, Auditable owner, AuditTrail audit) {
    return getText(ps, false, owner, audit);
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * @deprecated Use {@link #getText(PropertySource, String, Auditable, String)}
   */
  @Deprecated(since = "2021-12-01")
  public String getText(PropertySource ps, boolean localized) {
    return getText(ps, localized, null, null);
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
   * @deprecated Use {@link #getText(PropertySource, String, Auditable, String)} 
   */
  @Deprecated(since = "2021-12-01")
  public String getText(PropertySource ps, String def) {
    return getText(ps, def, null, (AuditTrail) null);
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * Use the supplied property source to find property values
   * If any property value is found to be null, use the supplied default value instead
   * Create an AuditTrail object if expression auditing enabled
   *
   * @param ps Property source to supply property values
   * @param def Default value for properties with no value
   * @param owner Owning component of this formatted string.
   * @param fieldKey Message key describing the editor field holding this formatted string
   * @return evaluated formatted String
   */
  public String getText(PropertySource ps, String def, Auditable owner, String fieldKey) {
    return  getText(ps, def, owner, AuditTrail.create(owner, getFormat(), Resources.getString(fieldKey)));
  }

  /**
   * Evaulate a formatted String and return unlocalized text
   * Use the supplied property source to find property values
   * If any property value is found to be null, use the supplied default value instead
   *
   * @param ps Property source to supply property values
   * @param def Default value for properties with no value
   * @param owner Owning component of this formatted string.
   * @param audit AuditTrail to use for expression auditing
   * @return evaluated formatted String
   */
  public String getText(PropertySource ps, String def, Auditable owner, AuditTrail audit) {
    String s = getText(ps, false, owner, audit);
    if (s == null || s.length() == 0) {
      s = def;
    }
    return s;
  }

  /**
   * Evaulate a formatted String and return localized text
   * Use the supplied property source to find property values
   * Create an AuditTrail object if expression auditing enabled
   *
   * @param owner Owning component of this formatted string.
   * @param fieldKey Message key describing the editor field holding this formatted string
   * @return evaluated formatted String
   */
  public String getLocalizedText(PropertySource ps, Auditable owner, String fieldKey) {
    return getText(ps, true, owner, AuditTrail.create(owner, fsdata.formatString, Resources.getString(fieldKey)));
  }

  /**
   * Evaulate a formatted String and return localized text
   * Use the supplied property source to find property values
   *
   * @param ps Property source to supply property values
   * @param owner Owning component of this formatted string.
   * @param audit AuditTrail to use for expression auditing
   * @return evaluated formatted String
   */
  public String getLocalizedText(PropertySource ps, Auditable owner, AuditTrail audit) {
    return getText(ps, true, owner, audit);
  }

  /**
   * Evaulate a formatted String and return localized text
   * @deprecated Use {@link #getLocalizedText(PropertySource, Auditable, String)}
   */
  @Deprecated(since = "2021-12-01")
  public String getLocalizedText(PropertySource ps) {
    return getLocalizedText(ps, null, (AuditTrail) null);
  }
  
  /**
   * Core implementation of getText. All other call signatures should eventually call this version.
   * 
   * Evaluate the supplied Formmatted String, using the supplied property source to replace any property references.
   * NOTE that evaluation is handled by the Beanshell Interpreter, so full Beanshell is supported in Formatted Strings (yikes!)
   * Use the supplied owner and audit trail for error reporting purposes.
   * 
   * @param ps Property source to use to supply property values
   * @param localized true if getLocalizedProperty() calls should be used to evaluate property values
   * @param owner Auditable owner of this Formmatted String for reporting purposes
   * @param audit Audit Trail for Expression evaluation error reporting (may be null) 
   * @return Evaluated formatted string
   */
  public String getText(PropertySource ps, boolean localized, Auditable owner, AuditTrail audit) {
    final PropertySource source = ps == null ? fsdata.defaultProperties : ps;
    try {
      RecursionLimiter.startExecution(this);
      return fsdata.format.tryEvaluate(source, props, localized, owner, audit);
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
    final AuditTrail audit = AuditTrail.create((Auditable) source, this, description);
    return getTextAsInt(ps, description, source, (Auditable) ps, audit);
  }

  public int getTextAsInt(PropertySource ps, String description, EditablePiece source, Auditable owner, AuditTrail audit) {
    int result = 0;
    final String value = getText(ps, "0", source, audit);
    try {
      result = Integer.parseInt(value);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport(
        source,
        Resources.getString("Error.non_number_error"),
        debugInfo(this, value, description), new AuditableException((Auditable) ps, audit)
      ));
    }
    return result;
  }

  public int getTextAsInt(PropertySource ps, String description, AbstractConfigurable source) {
    int result = 0;
    final AuditTrail audit = AuditTrail.create((Auditable) source, this, description);
    final String value = getText(ps, "0", source, audit);
    try {
      result = Integer.parseInt(value);
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport(
        source,
        Resources.getString("Error.non_number_error"),
        debugInfo(this, value, description), new AuditableException((Auditable) ps, audit)
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
