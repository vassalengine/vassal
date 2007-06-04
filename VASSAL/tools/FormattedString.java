/*
 * FormattedString.java
 * A String that can include options of the form $optionName$. Option values
 * are maintained in a property list and getText returns the string will all
 * options replaced by their value
 */

package VASSAL.tools;

import java.util.HashMap;
import java.util.Map;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.counters.GamePiece;

public class FormattedString {

  protected String formatString;
  protected Map<String,String> props = new HashMap<String,String>();
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
    this.formatString = formatString;
    this.defaultProperties = defaultProperties;
  }

  public void setFormat(String s) {
    formatString = s;
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

//  public String getText(GamePiece piece) {  GamePiece is now a PropertySource
//    return getText((PropertySource)piece);
//  }
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
  public String getLocalizedText(PropertySource ps) {
    return getText(ps, true);
  }
  
  protected String getText(PropertySource ps, boolean localized) {
    StringBuffer buffer = new StringBuffer();
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(formatString, '$');
    boolean isProperty = true;
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      isProperty = !isProperty;
      if (token.length() > 0) {
        /*
         * Only even numbered tokens with at least one token after them are valid $propertName$ strings.
         */
        if (!isProperty || ! st.hasMoreTokens()) {
          buffer.append(token);
        }
        else if (props.containsKey(token)) {
          String value = props.get(token);
          if (value != null) {
            buffer.append(value);
          }
        }
        else if (ps != null) {
          Object value = localized ? ps.getLocalizedProperty(token) : ps.getProperty(token);
          if (value != null) {
            buffer.append(value.toString());
          }
          else {
            buffer.append(token);
          } 
        }
        else {
          buffer.append(token);
        }
      }
    }

    return buffer.toString();
  }

  public PropertySource getDefaultProperties() {
    return defaultProperties;
  }

  public void setDefaultProperties(PropertySource defaultProperties) {
    this.defaultProperties = defaultProperties;
  }

}
