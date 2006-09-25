/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

/** 
 * Class tht implements a named Color Swatch
 */

package VASSAL.build.module.gamepieceimage;

import java.awt.Color;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.tools.SequenceEncoder;

public class ColorSwatch extends AbstractConfigurable implements Comparable {

  protected static final String NAME = "name";
  protected static final String COLOR = "color";
  
  public static final String BLACK = "BLACK";
  public static final String WHITE = "WHITE";
  public static final String CLEAR = "CLEAR";
  public static final String RED = "RED";
  
  protected Color color;

  public ColorSwatch() {
    super();
    name = "";
    color = null;
  }
  
  public ColorSwatch(String n, Color c) {
    this();
    name = n;
    color = c;
  }
  
  public ColorSwatch(String code) {
    this();
    decode(code);
  }
  
  public Color getColor() {
    return color;
  }
  
  public void setColor(Color c) {
    color = c;
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] { "Color Name:  ", "Color:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, Color.class};
  }
  
  public String[] getAttributeNames() {
    return new String[] { NAME, COLOR };
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (COLOR.equals(key)) {
      if (o instanceof String) {
        o = ColorConfigurer.stringToColor((String) o);
      }
      color = (Color) o;
    }
  }


  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else
      return null;
  }

  public void removeFrom(Buildable parent) {
  
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
  
  public static String getConfigureTypeName() {
    return "Named Color";
  }

  public void addTo(Buildable parent) {

  }
  
  public static ColorSwatch getBlack() {
    return new ColorSwatch(BLACK, Color.BLACK);
  }
  
  public static ColorSwatch getWhite() {
    return new ColorSwatch(WHITE, Color.WHITE);
  }

  public static ColorSwatch getRed() {
    return new ColorSwatch(RED, Color.RED);
  }
  
  public static ColorSwatch getClear() {
    return new ColorSwatch(CLEAR, null);
  }
  
  public String encode() {
    
//    SequenceEncoder se = new SequenceEncoder(';');
//    se.append(getConfigureName());
//    //se.append(getColor());
//    return se.getValue();
    return getConfigureName();
  }
  
  public void decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    setConfigureName(sd.nextToken());
    Color c = ColorManager.getColorManager().getColorByName(getConfigureName());
    if (c == null && !getConfigureName().equals(CLEAR)) {
      c = ColorManager.getColorManager().getColorByName("BLACK");
    }
    setColor(c);
    //setColor(sd.nextColor(Color.BLACK));
  }
  
  public static ColorSwatch getDefaultSwatch() {
    return ColorManager.getColorManager().getColorSwatch(ColorManager.DEFAULT_COLOR);
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo(Object c) {
    return name.compareTo(((ColorSwatch) c).name);
  }
}