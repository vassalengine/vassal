/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.gamepieceimage;

import java.awt.Color;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.TextConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class TextBoxItemInstance extends ItemInstance {

  protected static final String VALUE = "value"; //$NON-NLS-1$
  protected static final String BG_COLOR = "borderColor"; //$NON-NLS-1$

  protected String val = ""; //$NON-NLS-1$

  public TextBoxItemInstance() {
    super();
    setFgColor(ColorSwatch.getBlack());
    setBgColor(ColorManager.getColorManager().getColorSwatch(Color.LIGHT_GRAY));
  }

  public TextBoxItemInstance(String code, GamePieceImage defn) {
    super(defn);
    decode(code);
  }

  public TextBoxItemInstance(String name, String type, String location) {
    super(name, type, location);
    setFgColor(ColorSwatch.getBlack());
    setBgColor(ColorManager.getColorManager().getColorSwatch(Color.LIGHT_GRAY));
  }

  public void setValue(String value) {
    this.val = value;
  }

  public String getValue() {
    return val;
  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getFgColor().encode());
    se.append(getBgColor().encode());
    se.append(getValue());
    return se.getValue();
  }

  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    setType(sd.nextToken("")); //$NON-NLS-1$
    setName(sd.nextToken("")); //$NON-NLS-1$
    setLocation(sd.nextToken("")); //$NON-NLS-1$
    setFgColor(new ColorSwatch(sd.nextToken(""))); //$NON-NLS-1$
    setBgColor(new ColorSwatch(sd.nextToken(""))); //$NON-NLS-1$
    setValue(sd.nextToken("")); //$NON-NLS-1$
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
      "Value:  ",
      "Text Color:  ",
      "Background Color:  "
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      WrappingTextConfigurer.class,
      FgColorSwatchConfig.class,
      BgColorSwatchConfig.class
    };
  }

  public static class WrappingTextConfigurer implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new TextConfigurer(key, name, ((TextBoxItemInstance) c).val, true);
    }
  }

  public String[] getAttributeNames() {
    return new String[] { VALUE, FG_COLOR, BG_COLOR };
  }

  public void setAttribute(String key, Object o) {

    if (VALUE.equals(key)) {
      this.val = (String) o;
    }
    else if (FG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      fgColor = (ColorSwatch) o;
    }
    else if (BG_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      bgColor = (ColorSwatch)o;
    }
    if (myConfig != null) {
      myConfig.rebuildViz();
    }

  }

  public String getAttributeValueString(String key) {
    if (VALUE.equals(key)) {
      return val;
    }
    else if (FG_COLOR.equals(key)) {
      return fgColor.encode();
    }
    else if (BG_COLOR.equals(key)) {
      return bgColor.encode();
    }
    else
      return null;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (VALUE.equals(name)) {
      return valueCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition valueCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !((TextItem) getItem()).isFixed();
    }
  };

  public static class FgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getFgColor());
    }
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getBgColor());
    }
  }
}
