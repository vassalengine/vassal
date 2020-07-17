/*
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

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class TextItemInstance extends ItemInstance {

  protected static final String VALUE = "value"; //$NON-NLS-1$
  protected static final String OUTLINE_COLOR = "outlineColor"; //$NON-NLS-1$

  protected String val = ""; //$NON-NLS-1$
  protected ColorSwatch outlineColor = ColorSwatch.getRed();

  public TextItemInstance() {
    super();
    val = "Xx"; //$NON-NLS-1$
    outlineColor = ColorSwatch.getRed();
  }

  public TextItemInstance(String nam, String typ, String loc, String val) {
    super(nam, typ, loc);
    if (val == null) {
      switch (nam.length()) {
      case 0:
        setValue("Xx"); //$NON-NLS-1$
        break;
      case 1:
        setValue(nam);
        break;
      default:
        setValue(nam.substring(0, 2));
        break;
      }
    }
    else {
      setValue(val);
    }
  }

  public TextItemInstance(String code, GamePieceImage defn) {
    super(defn);
    decode(code);
  }

  public void setValue(String value) {
    this.val = value;
  }

  public String getValue() {
    return val;
  }

  public boolean isOutline() {
    TextItem item = (TextItem) getItem();
    return (item == null) ? false : item.isOutline();
  }

  public ColorSwatch getOutlineColor() {
    return outlineColor;
  }

  public void setOutlineColor(ColorSwatch c) {
    outlineColor = c;
  }

  @Override
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getFgColor().encode());
    se.append(getBgColor().encode());
    se.append(getValue());
    se.append(getOutlineColor().encode());
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
    setOutlineColor(new ColorSwatch(sd.nextToken(""))); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      "Value:  ",
      "Foreground Color:  ",
      "Background Color:  ",
      "Outline Color:  "
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      FgColorSwatchConfig.class,
      BgColorSwatchConfig.class,
      OutlineColorSwatchConfig.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      VALUE,
      FG_COLOR,
      BG_COLOR,
      OUTLINE_COLOR
    };
  }

  @Override
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
      bgColor = (ColorSwatch) o;
    }
    else if (OUTLINE_COLOR.equals(key)) {
      if (o instanceof String) {
        o = new ColorSwatch((String) o);
      }
      outlineColor = (ColorSwatch) o;
    }
    if (myConfig != null) {
      myConfig.rebuildViz();
    }

  }

  @Override
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
    else if (OUTLINE_COLOR.equals(key)) {
      return outlineColor.encode();
    }
    else
      return null;
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (VALUE.equals(name)) {
      return valueCond;
    }
    else if (OUTLINE_COLOR.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return isOutline();
        }};
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition valueCond = new VisibilityCondition() {
    @Override
    public boolean shouldBeVisible() {
      return !((TextItem) getItem()).isFixed();
    }
  };

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getBgColor());
    }
  }

  public static class FgColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((ItemInstance) c).getFgColor());
    }
  }

  public static class OutlineColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((TextItemInstance) c).getOutlineColor());
    }
  }

}
