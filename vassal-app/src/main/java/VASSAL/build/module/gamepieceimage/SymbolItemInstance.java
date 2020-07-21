/*
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

package VASSAL.build.module.gamepieceimage;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.tools.SequenceEncoder;

public class SymbolItemInstance extends ItemInstance {

  public static final String SIZE = "size"; //$NON-NLS-1$
  public static final String SIZE_COLOR = "sizeColor"; //$NON-NLS-1$
  public static final String SYMBOL1 = "symbol1"; //$NON-NLS-1$
  public static final String SYMBOL2 = "symbol2"; //$NON-NLS-1$

  protected String size;
  protected String symbol1;
  protected String symbol2;
  private ColorSwatch sizeColor = ColorSwatch.getBlack();

  public SymbolItemInstance() {
    super();
  }

  public SymbolItemInstance(String nam, String typ, String loc, String sz, String s1, String s2) {
    super(nam, typ, loc);
    setSize(sz);
    setSymbol1(s1);
    setSymbol2(s2);
  }

  public SymbolItemInstance(String code, GamePieceImage defn) {
    super(defn);
    decode(code);
  }

  @Override
  public String encode() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType());
    se.append(getName());
    se.append(getLocation());
    se.append(getFgColor().encode());
    se.append(getBgColor().encode());
    se.append(getSize());
    se.append(getSymbol1());
    se.append(getSymbol2());
    se.append(getSizeColor().encode());
    return se.getValue();
  }

  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    setType(sd.nextToken("")); //$NON-NLS-1$
    setName(sd.nextToken("")); //$NON-NLS-1$
    setLocation(sd.nextToken("")); //$NON-NLS-1$
    setFgColor(new ColorSwatch(sd.nextToken(""))); //$NON-NLS-1$
    setBgColor(new ColorSwatch(sd.nextToken(""))); //$NON-NLS-1$
    setSize(sd.nextToken("")); //$NON-NLS-1$
    setSymbol1(sd.nextToken("")); //$NON-NLS-1$
    setSymbol2(sd.nextToken("")); //$NON-NLS-1$
    setSizeColor(new ColorSwatch(sd.nextToken(""))); //$NON-NLS-1$
  }

  public void setSize(String size) {
    this.size = size;
  }

  public String getSize() {
    return size;
  }

  protected void setSizeColor(ColorSwatch sizeColor) {
    this.sizeColor = sizeColor;
  }

  protected ColorSwatch getSizeColor() {
    return sizeColor;
  }

  public void setSymbol1(String symbol1) {
    this.symbol1 = symbol1;
  }

  public String getSymbol1() {
    return symbol1;
  }

  public void setSymbol2(String symbol2) {
    this.symbol2 = symbol2;
  }

  public String getSymbol2() {
    return symbol2;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      "Unit Size:  ",
      "1st Symbol:  ",
      "2nd Symbol:  ",
      "Symbol Color:  ",
      "Background Color:  ",
      "Size Color:  "
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      SizeConfig.class,
      Symbol1Config.class,
      Symbol2Config.class,
      FgColorSwatchConfig.class,
      BgColorSwatchConfig.class,
      SizeColorSwatchConfig.class
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      SIZE,
      SYMBOL1,
      SYMBOL2,
      FG_COLOR,
      BG_COLOR,
      SIZE_COLOR
    };
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (SIZE.equals(key)) {
      size = (String) value;
    }
    else if (SYMBOL1.equals(key)) {
      symbol1 = (String) value;
    }
    else if (SYMBOL2.equals(key)) {
      symbol2 = (String) value;
    }
    else if (FG_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      fgColor = (ColorSwatch) value;
    }
    else if (BG_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      bgColor = (ColorSwatch) value;
    }
    else if (SIZE_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      sizeColor = (ColorSwatch) value;
    }
    if (myConfig != null) {
      myConfig.rebuildViz();
    }

  }

  @Override
  public String getAttributeValueString(String key) {
    if (SIZE.equals(key)) {
      return size;
    }
    else if (SYMBOL1.equals(key)) {
      return symbol1;
    }
    else if (SYMBOL2.equals(key)) {
      return symbol2;
    }
    else if (FG_COLOR.equals(key)) {
      return fgColor.encode();
    }
    else if (BG_COLOR.equals(key)) {
      return bgColor.encode();
    }
    else if (SIZE_COLOR.equals(key)) {
      return sizeColor.encode();
    }
    else
      return null;
  }

  public static class SizeConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new SizeConfigurer(key, name);
    }
  }

  public static class Symbol1Config implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new SymbolConfigurer(key, name);
    }
  }

  public static class Symbol2Config implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new SymbolConfigurer(key, name);
    }
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((SymbolItemInstance) c).getBgColor());
    }
  }

  public static class FgColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((SymbolItemInstance) c).getFgColor());
    }
  }

  public static class SizeColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((SymbolItemInstance) c).getFgColor());
    }
  }
}
