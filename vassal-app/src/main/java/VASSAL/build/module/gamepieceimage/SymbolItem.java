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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;

import VASSAL.configure.TranslatableStringEnum;
import VASSAL.i18n.Resources;
import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.AutoConfigurable;
import VASSAL.tools.SequenceEncoder;

public class SymbolItem extends Item {

  public static final String TYPE = "Symbol"; //$NON-NLS-1$

  protected static final String SET = "set"; //$NON-NLS-1$
  protected static final String WIDTH = "width"; //$NON-NLS-1$
  protected static final String HEIGHT = "height"; //$NON-NLS-1$
  protected static final String LINE_WIDTH = "linewidth"; //$NON-NLS-1$

  protected String symbolSet = ""; //$NON-NLS-1$
  protected int height = 30;
  protected int width = 40;
  protected double lineWidth = 1.0f;

  public SymbolItem() {
    super();
  }

  public SymbolItem(GamePieceLayout l) {
    super(l);
    width = getLayout().getLayoutWidth() / 2;
    height = (int) (width * 0.75);
  }

  public SymbolItem(GamePieceLayout l, String nam) {
    this(l);
    setConfigureName(nam);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.insert(
      2, super.getAttributeDescriptions(),
      Resources.getString("Editor.SymbolItem.symbol_set"),
      Resources.getString("Editor.width"),
      Resources.getString("Editor.height"),
      Resources.getString("Editor.SymbolItem.line_width")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.insert(
      2, super.getAttributeTypes(),
      SetConfig.class,
      Integer.class,
      Integer.class,
      Double.class);
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.insert(
      2, super.getAttributeNames(),
      SET,
      WIDTH,
      HEIGHT,
      LINE_WIDTH
    );
  }

  public static class SetConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return Symbol.SYMBOL_SETS;
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return Symbol.SYMBOL_SETS_DESC;
    }
  }


  @Override
  public void setAttribute(String key, Object o) {
    switch (key) {
    case WIDTH:
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      width = (Integer) o;
      if (width < 1) width = 1;
      break;
    case HEIGHT:
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      height = (Integer) o;
      if (height < 1) height = 1;
      break;
    case LINE_WIDTH:
      if (o instanceof String) {
        o = Double.valueOf((String) o);
      }
      lineWidth = (Double) o;
      if (lineWidth < 0) lineWidth = 0;
      break;
    default:
      super.setAttribute(key, o);
      break;
    }

    if (layout != null) {
      layout.refresh();
    }
  }

  @Override
  public String getAttributeValueString(String key) {

    if (SET.equals(key)) {
      return symbolSet;
    }
    else if (WIDTH.equals(key)) {
      return String.valueOf(width);
    }
    else if (HEIGHT.equals(key)) {
      return String.valueOf(height);
    }
    else if (LINE_WIDTH.equals(key)) {
      return String.valueOf(lineWidth);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }

  @Override
  public void draw(Graphics g, GamePieceImage defn) {
    SymbolItemInstance si = null;
    if (defn != null) {
      si = defn.getSymbolInstance(getConfigureName());
    }
    final Symbol symbol;
    if (si == null) {
      symbol = new Symbol(Symbol.NATO, Symbol.NatoUnitSymbolSet.INFANTRY, Symbol.NatoUnitSymbolSet.NONE, Symbol.NatoUnitSymbolSet.SZ_DIVISION);
      si = new SymbolItemInstance();
    }
    else {
      symbol = new Symbol(Symbol.NATO, si.getSymbol1(), si.getSymbol2(), si.getSize());
    }

    final Point origin = layout.getPosition(this);
    final Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());

    final Graphics2D g2d = (Graphics2D) g;

    if (getRotation() != 0) {
      final AffineTransform newXForm =
          AffineTransform.getRotateInstance(Math.toRadians(getRotation()), layout.getPosition(this).x, layout.getPosition(this).y);
      g2d.transform(newXForm);
    }

    final Object aa = g2d.getRenderingHint(RenderingHints.KEY_ANTIALIASING);
    g2d.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        isAntialias() ? RenderingHints.VALUE_ANTIALIAS_ON :
                        RenderingHints.VALUE_ANTIALIAS_OFF
    );

    symbol.draw(g, r, si.getFgColor().getColor(), si.getBgColor().getColor(), si.getSizeColor().getColor(), (float) lineWidth);

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, aa);
  }

  @Override
  public String getType() {
    return TYPE;
  }

  @Override
  public String getDisplayName() {
    return Resources.getString("Editor.SymbolItem.component_type");
  }

  @Override
  public Dimension getSize() {
    return new Dimension(getWidth(), getHeight());
  }

  public static Item decode(GamePieceLayout l, String s) {

    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');

    final SymbolItem item = new SymbolItem(l);

    sd.nextToken();
    item.width = sd.nextInt(54);
    item.height = sd.nextInt(54);
    item.lineWidth = sd.nextDouble(1.0f);

    return item;
  }

  @Override
  public String encode() {

    final SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');

    se1.append(width);
    se1.append(height);
    se1.append(lineWidth);

    final SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());

    return se2.getValue();
  }
}
