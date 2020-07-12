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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class ShapeItem extends Item {

  public static final String TYPE = "Box"; //$NON-NLS-1$

  protected static final String WIDTH = "width"; //$NON-NLS-1$
  protected static final String HEIGHT = "height"; //$NON-NLS-1$
  protected static final String SHAPE = "shape"; //$NON-NLS-1$
  protected static final String BEVEL = "bevel"; //$NON-NLS-1$

  protected static final String RECT = "Rectangle";
  protected static final String RRECT = "Rounded Rectangle";
  protected static final String OVAL = "Oval";

  protected int height = 30;
  protected int width = 40;
  protected int bevel = 5;
  protected String shape = RECT;


  public ShapeItem() {
    super();
  }

  public ShapeItem(GamePieceLayout l) {
    super(l);
  }

  public ShapeItem(GamePieceLayout l, String n) {
    this(l);
    setConfigureName(n);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.insert(
      2, super.getAttributeDescriptions(),
      "Width:  ",
      "Height:  ",
      "Shape:  ",
      "Bevel:  "
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.insert(
      2, super.getAttributeTypes(),
      new Class<?>[] {
        Integer.class,
        Integer.class,
        ShapeConfig.class,
        Integer.class
      }
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.insert(
      2, super.getAttributeNames(),
      WIDTH,
      HEIGHT,
      SHAPE,
      BEVEL
    );
  }

  public static class ShapeConfig extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { RECT, RRECT, OVAL };
    }
  }
  @Override
  public void setAttribute(String key, Object o) {
    if (WIDTH.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      width = (Integer) o;
      if (width < 1) width = 1;
    }
    else if (HEIGHT.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      height = (Integer) o;
      if (height < 1) height = 1;
    }
    else if (SHAPE.equals(key)) {
      shape = (String) o;
    }
    else if (BEVEL.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      bevel = (Integer) o;
      if (bevel < 0) bevel = 0;
    }
    else {
      super.setAttribute(key, o);
    }

    if (layout != null) {
      layout.refresh();
    }

  }

  @Override
  public String getAttributeValueString(String key) {

    if (WIDTH.equals(key)) {
      return String.valueOf(width);
    }
    else if (HEIGHT.equals(key)) {
      return String.valueOf(height);
    }
    else if (SHAPE.equals(key)) {
      return shape;
    }
    else if (BEVEL.equals(key)) {
      return String.valueOf(bevel);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ROTATION.equals(name)) {
      return falseCond;
    }
    else if (BEVEL.equals(name)) {
      return bevelCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition falseCond = new VisibilityCondition() {
    @Override
    public boolean shouldBeVisible() {
      return false;
    }
  };

  private VisibilityCondition bevelCond = new VisibilityCondition() {
    @Override
    public boolean shouldBeVisible() {
      return shape.equals(RRECT);
    }
  };

  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }

  @Override
  public void draw(Graphics g, GamePieceImage defn) {

    ShapeItemInstance si = null;
    if (defn != null) {
      si = defn.getShapeInstance(getConfigureName());
    }
    if (si == null) {
      si = new ShapeItemInstance();
    }

    Color fg = si.getFgColor().getColor();
    Color bg = si.getBorderColor().getColor();

    Point origin = layout.getPosition(this);
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());

    if (isAntialias()) {
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
    }
    else {
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF);
    }

    if (fg != null) {
      g.setColor(fg);
      if (shape.equals(RECT)) {
        g.fillRect(r.x, r.y, r.width, r.height);
      }
      else if (shape.equals(RRECT)) {
        g.fillRoundRect(r.x, r.y, r.width, r.height, bevel*2, bevel*2);
      }
      else if (shape.equals(OVAL)) {
        g.fillOval(r.x, r.y, r.width, r.height);
      }
    }

    if (bg != null) {
      g.setColor(bg);
      if (shape.equals(RECT)) {
        g.drawRect(r.x, r.y, r.width, r.height);
      }
      else if (shape.equals(RRECT)) {
        g.drawRoundRect(r.x, r.y, r.width, r.height, bevel*2, bevel*2);
      }
      else if (shape.equals(OVAL)) {
        g.drawOval(r.x, r.y, r.width, r.height);
      }
    }
  }

  @Override
  public String getType() {
    return TYPE;
  }

  @Override
  public String getDisplayName() {
    return "Shape";
  }

  @Override
  public Dimension getSize() {
    return new Dimension(getWidth(),getHeight());
  }


  public static Item decode(GamePieceLayout l, String s) {

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');

    ShapeItem item = new ShapeItem(l);

    sd.nextToken();
    item.width = sd.nextInt(30);
    item.height = sd.nextInt(40);
    item.shape = sd.nextToken(RECT);
    item.bevel = sd.nextInt(5);

    return item;
  }

  @Override
  public String encode() {

    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');

    se1.append(width);
    se1.append(height);
    se1.append(shape);
    se1.append(bevel);

    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());

    return se2.getValue();
  }


}
