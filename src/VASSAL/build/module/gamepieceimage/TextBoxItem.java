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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;

import javax.swing.JTextPane;

import VASSAL.configure.TextConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.ArrayUtils;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;

public class TextBoxItem extends TextItem {

  public static final String TYPE = "TextBox"; //$NON-NLS-1$

  protected static final String WIDTH = "width"; //$NON-NLS-1$
  protected static final String HEIGHT = "height"; //$NON-NLS-1$
  protected static final String USE_HTML = "use_html"; //$NON-NLS-1$

  protected int height = 30;
  protected int width = 40;
  protected boolean isHTML = false;

  public TextBoxItem() {
    super();
  }

  public TextBoxItem(GamePieceLayout l) {
    super(l);
  }

  public TextBoxItem(GamePieceLayout l, String n) {
    this(l);
    setConfigureName(n);
  }

  public String[] getAttributeDescriptions() {
    return ArrayUtils.insert(
      super.getAttributeDescriptions(), 2,
      "Width:  ",
      "Height:  ",
      "Use HTML:  "
    );
  }

  public Class<?>[] getAttributeTypes() {
    final Class<?> c[] = ArrayUtils.insert(
      super.getAttributeTypes(), 2,
      new Class<?>[] {
        Integer.class,
        Integer.class,
        Boolean.class
      }
    );

    final String[] names = getAttributeNames();
    // Change the type of the "Text" attribute to multi-line text
    for (int i = 0; i < names.length; i++) {
      if (TEXT.equals(names[i])) {
        c[i] = TextConfigurer.class;
        break;
      }
    }
    return c;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (FONT.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !isHTML;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public String[] getAttributeNames() {
    return ArrayUtils.insert(
      super.getAttributeNames(), 2,
      WIDTH,
      HEIGHT,
      USE_HTML
    );
  }

  public void setAttribute(String key, Object o) {
    if (WIDTH.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      width = ((Integer) o).intValue();
      if (width < 1) width = 1;
    }
    else if (HEIGHT.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      height = ((Integer) o).intValue();
      if (height < 1) height = 1;
    }
    else if (USE_HTML.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String)o);
      }
      isHTML = Boolean.TRUE.equals(o);
    }
    else {
      super.setAttribute(key, o);
    }

    if (layout != null) {
      layout.refresh();
    }

  }

  public String getAttributeValueString(String key) {

    if (WIDTH.equals(key)) {
      return String.valueOf(width);
    }
    else if (HEIGHT.equals(key)) {
      return String.valueOf(height);
    }
    else if (USE_HTML.equals(key)) {
      return String.valueOf(isHTML);
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

  public void draw(Graphics g, GamePieceImage defn) {

    TextBoxItemInstance tbi = null;
    if (defn != null) {
      tbi = defn.getTextBoxInstance(getConfigureName());
    }
    if (tbi == null) {
      tbi = new TextBoxItemInstance();
    }

    Color fg = tbi.getFgColor().getColor();
    Color bg = tbi.getBgColor().getColor();

    Point origin = layout.getPosition(this);
    Rectangle r = new Rectangle(origin.x, origin.y, getWidth(), getHeight());
    String s = null;
    if (textSource.equals(SRC_FIXED)) {
      s = text;
    }
    else {
      if (defn != null) {
        if (tbi != null) {
          s = tbi.getValue();
        }
      }
    }

    Graphics2D g2d = ((Graphics2D) g);
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, isAntialias() ?
      RenderingHints.VALUE_ANTIALIAS_ON : RenderingHints.VALUE_ANTIALIAS_OFF);

    AffineTransform saveXForm = null;
    if (getRotation() != 0) {
      saveXForm = g2d.getTransform();
      AffineTransform newXForm =
        AffineTransform.getRotateInstance(Math.toRadians(getRotation()), getLayout().getVisualizerWidth()/2, getLayout().getVisualizerHeight()/2);
      g2d.transform(newXForm);
    }

    if (bg != null) {
      g.setColor(bg);
      g.fillRect(r.x, r.y, r.width, r.height);
    }

    JTextPane l = new JTextPane();
    if (isHTML) l.setContentType("text/html"); //$NON-NLS-1$
    l.setText(s);
    l.setSize(width-2, height-2);
    l.setBackground(bg != null ? bg : new Color(0,true));
    l.setForeground(fg != null ? fg : new Color(0,true));
    FontStyle fs = FontManager.getFontManager().getFontStyle(fontStyleName);
    Font f = fs.getFont();
    l.setFont(f);

    final BufferedImage img = ImageUtils.createCompatibleTranslucentImage(
      Math.max(l.getWidth(), 1),
      Math.max(l.getHeight(), 1)
    );
    final Graphics2D big = img.createGraphics();
    l.paint(big);
    big.dispose();

    g2d.drawImage(img, origin.x+1, origin.y+1, null);

    if (saveXForm != null) {
      g2d.setTransform(saveXForm);
    }
  }

  public String getType() {
    return TextBoxItem.TYPE;
  }

  public String getDisplayName() {
    return "Text Box";
  }

  public Dimension getSize() {
    return new Dimension(getWidth(),getHeight());
  }


  public static Item decode(GamePieceLayout l, String s) {

    TextBoxItem item = new TextBoxItem(l);

    SequenceEncoder.Decoder sd1 = new SequenceEncoder.Decoder(s, ',');
    String s1 = sd1.nextToken(""); //$NON-NLS-1$

    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s1, ';');
    sd.nextToken();
    item.width = sd.nextInt(30);
    item.height = sd.nextInt(40);
    item.isHTML = sd.nextBoolean(false);

    TextItem.decode(item, sd1.nextToken("")); //$NON-NLS-1$

    return item;
  }

  public String encode() {

    SequenceEncoder se1 = new SequenceEncoder(TextBoxItem.TYPE, ';');

    se1.append(width);
    se1.append(height);
    se1.append(isHTML);

    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), ',');
    se2.append(super.encode());

    return se2.getValue();
  }


}
