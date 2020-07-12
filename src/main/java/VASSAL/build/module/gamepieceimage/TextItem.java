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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;

import javax.swing.KeyStroke;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class TextItem extends Item {

  public static final String TYPE = "Text"; //$NON-NLS-1$

  protected static final String FONT = "font"; //$NON-NLS-1$
  protected static final String SOURCE = "source"; //$NON-NLS-1$
  protected static final String TEXT = "text"; //$NON-NLS-1$

  protected static final String LEFT = "left"; //$NON-NLS-1$
  protected static final String CENTER = "center"; //$NON-NLS-1$
  protected static final String RIGHT = "right"; //$NON-NLS-1$
  protected static final String TOP = "top"; //$NON-NLS-1$
  protected static final String BOTTOM = "bottom"; //$NON-NLS-1$

  public static final String SRC_VARIABLE = "Specified in individual images"; //$NON-NLS-1$
  public static final String SRC_FIXED = "Fixed for this layout"; //$NON-NLS-1$

  protected static final String PIECE_NAME = "pieceName"; //$NON-NLS-1$
  protected static final String LABEL = "label"; //$NON-NLS-1$
  protected static final String DEFAULT_FORMAT = "$"+PIECE_NAME+"$"; //$NON-NLS-1$ //$NON-NLS-2$

  public static final int AL_CENTER = 0;
  public static final int AL_RIGHT = 1;
  public static final int AL_LEFT = 2;
  public static final int AL_TOP = 3;
  public static final int AL_BOTTOM = 4;

  protected String fontStyleName = "Default"; //$NON-NLS-1$
  protected String textSource = SRC_VARIABLE;
  protected String text = ""; //$NON-NLS-1$

  protected String changeCmd = ""; //$NON-NLS-1$
  protected KeyStroke changeKey;
  protected boolean lockable = false;
  protected String lockCmd = ""; //$NON-NLS-1$
  protected KeyStroke lockKey;

  public TextItem() {
    super();
  }

  public TextItem(GamePieceLayout l) {
    super(l);
  }

  public TextItem(GamePieceLayout l, String nam) {
    this(l);
    setConfigureName(nam);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.insert(
      2, super.getAttributeDescriptions(),
      "Font style:  ",
      "Text is:  ",
      "Text:  "
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.insert(
      2, super.getAttributeTypes(),
      new Class<?>[] {
        FontStyleConfig.class,
        TextSource.class,
        String.class
      }
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.insert(
      2, super.getAttributeNames(),
      new String[] {
        FONT,
        SOURCE,
        TEXT
      }
    );
  }

  public static class FontStyleConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new StringEnumConfigurer(key, name, FontManager.getFontManager().getFontNames());
    }
  }

  @Override
  public void setAttribute(String key, Object o) {
    if (FONT.equals(key)) {
      fontStyleName = (String)o;
    }
    else if (SOURCE.equals(key)) {
      textSource = (String) o;
    }
    else if (TEXT.equals(key)) {
      text = (String) o;
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

    if (FONT.equals(key)) {
      return fontStyleName;
    }
    else if (SOURCE.equals(key)) {
      return textSource + ""; //$NON-NLS-1$
    }
    else if (TEXT.equals(key)) {
      return text;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (TEXT.equals(name)) {
      return fixedCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition fixedCond = new VisibilityCondition() {
    @Override
    public boolean shouldBeVisible() {
      return textSource.equals(SRC_FIXED);
    }
  };

  @Override
  public void draw(Graphics g, GamePieceImage defn) {

    TextItemInstance ti = null;

    FontStyle fs = FontManager.getFontManager().getFontStyle(fontStyleName);
    Font f = fs.getFont();

    if (defn != null) {
      ti = defn.getTextInstance(name);
    }
    else {
      defn = new GamePieceImage(getLayout());
      ti = defn.getTextInstance(name);
    }

    if (ti == null) {
      ti = new TextItemInstance();
    }

    Color fg = ti.getFgColor().getColor();
    Color bg = ti.getBgColor().getColor();
    if (fg == null && bg == null) {
      return;
    }

    boolean outline = ti.isOutline();
    Color ol = ti.getOutlineColor().getColor();

    String compass = GamePieceLayout.getCompassPoint(getLocation());
    int hAlign = AL_CENTER;
    switch (compass.charAt(compass.length()-1)) {
    case 'W':
      hAlign = AL_LEFT;
      break;
    case 'E':
      hAlign = AL_RIGHT;
    }
    int vAlign = AL_CENTER;
    switch (compass.charAt(0)) {
    case 'N':
      vAlign = AL_TOP;
      break;
    case 'S':
      vAlign = AL_BOTTOM;
    }

    Point origin = layout.getPosition(this);
    String s = null;
    if (textSource.equals(SRC_FIXED)) {
      s = text;
    }
    else {
      // TODO condition is always "true"
      if (defn != null) {
        // TODO condition is always true
        if (ti != null) {
          s = ti.getValue();
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

    drawLabel(g, s, origin.x, origin.y, f, hAlign, vAlign, fg, bg, null, outline, ol);
    if (saveXForm != null) {
      g2d.setTransform(saveXForm);
    }
  }

  @Override
  public String getType() {
    return TYPE;
  }

  @Override
  public String getDisplayName() {
    return "Label";
  }

  @Override
  public Dimension getSize() {
    return new Dimension(0,0);
  }

  public static Item decode(GamePieceLayout l, String s) {

    TextItem item = new TextItem(l);
    decode(item, s);
    return item;
  }

  public static void decode(TextItem item, String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');

    sd.nextToken();
    item.fontStyleName = sd.nextToken(FontManager.DEFAULT);
    if (item.fontStyleName.length() == 0) {
      item.fontStyleName = FontManager.DEFAULT;
    }
    item.textSource = sd.nextToken(SRC_VARIABLE);
    item.text = sd.nextToken(""); //$NON-NLS-1$
    item.changeCmd = sd.nextToken(""); //$NON-NLS-1$
    item.changeKey = sd.nextKeyStroke(null);
    item.lockCmd = sd.nextToken(""); //$NON-NLS-1$
    item.lockKey = sd.nextKeyStroke(null);
    item.lockable = sd.nextBoolean(false);

  }

  @Override
  public String encode() {

    SequenceEncoder se1 = new SequenceEncoder(TextItem.TYPE, ';');

    se1.append(fontStyleName == null ? "" : fontStyleName); //$NON-NLS-1$
    se1.append(textSource);
    se1.append(text);
    se1.append(changeCmd);
    se1.append(changeKey);
    se1.append(lockCmd);
    se1.append(lockKey);
    se1.append(lockable);

    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());

    return se2.getValue();
  }

  public boolean isOutline() {
    return FontManager.getFontManager().getFontStyle(fontStyleName).isOutline();
  }

  public boolean isFixed() {
    return textSource.equals(SRC_FIXED);
  }

  public static class TextSource extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { SRC_VARIABLE, SRC_FIXED };
    }
  }

  public static class NameFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{PIECE_NAME, LABEL});
    }
  }

  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, boolean outline, Color outlineColor) {
    g.setFont(f);
    int buffer = g.getFontMetrics().getLeading();
    int width = g.getFontMetrics().stringWidth(text)+2*buffer;
    int height = g.getFontMetrics().getHeight();
    int x0 = x;
    int y0 = y;
    switch (hAlign) {
    case AL_CENTER:
      x0 = x - width / 2;
      break;
    case AL_RIGHT:
      x0 = x - width;
      break;
    }
    switch (vAlign) {
    case AL_CENTER:
      y0 = y - height / 2;
      break;
    case AL_BOTTOM:
      y0 = y - height;
      break;
    }

    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(x0, y0, width, height);
    }
    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(x0, y0, width, height);
    }

    int y1 = y0 + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent();
    int x1 = x0 + buffer;
    if (outline && outlineColor != null) {
      g.setColor(outlineColor);
      g.drawString(text, x1-1, y1-1);
      g.drawString(text, x1-1, y1+1);
      g.drawString(text, x1+1, y1-1);
      g.drawString(text, x1+1, y1+1);
    }

    g.setColor(fgColor);
    g.drawString(text, x1, y1);

  }
}
