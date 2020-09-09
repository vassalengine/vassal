/*
 * Copyright (c) 2000-2020 by Rodney Kinney, Joel Uckelman, Brent Easton
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

package VASSAL.tools.image;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import VASSAL.tools.swing.SwingUtils;

import javax.swing.JLabel;
import javax.swing.JPanel;

public class LabelUtils {
  private LabelUtils() {

  }

  public static final int CENTER = 0;
  public static final int RIGHT = 1;
  public static final int LEFT = 2;
  public static final int TOP = 3;
  public static final int BOTTOM = 4;

  public static void drawLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor) {
    drawLabel(g, text, x, y, new Font("Dialog", Font.PLAIN, 10), hAlign, vAlign, fgColor, bgColor, null);
  }

  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor) {
    ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                      RenderingHints.VALUE_ANTIALIAS_ON);

    g.setFont(f);
    final int width = g.getFontMetrics().stringWidth(text + "  ");
    final int height = g.getFontMetrics().getHeight();
    int x0 = x;
    int y0 = y;

    switch (hAlign) {
    case CENTER:
      x0 = x - width / 2;
      break;
    case LEFT:
      x0 = x - width;
      break;
    }

    switch (vAlign) {
    case CENTER:
      y0 = y - height / 2;
      break;
    case BOTTOM:
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

    g.setColor(fgColor);
    g.drawString(" " + text + " ", x0,
      y0 + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent());
  }


  public static void drawHTMLLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor, Component comp) {
    drawHTMLLabel(g, text, x, y, new Font("Dialog", Font.PLAIN, 10), hAlign, vAlign, fgColor, bgColor, null, comp);
  }


  public static void drawHTMLLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, Component comp) {
    ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    g.setFont(f);
    final int width = g.getFontMetrics().stringWidth(text + "  ");
    final int height = g.getFontMetrics().getHeight();
    int x0 = x;
    int y0 = y;

    switch (hAlign) {
    case CENTER:
      x0 = x - width / 2;
      break;
    case LEFT:
      x0 = x - width;
      break;
    }

    switch (vAlign) {
    case CENTER:
      y0 = y - height / 2;
      break;
    case BOTTOM:
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

    g.setColor(fgColor);

    // prepare the target image
    final BufferedImage im = ImageUtils.createCompatibleImage(
      width,
      height,
      bgColor == null || bgColor.getTransparency() != Color.OPAQUE
    );

    final Graphics2D gTemp = im.createGraphics();
    gTemp.addRenderingHints(SwingUtils.FONT_HINTS);
    gTemp.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    JLabel j = new JLabel(" " + text + " ");
    j.setForeground(fgColor);
    j.setFont(f);
    Dimension size = j.getPreferredSize();
    j.setSize(size);

    j.paint(gTemp);

    g.drawImage(im, x0, y0, comp);
    //g.drawString(" " + text + " ", x0,
    //  y0 + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent());
  }

}
