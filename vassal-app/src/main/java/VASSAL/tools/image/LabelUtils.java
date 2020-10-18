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

import VASSAL.tools.QuickColors;
import VASSAL.tools.swing.SwingUtils;

import javax.swing.JLabel;

public class LabelUtils {
  private LabelUtils() {

  }

  public static final int CENTER = 0;
  public static final int RIGHT = 1;
  public static final int LEFT = 2;
  public static final int TOP = 3;
  public static final int BOTTOM = 4;

  /**
   * Draw a non-HTML text label with appropriate alignment & foreground/background color
   * @param g Graphics Object
   * @param text text to draw
   * @param x x location
   * @param y y location
   * @param hAlign Horizontal alignment (LEFT, RIGHT, or CENTER)
   * @param vAlign Vertical alignment (TOP, BOTTOM, or CENTER)
   * @param fgColor Foreground Color
   * @param bgColor Background Color
   */
  public static void drawLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor) {
    drawLabel(g, text, x, y, new Font(Font.DIALOG, Font.PLAIN, 10), hAlign, vAlign, fgColor, bgColor, null);
  }

  /**
   * Draw a non-HTML text label with appropriate alignment & foreground/background color, plus a border box
   * @param g Graphics Object
   * @param text text to draw
   * @param x x location
   * @param y y location
   * @param hAlign Horizontal alignment (LEFT, RIGHT, or CENTER)
   * @param vAlign Vertical alignment (TOP, BOTTOM, or CENTER)
   * @param fgColor Foreground Color
   * @param bgColor Background Color
   * @param borderColor Box color around border
   */
  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor) {
    drawLabel(g, text, x, y, f, hAlign, vAlign, fgColor, bgColor, borderColor, 0, 0, 0, 0);
  }

  /**
   * Draw a non-HTML text label with appropriate alignment & foreground/background color, plus a border box, and extra configuration parameters
   * @param g Graphics Object
   * @param text text to draw
   * @param x x location
   * @param y y location
   * @param hAlign Horizontal alignment (LEFT, RIGHT, or CENTER)
   * @param vAlign Vertical alignment (TOP, BOTTOM, or CENTER)
   * @param fgColor Foreground Color
   * @param bgColor Background Color
   * @param borderColor Box color around border
   * @param objectWidth 0 for default, or width of an optional "master object" inside of which the label is being drawn (allows better alignment options)
   * @param textPad 0 for default, or extra padding around text in all 4 directions
   * @param minWidth 0 for default, or minimum width of text box
   * @param extraBorder 0 for default, or number of pixels of extra thickness of border box
   */
  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, int objectWidth, int textPad, int minWidth, int extraBorder) {
    ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                      RenderingHints.VALUE_ANTIALIAS_ON);

    g.setFont(f);
    int width = g.getFontMetrics().stringWidth(text + "  ") + textPad*2 + extraBorder;
    final int height = g.getFontMetrics().getHeight() + textPad*2 + extraBorder*2;

    int width2 = Math.max(width, minWidth + extraBorder);

    x -= extraBorder;
    y -= extraBorder;

    int x0 = x;
    int y0 = y;
    int xBox;

    // If objectWidth is 0 (default), then x is the position for the text box (subject to alignment choice)
    // If objectWidth is > 0, then x is the left side of a master object with a precise width we are to draw within.
    if (objectWidth <= 0) {
      switch (hAlign) {
      case CENTER:
        x0 = x - width / 2;
        break;
      case LEFT:
        x0 = x - width;
        break;
      case RIGHT:
        x0 = x;
        break;
      }
      xBox = x0;
    }
    else {
      switch (hAlign) {
      case CENTER:
        x0 = x + objectWidth/2 - width / 2;
        break;
      case LEFT:
        x0 = x + objectWidth;
        break;
      case RIGHT:
        x0 = x;
        break;
      }
      xBox = ((minWidth > 0) && (width2 > width)) ? x : x0;
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
      g.fillRect(xBox, y0, width2, height);
    }

    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(xBox, y0, width2, height);

      if (extraBorder > 0) {
        Dimension size4 = new Dimension(width2, height);
        int x1 = xBox;
        int y1 = y0;
        for (int extra = 0; extra < extraBorder; extra++) {
          x1 += 1;
          y1 += 1;
          size4.width -= 2;
          size4.height -= 2;
          g.drawRect(x1, y1, size4.width, size4.height);
        }
      }
    }

    g.setColor(fgColor);
    g.drawString(" " + text + " ", x0 + textPad + extraBorder,
      y0 + textPad + extraBorder + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent());
  }


  /**
   * Draw an HTML-compliant text label with appropriate alignment & foreground/background color, plus a border box, and extra configuration parameters.
   * Supports "Quick Colors".
   * @param g Graphics Object
   * @param text text to draw
   * @param x x location
   * @param y y location
   * @param hAlign Horizontal alignment (LEFT, RIGHT, or CENTER)
   * @param vAlign Vertical alignment (TOP, BOTTOM, or CENTER)
   * @param fgColor Foreground Color
   * @param bgColor Background Color
   * @param borderColor Box color around border
   * @param comp Component we are drawing on
   * @param objectWidth 0 for default, or width of an optional "master object" inside of which the label is being drawn (allows better alignment options)
   * @param textPad 0 for default, or extra padding around text in all 4 directions
   * @param minWidth 0 for default, or minimum width of text box
   * @param extraBorder 0 for default, or number of pixels of extra thickness of border box
   */
  public static void drawHTMLLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, Component comp, int objectWidth, int textPad, int minWidth, int extraBorder) {
    ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    // "Quick Colors"
    String style = (QuickColors.getQuickColor(text) >= 0) ? QuickColors.getQuickColorHTMLStyle(text) + "color" : ""; //NON-NLS
    String baseString = QuickColors.stripQuickColorTag(text);

    // If user already put <html> tags in, don't re-wrap.
    boolean addTags = (text.length() <= 6) || !("<html>".equalsIgnoreCase(text.substring(0, 6))); //NON-NLS

    // HTML Niceties - Rather than make the user type a bunch of repetitive stuff, by default we wrap these up nicely.
    String htmlString = (addTags ? "<html>" + (!style.isEmpty() ? "<div class=" + style + ">" : "<div>") + "&nbsp;" : "") + baseString + (addTags ? "&nbsp;</div></html>" : ""); //NON-NLS

    // Chapter 3, in which Winnie the Pooh kidnaps a JLabel and makes it rob banks...
    JLabel j = new JLabel(htmlString);
    j.setForeground(fgColor);
    j.setFont(f);
    Dimension size = j.getPreferredSize();
    j.setSize(size);

    x -= extraBorder;
    y -= extraBorder;

    // Dimensions including extra text padding and extra border.
    Dimension size2 = new Dimension();
    size2.width  = size.width + textPad*2 + extraBorder;
    size2.height = size.height + textPad*2 + extraBorder*2;

    // Dimensions also including any forced-stretch of width. This will be the outer bounds of the box we draw.
    Dimension size3 = new Dimension();
    size3.width   = Math.max(size2.width, minWidth + extraBorder);
    size3.height  = size2.height;

    g.setFont(f);
    int x0 = x;
    int y0 = y;
    int xBox;

    // If objectWidth is 0 (default), then x is the position for the text box (subject to alignment choice)
    // If objectWidth is > 0, then x is the left side of a master object with a precise width we are to draw within.
    if (objectWidth <= 0) {
      switch (hAlign) {
      case CENTER:
        x0 = x - size2.width / 2;
        break;
      case LEFT:
        x0 = x - size2.width;
        break;
      case RIGHT:
        x0 = x;
        break;
      }
      xBox = x0;
    }
    else {
      switch (hAlign) {
      case CENTER:
        x0 = x + objectWidth/2 - size2.width / 2;
        break;
      case LEFT:
        x0 = x + objectWidth - size2.width;
        break;
      case RIGHT:
        x0 = x;
        break;
      }
      xBox = ((minWidth > 0) && (size3.width > size2.width)) ? x : x0;
    }

    switch (vAlign) {
    case CENTER:
      y0 = y - size2.height / 2;
      break;
    case BOTTOM:
      y0 = y - size2.height;
      break;
    }

    // Draws our background color
    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(xBox, y0, size3.width, size3.height);
    }

    // Draws our border
    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(xBox, y0, size3.width, size3.height);

      if (extraBorder > 0) {
        Dimension size4 = new Dimension(size3);
        int x1 = xBox;
        int y1 = y0;
        for (int extra = 0; extra < extraBorder; extra++) {
          x1 += 1;
          y1 += 1;
          size4.width -= 2;
          size4.height -= 2;
          g.drawRect(x1, y1, size4.width, size4.height);
        }
      }
    }

    g.setColor(fgColor);

    final BufferedImage im = ImageUtils.createCompatibleImage(
      size.width,
      size.height,
      true
    );

    final Graphics2D gTemp = im.createGraphics();
    gTemp.addRenderingHints(SwingUtils.FONT_HINTS);
    gTemp.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    j.paint(gTemp);

    // If no extra padding or border was specified, we can draw the label directly. Otherwise we need an extra
    // layer of indirection "lest our JLabel wriggle from our grasp"
    if ((textPad <= 0) && (extraBorder <= 0)) {
      g.drawImage(im, x0, y0, comp);
    }
    else {
      final BufferedImage im2 = ImageUtils.createCompatibleImage(
        size3.width,
        size3.height,
        true
      );
      final Graphics2D gTemp2 = im2.createGraphics();
      gTemp2.addRenderingHints(SwingUtils.FONT_HINTS);
      gTemp2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON);

      gTemp2.drawImage(im, textPad + extraBorder, textPad + extraBorder, null);
      g.drawImage(im2, x0, y0, comp);
    }
  }
}
