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

import VASSAL.i18n.Resources;
import VASSAL.tools.QuickColors;
import VASSAL.tools.swing.SwingUtils;

import javax.swing.JLabel;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

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
   *
   *
   */
  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, int objectWidth, int textPad, int minWidth, int extraBorder) {
    ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                      RenderingHints.VALUE_ANTIALIAS_ON);

    g.setFont(f);
    final int width = g.getFontMetrics().stringWidth(text + "  ") + textPad*2 + extraBorder;
    final int height = g.getFontMetrics().getHeight() + textPad*2 + extraBorder*2;

    final int width2 = Math.max(width, minWidth + extraBorder);

    x -= extraBorder;
    y -= extraBorder;

    int x0 = x;
    int y0 = y;
    final int xBox;

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
        final Dimension size4 = new Dimension(width2, height);
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


  public static void drawHTMLLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, Component comp, int objectWidth, int textPad, int minWidth, int extraBorder) {
    drawHTMLLabel(g, text, x, y, f, hAlign, vAlign, fgColor, bgColor, borderColor, comp, objectWidth, textPad, minWidth, extraBorder, extraBorder, extraBorder);
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
  public static void drawHTMLLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor, Component comp, int objectWidth, int textPad, int minWidth, int extraBorder, int extraTop, int extraBottom) {
    ((Graphics2D) g).addRenderingHints(SwingUtils.FONT_HINTS);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    // "Quick Colors"
    final String style = (QuickColors.getQuickColor(text) >= 0) ? QuickColors.getQuickColorHTMLStyle(text) + "color" : ""; //NON-NLS
    final String baseString = QuickColors.stripQuickColorTag(text);

    // If user already put <html> tags in, don't re-wrap.
    final boolean addTags = (text.length() <= 6) || !("<html>".equalsIgnoreCase(text.substring(0, 6))); //NON-NLS

    // HTML Niceties - Rather than make the user type a bunch of repetitive stuff, by default we wrap these up nicely.
    final String htmlString = (addTags ? "<html>" + (!style.isEmpty() ? "<div class=" + style + ">" : "<div>") + "&nbsp;" : "") + baseString + (addTags ? "&nbsp;</div></html>" : ""); //NON-NLS

    // Chapter 3, in which Winnie the Pooh kidnaps a JLabel and makes it rob banks...
    final JLabel j = new JLabel(htmlString);
    j.setForeground(fgColor);
    j.setFont(f);

    final Dimension size = j.getPreferredSize();
    if (size.width <= 0 || size.height <= 0) {
      // If the label renders to nothing, bail out early.
      return;
    }

    j.setSize(size);

    if ((extraBorder < 0) || (extraTop < 0) || (extraBottom < 0)) {
      extraBorder = extraTop = extraBottom = 0;
      borderColor = null;
    }

    // Dimensions including extra text padding and extra border.
    final Dimension size2 = new Dimension();
    size2.width  = size.width + textPad*2 + extraBorder*2;
    size2.height = size.height + textPad*2 + extraTop + extraBottom;

    // Dimensions also including any forced-stretch of width. This will be the outer bounds of the box we draw.
    final Dimension size3 = new Dimension();
    size3.width   = Math.max(size2.width, minWidth);
    size3.height  = size2.height;

    g.setFont(f);
    int x0 = x;
    int y0 = y;
    final int xBox;

    // If objectWidth is 0 (default), then x is the position for the text box (subject to alignment choice)
    // If objectWidth is > 0, then x is the left side of a master object with a precise width we are to draw within.
    if (objectWidth <= 0) {
      switch (hAlign) {
      case CENTER:
        x0 = x0 - size2.width / 2;
        break;
      case LEFT:
        x0 = x0 - size2.width;
        break;
      case RIGHT:
        break;
      }
      xBox = x0 - extraBorder;
    }
    else {
      switch (hAlign) {
      case CENTER:
        x0 = x0 + objectWidth/2 - size2.width / 2;
        break;
      case LEFT:
        x0 = x0 + objectWidth - size2.width;
        break;
      case RIGHT:
        break;
      }
      xBox = (((minWidth > 0) && (size3.width > size2.width)) ? x - extraBorder : x0);
    }

    switch (vAlign) {
    case CENTER:
      y0 = y0 - size2.height / 2;
      break;
    case BOTTOM:
      y0 = y0 - size2.height;
      break;
    }

    final int yBox = y0 - extraTop;

    // Draws our background color
    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(xBox, yBox, size3.width, size3.height);
    }

    // Draws our border
    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(xBox, yBox, size3.width - 1, size3.height - 1); // The basic single box for 0 extra height/width

      if ((extraBorder > 0) || (extraTop > 0) || (extraBottom > 0)) {
        int x1 = xBox;
        int y1 = yBox;

        if (extraBorder > 0) {
          int width = size3.width;
          for (int extra = 0; extra < extraBorder; extra++) {
            x1 += 1;
            width -= 2;
            g.drawRect(x1, y1, width - 1, size3.height - 1);
          }
        }

        if (extraTop > 0) {
          for (int extra = 0; extra < extraTop; extra++) {
            y1 += 1;
            g.drawRect(xBox, y1, size3.width - 1, 0);
          }
        }

        if (extraBottom > 0) {
          y1 = yBox + size3.height - 1;
          for (int extra = 0; extra < extraBottom; extra++) {
            y1 -= 1;
            g.drawRect(xBox, y1, size3.width - 1, 0);
          }
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

    // If no extra padding or border was specified, we can draw the label
    // directly. Otherwise we need an extra layer of indirection "lest our
    // JLabel wriggle from our grasp"
    //if ((textPad <= 0) && (extraBorder <= 0) && (extraTop <= 0)) {
    //  g.drawImage(im, x0, y0, comp);
    //}
    //else {
    final BufferedImage im2 = ImageUtils.createCompatibleImage(
      size3.width,
      size3.height,
      true
    );
    final Graphics2D gTemp2 = im2.createGraphics();
    gTemp2.addRenderingHints(SwingUtils.FONT_HINTS);
    gTemp2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    gTemp2.drawImage(im, textPad + extraBorder, textPad, null);
    g.drawImage(im2, x0, y0, comp);
    //}
  }

  public static int labelWidth(Font font, String s) {
    final Graphics2D g = (Graphics2D) ImageUtils.NULL_IMAGE.getGraphics();
    g.addRenderingHints(SwingUtils.FONT_HINTS);
    g.setFont(font);
    final int stringWidth = g.getFontMetrics().stringWidth(s);
    g.dispose();
    return stringWidth;
  }

  public static void drawLabelBox(Graphics2D g, Font font, String s, int imageWidth, int stringWidth, int height) {
    g.addRenderingHints(SwingUtils.FONT_HINTS);
    g.setFont(font);
    g.setColor(Color.WHITE);
    g.fillRect(0, 0, imageWidth - 1, height - 1);
    g.setColor(Color.BLACK);
    g.drawRect(0, 0, imageWidth - 1, height - 1);
    g.drawString(s, (imageWidth - stringWidth) / 2 - 1, height / 2 + 4);
  }

  public static BufferedImage labelBoxImage(Font font, String s, int minWidth, int height) {
    final int stringWidth = labelWidth(font, s);
    final int imageWidth = Math.max(minWidth, stringWidth + 20);

    // Create a new image large enough to hold the string comfortably
    final BufferedImage image = ImageUtils.createCompatibleImage(imageWidth, height);
    final Graphics2D g = (Graphics2D) image.getGraphics();
    drawLabelBox(g, font, s, imageWidth, stringWidth, height);
    g.dispose();

    return image;
  }

  /**
   * Create a viewable representation of a null or empty image to
   * use as a place holder in Configurers.
   * *
   * The image will contain the translated text for the key
   * Editor.ImageUtils.no_image
   *
   * @param w Minimum width for generated the image
   * @param h Height of the generated image
   * @param scale Scale factor for font
   * @return Viewable null image
   */
  public static BufferedImage noImageBoxImage(int w, int h, double scale) {
    return LabelUtils.labelBoxImage(
      new Font(Font.DIALOG, Font.ITALIC, 12).deriveFont((float)(12 * scale)),
      Resources.getString("Editor.ImageUtils.no_image"),
      w,
      h
    );
  }

 /**
  * Create a viewable representation of a null or empty image to
  * use as a place holder in Configurers.
  *
  * The image will contain the translated text for the key
  * Editor.ImageUtils.no_image
  *
  * @return Viewable null image
  */
  public static BufferedImage noImageBoxImage() {
    return noImageBoxImage(64, 64, 1.0);
  }
}
