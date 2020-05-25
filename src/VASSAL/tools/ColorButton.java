/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.geom.AffineTransform;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.UIManager;

import VASSAL.tools.swing.SwingUtils;

/**
 * A {@link JButton} which displays a color swatch.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ColorButton extends JButton {
  private static final long serialVersionUID = 1L;
  private static final Font FONT = new Font("Dialog", 0, 10);

  private Color color;

  /**
   * Create a button with no color.
   */
  public ColorButton() {
    this(null);
  }

 /**
  * Create a button with the specified color.
  *
  * @param c the color to set
  */
  public ColorButton(Color c) {
    super();
    color = c;
    setIcon(new SwatchIcon(30,15));
    setMargin(new Insets(2,2,2,2));
  }

  /**
   * Set the color of the button.
   *
   * @param c the color to set
   */
  public void setColor(Color c) {
    color = c;
  }

  private class SwatchIcon implements Icon {
    private final int swatchWidth;
    private final int swatchHeight;

    public SwatchIcon(int width, int height) {
      swatchWidth = width;
      swatchHeight = height;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
      final Graphics2D g2d = (Graphics2D) g;
      final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final AffineTransform orig_t = g2d.getTransform();
      g2d.setTransform(SwingUtils.descaleTransform(orig_t));

      x *= os_scale;
      y *= os_scale;
      final int w = (int)(swatchWidth * os_scale);
      final int h = (int)(swatchHeight * os_scale);

      g.setColor(Color.black);
      g.drawRect(x, y, w-1, h-1);

      if (color != null) {
        g.setColor(color);
        g.fillRect(x+1, y+1, w-2, h-2);
      }
      else {
        // paint no color and a "nil" if the color is null
        g2d.addRenderingHints(SwingUtils.FONT_HINTS);
        g.setColor(UIManager.getColor("controlText"));
        final Font font = FONT.deriveFont((float)(FONT.getSize() * os_scale));
        g.setFont(font);
        g.drawString("nil",
          x+(w - g.getFontMetrics(font).stringWidth("nil"))/ 2,
          y+(h + g.getFontMetrics(font).getAscent())/2);
      }

      g2d.setTransform(orig_t);
    }

    @Override
    public int getIconWidth() {
      return swatchWidth;
    }

    @Override
    public int getIconHeight() {
      return swatchHeight;
    }
  }
}
