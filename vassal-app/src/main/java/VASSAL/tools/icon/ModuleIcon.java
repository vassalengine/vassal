/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

package VASSAL.tools.icon;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.AboutScreen;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageUtils;

/**
 * Draws a module's icon: the module's About image on a rounded tile or, for a
 * module without one, a generic tile of game pieces (cards, a counter, a pawn
 * and a die on a hex grid) in one of {@link #PALETTE}'s colours, chosen from
 * the module's name so that a module always gets the same colour. A badge
 * can be added in the bottom-right corner.
 *
 * <p>Used for the macOS Dock icon of a running module. Kept apart from that
 * so that other icons for a module, such as thumbnails stored in its files,
 * can be drawn the same way.</p>
 *
 * <p>The tile follows Apple's icon grid, leaving a transparent margin.</p>
 */
public final class ModuleIcon {
  private static final Logger logger = LoggerFactory.getLogger(ModuleIcon.class);

  /** The badge in the icon's bottom-right corner. */
  public enum Badge {
    /** no badge */
    NONE,
    /** the VASSAL logo */
    VASSAL
  }

  // Everything is drawn in a BASE px square and scaled to the size asked for.
  private static final int BASE = 512;
  private static final int SIZE = BASE;

  // Apple's icon grid: the tile is 824 of 1024 px, corner radius 185.
  private static final double TILE_INSET = SIZE * 100.0 / 1024.0;
  private static final double TILE = SIZE - 2 * TILE_INSET;
  private static final double TILE_RADIUS = TILE * 185.0 / 824.0;

  /**
   * Tile colours for modules without an About image. Mid-dark and well
   * separated so that white cards and dice, and the blue pawn, stand out
   * on every one.
   */
  static final Color[] PALETTE = {
    new Color(0x1E7A5A), // green
    new Color(0xA83A2E), // red
    new Color(0x3F4A57), // slate
    new Color(0x5F7224), // olive
    new Color(0xC0821C), // ochre
    new Color(0x3E3F9E), // indigo
    new Color(0x1F6E80), // teal
    new Color(0x6B3D7A), // purple
    new Color(0x7A4E2D), // brown
    new Color(0x7D2449), // burgundy
  };

  private ModuleIcon() {
  }

  /**
   * Draws the icon for a loaded module, from its About image (or a legacy
   * About Screen's image) if it has one, else from its name.
   *
   * @param module the module
   * @param size   width and height of the icon in pixels
   * @param badge  the corner badge
   * @return the icon
   */
  public static BufferedImage render(GameModule module, int size, Badge badge) {
    return render(module.getGameName(), AboutScreen.getModuleAboutImage(module), size, badge);
  }

  /**
   * @param moduleName the module's name, which picks the tile colour when
   *                   there is no About image
   * @param aboutImage the module's About image, or null
   * @param size       width and height of the icon in pixels
   * @param badge      the corner badge
   * @return a square icon with a transparent margin around the tile
   */
  public static BufferedImage render(String moduleName, BufferedImage aboutImage, int size, Badge badge) {
    final BufferedImage icon = new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB);
    final Graphics2D g = icon.createGraphics();
    try {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
      g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

      // all drawing is vector, so it scales cleanly to any size
      g.scale((double) size / BASE, (double) size / BASE);

      final Shape tile = new RoundRectangle2D.Double(
        TILE_INSET, TILE_INSET, TILE, TILE, 2 * TILE_RADIUS, 2 * TILE_RADIUS);

      drawShadow(g);

      final Shape oldClip = g.getClip();
      g.clip(tile);
      if (aboutImage != null) {
        drawCover(g, aboutImage);
      }
      else {
        drawTropes(g, colourFor(moduleName));
      }
      g.setClip(oldClip);

      g.setColor(new Color(0, 0, 0, 60));
      g.setStroke(new BasicStroke((float) (SIZE / 256.0)));
      g.draw(tile);

      if (badge == Badge.VASSAL) {
        drawBadge(g);
      }
    }
    finally {
      g.dispose();
    }
    return icon;
  }

  /**
   * @return the tile colour for a module name; the same name always gives
   * the same colour, on every system
   */
  static Color colourFor(String moduleName) {
    final String name = moduleName == null ? "" : moduleName.trim();
    // String.hashCode() is fixed by the Java spec, so this is stable
    return PALETTE[Math.floorMod(name.hashCode(), PALETTE.length)];
  }

  private static void drawShadow(Graphics2D g) {
    // a soft shadow under the tile, as other Dock icons have
    final double off = SIZE / 100.0;
    for (int i = 6; i >= 1; --i) {
      final double grow = i * SIZE / 256.0;
      g.setColor(new Color(0, 0, 0, 7));
      g.fill(new RoundRectangle2D.Double(
        TILE_INSET - grow, TILE_INSET - grow + off,
        TILE + 2 * grow, TILE + 2 * grow,
        2 * (TILE_RADIUS + grow), 2 * (TILE_RADIUS + grow)));
    }
  }

  /** Scales and crops the image to fill the tile, keeping its aspect ratio. */
  private static void drawCover(Graphics2D g, BufferedImage img) {
    final double scale = Math.max(TILE / img.getWidth(), TILE / img.getHeight());
    final double w = img.getWidth() * scale;
    final double h = img.getHeight() * scale;
    final double x = TILE_INSET + (TILE - w) / 2;
    final double y = TILE_INSET + (TILE - h) / 2;

    // draw on a white ground, for images with transparency
    g.setColor(Color.WHITE);
    g.fillRect(0, 0, SIZE, SIZE);
    g.drawImage(img, (int) Math.round(x), (int) Math.round(y),
      (int) Math.round(w), (int) Math.round(h), null);
  }

  //
  // The generic tile. Coordinates are fractions of the tile's size.
  //

  private static double tx(double f) {
    return TILE_INSET + f * TILE;
  }

  private static double ts(double f) {
    return f * TILE;
  }

  private static void drawTropes(Graphics2D g, Color base) {
    // background: the module's colour, lighter at the top
    g.setPaint(new GradientPaint(
      0, (float) TILE_INSET, brighter(base, 0.12),
      0, (float) (TILE_INSET + TILE), darker(base, 0.18)));
    g.fillRect(0, 0, SIZE, SIZE);

    drawHexGrid(g);

    final AffineTransform saved = g.getTransform();

    // the back card, fanned out to the left
    g.rotate(Math.toRadians(-9), tx(0.33), tx(0.52));
    drawCard(g, tx(0.12), tx(0.08), false);
    g.setTransform(saved);

    // the front card, the King of Diamonds
    g.rotate(Math.toRadians(-4), tx(0.40), tx(0.52));
    drawCard(g, tx(0.22), tx(0.06), true);
    g.setTransform(saved);

    // the counter
    g.rotate(Math.toRadians(-6), tx(0.70), tx(0.27));
    drawCounter(g, tx(0.535), tx(0.105), ts(0.33));
    g.setTransform(saved);

    drawPawn(g, tx(0.305), tx(0.52));

    // the die
    g.rotate(Math.toRadians(14), tx(0.575), tx(0.665));
    drawDie(g, tx(0.575), tx(0.665), ts(0.25));
    g.setTransform(saved);
  }

  private static void drawHexGrid(Graphics2D g) {
    // flat-topped hexes
    final double r = ts(0.085);
    final double w = 1.5 * r;
    final double h = Math.sqrt(3) * r;

    final Path2D grid = new Path2D.Double();
    for (int col = -1; col * w < SIZE + r; ++col) {
      final double cx = TILE_INSET + col * w;
      final double yoff = (col & 1) == 0 ? 0 : h / 2;
      for (int row = -1; row * h < SIZE + h; ++row) {
        final double cy = TILE_INSET + row * h + yoff;
        for (int i = 0; i < 6; ++i) {
          final double a = Math.toRadians(60 * i);
          final double x = cx + r * Math.cos(a);
          final double y = cy + r * Math.sin(a);
          if (i == 0) {
            grid.moveTo(x, y);
          }
          else {
            grid.lineTo(x, y);
          }
        }
        grid.closePath();
      }
    }

    g.setColor(new Color(255, 255, 255, 60));
    g.setStroke(new BasicStroke((float) (SIZE / 256.0)));
    g.draw(grid);
  }

  private static void dropShadow(Graphics2D g, Shape s) {
    final AffineTransform saved = g.getTransform();
    g.translate(SIZE / 170.0, SIZE / 110.0);
    g.setColor(new Color(0, 0, 0, 70));
    g.fill(s);
    g.setTransform(saved);
  }

  private static void drawCard(Graphics2D g, double x, double y, boolean face) {
    final double w = ts(0.33);
    final double h = ts(0.46);
    final Shape card = new RoundRectangle2D.Double(x, y, w, h, ts(0.05), ts(0.05));

    dropShadow(g, card);
    g.setColor(new Color(0xFAF8F2));
    g.fill(card);
    g.setColor(new Color(0x9A968C));
    g.setStroke(new BasicStroke((float) (SIZE / 300.0)));
    g.draw(card);

    if (!face) {
      // only the corner pip of the back card shows
      drawDiamond(g, x + ts(0.04), y + ts(0.05), ts(0.022), new Color(0xC8102E));
      return;
    }

    g.setColor(Color.BLACK);
    g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, (int) Math.round(ts(0.085))));
    g.drawString("K", (float) (x + ts(0.025)), (float) (y + ts(0.09))); //NON-NLS

    drawDiamond(g, x + w / 2, y + h / 2, ts(0.07), Color.BLACK);
  }

  private static void drawDiamond(Graphics2D g, double cx, double cy, double r, Color c) {
    final Path2D d = new Path2D.Double();
    d.moveTo(cx, cy - r * 1.3);
    d.lineTo(cx + r, cy);
    d.lineTo(cx, cy + r * 1.3);
    d.lineTo(cx - r, cy);
    d.closePath();
    g.setColor(c);
    g.fill(d);
  }

  private static void drawCounter(Graphics2D g, double x, double y, double s) {
    final Shape counter = new RoundRectangle2D.Double(x, y, s, s, s * 0.08, s * 0.08);
    dropShadow(g, counter);
    g.setColor(new Color(0x93A86A));
    g.fill(counter);
    g.setColor(new Color(0x2F3A1F));
    g.setStroke(new BasicStroke((float) (s * 0.025)));
    g.draw(counter);

    // the NATO infantry symbol
    final double bw = s * 0.56;
    final double bh = s * 0.32;
    final double bx = x + (s - bw) / 2;
    final double by = y + s * 0.17;
    final Path2D sym = new Path2D.Double();
    sym.append(new Rectangle2D.Double(bx, by, bw, bh), false);
    sym.moveTo(bx, by);
    sym.lineTo(bx + bw, by + bh);
    sym.moveTo(bx + bw, by);
    sym.lineTo(bx, by + bh);
    g.setStroke(new BasicStroke((float) (s * 0.035)));
    g.draw(sym);

    // combat factors
    g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, (int) Math.round(s * 0.2)));
    final String factors = "4-4-6"; //NON-NLS
    final FontMetrics fm = g.getFontMetrics();
    g.drawString(factors,
      (float) (x + (s - fm.stringWidth(factors)) / 2), (float) (y + s * 0.82));
  }

  private static void drawPawn(Graphics2D g, double cx, double top) {
    final double headR = ts(0.06);
    final Path2D pawn = new Path2D.Double();
    pawn.append(new Ellipse2D.Double(cx - headR, top, 2 * headR, 2 * headR), false);

    // body: a neck flaring out to a skirt
    final double neckY = top + headR * 1.7;
    final double skirtY = top + ts(0.25);
    final Path2D body = new Path2D.Double();
    body.moveTo(cx - ts(0.03), neckY);
    body.lineTo(cx + ts(0.03), neckY);
    body.curveTo(cx + ts(0.035), neckY + ts(0.08), cx + ts(0.08), skirtY - ts(0.03), cx + ts(0.085), skirtY);
    body.lineTo(cx - ts(0.085), skirtY);
    body.curveTo(cx - ts(0.08), skirtY - ts(0.03), cx - ts(0.035), neckY + ts(0.08), cx - ts(0.03), neckY);
    body.closePath();
    pawn.append(body, false);

    // base
    pawn.append(new RoundRectangle2D.Double(
      cx - ts(0.11), skirtY - ts(0.005), ts(0.22), ts(0.05), ts(0.03), ts(0.03)), false);

    dropShadow(g, pawn);
    g.setPaint(new GradientPaint(
      (float) (cx - ts(0.1)), 0, new Color(0x3C6CC8),
      (float) (cx + ts(0.1)), 0, new Color(0x1F3F8A)));
    g.fill(pawn);
  }

  private static void drawDie(Graphics2D g, double cx, double cy, double s) {
    final Shape die = new RoundRectangle2D.Double(cx - s / 2, cy - s / 2, s, s, s * 0.22, s * 0.22);
    dropShadow(g, die);
    g.setColor(Color.WHITE);
    g.fill(die);
    g.setColor(new Color(0x8C8C8C));
    g.setStroke(new BasicStroke((float) (s * 0.02)));
    g.draw(die);

    // three pips on the diagonal
    final double pr = s * 0.085;
    g.setColor(new Color(0x111111));
    for (int i = -1; i <= 1; ++i) {
      final double px = cx + i * s * 0.25;
      final double py = cy + i * s * 0.25;
      g.fill(new Ellipse2D.Double(px - pr, py - pr, 2 * pr, 2 * pr));
    }
  }

  /** The VASSAL logo on a pale disc in the tile's bottom-right corner. */
  private static void drawBadge(Graphics2D g) {
    final double r = ts(0.175);
    final double cx = TILE_INSET + TILE - r * 0.95;
    final double cy = TILE_INSET + TILE - r * 0.95;
    final Shape disc = new Ellipse2D.Double(cx - r, cy - r, 2 * r, 2 * r);

    dropShadow(g, disc);
    g.setColor(new Color(0xEEF0EA));
    g.fill(disc);
    g.setColor(new Color(0, 0, 0, 50));
    g.setStroke(new BasicStroke((float) (SIZE / 300.0)));
    g.draw(disc);

    try {
      final BufferedImage logo = ImageUtils.getImageResource("/images/" + ApplicationIcons.VASSAL_ICON_LARGE); //NON-NLS
      final double ls = r * 1.6;
      g.drawImage(logo, (int) Math.round(cx - ls / 2), (int) Math.round(cy - ls / 2),
        (int) Math.round(ls), (int) Math.round(ls), null);
    }
    catch (ImageIOException e) {
      logger.warn("Unable to load the VASSAL logo", e); //NON-NLS
    }
  }

  private static Color brighter(Color c, double f) {
    return new Color(
      (int) Math.round(c.getRed() + (255 - c.getRed()) * f),
      (int) Math.round(c.getGreen() + (255 - c.getGreen()) * f),
      (int) Math.round(c.getBlue() + (255 - c.getBlue()) * f));
  }

  private static Color darker(Color c, double f) {
    return new Color(
      (int) Math.round(c.getRed() * (1 - f)),
      (int) Math.round(c.getGreen() * (1 - f)),
      (int) Math.round(c.getBlue() * (1 - f)));
  }
}
