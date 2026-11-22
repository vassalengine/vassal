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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

public class ModuleIconTest {
  private static final int SIZE = 512;

  @Test
  public void colourIsStableForAName() {
    // String.hashCode() is fixed by the spec, so these never change
    assertEquals(ModuleIcon.colourFor("Here I Stand"), ModuleIcon.colourFor("Here I Stand"));
    assertEquals(ModuleIcon.colourFor("Here I Stand"), ModuleIcon.colourFor("  Here I Stand "));
    assertNotNull(ModuleIcon.colourFor(null));
  }

  @Test
  public void namesSpreadOverThePalette() {
    final Set<Color> used = new HashSet<>();
    for (int i = 0; i < 200; ++i) {
      used.add(ModuleIcon.colourFor("Module " + i));
    }
    assertEquals(ModuleIcon.PALETTE.length, used.size());
  }

  @Test
  public void paletteColoursAreDistinct() {
    final Set<Color> colours = new HashSet<>(Arrays.asList(ModuleIcon.PALETTE));
    assertEquals(ModuleIcon.PALETTE.length, colours.size());
  }

  @Test
  public void generatedIconHasATransparentMarginAndAColouredTile() {
    final BufferedImage icon = ModuleIcon.render("Paths of Glory", null, SIZE, ModuleIcon.Badge.VASSAL);
    assertEquals(SIZE, icon.getWidth());
    assertEquals(SIZE, icon.getHeight());

    // the corner is outside the tile
    assertEquals(0, icon.getRGB(0, 0) >>> 24);

    // the middle of the left edge of the tile shows the module's colour
    final int argb = icon.getRGB(SIZE / 8, SIZE * 3 / 4);
    assertEquals(255, argb >>> 24);
  }

  @Test
  public void aboutImageFillsTheTile() {
    final BufferedImage about = new BufferedImage(300, 200, BufferedImage.TYPE_INT_RGB);
    final Graphics2D g = about.createGraphics();
    g.setColor(Color.MAGENTA);
    g.fillRect(0, 0, 300, 200);
    g.dispose();

    final BufferedImage icon = ModuleIcon.render("Anything", about, SIZE, ModuleIcon.Badge.VASSAL);
    // top-left area of the tile, away from the rounded corner and the badge
    final Color c = new Color(icon.getRGB(SIZE / 3, SIZE / 4), true);
    assertEquals(Color.MAGENTA, c);

    // and without the About image, it isn't magenta
    final Color t = new Color(ModuleIcon.render("Anything", null, SIZE, ModuleIcon.Badge.VASSAL)
      .getRGB(SIZE / 3, SIZE / 4), true);
    assertNotEquals(Color.MAGENTA, t);
    assertTrue(t.getAlpha() > 0);
  }

  @Test
  public void rendersAtAnySize() {
    final BufferedImage small = ModuleIcon.render("Here I Stand", null, 64, ModuleIcon.Badge.VASSAL);
    assertEquals(64, small.getWidth());
    assertEquals(64, small.getHeight());
    assertEquals(0, small.getRGB(0, 0) >>> 24);
  }

  @Test
  public void badgeIsOptional() {
    // the badge's pale disc covers this point only when there is a badge
    final int x = SIZE * 3 / 4;
    final int y = SIZE * 3 / 4;
    final Color with = new Color(ModuleIcon.render("Here I Stand", null, SIZE, ModuleIcon.Badge.VASSAL).getRGB(x, y));
    final Color without = new Color(ModuleIcon.render("Here I Stand", null, SIZE, ModuleIcon.Badge.NONE).getRGB(x, y));
    assertNotEquals(with, without);
  }
}
