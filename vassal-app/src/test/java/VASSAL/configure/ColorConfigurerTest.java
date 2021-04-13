/*
 *
 * Copyright (c) 2020 by Rodney Kinney, Brent Easton, Joel Uckelman, and Jason Stewart
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
 package VASSAL.configure;

// import VASSAL.tools.DataArchive;
// import VASSAL.build.GameModule;
import java.awt.Color;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class ColorConfigurerTest {
  // private static final String modFile = "../../../resources/test.vmod";
  // final DataArchive arch = new DataArchive(modFile);
  // final GameModule module = new GameModule(arch);
  // GameModule.build();

  @Test
  public void testTransparencyBug13307() {
    final String key = "key";
    final String name = "name";
    final int R = 127;
    final int G = 128;
    final int B = 129;
    final int A = 15;
    final String colorString = "" + R + "," + G + "," + B + "," + A;
    final Color color = new Color(R,G,B,A);
    final ColorConfigurer cc = new ColorConfigurer(key, name, color);

    final Color newColor = cc.stringToColor(colorString);
    assertEquals(color.getBlue(), newColor.getBlue());
    assertEquals(color.getRed(), newColor.getRed());
    assertEquals(color.getGreen(), newColor.getGreen());
    assertEquals(color.getAlpha(), newColor.getAlpha());

    assertEquals(cc.getValueString(),colorString);
  }
}
