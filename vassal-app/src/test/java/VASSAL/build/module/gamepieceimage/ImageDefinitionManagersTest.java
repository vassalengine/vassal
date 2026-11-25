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
package VASSAL.build.module.gamepieceimage;

import java.awt.Color;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

/**
 * The ColorManager and FontManager of a module's Game Piece Image Definitions
 * are reachable before either has been built, because
 * GamePieceImageDefinitions.build() builds every GamePieceImage in the
 * buildFile first -- reading one calls ColorSwatch.decode() and drawing one
 * calls TextItem.isOutline(), and both ask for the manager. A module whose
 * buildFile declares neither, or declares them after its images, used to fail
 * to open at all.
 *
 * <p>See vassalengine/vassal#13201, #13361, #13708 and #13709.</p>
 */
public class ImageDefinitionManagersTest {

  private ColorManager savedColors;
  private FontManager savedFonts;

  /**
   * Both managers keep a static instance set by addTo(), and another test --
   * or another case here -- may have built one. Put back whatever was there.
   */
  @BeforeEach
  public void rememberInstances() {
    savedColors = ColorManager.getColorManager();
    savedFonts = FontManager.getFontManager();
    ColorManager.instance = null;
    FontManager.instance = null;
  }

  @AfterEach
  public void restoreInstances() {
    ColorManager.instance = savedColors;
    FontManager.instance = savedFonts;
  }

  @Test
  public void colorManagerIsThereBeforeOneHasBeenBuilt() {
    assertThat(ColorManager.getColorManager(), is(notNullValue()));
  }

  @Test
  public void fontManagerIsThereBeforeOneHasBeenBuilt() {
    assertThat(FontManager.getFontManager(), is(notNullValue()));
  }

  /**
   * #13361: reading a GamePieceImage's attributes is enough, through
   * ColorSwatch's own constructor.
   */
  @Test
  public void aColourSwatchDecodesBeforeAColorManagerHasBeenBuilt() {
    final ColorSwatch swatch =
      assertDoesNotThrow(() -> new ColorSwatch("BLACK"));
    assertThat(swatch.getColor(), is(Color.BLACK));
  }

  /**
   * The standard colours are held statically, so the stand-in answers for them
   * exactly as a built ColorManager with no user colours would.
   */
  @Test
  public void theStandInKnowsTheStandardColours() {
    assertThat(ColorManager.getColorManager().getColorByName("RED"), is(Color.RED));
  }

  /**
   * The point is not what the answer is but that it is the same answer: the
   * stand-in must behave as a built ColorManager with no user colours does,
   * for a standard name and for one it has never heard of alike.
   */
  @Test
  public void theStandInAnswersAsAnEmptyBuiltManagerWould() {
    final ColorManager standIn = ColorManager.getColorManager();
    final ColorManager built = new ColorManager();

    for (final String name : new String[] {"RED", "BLACK", "CLEAR", "CHARTREUSE"}) {
      assertThat(name, standIn.getColorByName(name), is(built.getColorByName(name)));
    }
  }

  /**
   * #13201, #13708, #13709: drawing a GamePieceImage reaches TextItem.isOutline(),
   * which asks the FontManager for a style by name.
   */
  @Test
  public void aFontStyleIsFoundBeforeAFontManagerHasBeenBuilt() {
    final FontStyle style = assertDoesNotThrow(
      () -> FontManager.getFontManager().getFontStyle(FontManager.DEFAULT));
    assertThat(style, is(notNullValue()));
    assertThat(style.getFont(), is(notNullValue()));
  }

  /**
   * A module's own manager still wins once it has been built.
   */
  @Test
  public void aBuiltManagerReplacesTheStandIn() {
    final ColorManager colors = new ColorManager();
    colors.addTo(null);
    assertThat(ColorManager.getColorManager(), is(colors));

    final FontManager fonts = new FontManager();
    fonts.addTo(null);
    assertThat(FontManager.getFontManager(), is(fonts));
  }
}
