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
package VASSAL.build.module;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;

/**
 * Tests that an Animated Dice Button survives a die face whose image it cannot
 * produce, and a die with too few faces to animate.
 *
 * <p>{@code BaseResultsIcon.getFaceImage()} is documented to return null and has
 * three ways of doing so: the die does not exist, the face has no image name, or
 * the named image is not in the module. They converge on one null, and
 * vassalengine/vassal#15368, #15367 and #15366 all arrived by the third --- a
 * module shipped without one of its die face images. The tests below use the
 * second, which produces the identical null without needing a GameModule and its
 * DataArchive.</p>
 *
 * <p>The icons are private inner classes with no accessor, so they have to be
 * reached by reflection. The button itself is built without running its
 * constructor, which opens a JDialog and so cannot run headless; the fields that
 * constructor would have set are filled in below.</p>
 */
public class AnimatedDiceButtonTest {
  private static final int STEPS = 4;

  /**
   * A die whose faces all exist but none of which names an image --- what you
   * get from a die that has been given faces but no art yet.
   */
  private static SpecialDie dieWithBlankFaces(int faces) {
    final SpecialDie die = new SpecialDie();
    for (int i = 0; i < faces; i++) {
      final SpecialDieFace face = new SpecialDieFace();
      // ICON, not IMAGE: IMAGE is declared but nothing reads it.
      face.setAttribute(SpecialDieFace.ICON, "");
      die.addFace(face);
    }
    return die;
  }

  /**
   * A die whose faces have never had their image attribute set at all, so
   * SpecialDieFace.getImageName() answers null rather than the empty string.
   */
  private static SpecialDie dieWithUnsetFaceImages(int faces) {
    final SpecialDie die = new SpecialDie();
    for (int i = 0; i < faces; i++) {
      die.addFace(new SpecialDieFace());
    }
    return die;
  }

  /** The reported crash: the rolled face has no image. */
  @Test
  public void aFaceWithNoImageDoesNotCrashTheAnimationBug15368() {
    final Object icon = animIcon(dieWithBlankFaces(6));
    assertDoesNotThrow(() -> setResults(icon, 0));
    assertNoFrameWasDrawn(icon);
  }

  /** The same, for a face whose image attribute was never set at all. */
  @Test
  public void aFaceWithNoImageNameDoesNotCrashTheAnimationBug15368() {
    final Object icon = animIcon(dieWithUnsetFaceImages(6));
    assertDoesNotThrow(() -> setResults(icon, 0));
    assertNoFrameWasDrawn(icon);
  }

  /** The launch button icon takes the same results and must survive them too. */
  @Test
  public void aFaceWithNoImageDoesNotCrashTheButtonIcon() {
    final Object icon = buttonIcon(dieWithBlankFaces(6));
    assertDoesNotThrow(() -> setResults(icon, 0));
  }

  /**
   * A die with no faces at all. The animation picks its intermediate frames from
   * the faces other than the one rolled, and sized that pool at faceCount - 1.
   */
  @Test
  public void aDieWithNoFacesDoesNotCrashTheAnimation() {
    final Object icon = animIcon(dieWithBlankFaces(0));
    assertDoesNotThrow(() -> setResults(icon, 0));
    assertNoFrameWasDrawn(icon);
  }

  /**
   * A die with exactly one face. There is then no other face to animate through,
   * and the loop that fills the frame list never advanced over the empty pool ---
   * so this hung rather than threw, with no error report of any kind.
   */
  @Test
  public void aDieWithOneFaceDoesNotHangTheAnimation() {
    final Object icon = animIcon(dieWithBlankFaces(1));
    assertTimeoutPreemptively(Duration.ofSeconds(10),
      () -> assertDoesNotThrow(() -> setResults(icon, 0)));
    assertNoFrameWasDrawn(icon);
  }

  /**
   * More results than there are dice. A ShowAnimResults command carries as many
   * results as the sender's copy of the module rolled, so a peer running a module
   * with more dice sends a longer array than this copy has dice for. setResults()
   * warns about exactly this and then proceeds.
   */
  @Test
  public void moreResultsThanDiceDoesNotCrashTheAnimation() {
    final Object icon = animIcon(dieWithBlankFaces(6));
    assertDoesNotThrow(() -> setResults(icon, 0, 0, 0));
  }

  /** Nothing could be drawn, so every frame of every die is empty. */
  private static void assertNoFrameWasDrawn(Object icon) {
    final ImageOpGrid grid = iconsOf(icon);
    assertEquals(STEPS + 1, grid.rows, "one row per animation step, plus the result");
    for (int row = 0; row < grid.rows; row++) {
      for (int col = 0; col < grid.cols; col++) {
        assertNull(grid.at(row, col), "frame " + row + " of die " + col);
      }
    }
  }

  // --- reflective plumbing ------------------------------------------------

  /** The icons are inner classes, so they need an outer instance to hang on. */
  private static AnimatedDiceButton button(SpecialDie die) {
    // The real constructor opens a JDialog, which throws in a headless build, so
    // the instance is made without it and the fields it would have set are filled
    // in here.
    final AnimatedDiceButton b = mock(AnimatedDiceButton.class, CALLS_REAL_METHODS);
    final List<SpecialDie> dice = new ArrayList<>();
    dice.add(die);
    set(b, SpecialDiceButton.class, "dice", dice);
    set(b, SpecialDiceButton.class, "ran", new Random(20261119L));
    set(b, AnimatedDiceButton.class, "steps", STEPS);
    set(b, AnimatedDiceButton.class, "buttonScale", 0.5);
    return b;
  }

  private static Object animIcon(SpecialDie die) {
    return innerIcon(button(die), "AnimResultsIcon");
  }

  private static Object buttonIcon(SpecialDie die) {
    return innerIcon(button(die), "ButtonResultsIcon");
  }

  private static Object innerIcon(AnimatedDiceButton outer, String simpleName) {
    try {
      final Class<?> cls =
        Class.forName(AnimatedDiceButton.class.getName() + "$" + simpleName);
      final Constructor<?> ctor =
        cls.getDeclaredConstructor(AnimatedDiceButton.class);
      ctor.setAccessible(true);
      return ctor.newInstance(outer);
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }

  /** What the dialog would paint: the step-by-step grid of frames. */
  private static ImageOpGrid iconsOf(Object icon) {
    final Object[][] icons = (Object[][]) get(icon, icon.getClass(), "icons");
    return new ImageOpGrid(icons);
  }

  private static final class ImageOpGrid {
    private final Object[][] icons;
    private final int rows;
    private final int cols;

    private ImageOpGrid(Object[][] icons) {
      this.icons = icons;
      this.rows = icons == null ? 0 : icons.length;
      this.cols = rows == 0 || icons[0] == null ? 0 : icons[0].length;
    }

    private Object at(int row, int col) {
      return icons[row][col];
    }
  }

  /**
   * Hand the icon a set of die results, as a roll or a peer's command does.
   * Reflection wraps whatever it throws, so the original is put back.
   */
  private static void setResults(Object icon, int... results) {
    try {
      final Method m = icon.getClass().getSuperclass()
        .getDeclaredMethod("setResults", int[].class);
      m.setAccessible(true);
      m.invoke(icon, (Object) results);
    }
    catch (InvocationTargetException e) {
      final Throwable cause = e.getCause();
      if (cause instanceof RuntimeException) {
        throw (RuntimeException) cause;
      }
      if (cause instanceof Error) {
        throw (Error) cause;
      }
      throw new AssertionError(cause);
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }

  private static void set(Object target, Class<?> declaring, String name, Object value) {
    try {
      final Field f = declaring.getDeclaredField(name);
      f.setAccessible(true);
      f.set(target, value);
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }

  private static Object get(Object target, Class<?> declaring, String name) {
    try {
      final Field f = declaring.getDeclaredField(name);
      f.setAccessible(true);
      return f.get(target);
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }
}
