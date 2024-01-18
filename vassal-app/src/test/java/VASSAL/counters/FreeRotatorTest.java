/*
 * Copyright 2020 Vassal Development Team
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

package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.tools.NamedKeyStroke;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.lang.reflect.InvocationTargetException;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import javax.swing.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


public class FreeRotatorTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    FreeRotator trait = new FreeRotator();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Fixed rotation test
    trait = new FreeRotator();
    trait.validAngles = new double[] { 0, 90, 180, 270 };
    trait.rotateCWKey = NamedKeyStroke.of("rotateCWKey");
    trait.rotateCCWKey = NamedKeyStroke.of("rotateCCWKey");
    trait.rotateCWText = "rotateCW";
    trait.rotateCCWText = "rotateCCW";
    trait.rotateRNDKey = NamedKeyStroke.of("rotateRNDKey");
    trait.rotateRNDText = "rotateRND";
    trait.name = "xyzzy";
    trait.angleIndex = 2;
    trait.description = "plover";
    serializeTest("Fixed Rotations", trait); // NON-NLS

    // Free Rotation test
    trait = new FreeRotator();
    trait.validAngles = new double[] { 42.0 };
    trait.setAngleKey = NamedKeyStroke.of("setAngleKey");
    trait.setAngleText = "setAngle";
    trait.rotateRNDText = "rotateRND";
    trait.name = "xyzzy";
    trait.description = "plover";
    serializeTest("Fixed Rotations", trait); // NON-NLS

  }
  @Test
  public void clockwiseFacingRotation() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final NamedKeyStroke onKey = NamedKeyStroke.of("RotateCmd");

      FreeRotator fr = new FreeRotator();
      fr.setInner(new FreeRotatorTest.DummyPiece());
      fr.mySetType("rotate;10");
      fr.rotateCWKey = onKey;
      fr.myGetKeyCommands();

      // Repeat key stroke for a full 360 rotation.
      fr.mySetState("9");
      for (int i = 0; i < 10; i++) {
        fr.myKeyEvent(onKey.getKeyStroke());
        assertEquals(-36 * i, fr.getAngle());
      }
    }
  }

  @Test
  public void counterClockwiseFacingRotation() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final NamedKeyStroke onKey = NamedKeyStroke.of("RotateCmd");

      FreeRotator fr = new FreeRotator();
      fr.setInner(new FreeRotatorTest.DummyPiece());
      fr.mySetType("rotate;10");
      fr.rotateCCWKey = onKey;
      fr.myGetKeyCommands();

      fr.mySetState("0");
      for (int i = 0; i < 10; i++) {
        fr.myKeyEvent(onKey.getKeyStroke());
        assertEquals((36 * (i+1)) -360, fr.getAngle());
      }
    }
  }

  @Test
  public void rotateDirectFacing() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final NamedKeyStroke onKey = NamedKeyStroke.of("RotateCmd");

      FreeRotator fr = new FreeRotator();
      fr.setInner(new FreeRotatorTest.DummyPiece());
      // Setup for 10 facings.
      fr.mySetType("rotate;10");
      fr.rotateDirectKey = onKey;

      // Max value
      fr.directExpression.setFormat("10");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("9", fr.myGetState());
      assertEquals(-36 * 9, fr.getAngle());

      // Out of range greater than max.
      fr.directExpression.setFormat("11");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("0", fr.myGetState());
      assertEquals(0, fr.getAngle());

      fr.directExpression.setFormat("12");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("1", fr.myGetState());
      assertEquals(-36, fr.getAngle());

      // Mid-range value
      fr.directExpression.setFormat("5");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("4", fr.myGetState());
      assertEquals(-36 * 4, fr.getAngle());

      // Minimum valid value
      fr.directExpression.setFormat("1");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("0", fr.myGetState());
      assertEquals(0, fr.getAngle());

      // Negative and below valid range
      fr.directExpression.setFormat("-1");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("8", fr.myGetState());
      assertEquals(-36 * 8, fr.getAngle());

      // Switch to not a directTypeFacing type. The angle should round to the nearest facing.
      fr.directTypeFacing = false;
      fr.directExpression.setFormat("181");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("5", fr.myGetState());
      // Rounds to nearest facing.
      assertEquals(-180, fr.getAngle());
    }
  }

  @Test
  public void rotateDirectFreeRotation() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final NamedKeyStroke onKey = NamedKeyStroke.of("RotateCmd");

      FreeRotator fr = new FreeRotator();
      fr.setInner(new FreeRotatorTest.DummyPiece());
      fr.name = "rotateDirectFreeRotation";

      // Setup for FreeRotation
      fr.mySetType("rotate;1");
      fr.directTypeFacing = false;
      fr.rotateDirectKey = onKey;

      // Mid-range value
      fr.directExpression.setFormat("-9");
      fr.myKeyEvent(onKey.getKeyStroke());
      final String degrees = (String)fr.getProperty(fr.name + FreeRotator.DEGREES);
      assertEquals(-351, fr.getAngle());
      assertEquals( "351", degrees);

      // Over-range value. Output limited to 0-359 range.
      fr.directExpression.setFormat("710");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals(-350, fr.getAngle());
      assertEquals("350", fr.getProperty(fr.name+FreeRotator.DEGREES));

      // Negative value
      fr.directExpression.setFormat("-20");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals(-340, fr.getAngle());
    }
  }

  /**
   * Simulate the pressing and dragging of the mouse to test free rotation from mouse input.
   *
   * @param fr The FreeRotator object under test.
   * @param path The path of the mouse drag. The mouse button goes down at the first point.
   * Mouse button up is at the last point. The mouse can be dragged through an number of
   * intermediate points.
   *///
  private void DragMouseMove(FreeRotator fr, Point[] path) {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final Map map = mock(Map.class);
      final JComponent view = mock(JComponent.class);
      when(map.getView()).thenReturn(view);

      // Return the input parameter, simple one-to-one mapping.
      when(map.componentToMap(org.mockito.Mockito.any(Point.class))).thenAnswer(
          input -> {
            return input.getArgument(0);
          });

      // Mock a component for the mouse events.
      final Point origin = new Point(0,0);
      final Component comp = mock(Component.class);
      when(comp.getLocationOnScreen()).thenReturn(origin);

      // Setup for FreeRotation with initialized command keys.
      fr.mySetType("rotate;1;49,130;Rotate;50,130;Random;FreeRotator;;51,130;Direct;0;true");
      fr.setMap(map);

      if (path.length > 0) {
        long when = 1L;
        int index = 0;

        final MouseEvent press = new MouseEvent(comp, MouseEvent.MOUSE_PRESSED, when++, 0,
            path[index].x, path[index].y, 1, false, MouseEvent.BUTTON1);
        fr.myKeyEvent(fr.setAngleKey.getKeyStroke());
        fr.mousePressed(press);

        while (index < path.length) {
          final MouseEvent drag = new MouseEvent(comp, MouseEvent.MOUSE_PRESSED, when++, 0,
              path[index].x, path[index].y, 1, false, MouseEvent.BUTTON1);
          fr.mouseDragged(drag);
          ++index;
        }
        index = path.length - 1;
        final MouseEvent release = new MouseEvent(comp, MouseEvent.MOUSE_RELEASED, when++, 0,
            path[index].x, path[index].y, 1, false, MouseEvent.BUTTON1);
        fr.mouseReleased(release);
      }
    }
  }

  // Rotate clockwise from the default orientation.
    @Test
  public void rotateClockwiseFreeRotation() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());

    // Using the origin as the third point, define a right angle triangle with 45 degree angles.
    final Point[] path = new Point[] {
        new Point(0, -10),
        new Point(5, -5)
    };

    DragMouseMove(fr, path);

    assertEquals("45", fr.getProperty(fr.name+FreeRotator.DEGREES));
    assertEquals(-45.0, fr.getAngle());
  }

  // Rotate counter-clockwise from the default orientation.
  @Test
  public void rotateClockwiseDegrees200() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());

    // Move the piece off of the origin.
    final Point center = new Point(50, 80);
    fr.setPosition(center);

    // Rotation consisting of 90 degree segments
    final Point[] path = new Point[] {
        new Point(100, 0),   // Starting on right side of the piece
        new Point(0, 100),   // 90 degrees
        new Point(-100, 0),  // 90 degrees
        new Point(-100, -36) // 20 degrees, tan(20) = 36/100
    };

    // Offset mouse path by the position of the piece.
    for (Point p : path) {
      p.x += center.x;
      p.y += center.y;
    }
    DragMouseMove(fr, path);

    assertEquals("200", fr.getProperty(fr.name+FreeRotator.DEGREES));
    assertEquals(-200.0, fr.getAngle(), 0.5);
  }

  // A rotation greater than 360 degrees.
  @Test
  public void rotateClockwiseDegrees405() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());

    // Rotation consisting of 45 and 90 degree segments
    final Point[] path = new Point[] {
        new Point(0, 100),      // Starting from below the piece
        new Point(-100, 100),   // 45 degrees
        new Point(-100, -100),  // 90 degrees
        new Point(-0, -100),    // 45 degrees
        new Point(100, 0),      // 90 degrees
        new Point(100, 100),    // 45 degrees
        new Point(-50, 50)      // 90 degrees, total = 405
    };

    DragMouseMove(fr, path);

    assertEquals("45", fr.getProperty(fr.name+FreeRotator.DEGREES));
    assertEquals(-45.0, fr.getAngle(), 0.5);
  }

  @Test
  public void rotateCounterClockwiseFreeRotation() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());

    // Using the origin as the third point, define a right angle triangle with a 30 degree angle.
    final Point[] path = new Point[] {
        new Point(0, -173),
        new Point(-100, -173)
    };

    DragMouseMove(fr, path);

    assertEquals("330", fr.getProperty(fr.name+FreeRotator.DEGREES));
    assertEquals(-330.0, fr.getAngle(), 0.5);
  }

  @Test
  public void rotateCounterClockwiseDegrees280() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());

    // Rotation consisting of 90 degree segments and a small final angle.
    final Point[] path = new Point[] {
        new Point(0, -100),
        new Point(-100, 0),  // 90 degrees
        new Point(0, 100),   // 90 degrees
        new Point(100, 0),   // 90 degrees
        new Point(100, -17)  // 10 degrees, tan(10) = 17/100
    };

    DragMouseMove(fr, path);

    assertEquals("80", fr.getProperty(fr.name+FreeRotator.DEGREES));
    assertEquals(-80.0, fr.getAngle(), 0.5);
  }

  // The rotateDirect key command.
  @Test
  public void rotateDirect() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final NamedKeyStroke onKey = NamedKeyStroke.of("RotateCmd");

      FreeRotator fr = new FreeRotator();
      fr.setInner(new FreeRotatorTest.DummyPiece());

      fr.name = "rotateDirect";
      fr.mySetType("rotate;36");
      fr.directTypeFacing = false;
      fr.rotateDirectKey = onKey;

      // Over-range value
      fr.directExpression.setFormat("710");
      fr.myKeyEvent(onKey.getKeyStroke());
      assertEquals("35", fr.myGetState());
      assertEquals(-350, fr.getAngle());
      assertEquals("36", fr.getProperty(fr.name+FreeRotator.FACING));
    }
  }

  @Test
  public void mouseMoveThroughOrigin() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());

    final Point[] path = new Point[] {
        new Point(0, 0),
        new Point(5, -5)
    };

    DragMouseMove(fr, path);

    assertEquals("45", fr.getProperty(fr.name+FreeRotator.DEGREES));
    assertEquals(-45.0, fr.getAngle());
  }

  // Set the angle in degrees, then read back the internal facing value (i.e. zero based).
  @Test
  public void setAngleReadFacing() {
    FreeRotator fr = new FreeRotator();
    fr.setInner(new FreeRotatorTest.DummyPiece());
    fr.mySetType("rotate;90;;;;;;;Name");

    fr.setAngle(4);
    assertEquals("89", fr.myGetState());
    assertEquals("90", fr.getProperty(fr.name+FreeRotator.FACING));
    fr.setAngle(0);
    assertEquals("0", fr.myGetState());
    fr.setAngle(3);
    assertEquals("89", fr.myGetState());
    fr.setAngle(2);
    assertEquals("0", fr.myGetState());
    fr.setAngle(360 + 90);
    assertEquals("68", fr.myGetState());
    fr.setAngle(-4);
    assertEquals("1", fr.myGetState());
    fr.setAngle(-269);
    assertEquals("67", fr.myGetState());
    fr.setAngle(360);
    assertEquals("0", fr.myGetState());
  }

  class DummyPiece extends BasicPiece {
    @Override
    public Object getPublicProperty(Object key) {
      return null;
    }
  }

}
