/*
 *
 * Copyright (c) 2020 by Joel Uckelman
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
package VASSAL.tools.swing;

import java.awt.Toolkit;
import java.awt.dnd.DragGestureEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.util.Map;

import javax.swing.SwingUtilities;

import org.apache.commons.lang3.SystemUtils;

public class SwingUtils {
  public static AffineTransform descaleTransform(AffineTransform t) {
    return new AffineTransform(
      1.0, 0.0,
      0.0, 1.0,
      t.getTranslateX(), t.getTranslateY()
    );
  }

  public static final Map<?,?> FONT_HINTS = (Map<?,?>) Toolkit.getDefaultToolkit().getDesktopProperty("awt.font.desktophints");

  private interface InputClassifier {
    /*
     * @return whether the event is effectively for the left button
     */
    boolean isLeftMouseButton(MouseEvent e);

    /*
     * @return whether the event is effectively for the right button
     */
    boolean isRightMouseButton(MouseEvent e);

    /*
     * @return whether the event effectively has Control down
     */
    boolean isControlDown(MouseEvent e);
  }

  private static class DefaultInputClassifier implements InputClassifier {
    public boolean isLeftMouseButton(MouseEvent e) {
      return SwingUtilities.isLeftMouseButton(e);
    }

    public boolean isRightMouseButton(MouseEvent e) {
      return SwingUtilities.isRightMouseButton(e);
    }

    public boolean isControlDown(MouseEvent e) {
      return e.isControlDown();
    }
  }

  private static class MacInputClassifier implements InputClassifier {
    private static final int B1_MASK = MouseEvent.BUTTON1_DOWN_MASK |
                                       MouseEvent.CTRL_DOWN_MASK;

    public boolean isLeftMouseButton(MouseEvent e) {
      // The left button on a Mac is the left button, except when it's the
      // right button because Ctrl is depressed.
      switch (e.getID()) {
      case MouseEvent.MOUSE_PRESSED:
      case MouseEvent.MOUSE_RELEASED:
      case MouseEvent.MOUSE_CLICKED:
        return e.getButton() == MouseEvent.BUTTON1 &&
               (e.getModifiersEx() & MouseEvent.CTRL_DOWN_MASK) == 0;

      case MouseEvent.MOUSE_ENTERED:
      case MouseEvent.MOUSE_EXITED:
      case MouseEvent.MOUSE_DRAGGED:
        return (e.getModifiersEx() & B1_MASK) == MouseEvent.BUTTON1_DOWN_MASK;

      default:
        return (e.getModifiersEx() & B1_MASK) == MouseEvent.BUTTON1_DOWN_MASK ||
               (e.getButton() == MouseEvent.BUTTON1 &&
                (e.getModifiersEx() & MouseEvent.CTRL_DOWN_MASK) == 0);
      }
    }

    public boolean isRightMouseButton(MouseEvent e) {
      // The right button on a Mac can be either a real right button, or
      // Ctrl + the left button.
      switch (e.getID()) {
      case MouseEvent.MOUSE_PRESSED:
      case MouseEvent.MOUSE_RELEASED:
      case MouseEvent.MOUSE_CLICKED:
        return e.getButton() == MouseEvent.BUTTON3 ||
               (e.getButton() == MouseEvent.BUTTON1 &&
                (e.getModifiersEx() & MouseEvent.CTRL_DOWN_MASK) != 0);

      case MouseEvent.MOUSE_ENTERED:
      case MouseEvent.MOUSE_EXITED:
      case MouseEvent.MOUSE_DRAGGED:
        return (e.getModifiersEx() & MouseEvent.BUTTON3_DOWN_MASK) != 0 ||
               (e.getModifiersEx() & B1_MASK) == B1_MASK;

      default:
        return (e.getModifiersEx() & MouseEvent.BUTTON3_DOWN_MASK) != 0 ||
               (e.getModifiersEx() & B1_MASK) == B1_MASK ||
               e.getButton() == MouseEvent.BUTTON3 ||
               (e.getButton() == MouseEvent.BUTTON1 &&
               (e.getModifiersEx() & MouseEvent.CTRL_DOWN_MASK) != 0);
      }
    }

    public boolean isControlDown(MouseEvent e) {
      return e.isControlDown();
    }
  }

  private static final InputClassifier inputClassifier =
    SystemUtils.IS_OS_MAC_OSX ?
      new MacInputClassifier() : new DefaultInputClassifier();

  /*
   * @return whether the event is effectively for the left button
   */
  public static boolean isLeftMouseButton(MouseEvent e) {
    return inputClassifier.isLeftMouseButton(e);
  }

  /*
   * @return whether the event is effectively for the right button
   */
  public static boolean isRightMouseButton(MouseEvent e) {
    return inputClassifier.isRightMouseButton(e);
  }

  /*
   * @return whether the drag is non-mouse or effectively from the left button
   */
  public static boolean isDragTrigger(DragGestureEvent e) {
    final InputEvent te = e.getTriggerEvent();
    return !(te instanceof MouseEvent) || isLeftMouseButton((MouseEvent) te);
  }

  /*
   * @return whether the event effectively has Control down
   */
  public static boolean isControlDown(MouseEvent e) {
    return inputClassifier.isControlDown(e);
  }
}
