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
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.util.Map;

import javax.swing.SwingUtilities;

import org.apache.commons.lang3.SystemUtils;

public class SwingUtils {
  
  private static boolean macLegacy;
    
  /**
   * Supports swapping most Control and Command button functions on Mac so as not to punish long-time users for our "old non-standard implementations"
   */
  public static boolean isMacLegacy() {
    return macLegacy;
  }
  
  /**
   * Supports swapping most Control and Command button functions on Mac so as not to punish long-time users for our "old non-standard implementations"
   */
  public static void setMacLegacy(boolean b) {
    macLegacy = b;
  }
  
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

    /*
     * @return whether this is key/mouse combo that brings up context menus on our platform (whether or not it brings them up right this moment)
     */
    boolean isContextMouseButtonDown(MouseEvent e);
        
    /*
     * @return whether this the key/mouse combo for clicking on things to select them. (normally just plain old Left Mouse Button - but on Mac only when not pretending to be right button)
     */
    boolean isVanillaLeftButtonDown(MouseEvent e);
    
    /*
     * @return whether this is key/mouse combo that toggles items in and out of selections on our platform (e.g. Ctrl+LeftClick on non-Mac)
     */
    boolean isSelectionToggle(MouseEvent e);
    
    /*
     * @return whether key that activates primary shortcut keys is down (e.g. Ctrl on most platforms, Command on Mac) 
     */
    boolean isShortcutKeyDown(MouseEvent e);
        
    /*
     * @returns key that activates primary shortcut keys for our platform (e.g. Ctrl on most platforms, Command on Mac)
     */
    int getShortcutKey();
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
    
    public boolean isContextMouseButtonDown(MouseEvent e) {
      return SwingUtilities.isRightMouseButton(e);  
    }
        
    public boolean isVanillaLeftButtonDown(MouseEvent e) {
      return SwingUtilities.isLeftMouseButton(e);
    }
    
    public boolean isSelectionToggle(MouseEvent e) {
      return e.isControlDown();
    }
    
    public boolean isShortcutKeyDown(MouseEvent e) {
      return e.isControlDown(); 
    }
    
    public int getShortcutKey() {
      return InputEvent.CTRL_DOWN_MASK;
    }           
  }

  
  private static class MacInputClassifier implements InputClassifier {
    private static final int B1_MASK = MouseEvent.BUTTON1_DOWN_MASK |
                                       MouseEvent.CTRL_DOWN_MASK;

    @Deprecated
    public boolean isLeftMouseButton(MouseEvent e) {
      return isVanillaLeftButtonDown(e);
    }

    @Deprecated
    public boolean isRightMouseButton(MouseEvent e) {
      return isContextMouseButtonDown(e);
    }

    @Deprecated
    public boolean isControlDown(MouseEvent e) {
      // The Command key on a Mac corresponds to Control everywhere else,
      // but it obviously can't be called "Command" in Java; here it's
      // called "Meta".
      return e.isMetaDown();
    }
        
    
    public boolean isContextMouseButtonDown(MouseEvent e) {
      // A "right click" on a Mac can be either a real right button, or
      // Ctrl + the left button (or Command + left button in legacy)
      switch (e.getID()) {
      case MouseEvent.MOUSE_PRESSED:
      case MouseEvent.MOUSE_RELEASED:
      case MouseEvent.MOUSE_CLICKED:
        return e.getButton() == MouseEvent.BUTTON3 ||
               (e.getButton() == MouseEvent.BUTTON1 &&
                (e.getModifiersEx() & (macLegacy ? MouseEvent.META_DOWN_MASK : MouseEvent.CTRL_DOWN_MASK)) != 0);

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
               (e.getModifiersEx() & (macLegacy ? MouseEvent.META_DOWN_MASK : MouseEvent.CTRL_DOWN_MASK)) != 0);
      }                 
    }
    
    public boolean isVanillaLeftButtonDown(MouseEvent e) {
      // The left button on a Mac is the left button, except when it's the
      // right button because Ctrl is depressed.
      switch (e.getID()) {
      case MouseEvent.MOUSE_PRESSED:
      case MouseEvent.MOUSE_RELEASED:
      case MouseEvent.MOUSE_CLICKED:
        return e.getButton() == MouseEvent.BUTTON1 &&
               (e.getModifiersEx() & (macLegacy ? MouseEvent.META_DOWN_MASK : MouseEvent.CTRL_DOWN_MASK)) == 0;

      case MouseEvent.MOUSE_ENTERED:
      case MouseEvent.MOUSE_EXITED:
      case MouseEvent.MOUSE_DRAGGED:
        return (e.getModifiersEx() & B1_MASK) == MouseEvent.BUTTON1_DOWN_MASK;

      default:
        return (e.getModifiersEx() & B1_MASK) == MouseEvent.BUTTON1_DOWN_MASK ||
               (e.getButton() == MouseEvent.BUTTON1 &&
                (e.getModifiersEx() & (macLegacy ? MouseEvent.META_DOWN_MASK : MouseEvent.CTRL_DOWN_MASK)) == 0);
      }      
    }
    
    
    public boolean isSelectionToggle(MouseEvent e) {
      return macLegacy ? e.isControlDown() : e.isMetaDown();       
    }
    
    public boolean isShortcutKeyDown(MouseEvent e) {
      return macLegacy ? e.isControlDown() : e.isMetaDown(); 
    }
    
    public int getShortcutKey() {
      return macLegacy ? InputEvent.META_DOWN_MASK : InputEvent.CTRL_DOWN_MASK;
    }       
  }

  private static final InputClassifier INPUT_CLASSIFIER =
    SystemUtils.IS_OS_MAC_OSX ?
      new MacInputClassifier() : new DefaultInputClassifier();

  /**
   * @return whether the event is effectively for the left button
   */
  @Deprecated      
  public static boolean isLeftMouseButton(MouseEvent e) {
    return INPUT_CLASSIFIER.isLeftMouseButton(e);
  }

  /**
   * @return whether the event is effectively for the right button
   */
  @Deprecated
  public static boolean isRightMouseButton(MouseEvent e) {
    return INPUT_CLASSIFIER.isRightMouseButton(e);
  }

  /**
   * @return whether the event effectively has Control down
   */
  @Deprecated
  public static boolean isControlDown(MouseEvent e) {
    return INPUT_CLASSIFIER.isControlDown(e);
  }
  
  
  /**
   * @return whether the event has the key/mouse combo for Context Menu active, whether it would raise one "right now" or not. (normally plain right mouse button, but some funky mac bonuses)
   */
  public static boolean isContextMouseButtonDown(MouseEvent e) {
    return INPUT_CLASSIFIER.isContextMouseButtonDown(e);
  }
  
    
  /**
   * @return whether the event has the key/mouse combo for selecting things down (normally just plain left mouse button, but on Mac only if not pretending to be right button)
   */
  public static boolean isVanillaLeftButtonDown(MouseEvent e) {
    return INPUT_CLASSIFIER.isVanillaLeftButtonDown(e);
  }

  
  /**
   * @return whether the event has the key/mouse combo toggling targets in and out of selection (normally Ctrl+Click on most platforms, normally Command+Click on Mac)
   */  
  public static boolean isSelectionToggle(MouseEvent e) {
    return INPUT_CLASSIFIER.isSelectionToggle(e);
  }
  
  
  /**
   * @return whether the event has the primary shortcut key for this platform down (normally Ctrl for most platforms, normally Command on Mac)
   */  
  public static boolean isShortcutKeyDown(MouseEvent e) {
    return INPUT_CLASSIFIER.isShortcutKeyDown(e);
  }
  
  /**
   * @return return the primary shortcut key for this platform (normally Ctrl for most platforms, normally Command aka "Meta" for Mac)
   */  
  public static int getShortcutKey() {
    return INPUT_CLASSIFIER.getShortcutKey();
  }
  

  /**
   * @return whether the drag is non-mouse or effectively from the left button
   */
  public static boolean isDragTrigger(DragGestureEvent e) {
    // NB: Will any non-mouse drags happen? Not sure, but as we never checked
    // for them before, this won't change behavior by excluding them.
    final InputEvent te = e.getTriggerEvent();
    return !(te instanceof MouseEvent) || isVanillaLeftButtonDown((MouseEvent) te);
  }
}
