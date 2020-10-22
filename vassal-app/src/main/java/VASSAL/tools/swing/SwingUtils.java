/*
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

import java.awt.Component;
import java.awt.GraphicsConfiguration;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.dnd.DragGestureEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.util.Map;

import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.module.GlobalOptions;

import org.apache.commons.lang3.SystemUtils;

public class SwingUtils {
  
  private static boolean macLegacy = GlobalOptions.FORCE_MAC_LEGACY;
    
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

  public static final Map<?, ?> FONT_HINTS = (Map<?, ?>) Toolkit.getDefaultToolkit().getDesktopProperty("awt.font.desktophints");

  private interface InputClassifier {
    /**
     * @return whether this is key/mouse combo that brings up context menus on our platform (Usually a right-click, but sometimes also a special left click on e.g. Macs)
     */
    boolean isContextMouseButtonDown(MouseEvent e);
        
    /**
     * @return whether this the key/mouse combo for clicking on things to select them. (Left click, except on Macs when left click has a modifier that makes it pretend to be right click)
     */
    boolean isMainMouseButtonDown(MouseEvent e);
    
    /**
     * @return whether this is key/mouse combo that toggles items in and out of selections on our platform (e.g. Ctrl+LeftClick on non-Mac, and usually Command+LeftClick on Mac)
     */
    boolean isSelectionToggle(MouseEvent e);

    /**
     * @param e Key event
     * @return Returns true if CTRL key down on non-Mac, or Command key down on Mac. Intended primarily for use with Arrow keys.
     */
    boolean isModifierKeyDown(KeyEvent e);

    /**
     * @return translation of keystroke from local system to Vassal (to handle Mac platform support)
     */
    KeyStroke systemToGeneric (KeyStroke k);
    
    /**
     * @return translation of keystroke from Vassal to local system (to handle Mac platform support)
     */
    KeyStroke genericToSystem (KeyStroke k);    
  }

  
  /**
   * Pass through -- presently handles all systems except Macs
   */
  private static class DefaultInputClassifier implements InputClassifier {
    @Override
    public boolean isContextMouseButtonDown(MouseEvent e) {
      return SwingUtilities.isRightMouseButton(e);  
    }
        
    @Override
    public boolean isMainMouseButtonDown(MouseEvent e) {
      return SwingUtilities.isLeftMouseButton(e);
    }
    
    @Override
    public boolean isSelectionToggle(MouseEvent e) {
      return e.isControlDown();
    }

    @Override
    public boolean isModifierKeyDown(KeyEvent e) { return e.isControlDown(); }
    
    @Override
    public KeyStroke systemToGeneric (KeyStroke k) {
      return k;
    }
    
    @Override
    public KeyStroke genericToSystem (KeyStroke k) {
      return k;
    }
  }

  /**
   * All the special Mac-platform-specific mouse and keyboard code lives here
   */
  private static class MacInputClassifier implements InputClassifier {
    private static final int B1_MASK = MouseEvent.BUTTON1_DOWN_MASK |
                                       MouseEvent.CTRL_DOWN_MASK;
        
    /**
     * @return whether this event includes the mouse/key combo that brings up Context Menus on this platform. Does NOT necessarily mean that it's supposed to
     * bring one up right now. On most platforms this simply returns true if Right Mouse Button is down, but on Macs either Control+LeftClick (proper interface) 
     * or Command+LeftClick (legacy Vassal interface) will also "pretend to be right button".  
     */
    @Override
    public boolean isContextMouseButtonDown(MouseEvent e) {
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

    /**
     * @return whether this event should be treated as a regular Left Mouse Down button. On most platforms will always
     * return true if Left Mouse Button is down. But on Macs will return false if the special key modifier to "pretend to be the right mouse button"
     * (Control+Click for proper Mac interface, Command+Click for legacy Vassal interface) is present.
     */
    @Override
    public boolean isMainMouseButtonDown(MouseEvent e) {
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
    
    /**
     * @return whether this event includes the mouse/key combo to toggle items in and out of a selection.
     * This is "Ctrl+LeftClick" on most platforms. But on Mac it is Command+Click.
     */
    @Override
    public boolean isSelectionToggle(MouseEvent e) {
      return e.isMetaDown();       
    }

    /**
     * @return true if Command+Click on Mac or CTRL+Click on non-Mac.
     */
    @Override
    public boolean isModifierKeyDown(KeyEvent e) { return e.isMetaDown(); }

    /**
     * Translates a keystroke we've received from the system (our local platform) into a generic "platform-independent" one
     * suitable for storing in a module. On Mac this means we translate "Command" into "Control", unless the legacy preference
     * is set.
     * 
     * @return "Platform-independent" keystroke appropriate for storing in a module. Common key shortcuts will be remembered as "Ctrl".
     * 
     * Here we make "Meta" (Command) keystrokes from a Mac look like "Ctrl" keystrokes to Vassal
     */
    @Override
    @SuppressWarnings("deprecation")
    public KeyStroke systemToGeneric (KeyStroke k) {
      if (macLegacy) {
        return k;
      }
      int modifiers = k.getModifiers();
      if ((modifiers & InputEvent.META_DOWN_MASK) != 0) {
        // Here we make "Meta" (Command) keystrokes from a Mac look like "Ctrl" keystrokes to Vassal
        // We must also remove the deprecated META_MASK, or Java will end up restoring the META_DOWN_MASK.
        modifiers &= ~(InputEvent.META_DOWN_MASK | InputEvent.META_MASK);
        modifiers |= InputEvent.CTRL_DOWN_MASK;
        return KeyStroke.getKeyStroke(k.getKeyCode(), modifiers, k.isOnKeyRelease());
      }
      else if ((modifiers & InputEvent.CTRL_DOWN_MASK) != 0) {
        // Don't let Vassal recognize the Mac "Control" key as a "Ctrl" keystroke for keyboard commands. Because that would be bad-app-behavior, naughty naughty.
        modifiers &= ~(InputEvent.CTRL_DOWN_MASK | InputEvent.CTRL_MASK);
        return KeyStroke.getKeyStroke(k.getKeyCode(), modifiers, k.isOnKeyRelease());
      }
      else {
        return k; 
      }      
    }
    
    /**
     * Translates a platform-independent keystroke from our module into the appropriate one for the
     * Mac platform depending on our preference. Normally the "Ctrl" keystrokes from other platforms
     * are translated as "Command" keystrokes on Mac, unless the legacy preference is set.
     * 
     * @return Keystroke appropriate for our local platform's preferred interface.
     * 
     * Here we turn any "Ctrl" keystrokes stored in Vassal into "Meta" (Command) keystrokes for use in communicating with a Mac
     * We must also remove the deprecated CTRL_MASK, or Java will end up restoring the CTRL_DOWN_MASK
     */
    @Override
    @SuppressWarnings("deprecation")
    public KeyStroke genericToSystem (KeyStroke k) {
      if (macLegacy) {
        return k;
      }
      int modifiers = k.getModifiers();
      if ((modifiers & InputEvent.CTRL_DOWN_MASK) != 0) {
        modifiers &= ~(InputEvent.CTRL_DOWN_MASK | InputEvent.CTRL_MASK);
        modifiers |= InputEvent.META_DOWN_MASK;
        return KeyStroke.getKeyStroke(k.getKeyCode(), modifiers, k.isOnKeyRelease());
      } 
      else {
        return k; 
      }      
    }          
  } 

  private static final InputClassifier INPUT_CLASSIFIER =
    SystemUtils.IS_OS_MAC_OSX ?
      new MacInputClassifier() : new DefaultInputClassifier();

  /**
   * @return whether the event is effectively for the left button. 
   * 
   * @deprecated in favor of {@link #isMainMouseButtonDown(MouseEvent)}
   */
  @Deprecated      
  public static boolean isLeftMouseButton(MouseEvent e) {
    return INPUT_CLASSIFIER.isMainMouseButtonDown(e);
  }

  /**
   * @return whether the event is effectively for the right button. 
   * 
   * @deprecated in favor of {@link #isContextMouseButtonDown(MouseEvent)}
   */
  @Deprecated
  public static boolean isRightMouseButton(MouseEvent e) {
    return INPUT_CLASSIFIER.isContextMouseButtonDown(e);
  }

  /**
   * @return whether the event effectively has Control down. 
   * 
   * @deprecated The situation where this was needed with mouse events is now handled by {@link #isSelectionToggle(MouseEvent)}.
   */
  @Deprecated
  public static boolean isControlDown(MouseEvent e) {
    return INPUT_CLASSIFIER.isSelectionToggle(e);
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
  public static boolean isMainMouseButtonDown(MouseEvent e) {
    return INPUT_CLASSIFIER.isMainMouseButtonDown(e);
  }
  
  /**
   * @return whether the event has the key/mouse combo toggling targets in and out of selection (normally Ctrl+Click on most platforms, normally Command+Click on Mac)
   */  
  public static boolean isSelectionToggle(MouseEvent e) {
    return INPUT_CLASSIFIER.isSelectionToggle(e);
  }

  /**
   * @param e Key event
   * @return true if CTRL+keystroke on non-Mac or Command+keystroke on Mac
   */
  public static boolean isModifierKeyDown(KeyEvent e) {
    return INPUT_CLASSIFIER.isModifierKeyDown(e);
  }
  
  /**
   * @return translation of keystroke from local system to platform-independent Vassal keystroke (to handle Mac platform support)
   * 
   * The main idea here is that on Macs we want the common shortcut keys represented by e.g. Ctrl+C on 
   * Windows and Linux platforms to be represented by Command+C on the Mac, and likewise when module
   * designers on Mac implement a Command+C shortcut we want that to appear as Ctrl+C for the same module
   * running on other platforms. But we also support a "legacy" preference to allow Mac users used to 
   * Vassal 3.2.17 and prior mappings to continue with them.
   */
  public static KeyStroke systemToGeneric(KeyStroke k) {
    return INPUT_CLASSIFIER.systemToGeneric(k);
  }  

  /**
   * @return translation of keystroke from platform-independent Vassal to local system (to handle Mac platform support)
   * 
   * The main idea here is that on Macs we want the common shortcut keys represented by e.g. Ctrl+C on 
   * Windows and Linux platforms to be represented by Command+C on the Mac, and likewise when module
   * designers on Mac implement a Command+C shortcut we want that to appear as Ctrl+C for the same module
   * running on other platforms. But we also support a "legacy" preference to allow Mac users used to 
   * Vassal 3.2.17 and prior mappings to continue with them.
   */
  public static KeyStroke genericToSystem(KeyStroke k) {
    return INPUT_CLASSIFIER.genericToSystem(k);
  }    
  
  /**
   * @return translation of KeyEvent (local system) to Vassal (to handle Mac platform support)
   * 
   * The main idea here is that on Macs we want the common shortcut keys represented by e.g. Ctrl+C on 
   * Windows and Linux platforms to be represented by Command+C on the Mac, and likewise when module
   * designers on Mac implement a Command+C shortcut we want that to appear as Ctrl+C for the same module
   * running on other platforms. But we also support a "legacy" preference to allow Mac users used to 
   * Vassal 3.2.17 and prior mappings to continue with them.
   */
  public static KeyStroke getKeyStrokeForEvent (KeyEvent e) {
    return systemToGeneric(KeyStroke.getKeyStrokeForEvent(e));
  }

  /**
   * @return whether the drag is non-mouse or effectively from the left button
   */
  public static boolean isDragTrigger(DragGestureEvent e) {
    // NB: Will any non-mouse drags happen? Not sure, but as we never checked
    // for them before, this won't change behavior by excluding them.
    final InputEvent te = e.getTriggerEvent();
    return !(te instanceof MouseEvent) || isMainMouseButtonDown((MouseEvent) te);
  }

  /**
   * @return size of screen accounting for the screen insets (e.g., Windows
   * taskbar)
   */
  public static Rectangle getScreenBounds(Component c) {
    final Rectangle bounds =
      new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
    final GraphicsConfiguration config = c.getGraphicsConfiguration();
    final Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(config);
    bounds.translate(insets.left, insets.top);
    bounds.setSize(bounds.width - insets.left - insets.right,
                   bounds.height - insets.top - insets.bottom);
    return bounds;
  }
}
