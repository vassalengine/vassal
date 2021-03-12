/*
 *
 * Copyright (c) 2008-2009 Brent Easton
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
package VASSAL.script;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.PrivateMap;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.WarningDialog;

import bsh.EvalError;
import bsh.Interpreter;
import bsh.NameSpace;
import bsh.UtilEvalError;

import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

public abstract class AbstractInterpreter extends Interpreter {

  protected static final String THIS = "_interp"; //NON-NLS
  protected static final String SOURCE = "_source"; //NON-NLS
  private static final long serialVersionUID = 1L;

  protected NameSpace myNameSpace;

  /**
   * Set a variable and handle exceptions
   *
   * @param name
   *          Variable name
   * @param value
   *          Variable value
   */
  protected void setVar(String name, Object value) {
    try {
      set(name, value);
    }
    catch (EvalError e) {
      // FIXME: Error message
      WarningDialog.show(e, "");
    }
  }


  protected void setVar(String name, int value) {
    try {
      set(name, value);
    }
    catch (EvalError e) {
      // FIXME: Error message
      WarningDialog.show(e, "");
    }
  }

  protected void setVar(String name, float value) {
    try {
      set(name, value);
    }
    catch (EvalError e) {
      // FIXME: Error message
      WarningDialog.show(e, "");
    }
  }

  protected void setVar(String name, boolean value) {
    try {
      set(name, value);
    }
    catch (EvalError e) {
      // FIXME: Error message
      WarningDialog.show(e, "");
    }
  }

  protected void setVar(String name, Class<?> cl, Object value) {
    try {
      getNameSpace().setTypedVariable(name, cl, value, null);
    }
    catch (UtilEvalError e) {
      // FIXME: Error message
      WarningDialog.show(e, "");
    }
  }

  /*
   * Call-backs from BeanShell Scripts
   */

  /**
   * Alert(message) Display a message in a dialog box/
   *
   * @param message message to display
   */
  public Object alert(String message) {
    JOptionPane.showMessageDialog(GameModule.getGameModule().getPlayerWindow(), message);
    return "";
  }

  /**
   * Get a module level property value.
   *
   * @param name
   *          Property Name
   * @return Property value
   */
  public Object getModuleProperty(String name) {
    return BeanShell.wrap(GameModule.getGameModule().getProperty(name)
        .toString());
  }

  /**
   * Set the value of a module level property
   *
   * @param name
   *          Property name
   * @param value
   *          new value
   */
  public void setModuleProperty(String name, String value) {
    GameModule.getGameModule().getMutableProperty(name).setPropertyValue(value);
  }

  /**
   * Return a proxy reference to the named map as long as it is accessible to
   * us.
   *
   * @param mapName
   *          Map Name
   * @return Map proxy
   */
  public VASSAL.script.proxy.Map findMap(String mapName) {
    Map map = null;
    for (final Map m : GameModule.getGameModule().getAllDescendantComponentsOf(
        Map.class)) {
      if (m.getMapName().equals(mapName) && isAccessible(m)) {
        map = m;
        break;
      }
    }
    return map == null ? null : new VASSAL.script.proxy.Map(map);
  }

  /**
   * Is a map accessible to us?
   *
   * @param m
   *          Map
   * @return true if accessible
   */
  protected boolean isAccessible(Map m) {
    if (m instanceof PrivateMap) {
      final String mySide = PlayerRoster.getMySide();
      if (mySide == null) {
        return true;
      }
      else {
        return ((PrivateMap) m).isAccessibleTo(mySide);
      }
    }
    else {
      return true;
    }
  }

  /**
   * Fire off a Global Hot Key
   *
   * @param stroke
   *          Keystroke
   */
  public void globalHotKey(KeyStroke stroke) {
    GameModule.getGameModule().fireKeyStroke(NamedKeyStroke.of(stroke));
  }
}
