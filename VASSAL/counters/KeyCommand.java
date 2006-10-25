/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import javax.swing.AbstractAction;
import javax.swing.KeyStroke;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;

public class KeyCommand extends AbstractAction {
  private String name;
  private KeyStroke stroke;
  private GamePiece target;
  private boolean global;
  private boolean enabled = true;


  public KeyCommand(String name, KeyStroke key, GamePiece target) {
    super(key == null ? name : name + "  " + HotKeyConfigurer.getString(key));
    this.target = target;
    this.name = name;
    this.stroke = key;
  }

  public KeyCommand(String name, KeyStroke key, GamePiece target, boolean enabled) {
    this(name, key, target);
    setEnabled(enabled);
  }
  
  public String getName() {
    return name;
  }

  public boolean matches(KeyStroke key) {
    return isEnabled() && key != null && key.equals(stroke);
  }

  public KeyStroke getKeyStroke() {
    return stroke;
  }

  public GamePiece getTarget() {
    return target;
  }

  public boolean isEnabled() {
    return enabled;
  }
  
  public void setEnabled(boolean b) {
    enabled = b;
  }
  
  /**
   * If true, then this action will apply to all selected pieces
   * @return
   */
  public boolean isGlobal() {
    return global;
  }

  /**
   * If true, then this action will apply to all selected pieces
   * @param global
   */
  public void setGlobal(boolean global) {
    this.global = global;
  }

  public void actionPerformed(java.awt.event.ActionEvent evt) {
    if (global) {
      GameModule.getGameModule().sendAndLog(KeyBuffer.getBuffer().keyCommand(stroke));
    }
    else {
      BoundsTracker t = new BoundsTracker();
      GamePiece outer = Decorator.getOutermost(target);
      t.addPiece(outer);
      outer.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(outer)); // save state prior to command
      Command c = outer.keyEvent(stroke);
      if (target.getId() != null) {
        GameModule.getGameModule().sendAndLog(c);
      }
      t.addPiece(outer);
      t.repaint();
    }
  }
}


