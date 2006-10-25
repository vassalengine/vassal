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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jun 13, 2002
 * Time: 9:52:40 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import javax.swing.KeyStroke;
import VASSAL.build.GameModule;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringArrayConfigurer;

/**
 * A GamePiece with the Restricted trait can only be manipulated by the player playing a specific side
 */
public class Restricted extends Decorator implements EditablePiece {
  public static final String ID = "restrict;";
  private String[] side;

  public Restricted() {
    this(ID, null);
  }

  public Restricted(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  public String getDescription() {
    return "Restricted Access";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RestrictedAccess.htm");
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    side = StringArrayConfigurer.stringToArray(type);
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  public boolean isRestricted() {
    if (PlayerRoster.isActive()
      && GameModule.getGameModule().getGameState().isGameStarted()) {
      for (int i = 0; i < side.length; ++i) {
        if (side[i].equals(PlayerRoster.getMySide())) {
          return false;
        }
      }
      return true;
    }
    else {
      return false;
    }
  }

  protected KeyCommand[] getKeyCommands() {
    if (!isRestricted()) {
      return super.getKeyCommands();
    }
    else {
      return new KeyCommand[0];
    }
  }

  public Object getProperty(Object key) {
    if (Properties.RESTRICTED.equals(key)) {
      return new Boolean(isRestricted());
    }
    else {
      return super.getProperty(key);
    }
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return ID + StringArrayConfigurer.arrayToString(side);
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public Command keyEvent(KeyStroke stroke) {
    if (!isRestricted()) {
      return super.keyEvent(stroke);
    }
    else {
      return null;
    }
  }

  public void mySetState(String newState) {
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {
    private StringArrayConfigurer config;

    public Ed(Restricted r) {
      config = new StringArrayConfigurer(null, "Belongs to side", r.side);
    }

    public Component getControls() {
      return config.getControls();
    }

    public String getState() {
      return "";
    }

    public String getType() {
      return ID + config.getValueString();
    }
  }
}
