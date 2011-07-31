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
import java.util.Iterator;

import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * A GamePiece with the Restricted trait can only be manipulated by the player playing a specific side
 */
public class Restricted extends Decorator implements EditablePiece {
  public static final String ID = "restrict;";
  private String[] side;
  private boolean restrictByPlayer;
  private String owningPlayer="";
  private boolean restrictMovement = true;
  private static PlayerRoster.SideChangeListener handleRetirement;

  public Restricted() {
    this(ID, null);
  }

  public Restricted(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
    if (handleRetirement == null) {
      handleRetirement = new RetirementHandler();
      PlayerRoster.addSideChangeListener(handleRetirement);
    }
  }

  public String getDescription() {
    return "Restricted Access";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RestrictedAccess.htm");
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type,';');
    side = st.nextStringArray(0);
    restrictByPlayer = st.nextBoolean(false);
    restrictMovement = st.nextBoolean(true);
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
    boolean restricted = false;
    if (restrictByPlayer) {
      restricted = owningPlayer.length() > 0 && !GameModule.getUserId().equals(owningPlayer);
    }
    if ((restricted || !restrictByPlayer)
        && PlayerRoster.isActive()
        && GameModule.getGameModule().getGameState().isGameStarted()) {
      restricted = true;
      for (int i = 0; i < side.length; ++i) {
        if (side[i].equals(PlayerRoster.getMySide())) {
          restricted = false;
          break;
        }
      }
    }
    return restricted;
  }

/*  @Override
  public void setMap(Map m) {
    if (m != null && restrictByPlayer && owningPlayer.length() == 0) {
      owningPlayer = GameModule.getUserId();
    }
    super.setMap(m);
  }
*/
  @Override
  public void setProperty(Object key, Object val) {
    if (Properties.SELECTED.equals(key) && Boolean.TRUE.equals(val) && restrictByPlayer && owningPlayer.length() == 0) {
      if (getMap() != null) {
        owningPlayer = GameModule.getUserId();
      }
      else {
        System.err.println("Selected, but map == null");
      }
    }
    super.setProperty(key, val);
  }

  protected KeyCommand[] getKeyCommands() {
    if (!isRestricted()) {
      return super.getKeyCommands();
    }
    else {
      return new KeyCommand[0];
    }
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.RESTRICTED.equals(key)) {
      return Boolean.valueOf(isRestricted());
    }
    else if (Properties.RESTRICTED_MOVEMENT.equals(key)) {
      return Boolean.valueOf(isRestricted() && restrictMovement);
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  public Object getProperty(Object key) {
    if (Properties.RESTRICTED.equals(key)) {
      return Boolean.valueOf(isRestricted());
    }
    else if (Properties.RESTRICTED_MOVEMENT.equals(key)) {
      return Boolean.valueOf(isRestricted() && restrictMovement);
    }
    else {
      return super.getProperty(key);
    }
  }

  public String myGetState() {
    return owningPlayer;
  }

  public String myGetType() {
    return ID + new SequenceEncoder(';').append(side).append(restrictByPlayer).append(restrictMovement).getValue();
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
    owningPlayer = newState;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {
    private BooleanConfigurer byPlayer;
    private StringArrayConfigurer config;
    private BooleanConfigurer movementConfig;
    private Box box;

    public Ed(Restricted r) {
      byPlayer = new BooleanConfigurer(null,"Also belongs to initially-placing player?",r.restrictByPlayer);
      config = new StringArrayConfigurer(null, "Belongs to side", r.side);
      movementConfig = new BooleanConfigurer(null, "Prevent non-owning players from moving piece?", r.restrictMovement);
      box = Box.createVerticalBox();
      ((JComponent)byPlayer.getControls()).setAlignmentX(Box.LEFT_ALIGNMENT);
      ((JComponent)movementConfig.getControls()).setAlignmentX(Box.LEFT_ALIGNMENT);
      box.add(config.getControls());
      box.add(byPlayer.getControls());
      box.add(movementConfig.getControls());
    }

    public Component getControls() {
      return box;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      return ID + new SequenceEncoder(';').append(config.getValueString()).append(byPlayer.booleanValue()).append(movementConfig.booleanValue()).getValue();
    }
  }
  /**
   * When a player changes sides to become an observer, relinquish ownership of all pieces
   * @author rodneykinney
   *
   */
  private static class RetirementHandler implements PlayerRoster.SideChangeListener, PieceVisitor {

    public void sideChanged(String oldSide, String newSide) {
      if (newSide == null) {
        PieceVisitorDispatcher d = new PieceVisitorDispatcher(this);
        Command c = new NullCommand();
        for(Map m : GameModule.getGameModule().getComponentsOf(Map.class)) {
          for (GamePiece piece : m.getPieces()) {
            c = c.append((Command)d.accept(piece));
          }
        }
        GameModule.getGameModule().sendAndLog(c);
      }
    }

    public Object visitDefault(GamePiece p) {
      Restricted r = (Restricted)Decorator.getDecorator(p, Restricted.class);
      if (r != null
          && r.restrictByPlayer
          && GameModule.getUserId().equals(r.owningPlayer)) {

        ChangeTracker t = new ChangeTracker(p);
        r.owningPlayer = "";
        return t.getChangeCommand();
      }
      return null;
    }

    public Object visitStack(Stack s) {
      Command c = new NullCommand();
      for (Iterator<GamePiece> it = s.getPiecesIterator(); it.hasNext();) {
        c = c.append((Command)visitDefault(it.next()));
      }
      return c;
    }

  }
}
