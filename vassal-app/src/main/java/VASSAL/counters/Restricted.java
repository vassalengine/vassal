/*
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

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Arrays;
import java.util.Objects;

import javax.swing.KeyStroke;

/**
 * A GamePiece with the Restricted trait can only be manipulated by the player playing a specific side
 */
public class Restricted extends Decorator implements EditablePiece {
  public static final String ID = "restrict;"; // NON-NLS
  private String[] side;
  private boolean restrictByPlayer;
  private String owningPlayer = "";
  private boolean restrictMovement = true;
  private static PlayerRoster.SideChangeListener handleRetirement;
  private String description = "";

  public Restricted() {
    this(ID, null);
  }

  public Restricted(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
    if (handleRetirement == null) {
      handleRetirement = new RetirementHandler();
      GameModule.getGameModule().addSideChangeListenerToPlayerRoster(handleRetirement);
    }
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.Restricted.trait_description", description);
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RestrictedAccess.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    side = st.nextStringArray(0);
    restrictByPlayer = st.nextBoolean(false);
    restrictMovement = st.nextBoolean(true);
    description = st.nextToken("");
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
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
      for (final String s : side) {
        if (s.equals(PlayerRoster.getMySide())) {
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
    }
    super.setProperty(key, val);
  }

  @Override
  protected KeyCommand[] getKeyCommands() {
    return isRestricted() ? KeyCommand.NONE : super.getKeyCommands();
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.RESTRICTED.equals(key)) {
      return isRestricted();
    }
    else if (Properties.RESTRICTED_MOVEMENT.equals(key)) {
      return isRestricted() && restrictMovement;
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (Properties.RESTRICTED.equals(key)) {
      return isRestricted();
    }
    else if (Properties.RESTRICTED_MOVEMENT.equals(key)) {
      return isRestricted() && restrictMovement;
    }
    else {
      return super.getProperty(key);
    }
  }

  @Override
  public String myGetState() {
    return owningPlayer;
  }

  @Override
  public String myGetType() {
    return ID + new SequenceEncoder(';').append(side).append(restrictByPlayer).append(restrictMovement).append(description).getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public Command keyEvent(KeyStroke stroke) {
    if (!isRestricted()) {
      return super.keyEvent(stroke);
    }
    else {
      return null;
    }
  }

  @Override
  public void mySetState(String newState) {
    owningPlayer = newState;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Restricted)) return false;
    final Restricted c = (Restricted) o;
    if (!Arrays.equals(side, c.side)) return false;
    if (! Objects.equals(restrictByPlayer, c.restrictByPlayer)) return false;
    if (! Objects.equals(restrictMovement, c.restrictMovement)) return false;
    return Objects.equals(owningPlayer, c.owningPlayer);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }


  public static class Ed implements PieceEditor {
    private final BooleanConfigurer byPlayer;
    private final StringArrayConfigurer config;
    private final BooleanConfigurer movementConfig;
    private final TraitConfigPanel box;
    private final StringConfigurer descInput;

    public Ed(Restricted r) {

      box = new TraitConfigPanel();

      descInput = new StringConfigurer(r.description);
      descInput.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descInput);

      config = new StringArrayConfigurer(r.side);
      config.setHintKey("Editor.PieceAccessConfigurer.side_hint");
      box.add("Editor.Restricted.belongs_to_side", config);

      byPlayer = new BooleanConfigurer(r.restrictByPlayer);
      box.add("Editor.Restricted.also_belongs", byPlayer);

      movementConfig = new BooleanConfigurer(r.restrictMovement);
      box.add("Editor.Restricted.prevent_non_owning", movementConfig);
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      return ID + new SequenceEncoder(';').append(config.getValueString()).append(byPlayer.booleanValue()).append(movementConfig.booleanValue()).append(descInput.getValueString()).getValue();
    }
  }
  /**
   * When a player changes sides to become an observer, relinquish ownership of all pieces
   * @author rodneykinney
   *
   */
  private static class RetirementHandler implements PlayerRoster.SideChangeListener, PieceVisitor {

    @Override
    public void sideChanged(String oldSide, String newSide) {
      if (newSide == null) {
        final PieceVisitorDispatcher d = new PieceVisitorDispatcher(this);
        Command c = new NullCommand();
        for (final Map m : GameModule.getGameModule().getComponentsOf(Map.class)) {
          for (final GamePiece piece : m.getPieces()) {
            c = c.append((Command)d.accept(piece));
          }
        }
        GameModule.getGameModule().sendAndLog(c);
      }
    }

    @Override
    public Object visitDefault(GamePiece p) {
      final Restricted r = (Restricted)Decorator.getDecorator(p, Restricted.class);
      if (r != null
          && r.restrictByPlayer
          && GameModule.getUserId().equals(r.owningPlayer)) {

        final ChangeTracker t = new ChangeTracker(p);
        r.owningPlayer = "";
        return t.getChangeCommand();
      }
      return null;
    }

    @Override
    public Object visitStack(Stack s) {
      Command c = new NullCommand();
      for (final GamePiece gamePiece : s.asList()) {
        c = c.append((Command)visitDefault(gamePiece));
      }
      return c;
    }

  }
}
