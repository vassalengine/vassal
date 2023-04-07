/*
 * Copyright (c) 2021 by The VASSAL Development team
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

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP0;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP1;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP2;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP3;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP4;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP5;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP6;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP7;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP8;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP9;

/**
 * Allows creation of an "attachment" to one or more other pieces, which can then be sent GKCs very swiftly and whose
 * properties can be easily read.
 */
public class Attachment extends Decorator implements TranslatablePiece, RecursionLimiter.Loopable {
  public static final String ID = "attach;"; // NON-NLS
  public static final String ATTACH_NAME = "AttachName"; //NON-NLS
  public static final String ATTACH_LIST = "AttachList"; //NON-NLS
  public static final String ATTACH_COUNT = "AttachCount"; //NON-NLS

  protected String attachName;
  protected String desc;

  protected List<GamePiece> contents = new ArrayList<>();

  protected GlobalCommandTarget target = new GlobalCommandTarget(GlobalCommandTarget.GKCtype.COUNTER);
  protected KeyCommand[] command;
  protected String attachCommandName;
  protected String clearCommandName;
  protected NamedKeyStroke attachKey;
  protected NamedKeyStroke clearKey;
  protected GlobalAttach globalCommand = new GlobalAttach(this);
  protected PropertyExpression propertiesFilter = new PropertyExpression();
  protected boolean restrictRange;
  protected boolean fixedRange = true;
  protected int range;
  protected String rangeProperty = "";
  private KeyCommand myAttachCommand;
  private KeyCommand myClearCommand;
  protected String description;


  public Attachment() {
    this(ID, null);
  }

  public Attachment(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');

    attachName = st.nextToken();
    desc = st.nextToken();

    attachCommandName = st.nextToken(Resources.getString("Editor.Attachment.attach_command"));
    attachKey = st.nextNamedKeyStroke(null);
    clearCommandName = st.nextToken(Resources.getString("Editor.Attachment.clear_command"));
    clearKey = st.nextNamedKeyStroke(null);
    propertiesFilter.setExpression(st.nextToken(""));
    restrictRange = st.nextBoolean(false);
    range = st.nextInt(1);
    fixedRange = st.nextBoolean(true);
    rangeProperty = st.nextToken("");
    description = st.nextToken("");
    globalCommand.setSelectFromDeckExpression(st.nextToken("-1"));
    target.decode(st.nextToken(""));
    target.setGKCtype(GlobalCommandTarget.GKCtype.COUNTER);
    target.setCurPiece(this);

    command = null;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(attachName)
      .append(desc)
      .append(attachCommandName)
      .append(attachKey)
      .append(clearCommandName)
      .append(clearKey)
      .append(propertiesFilter)
      .append(restrictRange)
      .append(range)
      .append(fixedRange)
      .append(rangeProperty)
      .append(globalCommand.getSelectFromDeckExpression())
      .append(target.encode());
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      myAttachCommand = new KeyCommand(attachCommandName, attachKey, Decorator.getOutermost(this), this);
      myClearCommand  = new KeyCommand(clearCommandName, clearKey, Decorator.getOutermost(this), this);
      if (attachCommandName.length() > 0 && attachKey != null && !attachKey.isNull()) {
        if (clearCommandName.length() > 0 && clearKey != null && !clearKey.isNull()) {
          command = new KeyCommand[]{myAttachCommand, myClearCommand};
        }
        else {
          command = new KeyCommand[]{myAttachCommand};
        }
      }
      else if (clearCommandName.length() > 0 && clearKey != null && !clearKey.isNull()) {
        command = new KeyCommand[]{myClearCommand};
      }
      else {
        command = KeyCommand.NONE;
      }
    }
    for (final KeyCommand c : command) {
      c.setEnabled(getMap() != null);
    }
    return command;
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');

    se.append(contents.size());
    for (final GamePiece p : contents) {
      se.append(p.getId());
    }

    return se.getValue();
  }

  public Command attach() {

    ///
  }

  public Command clear() {
    if (contents.isEmpty()) {
      return new NullCommand();
    }

    final ChangeTracker ct = new ChangeTracker(this);
    contents.clear();
    return ct.getChangeCommand();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();

    if (myAttachCommand.matches(stroke)) {
      return attach();
    }
    else if (myClearCommand.matches(stroke)) {
      return clear();
    }

    return null;
  }

  public List<GamePiece> getContents() {
    return new ArrayList<>(contents);
  }


  @Override
  public void mySetState(String newState) {
    contents.clear();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, ';');
    final int num = st.nextInt(0);
    final GameState gs = GameModule.getGameModule().getGameState();
    for (int i = 0; i < num; i++) {
      final GamePiece piece = gs.getPieceForId(st.nextToken());
      //BR// getPieceForId can return null, and WILL during load-game if target piece hasn't loaded yet.
      if (piece != null) {
        contents.add(piece);
      }
    }

    //GameModule.getGameModule().setMatSupport(true);
  }

  /**
   * @param p a particular gamepiece
   * @return true if the given piece is on our list of targets
   */
  public boolean hasTarget(GamePiece p) {
    return contents.contains(p);
  }

  public int getTargetCount() {
    return contents.size();
  }

  /**
   * Adds a target and returns a command to duplicate the operation on another client
   * @param p target to add
   * @return Command that adds the target
   */
  public Command makeAddTargetCommand(GamePiece p) {
    if (hasTarget(p)) {
      return null;
    }

    final ChangeTracker ct = new ChangeTracker(this);
    contents.add(p);
    return ct.getChangeCommand();
  }

  /**
   * Removes a piece from our list of targets
   * @param p Cargo to remove
   */
  public void removeTarget(GamePiece p) {
    if ((p instanceof Decorator) && hasTarget(p)) {
      contents.remove(p);
    }
  }

  /**
   * Removes a piece from our list of targets, and returns a Command to duplicate the changes on another client
   * @param p targetGamePiece to be removed
   * @return Command to remove the piece
   */
  public Command makeRemoveTargetCommand(GamePiece p) {
    if (!hasTarget(p)) {
      return null;
    }
    final ChangeTracker ct  = new ChangeTracker(this);
    removeTarget(p);
    return ct.getChangeCommand();
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
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.Attachment.trait_description", attachName, desc);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.Attachment.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return desc;
  }

  @Override
  public Object getProperty(Object key) {
    if (ATTACH_NAME.equals(key)) {
      return attachName;
    }
    else if (ATTACH_LIST.equals(key)) {
      return new ArrayList<>(contents);
    }
    else if (ATTACH_COUNT.equals(key)) {
      return String.valueOf(contents.size());
    }
    else if (!attachName.isEmpty() && (key instanceof String)) {
      final String k = (String)key;
      if ((k.length() > attachName.length()) && k.startsWith(attachName)) {

      }
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (ATTACH_NAME.equals(key)) {
      return attachName;
    }
    else if (ATTACH_COUNT.equals(key)) {
      return String.valueOf(contents.size());
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public void setProperty(Object key, Object value) {
    if (ATTACH_NAME.equals(key)) {
      attachName = (String) value;
      return;
    }
    super.setProperty(key, value);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Attachment)) return false;
    final Attachment c = (Attachment) o;
    if (!Objects.equals(attachName, c.attachName)) return false;
    if (!Objects.equals(desc, c.desc)) return false;
    if (!Objects.equals(attachCommandName, c.attachCommandName)) return false;
    if (!Objects.equals(attachKey, c.attachKey)) return false;
    if (!Objects.equals(clearCommandName, c.clearCommandName)) return false;
    if (!Objects.equals(clearKey, c.clearKey)) return false;
    if (!Objects.equals(propertiesFilter.getExpression(), c.propertiesFilter.getExpression())) return false;
    if (!Objects.equals(restrictRange, c.restrictRange)) return false;
    if (!Objects.equals(range, c.range)) return false;
    if (!Objects.equals(fixedRange, c.fixedRange)) return false;
    if (!Objects.equals(rangeProperty, c.rangeProperty)) return false;
    if (!Objects.equals(target, c.target)) return false;
    return Objects.equals(globalCommand.getSelectFromDeckExpression(), c.globalCommand.getSelectFromDeckExpression());
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Attachment.html"); // NON-NLS
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    return Arrays.asList(ATTACH_NAME, ATTACH_COUNT);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer matNameInput;
    private final StringConfigurer descInput;
    private final TraitConfigPanel controls;

    public Ed(Attachment p) {
      controls = new TraitConfigPanel();

      matNameInput = new StringConfigurer(p.matName);
      matNameInput.setHintKey("Editor.Mat.name_hint");
      controls.add("Editor.Mat.name_label", matNameInput);

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);
    }


    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(matNameInput.getValueString());
      se.append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
