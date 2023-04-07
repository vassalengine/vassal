/*
 * Copyright (c) 2023 by The VASSAL Development team, Brian Reynolds
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
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.AuditTrail;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;

import javax.swing.JLabel;
import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Trait allowing creation of an "attachment" to one or more other pieces, which can then be sent GKCs very swiftly and whose
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

  public Attachment() {
    this(ID + ";", null);
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
    final GamePiece outer = Decorator.getOutermost(this);
    globalCommand.setPropertySource(outer); // Doing this here ensures trait is linked into GamePiece before finding source
    final AuditTrail audit = AuditTrail.create(this, propertiesFilter.getExpression(), Resources.getString("Editor.GlobalKeyCommand.matching_properties"));
    PieceFilter filter = propertiesFilter.getFilter(outer, this, audit);
    Command c = new NullCommand();
    if (restrictRange) {
      int r = range;
      if (!fixedRange) {
        final String rangeValue = (String) Decorator.getOutermost(this).getProperty(rangeProperty);
        try {
          r = Integer.parseInt(rangeValue);
        }
        catch (NumberFormatException e) {
          reportDataError(this, Resources.getString("Error.non_number_error"), "range[" + rangeProperty + "]=" + rangeValue, e); // NON-NLS
        }
      }
      filter = new BooleanAndPieceFilter(filter, new RangeFilter(getMap(), getPosition(), r));
    }

    c = c.append(globalCommand.apply(Map.getMapList().toArray(new Map[0]), filter, target, audit));

    return c;
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
    String d = Resources.getString("Editor.Attachment.trait_description");
    if (desc.length() > 0) {
      d += " - " + desc;
    }

    if (attachKey != null) {
      d += getCommandDesc(attachCommandName, attachKey);
    }

    if (clearKey != null) {
      d += getCommandDesc(clearCommandName, clearKey);
    }

    return d;
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
    else if (!attachName.isEmpty() && (key instanceof String) && !contents.isEmpty()) {
      // If property name starts with our non-blank name plus an underscore, use the rest of the string as a property key for our first *target* piece instead.
      final String k = (String)key;
      if ((k.length() > attachName.length() + 1) && k.startsWith(attachName) && (k.charAt(attachName.length()) == '_')) {
        return contents.get(0).getProperty(k.substring(attachName.length() + 1));
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
    else if (!attachName.isEmpty() && (key instanceof String) && !contents.isEmpty()) {
      // If property name starts with our non-blank name plus an underscore, use the rest of the string as a property key for our first *target* piece instead.
      final String k = (String)key;
      if ((k.length() > attachName.length() + 1) && k.startsWith(attachName) && (k.charAt(attachName.length()) == '_')) {
        return contents.get(0).getLocalizedProperty(k.substring(attachName.length() + 1));
      }
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
    private final StringConfigurer attachNameInput;
    private final StringConfigurer descInput;
    private final TraitConfigPanel traitPanel;
    private final StringConfigurer attachCommandNameInput;
    protected NamedHotKeyConfigurer attachKeyInput;
    private final StringConfigurer clearCommandNameInput;
    protected NamedHotKeyConfigurer clearKeyInput;

    protected PropertyExpressionConfigurer propertyMatch;
    protected MassKeyCommand.DeckPolicyConfig deckPolicy;
    protected BooleanConfigurer restrictRange;
    protected BooleanConfigurer fixedRange;
    protected JLabel fixedRangeLabel;
    protected IntConfigurer range;
    protected JLabel rangeLabel;
    protected StringConfigurer rangeProperty;
    protected JLabel rangePropertyLabel;

    protected GlobalCommandTargetConfigurer targetConfig;

    protected Attachment attachment;


    public Ed(Attachment p) {
      final PropertyChangeListener pl = evt -> {

        final boolean isRange = Boolean.TRUE.equals(restrictRange.getValue());
        final boolean isFixed = Boolean.TRUE.equals(fixedRange.getValue());

        range.getControls().setVisible(isRange && isFixed);
        rangeLabel.setVisible(isRange && isFixed);
        fixedRange.getControls().setVisible(isRange);
        fixedRangeLabel.setVisible(isRange);
        rangeProperty.getControls().setVisible(isRange && !isFixed);
        rangePropertyLabel.setVisible(isRange && !isFixed);

        repack(range);
      };

      attachment = p;
      traitPanel = new TraitConfigPanel();

      attachNameInput = new StringConfigurer(p.attachName);
      attachNameInput.setHintKey("Editor.Attachment.name_hint");
      traitPanel.add("Editor.Attachment.name_label", attachNameInput);

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      traitPanel.add("Editor.description_label", descInput);

      attachCommandNameInput = new StringConfigurer(p.attachCommandName);
      attachCommandNameInput.setHintKey("Editor.menu_command_hint");
      traitPanel.add("Editor.Attachment.attach_menu_command", attachCommandNameInput);

      attachKeyInput = new NamedHotKeyConfigurer(p.attachKey);
      traitPanel.add("Editor.Attachment.attach_key_command", attachKeyInput);

      clearCommandNameInput = new StringConfigurer(p.clearCommandName);
      clearCommandNameInput.setHintKey("Editor.menu_command_hint");
      traitPanel.add("Editor.Attachment.clear_menu_command", clearCommandNameInput);

      clearKeyInput = new NamedHotKeyConfigurer(p.clearKey);
      traitPanel.add("Editor.Attachment.clear_key_command", clearKeyInput);

      targetConfig = new GlobalCommandTargetConfigurer(p.target);
      traitPanel.add("Editor.GlobalKeyCommand.pre_select", targetConfig);

      propertyMatch = new PropertyExpressionConfigurer(p.propertiesFilter);
      traitPanel.add("Editor.GlobalKeyCommand.matching_properties", propertyMatch);

      deckPolicy = new MassKeyCommand.DeckPolicyConfig(false);
      deckPolicy.setValue(p.globalCommand.getSelectFromDeckExpression());
      traitPanel.add("Editor.GlobalKeyCommand.deck_policy", deckPolicy);

      restrictRange = new BooleanConfigurer(p.restrictRange);
      traitPanel.add("Editor.GlobalKeyCommand.restrict_range", restrictRange);
      restrictRange.addPropertyChangeListener(pl);

      fixedRange = new BooleanConfigurer(p.fixedRange);
      fixedRangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.fixed_range"));
      traitPanel.add(fixedRangeLabel, fixedRange);
      fixedRange.addPropertyChangeListener(pl);

      range = new IntConfigurer(p.range);
      rangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range"));
      traitPanel.add(rangeLabel, range);

      rangeProperty = new StringConfigurer(p.rangeProperty);
      rangeProperty.setHintKey("Editor.GlobalKeyCommand.range_property_hint");
      rangePropertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range_property"));
      traitPanel.add(rangePropertyLabel, rangeProperty);

      pl.propertyChange(null);
    }

    @Override
    public Component getControls() {
      return traitPanel;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(attachNameInput.getValueString())
        .append(descInput.getValueString())
        .append(attachCommandNameInput.getValueString())
        .append(attachKeyInput.getValueString())
        .append(clearCommandNameInput.getValueString())
        .append(clearKeyInput.getValueString())
        .append(propertyMatch.getValueString())
        .append(restrictRange.getValueString())
        .append(range.getValueString())
        .append(fixedRange.getValueString())
        .append(rangeProperty.getValueString())
        .append(deckPolicy.getValueString())
        .append(targetConfig.getValueString());

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return attachment.myGetState();
    }
  }
}
