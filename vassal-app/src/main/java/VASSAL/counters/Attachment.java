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
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.AuditTrail;
import VASSAL.search.SearchTarget;
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
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Trait allowing creation of an "attachment" to one or more other pieces, which can then be sent GKCs very swiftly and whose
 * properties can be easily read (and if a Dynamic Property can also be set with a Set Global Property trait)
 */
public class Attachment extends Decorator implements TranslatablePiece, RecursionLimiter.Loopable {
  public static final String ID = "attach;"; // NON-NLS
  public static final String ATTACH_NAME = "AttachName"; //NON-NLS
  public static final String ATTACH_LIST = "AttachList"; //NON-NLS
  public static final String ATTACH_COUNT = "AttachCount"; //NON-NLS

  public static final String BEFORE_ATTACH_NOTHING = "nothing"; //NON-NLS
  public static final String BEFORE_ATTACH_CLEAR = "clear"; //NON-NLS
  protected static final String[] BEFORE_ATTACH_OPTIONS = {
    BEFORE_ATTACH_NOTHING, BEFORE_ATTACH_CLEAR
  };
  protected static final String[] BEFORE_ATTACH_KEYS = {
    "Editor.Attachment.leave_existing",
    "Editor.Attachment.clear_existing"
  };


  public static final String ON_ATTACH_NOTHING = "nothing"; //NON-NLS
  public static final String ON_ATTACH_FOLLOW_BACK = "follow"; //NON-NLS
  public static final String ON_ATTACH_ATTACH_ALL = "attachAll"; //NON-NLS
  protected static final String[] ON_ATTACH_OPTIONS = {
    ON_ATTACH_NOTHING, ON_ATTACH_FOLLOW_BACK, ON_ATTACH_ATTACH_ALL
  };
  protected static final String[] ON_ATTACH_KEYS = {
    "Editor.Attachment.no_additional_action",
    "Editor.Attachment.follow_back",
    "Editor.Attachment.attach_to_all"
  };


  public static final String ON_DETACH_NOTHING = "nothing"; //NON-NLS
  public static final String ON_DETACH_REMOVE  = "remove"; //NON-NLS
  protected static final String[] ON_DETACH_OPTIONS = {
    ON_DETACH_NOTHING, ON_DETACH_REMOVE
  };
  protected static final String[] ON_DETACH_KEYS = {
    "Editor.Attachment.no_additional_action",
    "Editor.Attachment.remove_incoming_attachment",
  };

  protected String attachName;
  protected String desc;

  protected List<GamePiece> contents = new ArrayList<>();

  protected GlobalCommandTarget target = new GlobalCommandTarget(GlobalCommandTarget.GKCtype.COUNTER);
  protected GlobalCommandTarget clearTarget = new GlobalCommandTarget(GlobalCommandTarget.GKCtype.COUNTER);
  protected KeyCommand[] command;
  protected String attachCommandName;
  protected NamedKeyStroke attachKey;
  protected GlobalAttach globalAttach = new GlobalAttach(this);
  protected PropertyExpression propertiesFilter = new PropertyExpression();
  protected boolean restrictRange;
  protected boolean fixedRange = true;
  protected int range;
  protected String rangeProperty = "";
  protected String clearAllCommandName;
  protected NamedKeyStroke clearAllKey;
  protected String clearMatchingCommandName;
  protected NamedKeyStroke clearMatchingKey;
  protected PropertyExpression clearMatchingFilter = new PropertyExpression();
  protected String onAttach = ON_ATTACH_NOTHING;
  protected String onDetach = ON_DETACH_NOTHING;
  protected String beforeAttach = BEFORE_ATTACH_CLEAR;
  protected boolean allowSelfAttach = false;
  protected boolean autoAttach = true;

  private KeyCommand myAttachCommand;
  private KeyCommand myClearAllCommand;
  private KeyCommand myClearMatchingCommand;

  private String attachCountName = "";

  private final GlobalDetach globalDetach = new GlobalDetach(this);

  public Attachment() {
    this(ID + ";", null);
  }

  public Attachment(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public boolean isAutoAttach() {
    return autoAttach;
  }

  public boolean isAllowSelfAttach() {
    return allowSelfAttach;
  }

  /**
   * Return the named attachment list on the supplied GamePiece
   * @param attachmentName Attachment name to look up
   * @return               List of attached pieces
   */
  public static List<GamePiece> getAttachList(GamePiece piece, String attachmentName) {
    GamePiece p = Decorator.getOutermost(piece);
    while (p instanceof Decorator) {
      if (p instanceof Attachment) {
        final Attachment a = (Attachment) p;
        if (a.getAttachName().equals(attachmentName)) {
          return a.getAttachList();
        }
      }
      p = ((Decorator) p).getInner();
    }
    return Collections.emptyList();
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');

    setAttachName(st.nextToken());
    desc = st.nextToken();

    attachCommandName = st.nextToken(Resources.getString("Editor.Attachment.attach_command"));
    attachKey = st.nextNamedKeyStroke(null);
    clearAllCommandName = st.nextToken(Resources.getString("Editor.Attachment.clear_all_command"));
    clearAllKey = st.nextNamedKeyStroke(null);
    propertiesFilter.setExpression(st.nextToken(""));
    restrictRange = st.nextBoolean(false);
    range = st.nextInt(1);
    fixedRange = st.nextBoolean(true);
    rangeProperty = st.nextToken("");
    globalAttach.setSelectFromDeckExpression(st.nextToken("-1"));
    target.decode(st.nextToken(""));
    target.setGKCtype(GlobalCommandTarget.GKCtype.COUNTER);
    target.setCurPiece(this);

    clearMatchingCommandName = st.nextToken(Resources.getString("Editor.Attachment.clear_matching_command"));
    clearMatchingKey = st.nextNamedKeyStroke(null);
    clearMatchingFilter.setExpression(st.nextToken(""));

    onAttach = st.nextToken(ON_ATTACH_NOTHING);
    onDetach = st.nextToken(ON_DETACH_NOTHING);
    beforeAttach = st.nextToken(BEFORE_ATTACH_CLEAR);
    allowSelfAttach = st.nextBoolean(false);
    autoAttach = st.nextBoolean(true);

    command = null;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(attachName)
      .append(desc)
      .append(attachCommandName)
      .append(attachKey)
      .append(clearAllCommandName)
      .append(clearAllKey)
      .append(propertiesFilter)
      .append(restrictRange)
      .append(range)
      .append(fixedRange)
      .append(rangeProperty)
      .append(globalAttach.getSelectFromDeckExpression())
      .append(target.encode())
      .append(clearMatchingCommandName)
      .append(clearMatchingKey)
      .append(clearMatchingFilter)
      .append(onAttach)
      .append(onDetach)
      .append(beforeAttach)
      .append(allowSelfAttach)
      .append(autoAttach);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (autoAttach) {
      return KeyCommand.NONE;
    }

    if (command == null) {
      myAttachCommand = new KeyCommand(attachCommandName, attachKey, Decorator.getOutermost(this), this);
      myClearAllCommand  = new KeyCommand(clearAllCommandName, clearAllKey, Decorator.getOutermost(this), this);
      myClearMatchingCommand = new KeyCommand(clearMatchingCommandName, clearMatchingKey, Decorator.getOutermost(this), this);

      final boolean doAttach = (attachCommandName.length() > 0) && attachKey != null && !attachKey.isNull();
      final boolean doClearAll = (clearAllCommandName.length() > 0) && clearAllKey != null && !clearAllKey.isNull();
      final boolean doClearMatching = (clearMatchingCommandName.length() > 0) && clearMatchingKey != null && !clearMatchingKey.isNull();

      if (doAttach) {
        if (doClearAll) {
          if (doClearMatching) {
            command = new KeyCommand[]{myAttachCommand, myClearMatchingCommand, myClearAllCommand};
          }
          else {
            command = new KeyCommand[]{myAttachCommand, myClearAllCommand};
          }
        }
        else if (doClearMatching) {
          command = new KeyCommand[]{myAttachCommand, myClearMatchingCommand};
        }
        else {
          command = new KeyCommand[]{myAttachCommand};
        }
      }
      else if (doClearAll) {
        if (doClearMatching) {
          command = new KeyCommand[]{myClearMatchingCommand, myClearAllCommand};
        }
        else {
          command = new KeyCommand[]{myClearAllCommand};
        }
      }
      else if (doClearMatching) {
        command = new KeyCommand[]{myClearMatchingCommand};
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

  /**
   * Runs a search using the provided filter (same as a Global Key Command), and any matching pieces become our attachments (stored in our "contents" list)
   *
   * @return Command to replicate action
   */
  public Command attach() {
    Command c = new NullCommand();

    if (BEFORE_ATTACH_CLEAR.equals(beforeAttach)) {
      c = c.append(clearAll());
    }

    final GamePiece outer = Decorator.getOutermost(this);
    globalAttach.setPropertySource(outer); // Doing this here ensures trait is linked into GamePiece before finding source

    // Make piece properties filter
    final AuditTrail audit = AuditTrail.create(this, propertiesFilter.getExpression(), Resources.getString("Editor.GlobalKeyCommand.matching_properties"));
    PieceFilter filter = propertiesFilter.getFilter(outer, this, audit);

    // Make a range filter if applicable
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

    // Now apply our filter globally & add any matching pieces as attachments
    c = c.append(globalAttach.apply(Map.getMapList().toArray(new Map[0]), filter, target, audit));

    return c;
  }

  /**
   * Clears all existing attachment(s)
   *
   * @return Command to replicate action
   */
  public Command clearAll() {
    Command c = new NullCommand();

    // Auto-attachments are handled locally be each client with no Commands expected.
    if (isAutoAttach()) {
      contents.clear();
      return c;
    }

    if (contents.isEmpty()) {
      return c;
    }

    final List<GamePiece> targets = getAttachList();
    for (final GamePiece target : targets) {
      c = c.append(makeRemoveTargetCommand(target));
    }

    final ChangeTracker ct = new ChangeTracker(this);
    contents.clear();
    c = c.append(ct.getChangeCommand());

    return c;
  }

  public Command clearMatching() {
    final GamePiece outer = Decorator.getOutermost(this);

    clearTarget.fastMatchLocation = true;
    clearTarget.fastMatchProperty = false;
    clearTarget.setTargetType(GlobalCommandTarget.Target.CURATTACH);
    clearTarget.setCurPiece(this);
    globalDetach.setPropertySource(outer); // Doing this here ensures trait is linked into GamePiece before finding source

    // Make piece properties filter
    final AuditTrail audit = AuditTrail.create(this, clearMatchingFilter.getExpression(), Resources.getString("Editor.GlobalKeyCommand.matching_properties"));
    final PieceFilter filter = clearMatchingFilter.getFilter(outer, this, audit);

    // Now apply our filter globally & add any matching pieces as attachments
    return globalDetach.apply(Map.getMapList().toArray(new Map[0]), filter, clearTarget, audit);
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();

    if (autoAttach) return null;

    if (myAttachCommand.matches(stroke)) {
      return attach();
    }
    else if (myClearAllCommand.matches(stroke)) {
      return clearAll();
    }
    else if (myClearMatchingCommand.matches(stroke)) {
      return clearMatching();
    }

    return null;
  }

  public List<GamePiece> getContents() {
    return new ArrayList<>(contents);
  }

  public void setContents(List<GamePiece> newContents) {
    contents = newContents;
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
  }

  /**
   * Auto-attach to a piece
   * 1. We must be in auto-attah mode
   * 2. Must not already be attached to the piece.
   * NOTE: Auto-attach is handled locally by each client, no Commands are generated
   * @param attach Attachment trait within the target piece
   */
  public void autoAttach(Attachment attach) {
    if (isAutoAttach() && attach.getAttachName().equals(getAttachName())) {
      final GamePiece piece = Decorator.getOutermost(attach);
      if (! hasTarget(piece)) {
        contents.add(piece);
      }
    }
  }

  /**
   * @param p a particular gamepiece
   * @return true if the given piece is on our list of targets
   */
  public boolean hasTarget(GamePiece p) {
    return contents.contains(p);
  }

  /**
   * Adds a target and returns a command to duplicate the operation on another client
   * @param p target to add
   * @return Command that adds the target
   */
  public Command makeAddTargetCommand(GamePiece p) {
    Command c = new NullCommand();

    if (!allowSelfAttach && Decorator.getOutermost(this).equals(Decorator.getOutermost(p))) {
      return c;
    }

    // Attach to our target, if we aren't attached.
    if (!hasTarget(p)) {
      final ChangeTracker ct = new ChangeTracker(this);
      contents.add(p);
      c = c.append(ct.getChangeCommand());

      // If our target has "on attach" conditions in an equivalently named Attachment, process them
      GamePiece target = Decorator.getOutermost(p);
      while (target instanceof Decorator) {
        if (target instanceof Attachment) {
          final Attachment targetAttach = (Attachment) target;
          if (attachName.equals(targetAttach.attachName)) {
            // Found an attachment w/ the same name
            if (autoAttach || !targetAttach.onAttach.equals(ON_ATTACH_NOTHING)) {
              // They're either ON_ATTACH_FOLLOW_BACK or ON_ATTACH_ATTACH_ALL, so we probably want at last at an attach-back command
              c = c.append(targetAttach.makeAddTargetCommand(Decorator.getOutermost(this)));

              // If they're ON_ATTACH_ATTACH_ALL then they attach to ALL of our previously attached pieces
              if (autoAttach || targetAttach.onAttach.equals(ON_ATTACH_ATTACH_ALL)) {
                for (final GamePiece other : getAttachList()) {
                  c = c.append(targetAttach.makeAddTargetCommand(Decorator.getOutermost(other)));
                }
              }
            }
          }
        }
        target = ((Decorator) target).getInner();
      }
    }

    return c;
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
    Command c = new NullCommand();

    if (hasTarget(p)) {
      final ChangeTracker ct = new ChangeTracker(this);
      removeTarget(p);
      c = c.append(ct.getChangeCommand());

      // If our detach condition is ON_DETACH_REMOVE, then remove incoming attachment at the same time
      if (onDetach.equals(ON_DETACH_REMOVE)) {
        GamePiece target = Decorator.getOutermost(p);
        while (target instanceof Decorator) {
          if (target instanceof Attachment) {
            final Attachment targetAttach = (Attachment) target;
            if (attachName.equals(targetAttach.attachName)) {
              c = c.append(targetAttach.makeRemoveTargetCommand(Decorator.getOutermost(this)));
            }
          }
          target = ((Decorator) target).getInner();
        }
      }
    }

    return c;
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

    d += " - " + attachName;

    if (autoAttach) {
      d += " " + Resources.getString("Editor.Attachment.auto");
    }

    if (desc.length() > 0) {
      d += " - " + desc;
    }

    if (!autoAttach) {
      if (attachKey != null) {
        d += getCommandDesc(attachCommandName, attachKey);
      }

      if (clearAllKey != null) {
        d += getCommandDesc(clearAllCommandName, clearAllKey);
      }

      if (clearMatchingKey != null) {
        d += getCommandDesc(clearMatchingCommandName, clearMatchingKey);
      }
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

  /**
   * @return first piece in our list of attachments that still exists (hasn't been deleted), or null if none
   */
  public GamePiece getPropertyPiece() {
    if (contents.isEmpty()) return null;

    for (final GamePiece piece : contents) {
      if (piece.getMap() == null) continue;
      return piece;
    }

    return null;
  }

  /**
   * Checks if a property reference could be a reference to a property on an attached piece
   * @param key Property name to be checked if it's an attachment reference (it's a reference if it is attachName_propertyName
   * @return propertyName for use in reading property from attached piece, if the key referenced a valid attachment property. (null otherwise)
   */
  public String translatePropertyName(Object key) {
    if (!attachName.isEmpty() && (key instanceof String) && !contents.isEmpty()) {
      // If property name starts with our non-blank name plus an underscore, use the rest of the string as a property key for our first *target* piece instead.
      final String k = (String)key;
      if ((k.length() > attachName.length() + 1) && k.startsWith(attachName) && (k.charAt(attachName.length()) == '_')) {
        return k.substring(attachName.length() + 1);
      }
    }
    return null;
  }

  public String getAttachName() {
    return attachName;
  }

  void setAttachName(String name) {
    attachName = name;
    attachCountName = name + "_" + ATTACH_COUNT;
  }

  /**
   * @return number of attached pieces, not counting any that have since been deleted (from the game).
   */
  public int getAttachCount() {
    int count = 0;
    for (final GamePiece piece : contents) {
      if (piece.getMap() == null) continue;
      count++;
    }
    return count;
  }

  /**
   * @param index index (0-based)
   * @return the attached piece at a given index (0-based)
   */
  public GamePiece getAttachedPieceAt(int index) {
    int count = 0;
    for (final GamePiece piece : contents) {
      if (piece.getMap() == null) continue;

      if (count == index) {
        return piece;
      }

      count++;
    }
    return null;
  }

  /**
   * @return A list of attached pieces, not counting any that have since been deleted (from the game)
   */
  public List<GamePiece> getAttachList() {
    final List<GamePiece> attachList = new ArrayList<>();
    for (final GamePiece p : contents) {
      if (p.getMap() == null) continue;
      attachList.add(p);
    }
    return attachList;
  }


  @Override
  public Object getProperty(Object key) {
    if (ATTACH_NAME.equals(key)) {
      return attachName;
    }
    else if (ATTACH_LIST.equals(key)) {
      return getAttachList();
    }
    else if (attachCountName.equals(key) || ATTACH_COUNT.equals(key)) {
      return String.valueOf(getAttachCount());
    }
    else {
      final String attachProp = translatePropertyName(key);
      if (attachProp != null) {
        final GamePiece propPiece = getPropertyPiece();
        if (propPiece != null) {
          final Object getIt = propPiece.getProperty(attachProp);
          if (getIt != null) return getIt;
        }
      }
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (ATTACH_NAME.equals(key)) {
      return attachName;
    }
    else if (attachCountName.equals(key) || ATTACH_COUNT.equals(key)) {
      return String.valueOf(getAttachCount());
    }
    else  {
      final String attachProp = translatePropertyName(key);
      if (attachProp != null) {
        final GamePiece propPiece = getPropertyPiece();
        if (propPiece != null) {
          final Object getIt = propPiece.getLocalizedProperty(attachProp);
          if (getIt != null) return getIt;
        }
      }
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public void setProperty(Object key, Object value) {
    if (ATTACH_NAME.equals(key)) {
      setAttachName((String)value);
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
    if (!Objects.equals(clearAllCommandName, c.clearAllCommandName)) return false;
    if (!Objects.equals(clearAllKey, c.clearAllKey)) return false;
    if (!Objects.equals(propertiesFilter.getExpression(), c.propertiesFilter.getExpression())) return false;
    if (!Objects.equals(restrictRange, c.restrictRange)) return false;
    if (!Objects.equals(range, c.range)) return false;
    if (!Objects.equals(fixedRange, c.fixedRange)) return false;
    if (!Objects.equals(rangeProperty, c.rangeProperty)) return false;
    if (!Objects.equals(target, c.target)) return false;
    if (!Objects.equals(globalAttach.getSelectFromDeckExpression(), c.globalAttach.getSelectFromDeckExpression())) return false;
    if (!Objects.equals(clearMatchingCommandName, c.clearMatchingCommandName)) return false;
    if (!Objects.equals(clearMatchingKey, c.clearMatchingKey)) return false;
    if (!Objects.equals(clearMatchingFilter.getExpression(), c.clearMatchingFilter.getExpression())) return false;
    if (!Objects.equals(onAttach, c.onAttach)) return false;
    if (!Objects.equals(onDetach, c.onDetach)) return false;
    if (!Objects.equals(beforeAttach, c.beforeAttach)) return false;
    if (!Objects.equals(autoAttach, c.autoAttach)) return false;
    return Objects.equals(allowSelfAttach, c.allowSelfAttach);
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
    final List<String> l = new ArrayList<>();
    l.add(ATTACH_COUNT);
    l.add(attachCountName);
    return l;
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer attachNameInput;
    private final StringConfigurer descInput;
    private final TraitConfigPanel traitPanel;
    private final StringConfigurer attachCommandNameInput;
    protected NamedHotKeyConfigurer attachKeyInput;
    private final StringConfigurer clearAllCommandNameInput;
    protected NamedHotKeyConfigurer clearAllKeyInput;
    private final StringConfigurer clearMatchingCommandNameInput;
    protected NamedHotKeyConfigurer clearMatchingKeyInput;

    protected PropertyExpressionConfigurer propertyMatch;
    protected PropertyExpressionConfigurer clearMatchingMatch;

    protected MassKeyCommand.DeckPolicyConfig deckPolicy;
    protected BooleanConfigurer restrictRange;
    protected BooleanConfigurer fixedRange;
    protected JLabel fixedRangeLabel;
    protected IntConfigurer range;
    protected JLabel rangeLabel;
    protected StringConfigurer rangeProperty;
    protected JLabel rangePropertyLabel;

    protected GlobalCommandTargetConfigurer targetConfig;

    protected TranslatingStringEnumConfigurer onAttachInput;
    protected TranslatingStringEnumConfigurer onDetachInput;
    protected TranslatingStringEnumConfigurer beforeAttachInput;
    protected BooleanConfigurer allowSelfAttachInput;

    protected BooleanConfigurer autoAttachInput;

    protected JLabel beforeAttachLabel;
    protected JLabel onAttachLabel;
    protected JLabel attachCommandNameLabel;
    protected JLabel attachKeyLabel;
    protected JLabel targetLabel;
    protected JLabel propertyLabel;
    protected JLabel deckLabel;
    protected JLabel restrictLabel;
    protected JLabel onDetachLabel;
    protected JLabel clearAllCommandNameLabel;
    protected JLabel clearAllKeyLabel;
    protected JLabel clearMatchingKeyLabel;
    protected JLabel clearMatchingCommandNameLabel;
    protected JLabel clearMatchingMatchLabel;

    protected Attachment attachment;


    public Ed(Attachment p) {
      attachment = p;
      traitPanel = new TraitConfigPanel();

      attachNameInput = new StringConfigurer(p.attachName);
      attachNameInput.setHintKey("Editor.Attachment.name_hint");
      traitPanel.add("Editor.Attachment.name_label", attachNameInput);

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      traitPanel.add("Editor.description_label", descInput);

      autoAttachInput = new BooleanConfigurer(p.autoAttach);
      traitPanel.add("Editor.Attachment.auto_attach", autoAttachInput);

      allowSelfAttachInput = new BooleanConfigurer(p.allowSelfAttach);
      traitPanel.add("Editor.Attachment.allow_self_attach", allowSelfAttachInput);

      beforeAttachInput = new TranslatingStringEnumConfigurer(BEFORE_ATTACH_OPTIONS, BEFORE_ATTACH_KEYS);
      beforeAttachInput.setValue(BEFORE_ATTACH_CLEAR);
      for (final String option : BEFORE_ATTACH_OPTIONS) {
        if (option.equals(p.beforeAttach)) {
          beforeAttachInput.setValue(option);
        }
      }
      beforeAttachLabel = new JLabel(Resources.getString("Editor.Attachment.before_attach"));
      traitPanel.add(beforeAttachLabel, beforeAttachInput);

      onAttachInput = new TranslatingStringEnumConfigurer(ON_ATTACH_OPTIONS, ON_ATTACH_KEYS);
      onAttachInput.setValue(ON_ATTACH_NOTHING);
      for (final String option : ON_ATTACH_OPTIONS) {
        if (option.equals(p.onAttach)) {
          onAttachInput.setValue(option);
        }
      }
      onAttachLabel = new JLabel(Resources.getString("Editor.Attachment.on_attach"));
      traitPanel.add(onAttachLabel, onAttachInput);

      attachCommandNameInput = new StringConfigurer(p.attachCommandName);
      attachCommandNameInput.setHintKey("Editor.menu_command_hint");
      attachCommandNameLabel = new JLabel(Resources.getString("Editor.Attachment.attach_menu_command"));
      traitPanel.add(attachCommandNameLabel, attachCommandNameInput);

      attachKeyInput = new NamedHotKeyConfigurer(p.attachKey);
      attachKeyLabel = new JLabel(Resources.getString("Editor.Attachment.attach_key_command"));
      traitPanel.add(attachKeyLabel, attachKeyInput);

      targetConfig = new GlobalCommandTargetConfigurer(p.target, p);
      targetLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.pre_select"));
      traitPanel.add(targetLabel, targetConfig);

      propertyMatch = new PropertyExpressionConfigurer(p.propertiesFilter, p);
      propertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.matching_properties"));
      traitPanel.add(propertyLabel, propertyMatch);

      deckPolicy = new MassKeyCommand.DeckPolicyConfig(false, p);
      deckPolicy.setValue(p.globalAttach.getSelectFromDeckExpression());
      deckLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.deck_policy"));
      traitPanel.add(deckLabel, deckPolicy);

      restrictRange = new BooleanConfigurer(p.restrictRange);
      restrictLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.restrict_range"));
      traitPanel.add(restrictLabel, restrictRange);

      fixedRange = new BooleanConfigurer(p.fixedRange);
      fixedRangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.fixed_range"));
      traitPanel.add(fixedRangeLabel, fixedRange);

      range = new IntConfigurer(p.range);
      rangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range"));
      traitPanel.add(rangeLabel, range);

      rangeProperty = new StringConfigurer(p.rangeProperty);
      rangeProperty.setHintKey("Editor.GlobalKeyCommand.range_property_hint");
      rangePropertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range_property"));
      traitPanel.add(rangePropertyLabel, rangeProperty);

      onDetachInput = new TranslatingStringEnumConfigurer(ON_DETACH_OPTIONS, ON_DETACH_KEYS);
      onDetachInput.setValue(ON_DETACH_NOTHING);
      for (final String option : ON_DETACH_OPTIONS) {
        if (option.equals(p.onDetach)) {
          onDetachInput.setValue(option);
        }
      }
      onDetachLabel = new JLabel(Resources.getString("Editor.Attachment.on_detach"));
      traitPanel.add(onDetachLabel, onDetachInput);

      clearAllCommandNameInput = new StringConfigurer(p.clearAllCommandName);
      clearAllCommandNameInput.setHintKey("Editor.menu_command_hint");
      clearAllCommandNameLabel = new JLabel(Resources.getString("Editor.Attachment.clear_all_menu_command"));
      traitPanel.add(clearAllCommandNameLabel, clearAllCommandNameInput);

      clearAllKeyInput = new NamedHotKeyConfigurer(p.clearAllKey);
      clearAllKeyLabel = new JLabel(Resources.getString("Editor.Attachment.clear_all_key_command"));
      traitPanel.add(clearAllKeyLabel, clearAllKeyInput);

      clearMatchingCommandNameInput = new StringConfigurer(p.clearMatchingCommandName);
      clearMatchingCommandNameInput.setHintKey("Editor.menu_command_hint");
      clearMatchingCommandNameLabel = new JLabel(Resources.getString("Editor.Attachment.clear_matching_menu_command"));
      traitPanel.add(clearMatchingCommandNameLabel, clearMatchingCommandNameInput);

      clearMatchingKeyInput = new NamedHotKeyConfigurer(p.clearMatchingKey);
      clearMatchingKeyLabel = new JLabel(Resources.getString("Editor.Attachment.clear_matching_key_command"));
      traitPanel.add(clearMatchingKeyLabel, clearMatchingKeyInput);

      clearMatchingMatch = new PropertyExpressionConfigurer(p.clearMatchingFilter, p);
      clearMatchingMatchLabel = new JLabel(Resources.getString("Editor.Attachment.clear_matching_properties"));
      traitPanel.add(clearMatchingMatchLabel, clearMatchingMatch);

      final PropertyChangeListener pl = evt -> {

        final boolean isAuto = Boolean.TRUE.equals(autoAttachInput.getValue());
        final boolean isRange = Boolean.TRUE.equals(restrictRange.getValue());
        final boolean isFixed = Boolean.TRUE.equals(fixedRange.getValue());

        range.getControls().setVisible(isRange && isFixed && !isAuto);
        rangeLabel.setVisible(isRange && isFixed && !isAuto);
        fixedRange.getControls().setVisible(isRange && !isAuto);
        fixedRangeLabel.setVisible(isRange && !isAuto);
        rangeProperty.getControls().setVisible(isRange && !isFixed && !isAuto);
        rangePropertyLabel.setVisible(isRange && !isFixed && !isAuto);

        beforeAttachInput.getControls().setVisible(!isAuto);
        onAttachInput.getControls().setVisible(!isAuto);
        attachCommandNameInput.getControls().setVisible(!isAuto);
        attachKeyInput.getControls().setVisible(!isAuto);
        targetConfig.getControls().setVisible(!isAuto);
        propertyMatch.getControls().setVisible(!isAuto);
        deckPolicy.getControls().setVisible(!isAuto);
        restrictRange.getControls().setVisible(!isAuto);
        onDetachInput.getControls().setVisible(!isAuto);
        clearAllCommandNameInput.getControls().setVisible(!isAuto);
        clearAllKeyInput.getControls().setVisible(!isAuto);
        clearMatchingCommandNameInput.getControls().setVisible(!isAuto);
        clearMatchingKeyInput.getControls().setVisible(!isAuto);
        clearMatchingMatch.getControls().setVisible(!isAuto);

        beforeAttachLabel.setVisible(!isAuto);
        onAttachLabel.setVisible(!isAuto);
        attachCommandNameLabel.setVisible(!isAuto);
        attachKeyLabel.setVisible(!isAuto);
        targetLabel.setVisible(!isAuto);
        propertyLabel.setVisible(!isAuto);
        deckLabel.setVisible(!isAuto);
        restrictLabel.setVisible(!isAuto);
        onDetachLabel.setVisible(!isAuto);
        clearAllCommandNameLabel.setVisible(!isAuto);
        clearAllKeyLabel.setVisible(!isAuto);
        clearMatchingCommandNameLabel.setVisible(!isAuto);
        clearMatchingKeyLabel.setVisible(!isAuto);
        clearMatchingMatchLabel.setVisible(!isAuto);

        repack(range);
        repack(autoAttachInput);
      };

      autoAttachInput.addPropertyChangeListener(pl);
      restrictRange.addPropertyChangeListener(pl);
      fixedRange.addPropertyChangeListener(pl);

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
        .append(clearAllCommandNameInput.getValueString())
        .append(clearAllKeyInput.getValueString())
        .append(propertyMatch.getValueString())
        .append(restrictRange.getValueString())
        .append(range.getValueString())
        .append(fixedRange.getValueString())
        .append(rangeProperty.getValueString())
        .append(deckPolicy.getValueString())
        .append(targetConfig.getValueString())
        .append(clearMatchingCommandNameInput.getValueString())
        .append(clearMatchingKeyInput.getValueString())
        .append(clearMatchingMatch.getValueString())
        .append(onAttachInput.getValueString())
        .append(onDetachInput.getValueString())
        .append(beforeAttachInput.getValueString())
        .append(allowSelfAttachInput.getValueString())
        .append(autoAttachInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return attachment.myGetState();
    }
  }

  /**
   * {@link SearchTarget}
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> expList = target.getExpressionList();
    expList.add(propertiesFilter.getExpression());
    expList.add(clearMatchingFilter.getExpression());
    return expList;
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return Arrays.asList(attachCommandName, clearAllCommandName, clearMatchingCommandName);
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(attachKey, clearAllKey, clearMatchingKey);
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(attachName, rangeProperty);
  }
}
