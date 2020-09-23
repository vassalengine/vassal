/*
 *
 * Copyright (c) 2003 by Rodney Kinney
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.beans.PropertyChangeListener;

import java.util.Objects;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.Expression;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;

/**
 * Adds a menu item that applies a {@link GlobalCommand} to other pieces
 */
public class CounterGlobalKeyCommand extends Decorator
                                     implements TranslatablePiece,
                                                RecursionLimiter.Loopable {
  public static final String ID = "globalkey;"; // NON-NLS
  protected KeyCommand[] command;
  protected String commandName;
  protected NamedKeyStroke key;
  protected NamedKeyStroke globalKey;
  protected GlobalCommand globalCommand = new GlobalCommand(this);
  protected PropertyExpression propertiesFilter = new PropertyExpression();
  protected boolean restrictRange;
  protected boolean fixedRange = true;
  protected int range;
  protected String rangeProperty = "";
  private KeyCommand myCommand;
  protected String description;
  protected GlobalCommand.GlobalCommandTarget targetType = GlobalCommand.GlobalCommandTarget.GAME;
  protected Expression targetMap;
  protected Expression targetBoard;
  protected Expression targetZone;
  protected Expression targetRegion;
  protected Expression targetProperty;
  protected Expression targetValue;
  protected int targetX = 0;
  protected int targetY = 0;
  protected boolean targetExactMatch;


  public CounterGlobalKeyCommand() {
    this(ID, null);
  }

  public CounterGlobalKeyCommand(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken(Resources.getString("Editor.GlobalkeyCommand.command"));
    key = st.nextNamedKeyStroke('G');
    globalKey = st.nextNamedKeyStroke('K');
    propertiesFilter.setExpression(st.nextToken(""));
    restrictRange = st.nextBoolean(false);
    range = st.nextInt(1);
    globalCommand.setReportSingle(st.nextBoolean(true));
    globalCommand.setKeyStroke(globalKey);
    fixedRange = st.nextBoolean(true);
    rangeProperty = st.nextToken(""); //NON-NLS
    description = st.nextToken("");   //NON-NLS
    globalCommand.setSelectFromDeck(st.nextInt(-1));

    String typeToken = st.nextToken(GlobalCommand.GlobalCommandTarget.GAME.toString());
    targetType = GlobalCommand.GlobalCommandTarget.GAME;
    for (GlobalCommand.GlobalCommandTarget t : GlobalCommand.GlobalCommandTarget.values()) {
      if (typeToken.equals(t.toString())) {
        targetType = t;
        break;
      }
    }
    targetMap = Expression.createExpression(st.nextToken(""));    //NON-NLS
    targetBoard = Expression.createExpression(st.nextToken(""));  //NON-NLS
    targetZone = Expression.createExpression(st.nextToken(""));   //NON-NLS
    targetRegion = Expression.createExpression(st.nextToken("")); //NON-NLS
    targetX = st.nextInt(0);
    targetY = st.nextInt(0);
    targetExactMatch = st.nextBoolean(false);
    targetProperty = Expression.createExpression(st.nextToken(""));
    targetValue = Expression.createExpression(st.nextToken(""));

    command = null;
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName)
        .append(key)
        .append(globalKey)
        .append(propertiesFilter.getExpression())
        .append(restrictRange)
        .append(range)
        .append(globalCommand.isReportSingle())
      .append(fixedRange)
      .append(rangeProperty)
      .append(description)
      .append(globalCommand.getSelectFromDeck())
      .append(targetType.toString())
      .append(targetMap.getExpression())
      .append(targetBoard.getExpression())
      .append(targetZone.getExpression())
      .append(targetRegion.getExpression())
      .append(targetX)
      .append(targetY)
      .append(targetExactMatch)
      .append(targetProperty.getExpression())
      .append(targetValue.getExpression());

    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      myCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), this);
      if (commandName.length() > 0 && key != null && ! key.isNull()) {
        command = new KeyCommand[]{ myCommand };
      }
      else {
        command = new KeyCommand[0];
      }
    }
    if (command.length > 0) {
      command[0].setEnabled(getMap() != null);
    }
    return command;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    return myCommand.matches(stroke) ? apply() : null;
  }

  @Override
  public void mySetState(String newState) {
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
  
  
  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(key, globalKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(commandName);
  }

  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(propertiesFilter.getExpression());
  }

  /**
   * @return a list of any Message Format strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    if (globalCommand != null) {
      return List.of(globalCommand.getReportFormat());
    }
    return Collections.emptyList();
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public String getDescription() {
    String d = Resources.getString("Editor.GlobalkeyCommand.global_key_command");
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalKeyCommand.html"); // NON-NLS
  }

  public Command apply() {
    PieceFilter filter = propertiesFilter.getFilter(Decorator.getOutermost(this));
    Command c = new NullCommand();
    if (restrictRange) {
      int r = range;
      if (!fixedRange) {
        String rangeValue = (String) Decorator.getOutermost(this).getProperty(rangeProperty);
        try {
          r = Integer.parseInt(rangeValue);
        }
        catch (NumberFormatException e) {
          reportDataError(this, Resources.getString("Error.non_number_error"), "range[" + rangeProperty + "]=" + rangeValue, e); // NON-NLS
        }
      }
      filter = new BooleanAndPieceFilter(filter, new RangeFilter(getMap(), getPosition(), r));
    }

    globalCommand.setTargetExactMatch(targetExactMatch);
    if (targetExactMatch) {
      globalCommand.setTargetProperty(targetProperty.tryEvaluate(Decorator.getOutermost(this)));
      globalCommand.setTargetValue(targetValue.tryEvaluate(Decorator.getOutermost(this)));
    }

    globalCommand.setTargetType(targetType);

    if (targetType != GlobalCommand.GlobalCommandTarget.GAME) {
      globalCommand.setTargetMap(targetMap.tryEvaluate(Decorator.getOutermost(this)));
    }

    switch (targetType) {
    case ZONE:
      globalCommand.setTargetZone(targetZone.tryEvaluate(Decorator.getOutermost(this)));
      break;

    case REGION:
      globalCommand.setTargetRegion(targetRegion.tryEvaluate(Decorator.getOutermost(this)));
      break;

    case XY:
      globalCommand.setTargetBoard(targetBoard.tryEvaluate(Decorator.getOutermost(this)));
      globalCommand.setTargetX(targetX);
      globalCommand.setTargetY(targetY);
      break;
    }

    for (Map m : Map.getMapList()) {
      c = c.append(globalCommand.apply(m, filter));
    }

    return c;
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(commandName, getCommandDescription(description, Resources.getString("Editor.GlobalKeyCommand.command_description")));
  }

  public static final String[] TARGET_OPTIONS = Arrays.stream(GlobalCommand.GlobalCommandTarget.values())
    .map(Enum::toString)
    .toArray(String[]::new);

  public static final String[] TARGET_OPTIONS_KEYS = Arrays.stream(GlobalCommand.GlobalCommandTarget.values())
    .map(GlobalCommand.GlobalCommandTarget::toTranslatedString)
    .toArray(String[]::new);


  public static class TargetOption extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return TARGET_OPTIONS;
    }
    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return TARGET_OPTIONS_KEYS;
    }
  }


  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof CounterGlobalKeyCommand)) return false;
    CounterGlobalKeyCommand trait = (CounterGlobalKeyCommand) o;

    if (! Objects.equals(commandName, trait.commandName)) return false;
    if (! Objects.equals(key, trait.key)) return false;
    if (! Objects.equals(globalKey, trait.globalKey)) return false;
    if (! Objects.equals(propertiesFilter.getExpression(), trait.propertiesFilter.getExpression())) return false;
    if (! Objects.equals(restrictRange, trait.restrictRange)) return false;
    if (! Objects.equals(range, trait.range)) return false;
    if (! Objects.equals(globalCommand.isReportSingle(), trait.globalCommand.isReportSingle())) return false;
    if (! Objects.equals(fixedRange, trait.fixedRange)) return false;
    if (! Objects.equals(rangeProperty, trait.rangeProperty)) return false;
    if (! Objects.equals(description, trait.description)) return false;
    return Objects.equals(globalCommand.getSelectFromDeck(), trait.globalCommand.getSelectFromDeck());
  }

  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected NamedHotKeyConfigurer keyInput;
    protected NamedHotKeyConfigurer globalKey;
    protected PropertyExpressionConfigurer propertyMatch;
    protected MassKeyCommand.DeckPolicyConfig deckPolicy;
    protected BooleanConfigurer suppress;
    protected BooleanConfigurer restrictRange;
    protected BooleanConfigurer fixedRange;
    protected JLabel fixedRangeLabel;
    protected IntConfigurer range;
    protected JLabel rangeLabel;
    protected StringConfigurer rangeProperty;
    protected JLabel rangePropertyLabel;
    protected StringConfigurer descInput;
    protected JPanel controls;
    protected TraitConfigPanel traitPanel;

    protected TranslatingStringEnumConfigurer targetConfig;
    protected FormattedExpressionConfigurer targetMapConfig;
    protected FormattedExpressionConfigurer targetBoardConfig;
    protected FormattedExpressionConfigurer targetZoneConfig;
    protected FormattedExpressionConfigurer targetRegionConfig;
    protected IntConfigurer targetXConfig;
    protected IntConfigurer targetYConfig;
    protected BooleanConfigurer targetExactMatchConfig;
    protected FormattedExpressionConfigurer targetPropertyConfig;
    protected FormattedExpressionConfigurer targetValueConfig;

    public Ed(CounterGlobalKeyCommand p) {

      PropertyChangeListener pl = evt -> {

          boolean isRange = Boolean.TRUE.equals(restrictRange.getValue());
          boolean isFixed = Boolean.TRUE.equals(fixedRange.getValue());

          range.getControls().setVisible(isRange && isFixed);
        rangeLabel.setVisible(isRange && isFixed);
          fixedRange.getControls().setVisible(isRange);
        fixedRangeLabel.setVisible(isRange);
          rangeProperty.getControls().setVisible(isRange && !isFixed);
        rangePropertyLabel.setVisible(isRange && !isFixed);

        repack(range);
      };

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      descInput = new StringConfigurer(null, "Description:  ", p.description);
      controls.add(descInput.getControls());

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new NamedHotKeyConfigurer(null, "Keyboard Command:  ", p.key);
      controls.add(keyInput.getControls());

      globalKey = new NamedHotKeyConfigurer(null, "Global Key Command:  ", p.globalKey);
      controls.add(globalKey.getControls());

      targetConfig = new TranslatingStringEnumConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_matches_to"), GlobalCommand.GlobalCommandTarget.getKeys(), GlobalCommand.GlobalCommandTarget.geti18nKeys());
      targetConfig.setValue(p.targetType.toString());
      targetConfig.addPropertyChangeListener(e -> updateVisibility());
      controls.add(targetConfig.getControls());

      targetMapConfig = new FormattedExpressionConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_to_map"), p.targetMap.getExpression());
      controls.add(targetMapConfig.getControls());

      targetBoardConfig = new FormattedExpressionConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_to_board"), p.targetBoard.getExpression());
      controls.add(targetBoardConfig.getControls());

      targetZoneConfig = new FormattedExpressionConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_to_zone"), p.targetZone.getExpression());
      controls.add(targetZoneConfig.getControls());

      targetRegionConfig = new FormattedExpressionConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_to_region"), p.targetRegion.getExpression());
      controls.add(targetRegionConfig.getControls());

      targetXConfig = new IntConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_to_x_position"), p.targetX);
      controls.add(targetXConfig.getControls());
      targetYConfig = new IntConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.restrict_to_y_position"), p.targetY);
      controls.add(targetYConfig.getControls());

      targetExactMatchConfig = new BooleanConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.exact_match"), p.targetExactMatch);
      targetExactMatchConfig.addPropertyChangeListener(e -> updateVisibility());
      controls.add(targetExactMatchConfig.getControls());

      targetPropertyConfig = new FormattedExpressionConfigurer (null, Resources.getString("Editor.GlobalKeyCommand.exact_property"), p.targetProperty.getExpression());
      controls.add(targetPropertyConfig.getControls());
      targetValueConfig    = new FormattedExpressionConfigurer (null, Resources.getString("Editor.GlobalKeyCommand.exact_value"), p.targetValue.getExpression());
      controls.add(targetValueConfig.getControls());

      propertyMatch = new PropertyExpressionConfigurer(null, Resources.getString("Editor.GlobalKeyCommand.matching_properties"), p.propertiesFilter);
      controls.add(propertyMatch.getControls());

      deckPolicy = new MassKeyCommand.DeckPolicyConfig(false);
      deckPolicy.setValue(p.globalCommand.getSelectFromDeck());
      traitPanel.add("Editor.GlobalKeyCommand.deck_policy", deckPolicy);

      restrictRange = new BooleanConfigurer(null, "Restrict Range?", p.restrictRange);
      controls.add(restrictRange.getControls());
      restrictRange.addPropertyChangeListener(pl);

      fixedRange = new BooleanConfigurer(null, "Fixed Range?", p.fixedRange);
      controls.add(fixedRange.getControls());
      fixedRange.addPropertyChangeListener(pl);

      range = new IntConfigurer(null, "Range:  ", p.range);
      controls.add(range.getControls());

      rangeProperty = new StringConfigurer(null, "Range Property:  ", p.rangeProperty);
      controls.add(rangeProperty.getControls());

      suppress = new BooleanConfigurer(null, "Suppress individual reports?", p.globalCommand.isReportSingle());
      controls.add(suppress.getControls());

      updateVisibility();

      pl.propertyChange(null);
    }

    public void updateVisibility () {
      String value = targetConfig.getValueString();
      targetMapConfig.getControls().setVisible(!GlobalCommand.GlobalCommandTarget.GAME.toString().equals(value));
      targetBoardConfig.getControls().setVisible(GlobalCommand.GlobalCommandTarget.XY.toString().equals(value));
      targetZoneConfig.getControls().setVisible(GlobalCommand.GlobalCommandTarget.ZONE.toString().equals(value));
      targetRegionConfig.getControls().setVisible(GlobalCommand.GlobalCommandTarget.REGION.toString().equals(value));
      targetXConfig.getControls().setVisible(GlobalCommand.GlobalCommandTarget.XY.toString().equals(value));
      targetYConfig.getControls().setVisible(GlobalCommand.GlobalCommandTarget.XY.toString().equals(value));

      targetPropertyConfig.getControls().setVisible(targetExactMatchConfig.getValueBoolean());
      targetValueConfig.getControls().setVisible(targetExactMatchConfig.getValueBoolean());

      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
          .append(keyInput.getValueString())
          .append(globalKey.getValueString())
          .append(propertyMatch.getValueString())
          .append(restrictRange.getValueString())
          .append(range.getValueString())
          .append(suppress.booleanValue())
        .append(fixedRange.booleanValue())
        .append(rangeProperty.getValueString())
        .append(descInput.getValueString())
        .append(deckPolicy.getIntValue())
        .append(targetConfig.getValueString())
        .append(targetMapConfig.getValueString())
        .append(targetBoardConfig.getValueString())
        .append(targetZoneConfig.getValueString())
        .append(targetRegionConfig.getValueString())
        .append(targetXConfig.getValueString())
        .append(targetYConfig.getValueString())
        .append(targetExactMatchConfig.getValueString())
        .append(targetPropertyConfig.getValueString())
        .append(targetValueConfig.getValueString());

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    } //NON-NLS
  }

  // Implement Loopable
  @Override
  public String getComponentName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  @Override
  public String getComponentTypeName() {
    return getDescription();
  }
}
