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
import java.util.Arrays;

import java.util.Objects;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
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
  protected FormattedString targetMap = new FormattedString("");
  protected FormattedString targetBoard = new FormattedString("");
  protected FormattedString targetZone = new FormattedString("");
  protected FormattedString targetRegion = new FormattedString("");
  protected int targetX = 0;
  protected int targetY = 0;


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
    rangeProperty = st.nextToken("");
    description = st.nextToken("");
    globalCommand.setSelectFromDeck(st.nextInt(-1));

    String typeToken = st.nextToken(GlobalCommand.GlobalCommandTarget.GAME.toString());
    targetType = GlobalCommand.GlobalCommandTarget.GAME;
    for (GlobalCommand.GlobalCommandTarget t : GlobalCommand.GlobalCommandTarget.values()) {
      if (typeToken.equals(t.toString())) {
        targetType = t;
        break;
      }
    }
    targetMap.setFormat(st.nextToken(""));
    targetBoard.setFormat(st.nextToken(""));
    targetZone.setFormat(st.nextToken(""));
    targetRegion.setFormat(st.nextToken(""));
    targetX = st.nextInt(0);
    targetY = st.nextInt(0);

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
      .append(targetMap.getFormat())
      .append(targetBoard.getFormat())
      .append(targetZone.getFormat())
      .append(targetRegion.getFormat())
      .append(targetX)
      .append(targetY);

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

    String matchMap;
    String matchBoard;
    String matchZone;
    String matchRegion;

    for (Map m : Map.getMapList()) {
      c = c.append(globalCommand.apply(m, filter));
    }

    return c;
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(commandName, getCommandDescription(description, Resources.getString("Editor.menu_command")));
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
    protected FormattedStringConfigurer targetMapConfig;
    protected FormattedStringConfigurer targetBoardConfig;
    protected FormattedStringConfigurer targetZoneConfig;
    protected FormattedStringConfigurer targetRegionConfig;
    protected IntConfigurer targetXConfig;
    protected IntConfigurer targetYConfig;

    /**
     * A configurer for our reportFormat, which includes the unique $FlareLocation$, $FlareZone$, $FlareMap$ properties as
     * well as $PlayerName$ and $PlayerSide$ in the "Insert" pulldown.
     */
    //public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    //  @Override
    //  public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
    //    return new FlareFormattedStringConfigurer(key, name, new String[0]);
    //  }
   // }

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

      traitPanel = new TraitConfigPanel();
      controls = traitPanel;

      descInput = new StringConfigurer(p.description);
      traitPanel.add("Editor.description_label", descInput);

      nameInput = new StringConfigurer(p.commandName);
      traitPanel.add("Editor.menu_command", nameInput);

      targetConfig = new TranslatingStringEnumConfigurer(null, "Restrict Matches To:  ", TARGET_OPTIONS, TARGET_OPTIONS_KEYS);
      targetConfig.setValue(p.targetType);
      controls.add(targetConfig.getControls());

      targetMapConfig = new FormattedExpressionConfigurer(null, "Restrict to Map:  ", p.targetMap.getFormat(), p);
      controls.add(targetMapConfig.getControls());
      targetBoardConfig = new FormattedExpressionConfigurer(null, "Restrict to Board:  ", p.targetBoard.getFormat(), p);
      controls.add(targetBoardConfig.getControls());
      targetZoneConfig = new FormattedExpressionConfigurer(null, "Restrict to Zone:  ", p.targetZone.getFormat(), p);
      controls.add(targetZoneConfig.getControls());
      targetRegionConfig = new FormattedExpressionConfigurer(null, "Restrict to Region:  ", p.targetRegion.getFormat(), p);
      controls.add(targetRegionConfig.getControls());
      targetXConfig = new IntConfigurer(null, "Restrict to X location:  ", p.targetX);
      controls.add(targetXConfig.getControls());
      targetYConfig = new IntConfigurer(null, "Restrict to Y location:  ", p.targetY);
      controls.add(targetYConfig.getControls());

      propertyMatch = new PropertyExpressionConfigurer(p.propertiesFilter);
      traitPanel.add("Editor.GlobalKeyCommand.matching_properties", propertyMatch);

      deckPolicy = new MassKeyCommand.DeckPolicyConfig(false);
      deckPolicy.setValue(p.globalCommand.getSelectFromDeck());
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
      rangePropertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range_property"));
      traitPanel.add(rangePropertyLabel, rangeProperty);

      suppress = new BooleanConfigurer(p.globalCommand.isReportSingle());
      traitPanel.add("Editor.GlobalKeyCommand.Editor_MassKey_suppress", suppress);

      pl.propertyChange(null);
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
        .append(deckPolicy.getIntValue());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
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
