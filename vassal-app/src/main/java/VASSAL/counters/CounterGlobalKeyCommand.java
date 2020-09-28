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
import java.awt.Window;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import java.util.Objects;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;
import net.miginfocom.swing.MigLayout;

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
      .append(globalCommand.getSelectFromDeck());
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

    for (Map m : Map.getMapList()) {
      c = c.append(globalCommand.apply(m, filter));
    }

    return c;
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(commandName, getCommandDescription(description, Resources.getString("Editor.command_name")));
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

        Window w = SwingUtilities.getWindowAncestor(range.getControls());
        if (w != null) {
          w.pack();
        }
      };

      controls = new JPanel();
      controls.setLayout(new MigLayout("hidemode 3, gapy 1,ins 0, wrap 2", "[right]rel[fill,grow]")); // NON-NLS

      descInput = new StringConfigurer(p.description);
      controls.add(new JLabel(Resources.getString("Editor.description_label")));
      controls.add(descInput.getControls());

      nameInput = new StringConfigurer(p.commandName);
      controls.add(new JLabel(Resources.getString("Editor.command_name")));
      controls.add(nameInput.getControls());

      keyInput = new NamedHotKeyConfigurer(p.key);
      controls.add(new JLabel(Resources.getString("Editor.keyboard_command")));
      controls.add(keyInput.getControls());

      globalKey = new NamedHotKeyConfigurer(p.globalKey);
      controls.add(new JLabel(Resources.getString("Editor.GlobalkeyCommand.global_key_command")));
      controls.add(globalKey.getControls());

      propertyMatch = new PropertyExpressionConfigurer(null, null, p.propertiesFilter);
      controls.add(new JLabel(Resources.getString("Editor.GlobalKeyCommand.matching_properties")));
      controls.add(propertyMatch.getControls());

      deckPolicy = new MassKeyCommand.DeckPolicyConfig(false);
      deckPolicy.setValue(p.globalCommand.getSelectFromDeck());
      controls.add(new JLabel(Resources.getString("Editor.GlobalKeyCommand.deck_policy")));
      controls.add(deckPolicy.getControls());

      restrictRange = new BooleanConfigurer(null, null, p.restrictRange);
      controls.add(new JLabel(Resources.getString("Editor.GlobalKeyCommand.restrict_range")));
      controls.add(restrictRange.getControls());
      restrictRange.addPropertyChangeListener(pl);

      fixedRange = new BooleanConfigurer(null, null, p.fixedRange);
      fixedRangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.fixed_range"));
      controls.add(fixedRangeLabel);
      controls.add(fixedRange.getControls());
      fixedRange.addPropertyChangeListener(pl);

      range = new IntConfigurer(p.range);
      rangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range"));
      controls.add(rangeLabel);
      controls.add(range.getControls());

      rangeProperty = new StringConfigurer(p.rangeProperty);
      rangePropertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range_property"));
      controls.add(rangePropertyLabel);
      controls.add(rangeProperty.getControls());

      suppress = new BooleanConfigurer(null, null, p.globalCommand.isReportSingle());
      controls.add(new JLabel(Resources.getString("Editor.GlobalKeyCommand.Editor_MassKey_suppress")));
      controls.add(suppress.getControls());

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
