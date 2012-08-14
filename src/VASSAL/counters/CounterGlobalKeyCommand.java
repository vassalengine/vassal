/*
 * $Id$
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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BoxLayout;
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

/**
 * Adds a menu item that applies a {@link GlobalCommand} to other pieces
 */
public class CounterGlobalKeyCommand extends Decorator
                                     implements TranslatablePiece,
                                                RecursionLimiter.Loopable {
  public static final String ID = "globalkey;";
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

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("Global Command");
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

  public String myGetState() {
    return "";
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    return myCommand.matches(stroke) ? apply() : null;
  }

  public void mySetState(String newState) {
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

  public Shape getShape() {
    return piece.getShape();
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public String getDescription() {
    String d = "Global Key Command";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalKeyCommand.htm");
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
          reportDataError(this, Resources.getString("Error.non_number_error"), "range["+rangeProperty+"]="+rangeValue, e);
        }
      }
      filter = new BooleanAndPieceFilter(filter,new RangeFilter(getMap(), getPosition(), r));
    }

    for (Map m : Map.getMapList()) {
      c = c.append(globalCommand.apply(m, filter));
    }

    return c;
  }

  public PieceI18nData getI18nData() {
    return getI18nData(commandName, getCommandDescription(description, "Command name"));
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
    protected IntConfigurer range;
    protected StringConfigurer rangeProperty;
    protected StringConfigurer descInput;
    protected JPanel controls;

    public Ed(CounterGlobalKeyCommand p) {

      PropertyChangeListener pl = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {

          boolean isRange = Boolean.TRUE.equals(restrictRange.getValue());
          boolean isFixed = Boolean.TRUE.equals(fixedRange.getValue());

          range.getControls().setVisible(isRange && isFixed);
          fixedRange.getControls().setVisible(isRange);
          rangeProperty.getControls().setVisible(isRange && !isFixed);

          Window w = SwingUtilities.getWindowAncestor(range.getControls());
          if (w != null) {
            w.pack();
          }
        }
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

      propertyMatch = new PropertyExpressionConfigurer(null, "Matching Properties:  ", p.propertiesFilter);
      controls.add(propertyMatch.getControls());

      deckPolicy = new MassKeyCommand.DeckPolicyConfig();
      deckPolicy.setValue(p.globalCommand.getSelectFromDeck());
      controls.add(deckPolicy.getControls());

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

      pl.propertyChange(null);
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
          .append(keyInput.getValueString())
          .append(globalKey.getValueString())
          .append(propertyMatch.getValueString())
          .append(restrictRange.getValueString())
          .append(range.getValueString())
          .append(suppress.booleanValue().booleanValue())
        .append(fixedRange.booleanValue().booleanValue())
        .append(rangeProperty.getValueString())
        .append(descInput.getValueString())
        .append(deckPolicy.getIntValue());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }

  // Implement Loopable
  public String getComponentName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  public String getComponentTypeName() {
    return getDescription();
  }

}
