/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASSAL.build.module.map;

import java.awt.event.ActionEvent;

import javax.swing.KeyStroke;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.PropertyExpression;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;

/**
 * This version of {@link MassKeyCommand} is added directly to a
 * {@link VASSAL.build.GameModule} and applies to all maps
 */
public class DeckGlobalKeyCommand extends MassKeyCommand {

  public DeckGlobalKeyCommand() {
    globalCommand = new DeckGlobalCommand(this);
    setConfigureName("");
  }

  public DeckGlobalKeyCommand(String code) {
    this();
    decode(code);
  }

  public DeckGlobalKeyCommand(String code, PropertySource source) {
    this(code);
    propertySource = source;
    globalCommand.setPropertySource(source);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DeckGlobalKeyCommand.component_type"); //$NON-NLS-1$
  }

  public void addTo(Buildable parent) {
    if (parent instanceof Map) {
      map = (Map) parent;
    }
    if (parent instanceof PropertySource) {
      propertySource = (PropertySource) parent;
    }
    ((DrawPile) parent).addGlobalKeyCommand(this);
    globalCommand.setPropertySource(propertySource);
  }

  public void removeFrom(Buildable parent) {
    ((DrawPile) parent).removeGlobalKeyCommand(this);
  }

  public KeyCommand getKeyCommand(Deck deck) {
    return new DeckKeyCommand(getLocalizedConfigureName(), null, deck);
  }

  class DeckKeyCommand extends KeyCommand {
    private static final long serialVersionUID = 1L;
    protected Deck deck;
    public DeckKeyCommand(String name, KeyStroke key, Deck deck) {
      super(name, key, deck);
      this.deck = deck;
    }
    public void actionPerformed(ActionEvent e) {
      apply(deck);
    }
  }

  /**
   * Since we also limit application of a Deck Global Key command to a specified number of pieces in the
   * Deck, a null match expression should match all pieces, not reject them all.
   */
  public PieceFilter getFilter() {
    if (propertiesFilter == null || propertiesFilter.getExpression() == null || propertiesFilter.getExpression().length() == 0) {
      return null;
    }
    return super.getFilter();
  }

  public void apply(Deck deck) {
    GameModule.getGameModule().sendAndLog(((DeckGlobalCommand) globalCommand).apply(deck, getFilter()));
  }

  public String encode() {
    SequenceEncoder se = new SequenceEncoder('|');
    se.append(getConfigureName())
      .append(getAttributeValueString(KEY_COMMAND))
      .append(getAttributeValueString(PROPERTIES_FILTER))
      .append(getAttributeValueString(DECK_COUNT))
      .append(getAttributeValueString(REPORT_FORMAT))
      .append(getLocalizedConfigureName());
    return se.getValue();
  }

  public void decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    setConfigureName(sd.nextToken(""));
    setAttribute(KEY_COMMAND, sd.nextNamedKeyStroke('A'));
    setAttribute(PROPERTIES_FILTER, sd.nextToken(null));
    setAttribute(DECK_COUNT, sd.nextInt(0));
    setAttribute(REPORT_FORMAT, sd.nextToken(""));
    localizedName = sd.nextToken(getConfigureName());
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString("Editor.DeckGlobalKeyCommand.command"), //$NON-NLS-1$
      Resources.getString("Editor.DeckGlobalKeyCommand.matching_properties"), //$NON-NLS-1$
      Resources.getString("Editor.DeckGlobalKeyCommand.affects"), //$NON-NLS-1$
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
    };
  }

  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      KEY_COMMAND,
      PROPERTIES_FILTER,
      DECK_COUNT,
      REPORT_FORMAT
    };
  }


  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      NamedKeyStroke.class,
      PropertyExpression.class,
      DeckPolicyConfig2.class,
      ReportFormatConfig.class
    };
  }

  public static class DeckPolicyConfig2 extends DeckPolicyConfig {
    public DeckPolicyConfig2() {
      super();
      typeConfig.setValidValues(new String[]{ALL, FIXED});
      prompt.setText("Affects:  ");
    }
  }

  public static class DeckGlobalCommand extends GlobalCommand {

    public DeckGlobalCommand(Loopable l) {
      super(l);
    }

    public Command apply(Deck d, PieceFilter filter) {
      String reportText = reportFormat.getText(source);
      Command c;
      if (reportText.length() > 0) {
        c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
        c.execute();
      }
      else {
        c = new NullCommand();
      }
      Visitor visitor = new Visitor(c, filter, keyStroke);
      DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);

      dispatcher.accept(d);
      visitor.getTracker().repaint();

      c = visitor.getCommand();
      return c;
    }
  }

}
