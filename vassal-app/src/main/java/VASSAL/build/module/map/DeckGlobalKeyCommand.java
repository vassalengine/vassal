/*
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
import java.util.ArrayList;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.VisibilityCondition;
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

  @Override
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

  @Override
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
    @Override
    public void actionPerformed(ActionEvent e) {
      apply(deck);
    }
  }

  /**
   * Since we also limit application of a Deck Global Key command to a specified number of pieces in the
   * Deck, a null match expression should match all pieces, not reject them all.
   */
  @Override
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
      .append(getLocalizedConfigureName())
      .append(getAttributeValueString(TARGET));
//      .append(getAttributeValueString(TARGET_TYPE))
//      .append(getAttributeValueString(TARGET_MAP))
//      .append(getAttributeValueString(TARGET_BOARD))
//      .append(getAttributeValueString(TARGET_ZONE))
//      .append(getAttributeValueString(TARGET_REGION))
//      .append(getAttributeValueString(TARGET_X))
//      .append(getAttributeValueString(TARGET_Y))
//      .append(getAttributeValueString(TARGET_EXACT_MATCH))
//      .append(getAttributeValueString(TARGET_PROPERTY))
//      .append(getAttributeValueString(TARGET_VALUE));
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
    setAttribute(TARGET, sd.nextToken(""));
//    setAttribute(TARGET_TYPE, sd.nextToken(""));
//    setAttribute(TARGET_MAP, sd.nextToken(""));
//    setAttribute(TARGET_BOARD, sd.nextToken(""));
//    setAttribute(TARGET_ZONE, sd.nextToken(""));
//    setAttribute(TARGET_REGION, sd.nextToken(""));
//    setAttribute(TARGET_X, sd.nextInt(0));
//    setAttribute(TARGET_Y, sd.nextInt(0));
//    setAttribute(TARGET_EXACT_MATCH, sd.nextToken(""));
//    setAttribute(TARGET_PROPERTY, sd.nextToken(""));
//    setAttribute(TARGET_VALUE, sd.nextToken(""));
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString("Editor.GlobalKeyCommand.command"), //$NON-NLS-1$

      Resources.getString ("Editor.GlobalKeyCommand.pre_select"),
//      Resources.getString("Editor.GlobalKeyCommand.restrict_matches_to"),     // Restrict by location? (fast match)
//      Resources.getString("Editor.GlobalKeyCommand.restrict_to_map"),         // Restrict to map
//      Resources.getString("Editor.GlobalKeyCommand.restrict_to_board"),       // Restrict to board
//      Resources.getString("Editor.GlobalKeyCommand.restrict_to_zone"),        // Restrict to zone
//      Resources.getString("Editor.GlobalKeyCommand.restrict_to_region"),      // Restrict to region
//      Resources.getString("Editor.GlobalKeyCommand.restrict_to_x_position"),  // Restrict to X position
//      Resources.getString("Editor.GlobalKeyCommand.restrict_to_y_position"),  // Restrict to Y position
//      Resources.getString("Editor.GlobalKeyCommand.exact_match"),             // Exact property match (fast match)
//      Resources.getString("Editor.GlobalKeyCommand.exact_property"),          // Property name for fast match
//      Resources.getString("Editor.GlobalKeyCommand.exact_value"),             // Property value for fast match

      Resources.getString("Editor.GlobalKeyCommand.matching_properties"), //$NON-NLS-1$
      Resources.getString("Editor.DeckGlobalKeyCommand.affects"), //$NON-NLS-1$
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      KEY_COMMAND,

      TARGET,
//      TARGET_TYPE,        // Restrict by location? (fast match)
//      TARGET_MAP,         // Restrict to map
//      TARGET_BOARD,       // Restrict to board
//      TARGET_ZONE,        // Restrict to zone
//      TARGET_REGION,      // Restrict to region
//      TARGET_X,           // Restrict to X position
//      TARGET_Y,           // Restrict to Y position
//      TARGET_EXACT_MATCH, // Exact property match (fast match)
//      TARGET_PROPERTY,    // Property name for fast match
//      TARGET_VALUE,       // Property value for fast match

      PROPERTIES_FILTER,
      DECK_COUNT,
      REPORT_FORMAT
    };
  }


  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      NamedKeyStroke.class,

      GlobalCommand.GlobalCommandTargetConfigurer.class,  // Restrict by location? (fast match)
      PropertyExpression.class,                 // Restrict to map
      PropertyExpression.class,                 // Restrict to board
      PropertyExpression.class,                 // Restrict to zone
      PropertyExpression.class,                 // Restrict to region
      Integer.class,                            // Restrict to X position
      Integer.class,                            // Restrict to Y position
      Boolean.class,                            // Exact property match (fast match)
      PropertyExpression.class,                 // Property name for fast match
      PropertyExpression.class,                 // Property value for fast match

      PropertyExpression.class,
      DeckPolicyConfig2.class,
      ReportFormatConfig.class
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
//    if (TARGET_MAP.equals(key)) {
//      return () -> (targetType != GlobalCommand.GlobalCommandTarget.GAME) && (condition == null);
//    }
//    else if (TARGET_ZONE.equals(key)) {
//      return () -> (targetType == GlobalCommand.GlobalCommandTarget.ZONE) && (condition == null);
//    }
//    else if (TARGET_REGION.equals(key)) {
//      return () -> (targetType == GlobalCommand.GlobalCommandTarget.REGION) && (condition == null);
//    }
//    else if (TARGET_X.equals(key) || TARGET_Y.equals(key) || TARGET_BOARD.equals(key)) {
//      return () -> (targetType == GlobalCommand.GlobalCommandTarget.XY) && (condition == null);
//    }
//    else if (TARGET_PROPERTY.equals(key) || TARGET_VALUE.equals(key)) {
//      return () -> targetExactMatch && (condition == null);
//    }
//    else if (TARGET_TYPE.equals(key) || TARGET_EXACT_MATCH.equals(key)) {
//      return () -> (condition == null);
//    }

    return () -> true;
  }

  public static class DeckPolicyConfig2 extends DeckPolicyConfig {
    public DeckPolicyConfig2() {
      super();
      typeConfig.setValidValues(new String[]{ALL, FIXED}, new String[] {
        "Editor.GlobalKeyCommand.all_pieces",
        "Editor.GlobalKeyCommand.fixed_number_of_pieces"
      });
      prompt.setText(Resources.getString("Editor.DeckGlobalKeyCommand.affects"));
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

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurable's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    List<String> l = new ArrayList<>(super.getExpressionList());
    l.add(propertiesFilter.getExpression());
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    List<String> l = new ArrayList<>(super.getFormattedStringList());
    l.add(reportFormat.getFormat());
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    List<NamedKeyStroke> l = new ArrayList<>(super.getNamedKeyStrokeList());
    l.add(NamedHotKeyConfigurer.decode(getAttributeValueString(KEY_COMMAND)));
    return l;
  }
}
