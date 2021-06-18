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

import VASSAL.build.AbstractFolder;
import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.CounterGlobalKeyCommand;
import VASSAL.counters.Deck;
import VASSAL.counters.DeckVisitorDispatcher;
import VASSAL.counters.GlobalCommand;
import VASSAL.counters.GlobalCommandTarget;
import VASSAL.counters.KeyCommand;
import VASSAL.counters.PieceFilter;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.Auditable;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.KeyStroke;

/**
 * This version of {@link MassKeyCommand} is added to a {@link DrawPile} (which holds a {@link Deck})
 * and applies to pieces/cards currently in the deck.
 *
 * The "Global Key Command" functionality, as the term is used in Vassal Modules, is spread out over several classes internally:
 * {@link GlobalCommand} - primary functionality for sending commands to multiple pieces based on matching parameters
 * {@link VASSAL.build.module.GlobalKeyCommand}         - Global Key Commands from a Module window
 * {@link VASSAL.build.module.StartupGlobalKeyCommand}  - Global Key Commands from a Module "At Startup"
 * {@link VASSAL.build.module.map.MassKeyCommand}       - Global Key Commands from a specific Map window
 * {@link VASSAL.build.module.map.DeckGlobalKeyCommand} - Global Key Commands from a Deck
 * {@link CounterGlobalKeyCommand}                      - Global Key Commands from a Game Piece
 *
 * Other important classes:
 * {@link GlobalCommandTarget}           - "Fast Match" parameters
 * {@link GlobalCommandTargetConfigurer} - configurer for "Fast Match" parameters
 *
 * NOTE: There is no need to support AuditTrails in a DeckGlobalKeyCommand since there is no matching properties expression.
 * Individual counters processing the GKC will generate their own internal audit trails.
 *
 */
public class DeckGlobalKeyCommand extends MassKeyCommand {

  public DeckGlobalKeyCommand() {
    globalCommand = new DeckGlobalCommand(this);
    globalCommand.setReportSingle(true);
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

  /**
   * @return Our type of Global Key Command (overrides the one from Mass Key Command). Affects what configurer options are shown. In particular no "Fast Match" parameters are shown for Deck GKCs.
   */
  @Override
  public GlobalCommandTarget.GKCtype getGKCtype() {
    return GlobalCommandTarget.GKCtype.DECK;
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }
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
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }
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
    final SequenceEncoder se = new SequenceEncoder('|');
    se.append(getConfigureName())
      .append(getAttributeValueString(KEY_COMMAND))
      .append(getAttributeValueString(PROPERTIES_FILTER))
      .append(getAttributeValueString(DECK_COUNT))
      .append(getAttributeValueString(REPORT_FORMAT))
      .append(getLocalizedConfigureName())
      .append(getAttributeValueString(TARGET))
      .append(getAttributeValueString(REPORT_SINGLE));
    return se.getValue();
  }

  public void decode(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    setConfigureName(sd.nextToken(""));
    setAttribute(KEY_COMMAND, sd.nextNamedKeyStroke('A'));
    setAttribute(PROPERTIES_FILTER, sd.nextToken(null));
    setAttribute(DECK_COUNT, sd.nextInt(0));
    setAttribute(REPORT_FORMAT, sd.nextToken(""));
    localizedName = sd.nextToken(getConfigureName());
    setAttribute(TARGET, sd.nextToken(""));
    setAttribute(REPORT_SINGLE, sd.nextBoolean(true));
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.MENU_COMMAND_LABEL),
      Resources.getString("Editor.GlobalKeyCommand.global_key_command"), //$NON-NLS-1$

      Resources.getString("Editor.GlobalKeyCommand.pre_select"), // Fast Match parameters (not displayed)

      Resources.getString("Editor.DeckGlobalKeyCommand.matching_properties"), //$NON-NLS-1$
      Resources.getString("Editor.DeckGlobalKeyCommand.affects"), //$NON-NLS-1$
      Resources.getString("Editor.report_format"), //$NON-NLS-1$
      Resources.getString("Editor.MassKey.suppress"),
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      AbstractToolbarItem.NAME,
      KEY_COMMAND,

      TARGET,             // Fast Match parameters (disabled for this variant)

      PROPERTIES_FILTER,
      DECK_COUNT,
      REPORT_FORMAT,
      REPORT_SINGLE,
    };
  }


  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      NamedKeyStroke.class,

      GlobalCommandTarget.class,  // Fast Match parameters (disabled for this variant)

      PropertyExpression.class,
      DeckPolicyConfig2.class,
      ReportFormatConfig.class,
      Boolean.class
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String key) {
    if (key.equals(TARGET)) {
      return () -> false; // No fast match for Deck Global Key Commands
    }
    return () -> true;
  }

  public static class DeckPolicyConfig2 extends DeckPolicyConfig {
    public DeckPolicyConfig2() {
      super();
      typeConfig.setValidValues(new String[]{ALL, FIXED}, new String[] {
        "Editor.GlobalKeyCommand.all_pieces",
        "Editor.GlobalKeyCommand.fixed_number_of_pieces"
      });
    }
  }

  public static class DeckGlobalCommand extends GlobalCommand {

    public DeckGlobalCommand(Loopable l) {
      super(l);
    }

    public Command apply(Deck d, PieceFilter filter) {
      final String reportText = reportFormat.getText(source, (Auditable) this, "Editor.report_format");
      Command c;
      if (reportText.length() > 0) {
        c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "*" + reportText);
        c.execute();
      }
      else {
        c = new NullCommand();
      }

      final Visitor visitor = new Visitor(c, filter, keyStroke);
      final DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(visitor);

      dispatcher.accept(d);
      visitor.getTracker().repaint();

      c = visitor.getCommand();
      return c;
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> l = new ArrayList<>(super.getExpressionList());
    l.add(propertiesFilter.getExpression());
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    final List<String> l = new ArrayList<>(super.getFormattedStringList());
    l.add(reportFormat.getFormat());
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    final List<NamedKeyStroke> l = new ArrayList<>(super.getNamedKeyStrokeList());
    l.add(NamedHotKeyConfigurer.decode(getAttributeValueString(KEY_COMMAND)));
    return l;
  }
}
