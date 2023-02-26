/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.build.GameModule;
import VASSAL.build.module.map.DrawPile;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Deck;
import VASSAL.counters.KeyCommand;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.FormattedStringExpression;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import org.apache.commons.lang3.ArrayUtils;

import java.awt.event.ActionEvent;
import java.util.Collections;
import java.util.List;

public class DeckSendKeyCommand extends AbstractDeckKeyCommand {

  public static final String TARGET_DECK = "targetDeck"; //NON-NLS
  public static final String VARIABLE_DECK = "variableDeck"; //NON-NLS
  public static final String DECK_EXPRESSION = "deckExpression"; //NON-NLS
  public static final String SEND_MATCHING = "sendMatching"; //NON-NLS
  public static final String MATCH_EXPRESSION = "matchExpression"; //NON-NLS
  public static final String LIMIT_TOTAL = "limitTotal"; //NON-NLS
  public static final String LIMIT_EXPRESSION = "limitExpression"; //NON-NLS
  public static final String STOP = "stop"; //NON-NLS
  public static final String STOP_EXPRESSION = "stopExpression"; //NON-NLS
  public static final String STOP_INCLUDE = "stopAlso"; //NON-NLS
  public static final String ORDER = "order"; //NON-NLS
  public static final String SENT_COUNT = "sentCount"; //NON-NLS
  public static final String APPLY_ON_MOVE = "applyOnMove"; //NON-NLS

  private NamedKeyStrokeListener sendListener;
  private String targetDeck = "";
  private boolean variableDeck = false;
  private final FormattedString deckExpression =  new FormattedString("{}");
  private boolean sendMatching = false;
  private final FormattedString matchExpression =  new FormattedString("{}");
  private boolean limitTotal = false;
  private final FormattedString limitExpression = new FormattedString("{}");
  private boolean stop = false;
  private final FormattedString stopExpresson =  new FormattedString("{}");
  private boolean stopInclude;
  private boolean applyOnMove = false;

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DeckSendKeyCommand.component_type"); //$NON-NLS-1$
  }

  public String getTargetDeck() {
    return targetDeck;
  }

  public NamedKeyStrokeListener getSendListener() {
    return sendListener;
  }

  public boolean isVariableDeck() {
    return variableDeck;
  }

  public FormattedString getDeckExpression() {
    return deckExpression;
  }

  public boolean isSendMatching() {
    return sendMatching;
  }

  public FormattedString getMatchExpression() {
    return matchExpression;
  }

  public boolean isLimitTotal() {
    return limitTotal;
  }

  public FormattedString getLimitExpression() {
    return limitExpression;
  }

  public boolean isStop() {
    return stop;
  }

  public FormattedString getStopExpresson() {
    return stopExpresson;
  }

  public boolean isStopInclude() {
    return stopInclude;
  }

  public boolean isApplyOnMove() {
    return applyOnMove;
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      super.getAttributeNames(),
      VARIABLE_DECK,
      TARGET_DECK,
      DECK_EXPRESSION,
      SEND_MATCHING,
      MATCH_EXPRESSION,
      LIMIT_TOTAL,
      LIMIT_EXPRESSION,
      STOP,
      STOP_EXPRESSION,
      STOP_INCLUDE,
      APPLY_ON_MOVE
    );
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.DeckSendKeyCommand.variable_deck"),
      Resources.getString("Editor.DeckSendKeyCommand.deck_name"),
      Resources.getString("Editor.DeckSendKeyCommand.deck_expression"),
      Resources.getString("Editor.DeckSendKeyCommand.send_matching"),
      Resources.getString("Editor.DeckSendKeyCommand.send_expression"),
      Resources.getString("Editor.DeckSendKeyCommand.limit"),
      Resources.getString("Editor.DeckSendKeyCommand.limit_expression"),
      Resources.getString("Editor.DeckSendKeyCommand.stop"),
      Resources.getString("Editor.DeckSendKeyCommand.stop_expression"),
      Resources.getString("Editor.DeckSendKeyCommand.stop_include"),
      Resources.getString("Editor.DeckSendKeyCommand.order"),
      Resources.getString("Editor.DeckSendKeyCommand.apply_on_move")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      super.getAttributeTypes(),
      Boolean.class,
      DrawPile.AssignedDeckPrompt.class,
      FormattedStringExpression.class,
      Boolean.class,
      FormattedStringExpression.class,
      Boolean.class,
      FormattedStringExpression.class,
      Boolean.class,
      FormattedStringExpression.class,
      Boolean.class,
      Boolean.class
    );
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (TARGET_DECK.equals(key)) {
      targetDeck = (String) value;
    }
    else if (VARIABLE_DECK.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      variableDeck = (Boolean) value;
    }
    else if (DECK_EXPRESSION.equals(key)) {
      deckExpression.setFormat((String) value);
    }
    else if (SEND_MATCHING.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      sendMatching = (Boolean) value;
    }
    else if (MATCH_EXPRESSION.equals(key)) {
      matchExpression.setFormat((String) value);
    }
    else if (LIMIT_TOTAL.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      limitTotal = (Boolean) value;
    }
    else if (LIMIT_EXPRESSION.equals(key)) {
      limitExpression.setFormat((String) value);
    }
    else if (STOP.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      stop = (Boolean) value;
    }
    else if (STOP_EXPRESSION.equals(key)) {
      stopExpresson.setFormat((String) value);
    }
    else if (STOP_INCLUDE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      stopInclude = (Boolean) value;
    }
    else if (APPLY_ON_MOVE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      applyOnMove = (Boolean) value;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (TARGET_DECK.equals(key)) {
      return targetDeck;
    }
    else if (VARIABLE_DECK.equals(key)) {
      return String.valueOf(variableDeck);
    }
    else if (DECK_EXPRESSION.equals(key)) {
      return deckExpression.getFormat();
    }
    else if (SEND_MATCHING.equals(key)) {
      return String.valueOf(sendMatching);
    }
    else if (MATCH_EXPRESSION.equals(key)) {
      return matchExpression.getFormat();
    }
    else if (LIMIT_TOTAL.equals(key)) {
      return String.valueOf(limitTotal);
    }
    else if (LIMIT_EXPRESSION.equals(key)) {
      return limitExpression.getFormat();
    }
    else if (STOP.equals(key)) {
      return String.valueOf(stop);
    }
    else if (STOP_EXPRESSION.equals(key)) {
      return stopExpresson.getFormat();
    }
    else if (STOP_INCLUDE.equals(key)) {
      return String.valueOf(stopInclude);
    }
    else if (APPLY_ON_MOVE.equals(key)) {
      return String.valueOf(applyOnMove);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (DECK_EXPRESSION.equals(name)) {
      return () -> variableDeck;
    }
    else if (TARGET_DECK.equals(name)) {
      return () -> ! variableDeck;
    }
    else if (MATCH_EXPRESSION.equals(name)) {
      return () -> sendMatching;
    }
    else if (LIMIT_EXPRESSION.equals(name)) {
      return () -> limitTotal;
    }
    else if (STOP_EXPRESSION.equals(name)) {
      return () -> stop;
    }
    else if (STOP_INCLUDE.equals(name)) {
      return () -> stop;
    }
    return null;
  }

  @Override
  public List<KeyCommand> getKeyCommands(Deck deck) {
    return List.of(new KeyCommand(getConfigureName(), NamedKeyStroke.NULL_KEYSTROKE, deck) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        doSend(deck);
      }
    });
  }

  @Override
  public void registerListeners(Deck deck) {
    if (sendListener == null && keyStroke != null && !keyStroke.isNull()) {
      sendListener =  new NamedKeyStrokeListener(e -> doSend(deck));
      sendListener.setKeyStroke(keyStroke);
      GameModule.getGameModule().addKeyStrokeListener(sendListener);
    }
  }

  @Override
  public void deregisterListeners() {
    if (sendListener != null) {
      GameModule.getGameModule().removeKeyStrokeListener(sendListener);
    }
  }

  private void doSend(Deck deck) {
    // FIXME FInd the target Deck name correctly.
    GameModule.getGameModule().sendAndLog(deck.extendedSend(this));
    deck.repaintMap();
  }

  @Override
  public String[] getAdditionalReportProperties() {
    return ArrayUtils.addAll(super.getAdditionalReportProperties(), TARGET_DECK, SENT_COUNT);
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    if (!variableDeck) {
      return List.of(deckExpression.getFormat(), matchExpression.getFormat(), limitExpression.getFormat());
    }
    return Collections.emptyList();
  }
}
