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

import VASSAL.build.module.map.DrawPile;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;

import VASSAL.script.expression.FormattedStringExpression;
import java.util.Collections;
import java.util.List;
import org.apache.commons.lang3.ArrayUtils;

public class DeckSendKeyCommand extends AbstractDeckKeyCommand {

  public static final String TARGET_DECK = "targetDeck";
  public static final String FIXED_DESTINATION = "fixedDestination";
  public static final String DECK_EXPRESSION = "deckExpression";

  private String targetDeck = "";
  private boolean fixed = true;
  private String deckExpression = "";

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DeckSendKeyCommand.component_type"); //$NON-NLS-1$
  }

  public String getTargetDeck() {
    return targetDeck;
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(super.getAttributeNames(), FIXED_DESTINATION, TARGET_DECK, DECK_EXPRESSION);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.DeckSendKeyCommand.fixed_destination"),
      Resources.getString("Editor.DeckSendKeyCommand.deck_name"),
      Resources.getString("Editor.DeckSendKeyCommand.deck_expression")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(super.getAttributeTypes(), Boolean.class, DrawPile.AssignedDeckPrompt.class, FormattedStringExpression.class);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (TARGET_DECK.equals(key)) {
      targetDeck = (String) value;
    }
    else if (FIXED_DESTINATION.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      fixed = (Boolean) value;
    }
    else if (DECK_EXPRESSION.equals(key)) {
      deckExpression = (String) value;
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
    else if (FIXED_DESTINATION.equals(key)) {
      return String.valueOf(fixed);
    }
    else if (DECK_EXPRESSION.equals(key)) {
      return deckExpression;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (DECK_EXPRESSION.equals(name)) {
      return () -> ! fixed;
    }
    else if (TARGET_DECK.equals(name)) {
      return () -> fixed;
    }
    return null;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    if (! fixed) {
      return List.of(deckExpression);
    }
    return Collections.emptyList();
  }

}
