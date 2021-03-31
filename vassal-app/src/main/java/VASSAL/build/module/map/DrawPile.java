/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Michael Blumoehr, Brent Easton
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.JPopupMenu;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.widget.CardSlot;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.GamePieceFormattedStringConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.UniqueIdManager;

public class DrawPile extends SetupStack implements PropertySource, PropertyNameSource {
  protected Deck dummy = new Deck(GameModule.getGameModule()); // Used for storing type information
  protected boolean reshufflable;
  protected Deck myDeck;
  protected PropertySource source;

  private final VisibilityCondition colorVisibleCondition = () -> dummy.isDrawOutline();

  private final VisibilityCondition reshuffleVisibleCondition = () -> reshufflable;

  private final VisibilityCondition faceDownFormatVisibleCondition = () -> dummy.getFaceDownOption().equals(USE_MENU);

  private final VisibilityCondition reverseFormatVisibleCondition = () -> dummy.isReversible();

  private final VisibilityCondition shuffleFormatVisibleCondition = () -> dummy.getShuffleOption().equals(USE_MENU);

  private final VisibilityCondition expressionCountingVisibleCondition = () -> dummy.doesExpressionCounting();

  private final VisibilityCondition hotkeyOnEmptyVisibleCondition = () -> dummy.isHotkeyOnEmpty();

  private final VisibilityCondition selectionAllowedVisibleCondition = () -> dummy.isAllowSelectDraw();

  private final VisibilityCondition restrictExpressionVisibleCondition = () -> dummy.isRestrictOption();

  protected static final UniqueIdManager idMgr = new UniqueIdManager("Deck"); //NON-NLS

  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);
    idMgr.add(this);
    setAttributeTranslatable(NAME, true);
    if (parent instanceof PropertySource) {
      source = (PropertySource) parent;
    }
  }

  protected JPopupMenu buildPopup() {
    final JPopupMenu popup = new JPopupMenu();
    return popup.getComponentCount() > 0 ? popup : null;
  }

  /**
   * Decks can contain child Global Key Commands that only apply
   * to cards in the Deck. Pass these to the dummy Deck component
   */
  public void addGlobalKeyCommand(DeckGlobalKeyCommand globalCommand) {
    dummy.addGlobalKeyCommand(globalCommand);
  }

  public void removeGlobalKeyCommand(DeckGlobalKeyCommand globalCommand) {
    dummy.removeGlobalKeyCommand(globalCommand);
  }

  /**
   * Return the DrawPile instance with the matching id or name
   * @param id the Id or ConfigureName of the target DrawPile
   * @return the matching {@link DrawPile}, or null if none found
   * @see DrawPile#getId
   * @see VASSAL.build.AbstractConfigurable#getConfigureName
   */
  public static DrawPile findDrawPile(String id) {
    return (DrawPile) idMgr.findInstance(id);
  }

  public static final String WIDTH = "width"; //NON-NLS
  public static final String HEIGHT = "height"; //NON-NLS
  public static final String ALLOW_MULTIPLE = "allowMultiple"; //NON-NLS
  public static final String ALLOW_SELECT = "allowSelect"; //NON-NLS
  public static final String SELECT_DISPLAY_PROPERTY = "selectDisplayProperty"; //NON-NLS
  public static final String SELECT_SORT_PROPERTY = "selectSortProperty"; //NON-NLS
  public static final String FACE_DOWN = "faceDown"; //NON-NLS
  public static final String DRAW_FACE_UP = "drawFaceUp"; //NON-NLS
  public static final String FACE_DOWN_REPORT_FORMAT = "faceDownFormat"; //NON-NLS
  public static final String FACE_DOWN_HOTKEY = "faceDownHotkey"; //NON-NLS
  public static final String SHUFFLE = "shuffle";                       // "Re-shuffle deck" - see below //NON-NLS
  public static final String SHUFFLE_REPORT_FORMAT = "shuffleFormat"; //NON-NLS
  public static final String SHUFFLE_HOTKEY = "shuffleHotkey"; //NON-NLS
  public static final String SHUFFLE_COMMAND = "shuffleCommand"; //NON-NLS //NON-NLS
  public static final String REVERSIBLE = "reversible"; //NON-NLS
  public static final String REVERSE_REPORT_FORMAT = "reverseFormat"; //NON-NLS
  public static final String REVERSE_HOTKEY = "reverseHotkey"; //NON-NLS
  public static final String REVERSE_COMMAND = "reverseCommand"; //NON-NLS
  public static final String DRAW = "draw"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS
  public static final String MAXSTACK = "maxStack"; //NON-NLS
  public static final String EXPRESSIONCOUNTING = "expressionCounting"; //NON-NLS
  public static final String COUNTEXPRESSIONS = "countExpressions"; //NON-NLS
  public static final String RESHUFFLABLE = "reshufflable";             // "Send to another deck" -- see above //NON-NLS
  public static final String RESHUFFLE_COMMAND = "reshuffleCommand"; //NON-NLS
  public static final String RESHUFFLE_TARGET = "reshuffleTarget"; //NON-NLS
  public static final String RESHUFFLE_MESSAGE = "reshuffleMessage"; //NON-NLS
  public static final String RESHUFFLE_HOTKEY = "reshuffleHotkey"; //NON-NLS
  public static final String REPORT_FORMAT = "reportFormat"; //NON-NLS
  public static final String CAN_SAVE = "canSave"; //NON-NLS
  public static final String HOTKEY_ON_EMPTY = "hotkeyOnEmpty"; //NON-NLS
  public static final String EMPTY_HOTKEY = "emptyHotkey"; //NON-NLS
  public static final String RESTRICT_OPTION = "restrictOption"; //NON-NLS
  public static final String RESTRICT_EXPRESSION = "restrictExpression"; //NON-NLS

  public static final String ALWAYS = "Always"; //NON-NLS
  public static final String NEVER = "Never"; //NON-NLS
  public static final String USE_MENU = "Via right-click Menu"; //NON-NLS

  public static final String COMMAND_NAME = "commandName"; //NON-NLS
  public static final String DECK_NAME = "deckName"; //NON-NLS

  public static class Prompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ ALWAYS, NEVER, USE_MENU };
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] { "Editor.always",
                            "Editor.never",
                            "Editor.DrawPile.use_menu"
      };
    }
  }

  /**
   * generates a prompt with the names of all decks already defined
   */
  public static class AssignedDeckPrompt extends TranslatableStringEnum {
    public static final String NONE = "<none>"; //NON-NLS
    public static final String NONE_NAME = Resources.getString("Editor.DrawPile.none");

    /**
     * For this one we need to use pre-translated display names.
     * @return true
     */
    @Override
    public boolean isDisplayNames() {
      return true;
    }

    /**
     * looks for the names of all decks already defined
     */
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      final ArrayList<String> l = new ArrayList<>();
      l.add(NONE);
      for (final GameComponent g :
           GameModule.getGameModule().getGameState().getGameComponents()) {
        if (g instanceof Map) {
          for (final DrawPile dp : ((Map) g).getComponentsOf(DrawPile.class)) {
            if (dp.getConfigureName() != null)
              l.add(dp.getConfigureName());
          }
        }
      }
      return l.toArray(new String[0]);
    }

    /**
     * looks for the names of all decks already defined
     */
    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      final List<String> l = new ArrayList<>();
      l.add(NONE_NAME);
      for (final GameComponent g :
        GameModule.getGameModule().getGameState().getGameComponents()) {
        if (g instanceof Map) {
          for (final DrawPile dp : ((Map) g).getComponentsOf(DrawPile.class)) {
            if (dp.getLocalizedConfigureName() != null)
              l.add(dp.getLocalizedConfigureName());
          }
        }
      }
      return l.toArray(new String[0]);
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      OWNING_BOARD,
      X_POSITION,
      Y_POSITION,
      WIDTH,
      HEIGHT,
      ALLOW_MULTIPLE,
      ALLOW_SELECT,
      SELECT_DISPLAY_PROPERTY,
      SELECT_SORT_PROPERTY,
      FACE_DOWN,
      DRAW_FACE_UP,
      FACE_DOWN_REPORT_FORMAT,
      SHUFFLE,                        // These commands match "Reshuffle" in the visible menus
      SHUFFLE_COMMAND,
      SHUFFLE_REPORT_FORMAT,
      SHUFFLE_HOTKEY,
      REVERSIBLE,
      REVERSE_COMMAND,
      REVERSE_REPORT_FORMAT,
      REVERSE_HOTKEY,
      DRAW,
      COLOR,
      HOTKEY_ON_EMPTY,
      EMPTY_HOTKEY,
      RESHUFFLABLE,                  // These commands match "Send to another deck" in the visible menus
      RESHUFFLE_COMMAND,
      RESHUFFLE_MESSAGE,
      RESHUFFLE_HOTKEY,
      RESHUFFLE_TARGET,
      CAN_SAVE,
      MAXSTACK,
      EXPRESSIONCOUNTING,
      COUNTEXPRESSIONS,
      RESTRICT_OPTION,
      RESTRICT_EXPRESSION
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString(Resources.NAME_LABEL),
        Resources.getString("Editor.DrawPile.owning_board"), //$NON-NLS-1$
        Resources.getString("Editor.x_position"), //$NON-NLS-1$
        Resources.getString("Editor.y_position"), //$NON-NLS-1$
        Resources.getString("Editor.width"), //$NON-NLS-1$
        Resources.getString("Editor.height"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.multi_draw"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.specific_draw"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.list_cards"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.sort_cards"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.facedown"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.faceup"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.facedown_report"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reshuffle"), //$NON-NLS-1$        // Internally these match "SHUFFLE"
        Resources.getString("Editor.DrawPile.reshuffle_text"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reshuffle_report"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reshuffle_key"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reverse"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reverse_text"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reverse_report"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.reverse_key"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.outline"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.color"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.empty_key"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.empty_keyfrom"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.send_deck"), //$NON-NLS-1$        // Internally these match "RESHUFFLE"
        Resources.getString("Editor.DrawPile.send_text"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.send_report"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.send_key"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.send_deck_name"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.saved"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.maxdisplay"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.perform_express"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.count_express"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.restrict_drag"), //$NON-NLS-1$
        Resources.getString("Editor.DrawPile.match_express"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class, // NAME
      OwningBoardPrompt.class, // OWNING_BOARD
      Integer.class, // X_POSITION
      Integer.class, // Y_POSITION
      Integer.class, // WIDTH
      Integer.class, // HEIGHT
      Boolean.class, // ALLOW_MULTIPLE
      Boolean.class, // ALLOW_SELECT
      PiecePropertyConfig.class, // SELECT_DISPLAY_PROPERTY
      String.class, // SELECT_SORT_PROPERTY
      Prompt.class, // FACE_DOWN
      Boolean.class, // DRAW_FACE_UP
      FormattedStringConfig.class, // FACE_DOWN_REPORT_FORMAT
      Prompt.class, // SHUFFLE                                    // These map to "re-shuffle"
      String.class, // SHUFFLE_COMMAND
      FormattedStringConfig.class, // SHUFFLE_REPORT_FORMAT
      NamedKeyStroke.class, // SHUFFLE_HOTKEY
      Boolean.class, // REVERSIBLE
      String.class, // REVERSE_COMMAND
      FormattedStringConfig.class, // REVERSE_REPORT_FORMAT
      NamedKeyStroke.class, // REVERSE_HOTKEY
      Boolean.class, // DRAW
      Color.class, // COLOR
      Boolean.class, // HOTKEY_ON_EMPTY
      NamedKeyStroke.class, // EMPTY_HOTKEY
      Boolean.class, // RESHUFFLABLE
      String.class, // RESHUFFLE_COMMAND                  // These map to "send to an other deck"
      FormattedStringConfig.class, // RESHUFFLE_MESSAGE
      NamedKeyStroke.class, // RESHUFFLE_HOTKEY
      AssignedDeckPrompt.class, // RESHUFFLE_TARGET
      Boolean.class, // CAN_SAVE
      Integer.class, // MAXSTACK
      Boolean.class, // EXPRESSIONCOUNTING
      String[].class, // COUNTEXPRESSIONS
      Boolean.class, // RESTRICT_OPTION
      PropertyExpression.class //RESTRICT_EXPRESSION
    };
  }

  public static class FormattedStringConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{DECK_NAME,
                                                                           COMMAND_NAME});
    }
  }

  public static class PiecePropertyConfig implements TranslatableConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new GamePieceFormattedStringConfigurer(key, name);
    }
  }

  public boolean isReshufflable() {
    return "true".equals(getAttributeValueString(RESHUFFLABLE));  //NON-NLS
  }

  @Override
  public String getAttributeValueString(String key) {
    if (WIDTH.equals(key)) {
      return String.valueOf(dummy.getSize().width);
    }
    else if (HEIGHT.equals(key)) {
      return String.valueOf(dummy.getSize().height);
    }
    else if (FACE_DOWN.equals(key)) {
      return dummy.getFaceDownOption();
    }
    else if (DRAW_FACE_UP.equals(key)) {
      return String.valueOf(dummy.isDrawFaceUp());
    }
    else if (SHUFFLE.equals(key)) {
      return dummy.getShuffleOption();
    }
    else if (REVERSIBLE.equals(key)) {
      return String.valueOf(dummy.isReversible());
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      return String.valueOf(dummy.isAllowMultipleDraw());
    }
    else if (ALLOW_SELECT.equals(key)) {
      return String.valueOf(dummy.isAllowSelectDraw());
    }
    else if (SELECT_DISPLAY_PROPERTY.equals(key)) {
      return dummy.getSelectDisplayProperty();
    }
    else if (SELECT_SORT_PROPERTY.equals(key)) {
      return dummy.getSelectSortProperty();
    }
    else if (DRAW.equals(key)) {
      return String.valueOf(dummy.isDrawOutline());
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(dummy.getOutlineColor());
    }
    else if (MAXSTACK.equals(key)) {
      return String.valueOf(dummy.getMaxStack());
    }
    else if (EXPRESSIONCOUNTING.equals(key)) {
      return String.valueOf(dummy.doesExpressionCounting());
    }
    else if (COUNTEXPRESSIONS.equals(key)) {
      return StringArrayConfigurer.arrayToString(dummy.getCountExpressions());
    }
    else if (RESHUFFLABLE.equals(key)) {
      // We check all of these due to Bug 11112 making the value of
      // reshufflable unreliable in modules created prior to 3.2.13.
      return String.valueOf(
        reshufflable ||
        dummy.getReshuffleCommand().length() > 0 ||
        dummy.getReshuffleTarget().length() > 0 ||
        dummy.getReshuffleMsgFormat().length() > 0 ||
        dummy.getReshuffleKey() != NamedKeyStroke.NULL_KEYSTROKE
      );
    }
    else if (RESHUFFLE_COMMAND.equals(key)) {
      return dummy.getReshuffleCommand();
    }
    else if (RESHUFFLE_TARGET.equals(key)) {
      return dummy.getReshuffleTarget();
    }
    else if (RESHUFFLE_MESSAGE.equals(key)) {
      return dummy.getReshuffleMsgFormat();
    }
    else if (RESHUFFLE_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(dummy.getReshuffleKey());
    }
    else if (SHUFFLE_COMMAND.equals(key)) {
      return dummy.getShuffleCommand();
    }
    else if (SHUFFLE_REPORT_FORMAT.equals(key)) {
      return dummy.getShuffleMsgFormat();
    }
    else if (SHUFFLE_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(dummy.getShuffleKey());
    }
    else if (REVERSE_COMMAND.equals(key)) {
      return dummy.getReverseCommand();
    }
    else if (REVERSE_REPORT_FORMAT.equals(key)) {
      return dummy.getReverseMsgFormat();
    }
    else if (REVERSE_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(dummy.getReverseKey());
    }
    else if (FACE_DOWN_REPORT_FORMAT.equals(key)) {
      return dummy.getFaceDownMsgFormat();
    }
    else if (CAN_SAVE.equals(key)) {
      return String.valueOf(dummy.isPersistable());
    }
    else if (HOTKEY_ON_EMPTY.equals(key)) {
      return String.valueOf(dummy.isHotkeyOnEmpty());
    }
    else if (EMPTY_HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(dummy.getNamedEmptyKey());
    }
    else if (RESTRICT_OPTION.equals(key)) {
      return String.valueOf(dummy.isRestrictOption());
    }
    else if (RESTRICT_EXPRESSION.equals(key)) {
      return dummy.getRestrictExpression().getExpression();
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (value == null) {
      return;
    }
    if (WIDTH.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      dummy.getSize().width = (Integer) value;
    }
    else if (HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      dummy.getSize().height = (Integer) value;
    }
    else if (FACE_DOWN.equals(key)) {
      dummy.setFaceDownOption((String) value);
    }
    else if (DRAW_FACE_UP.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setDrawFaceUp(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setDrawFaceUp("true".equals(value)); //NON-NLS
      }
    }
    else if (CAN_SAVE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setPersistable(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setPersistable("true".equals(value)); //NON-NLS
      }
    }
    else if (SHUFFLE.equals(key)) {
      dummy.setShuffleOption((String) value);
    }
    else if (REVERSIBLE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setReversible(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setReversible("true".equals(value)); //NON-NLS
      }
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setAllowMultipleDraw(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setAllowMultipleDraw("true".equals(value)); //NON-NLS
      }
    }
    else if (ALLOW_SELECT.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setAllowSelectDraw(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setAllowSelectDraw("true".equals(value)); //NON-NLS
      }
    }
    else if (SELECT_DISPLAY_PROPERTY.equals(key)) {
      dummy.setSelectDisplayProperty((String)value);
    }
    else if (SELECT_SORT_PROPERTY.equals(key)) {
      dummy.setSelectSortProperty((String)value);
    }
    else if (DRAW.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setDrawOutline(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setDrawOutline("true".equals(value)); //NON-NLS
      }
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      dummy.setOutlineColor((Color) value);
    }
    if (MAXSTACK.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      dummy.setMaxStack((Integer) value);
    }
    if (EXPRESSIONCOUNTING.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setExpressionCounting(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setExpressionCounting("true".equals(value)); //NON-NLS
      }
    }
    if (COUNTEXPRESSIONS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      dummy.setCountExpressions((String[]) value);
    }
    else if (RESHUFFLABLE.equals(key)) {
      reshufflable = "true".equals(value) || Boolean.TRUE.equals(value); //NON-NLS
      if (!reshufflable) {
        dummy.setReshuffleCommand("");
        dummy.setReshuffleKey(NamedKeyStroke.NULL_KEYSTROKE);
        dummy.setReshuffleTarget("");
        dummy.setReshuffleMsgFormat("");
      }
    }
    else if (RESHUFFLE_COMMAND.equals(key)) {
      dummy.setReshuffleCommand((String) value);
    }
    else if (RESHUFFLE_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      dummy.setReshuffleKey((NamedKeyStroke) value);
    }
    else if (RESHUFFLE_TARGET.equals(key)) {
      dummy.setReshuffleTarget((String) value);
    }
    else if (RESHUFFLE_MESSAGE.equals(key)) {
      dummy.setReshuffleMsgFormat((String) value);
    }
    else if (REVERSE_COMMAND.equals(key)) {
      dummy.setReverseCommand((String) value);
    }
    else if (REVERSE_REPORT_FORMAT.equals(key)) {
      dummy.setReverseMsgFormat((String) value);
    }
    else if (REVERSE_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      dummy.setReverseKey((NamedKeyStroke) value);
    }
    else if (SHUFFLE_COMMAND.equals(key)) {
      dummy.setShuffleCommand((String) value);
    }
    else if (SHUFFLE_REPORT_FORMAT.equals(key)) {
      dummy.setShuffleMsgFormat((String) value);
    }
    else if (SHUFFLE_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      dummy.setShuffleKey((NamedKeyStroke) value);
    }
    else if (FACE_DOWN_REPORT_FORMAT.equals(key)) {
      dummy.setFaceDownMsgFormat((String) value);
    }
    else if (NAME.equals(key)) {
      dummy.setDeckName((String) value);
      super.setAttribute(key, value);
    }
    else if (HOTKEY_ON_EMPTY.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setHotkeyOnEmpty(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setHotkeyOnEmpty("true".equals(value)); //NON-NLS
      }
    }
    else if (EMPTY_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      dummy.setEmptyKey((NamedKeyStroke) value);
    }
    else if (RESTRICT_OPTION.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setRestrictOption(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setRestrictOption("true".equals(value)); //NON-NLS
      }
    }
    else if (RESTRICT_EXPRESSION.equals(key)) {
      if (value instanceof String) {
        value = new PropertyExpression((String) value);
      }
      dummy.setRestrictExpression((PropertyExpression) value);
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return colorVisibleCondition;
    }
    else if (List.of(RESHUFFLE_COMMAND, RESHUFFLE_MESSAGE, RESHUFFLE_TARGET, RESHUFFLE_HOTKEY).contains(name)) {
      return reshuffleVisibleCondition;
    }
    else if (FACE_DOWN_REPORT_FORMAT.equals(name)) {
      return faceDownFormatVisibleCondition;
    }
    else if (List.of(SHUFFLE_REPORT_FORMAT, SHUFFLE_HOTKEY, SHUFFLE_COMMAND).contains(name)) {
      return shuffleFormatVisibleCondition;
    }
    else if (List.of(REVERSE_REPORT_FORMAT, REVERSE_HOTKEY, REVERSE_COMMAND).contains(name)) {
      return reverseFormatVisibleCondition;
    }
    else if (COUNTEXPRESSIONS.equals(name)) {
      return expressionCountingVisibleCondition;
    }
    else if (EMPTY_HOTKEY.equals(name)) {
      return hotkeyOnEmptyVisibleCondition;
    }
    else if (SELECT_DISPLAY_PROPERTY.equals(name) || SELECT_SORT_PROPERTY.equals(name)) {
      return selectionAllowedVisibleCondition;
    }
    else if (RESTRICT_EXPRESSION.equals(name)) {
      return restrictExpressionVisibleCondition;
    }
    else {
      return null;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{CardSlot.class, DeckGlobalKeyCommand.class};
  }

  public Point getPosition() {
    final Point p = new Point(pos);
    final Board b = map.getBoardByName(owningBoardName);
    if (b != null) {
      p.translate(b.bounds().x, b.bounds().y);
    }
    return p;
  }

  public Map getMap() {
    return map;
  }

  public Rectangle boundingBox() {
    return (myDeck == null) ? null : myDeck.boundingBox();
  }

  public Command addToContents(GamePiece p) {
    // If the piece is already in the Deck, do nothing
    if (myDeck != null && myDeck.indexOf(p) >= 0) {
      return new NullCommand();
    }
    // Merge it in
    return map.placeOrMerge(p, myDeck == null ? getPosition() : myDeck.getPosition());
  }

  @Override
  protected Stack initializeContents() {
    final Stack s = super.initializeContents();
    myDeck = new Deck(GameModule.getGameModule(), getDeckType());
    myDeck.setPropertySource(source);
    s.asList().forEach(gamePiece -> myDeck.add(gamePiece));
    myDeck.setFaceDown(!Deck.NEVER.equals(dummy.getFaceDownOption()));
    return myDeck;
  }

  public void setDeck(Deck deck) {
    myDeck = deck;
  }

  public Deck getDeck() {
    return myDeck;
  }

  @Override
  protected boolean placeNonStackingSeparately() {
    return false;
  }

  protected String getDeckType() {
    return dummy.getType();
  }

  public Dimension getSize() {
    return dummy.getSize();
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Deck.html"); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.DrawPile.deck"); //$NON-NLS-1$
  }

  @Override
  public ComponentI18nData getI18nData() {
    final ComponentI18nData myI18nData = super.getI18nData();
    myI18nData.setAttributeTranslatable(SELECT_DISPLAY_PROPERTY, false);
    myI18nData.setAttributeTranslatable(SELECT_SORT_PROPERTY, false);
    return myI18nData;
  }

  @Override
  public Object getProperty(Object key) {
    if (source != null) {
      return source.getProperty(key);
    }
    return null;
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (source != null) {
      return source.getLocalizedProperty(key);
    }
    return null;
  }

  /**
   * Implement PropertyNameSource - Expose the numPieces property
   * and any counting properties.
   */
  @Override
  public List<String> getPropertyNames() {
    final List<String> l = new ArrayList<>();
    l.add(getConfigureName() + "_numPieces"); //NON-NLS
    for (final String ce : dummy.getCountExpressions()) {
      l.add(getConfigureName() + "_" + (new Deck.CountExpression(ce)).getName());
    }
    return l;
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> l = new ArrayList<>(super.getExpressionList());
    if (dummy != null) {
      if (dummy.isRestrictOption()) {
        l.add(dummy.getRestrictExpression().getExpression());
      }
      if (dummy.doesExpressionCounting()) {
        l.addAll(Arrays.asList(dummy.getCountExpressions()));
      }
      l.add(dummy.getSelectDisplayProperty());
      l.add(dummy.getSelectSortProperty());
    }
    return l;
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    if (dummy != null) {
      return List.of(dummy.getFaceDownMsgFormat(), dummy.getReshuffleMsgFormat(), dummy.getReverseMsgFormat(), dummy.getShuffleMsgFormat());
    }
    else {
      return Collections.emptyList();
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    if (dummy != null) {
      return List.of(dummy.getSelectDisplayProperty(),
                     dummy.getSelectSortProperty());
    }
    return Collections.emptyList();
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    final List<String> l = new ArrayList<>();
    if (dummy != null) {
      if (USE_MENU.equals(dummy.getShuffleOption())) { // Confusingly, the term "shuffle" internally matches to "Re-shuffle" in external menus...
        l.add(dummy.getShuffleCommand());
      }

      if (isReshufflable()) {                          // ... whereas this one matches to "send to deck".
        l.add(dummy.getReshuffleCommand());
      }

      if (dummy.isReversible()) {
        l.add(dummy.getReverseCommand());
      }
    }

    return l;
  }

  @Override
  public void setup(boolean gameStarting) {
    super.setup(gameStarting);
    if (myDeck != null) {
      if (gameStarting) {
        myDeck.addListeners();
      }
      else {
        myDeck.removeListeners();
      }
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    if (dummy != null) {
      return Arrays.asList(dummy.getNamedEmptyKey(), dummy.getReshuffleKey(), dummy.getReverseKey(), dummy.getShuffleKey());
    }
    else {
      return Collections.emptyList();
    }
  }
}
