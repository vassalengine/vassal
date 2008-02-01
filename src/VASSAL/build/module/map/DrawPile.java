/*
 * $Id$
 *
 * Copyright (c) 2000-2004 by Rodney Kinney and Michael Blumoehr
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
import java.util.Enumeration;

import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.widget.CardSlot;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.GamePieceFormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.UniqueIdManager;

public class DrawPile extends SetupStack {
  protected Deck dummy = new Deck(); // Used for storing type information
  protected boolean reshufflable;
  protected Deck myDeck;
  private VisibilityCondition colorVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.isDrawOutline();
    }
  };
  private VisibilityCondition reshuffleVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return reshufflable;
    }
  };
  private VisibilityCondition faceDownFormatVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.getFaceDownOption().equals(USE_MENU);
    }
  };
  private VisibilityCondition reverseFormatVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.isReversible();
    }
  };
  private VisibilityCondition shuffleFormatVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.getShuffleOption().equals(USE_MENU);
    }
  };
  private VisibilityCondition expressionCountingVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.doesExpressionCounting();
    }
  };
  private VisibilityCondition hotkeyOnEmptyVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.isHotkeyOnEmpty();
    }
  };
  private VisibilityCondition selectionAllowedVisibleCondition = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return dummy.isAllowSelectDraw();
    }
  };
  protected static UniqueIdManager idMgr = new UniqueIdManager("Deck");

  public void addTo(Buildable parent) {
    super.addTo(parent);
    idMgr.add(this);
    setAttributeTranslatable(NAME, true);
  }

  protected JPopupMenu buildPopup() {
    JPopupMenu popup = new JPopupMenu();
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


  public final static String WIDTH = "width";
  public final static String HEIGHT = "height";
  public static final String ALLOW_MULTIPLE = "allowMultiple";
  public static final String ALLOW_SELECT = "allowSelect";
  public static final String SELECT_DISPLAY_PROPERTY = "selectDisplayProperty";
  public static final String SELECT_SORT_PROPERTY = "selectSortProperty";
  public static final String FACE_DOWN = "faceDown";
  public static final String DRAW_FACE_UP = "drawFaceUp";
  public static final String FACE_DOWN_REPORT_FORMAT = "faceDownFormat";
  public static final String SHUFFLE = "shuffle";
  public static final String SHUFFLE_REPORT_FORMAT = "shuffleFormat";
  public static final String SHUFFLE_HOTKEY = "shuffleHotkey";
  public static final String REVERSIBLE = "reversible";
  public static final String REVERSE_REPORT_FORMAT = "reverseFormat";
  public static final String DRAW = "draw";
  public static final String COLOR = "color";
  public static final String MAXSTACK = "maxStack";
  public static final String EXPRESSIONCOUNTING = "expressionCounting";
  public static final String COUNTEXPRESSIONS = "countExpressions";
  public static final String RESHUFFLABLE = "reshufflable";
  public static final String RESHUFFLE_COMMAND = "reshuffleCommand";
  public static final String RESHUFFLE_TARGET = "reshuffleTarget";
  public static final String RESHUFFLE_MESSAGE = "reshuffleMessage";
  public static final String RESHUFFLE_HOTKEY = "reshuffleHotkey";
  public static final String REPORT_FORMAT = "reportFormat";
  public static final String CAN_SAVE = "canSave";
  public static final String HOTKEY_ON_EMPTY = "hotkeyOnEmpty";
  public static final String EMPTY_HOTKEY = "emptyHotkey"; 

  public static final String ALWAYS = "Always";
  public static final String NEVER = "Never";
  public static final String USE_MENU = "Via right-click Menu";

  public static final String COMMAND_NAME = "commandName";
  public static final String DECK_NAME = "deckName";

  public static class Prompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALWAYS, NEVER, USE_MENU};
    }
  }

  /**
   * generates a prompt with the names of all decks already defined
   */
  public static class AssignedDeckPrompt extends StringEnum {
    public static final String NONE = "<none>";

    public AssignedDeckPrompt() {
      // do nothing
    }

    /**
     * looks for the names of all decks already defined
     */
    public String[] getValidValues(AutoConfigurable target) {
      ArrayList<String> l = new ArrayList<String>();
      l.add(NONE);
      for (GameComponent g :
           GameModule.getGameModule().getGameState().getGameComponents()) {
        if (g instanceof Map) {
          for (DrawPile dp : ((Map) g).getComponentsOf(DrawPile.class)) {
            if (dp.getConfigureName() != null)
              l.add(dp.getConfigureName());
          }
        }
      }
      return l.toArray(new String[l.size()]);
    }
  }

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
      SHUFFLE,
      SHUFFLE_REPORT_FORMAT,
      SHUFFLE_HOTKEY,
      REVERSIBLE,
      REVERSE_REPORT_FORMAT,
      DRAW,
      COLOR,
      HOTKEY_ON_EMPTY,
      EMPTY_HOTKEY,
      RESHUFFLABLE,
      RESHUFFLE_COMMAND,
      RESHUFFLE_MESSAGE,
      RESHUFFLE_HOTKEY,
      RESHUFFLE_TARGET,
      CAN_SAVE,
      MAXSTACK,
      EXPRESSIONCOUNTING,
      COUNTEXPRESSIONS
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Name:  ",
      "Belongs to board:  ",
      "X position:  ",
      "Y position:  ",
      "Width:  ",
      "Height:  ",
      "Allow Multiple Cards to be Drawn?",
      "Allow Specific Cards to be Drawn?",
      "When selecting, list cards using",
      "When selecting, sort cards by",
      "Contents are Face-down:  ",
      "Draw new cards face up?",
      "Face-down Report Format:  ",
      "Re-shuffle:  ",
      "Re-shuffle Report Format:  ",
      "Re-shuffle Hot Key:  ",
      "Reversible?",
      "Reverse Report Format:  ",
      "Draw Outline when empty?",
      "Color:  ",
      "Send Hotkey when empty?",
      "Hot Key to send when Deck empties:  ",
      "Include command to send entire deck to another deck?",
      "Send Menu text:  ",
      "Send Report Format:  ",
      "Send Hot Key:  ",
      "Name of deck to send to:  ",
      "Can be saved-to/loaded-from a file?",
      "Maximum Cards to display in Stack:",
      "Perform counting of property expressions?",
      "Expressions to count:"
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      OwningBoardPrompt.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Boolean.class,
      Boolean.class,
      PiecePropertyConfig.class,
      String.class,
      Prompt.class,
      Boolean.class,
      FormattedStringConfig.class,
      Prompt.class,
      FormattedStringConfig.class,
      KeyStroke.class,
      Boolean.class,
      FormattedStringConfig.class,
      Boolean.class,
      Color.class,
      Boolean.class,
      KeyStroke.class,
      Boolean.class,
      String.class,
      FormattedStringConfig.class,
      KeyStroke.class,
      AssignedDeckPrompt.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      String[].class
    };
  }

  public static class FormattedStringConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{DECK_NAME,
                                                                           COMMAND_NAME});
    }
  }

  public static class PiecePropertyConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new GamePieceFormattedStringConfigurer(key, name);
    }
  }
  public String getAttributeValueString(String key) {
    if (WIDTH.equals(key)) {
      return "" + dummy.getSize().width;
    }
    else if (HEIGHT.equals(key)) {
      return "" + dummy.getSize().height;
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
      return "" + dummy.isReversible();
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      return "" + dummy.isAllowMultipleDraw();
    }
    else if (ALLOW_SELECT.equals(key)) {
      return "" + dummy.isAllowSelectDraw();
    }
    else if (SELECT_DISPLAY_PROPERTY.equals(key)) {
      return dummy.getSelectDisplayProperty();
    }
    else if (SELECT_SORT_PROPERTY.equals(key)) {
      return dummy.getSelectSortProperty();
    }
    else if (DRAW.equals(key)) {
      return "" + dummy.isDrawOutline();
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(dummy.getOutlineColor());
    }
    else if (MAXSTACK.equals(key)) {
      return "" + dummy.getMaxStack();
    }
    else if (EXPRESSIONCOUNTING.equals(key)) {
      return "" + dummy.doesExpressionCounting();
    }
    else if (COUNTEXPRESSIONS.equals(key)) {
      return StringArrayConfigurer.arrayToString(dummy.getCountExpressions());
    }
    else if (RESHUFFLABLE.equals(key)) {
      return "" + (dummy.getReshuffleCommand().length() > 0);
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
      return HotKeyConfigurer.encode(dummy.getReshuffleKey());
    }
    else if (SHUFFLE_REPORT_FORMAT.equals(key)) {
      return dummy.getShuffleMsgFormat();
    }
    else if (SHUFFLE_HOTKEY.equals(key)) {
      return HotKeyConfigurer.encode(dummy.getShuffleKey());
    }
    else if (REVERSE_REPORT_FORMAT.equals(key)) {
      return dummy.getReverseMsgFormat();
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
      return HotKeyConfigurer.encode(dummy.getEmptyKey());
    }
    else {
      return super.getAttributeValueString(key);
    }
  }


  public void setAttribute(String key, Object value) {
    if (value == null) {
      return;
    }
    if (WIDTH.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      dummy.getSize().width = ((Integer) value).intValue();
    }
    else if (HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      dummy.getSize().height = ((Integer) value).intValue();
    }
    else if (FACE_DOWN.equals(key)) {
      dummy.setFaceDownOption((String) value);
    }
    else if (DRAW_FACE_UP.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setDrawFaceUp(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setDrawFaceUp("true".equals(value));
      }
    }
    else if (CAN_SAVE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setPersistable(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setPersistable("true".equals(value));
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
        dummy.setReversible("true".equals(value));
      }
    }
    else if (ALLOW_MULTIPLE.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setAllowMultipleDraw(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setAllowMultipleDraw("true".equals(value));
      }
    }
    else if (ALLOW_SELECT.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setAllowSelectDraw(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setAllowSelectDraw("true".equals(value));
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
        dummy.setDrawOutline("true".equals(value));
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
        value = new Integer((String) value);
      }
      dummy.setMaxStack(((Integer) value).intValue());
    }    
    if (EXPRESSIONCOUNTING.equals(key)) {
      if (value instanceof Boolean) {
        dummy.setExpressionCounting(Boolean.TRUE.equals(value));
      }
      else {
        dummy.setExpressionCounting("true".equals(value));
      }
    }    
    if (COUNTEXPRESSIONS.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      dummy.setCountExpressions((String[]) value);
    }    
    else if (RESHUFFLABLE.equals(key)) {
      reshufflable = "true".equals(value) || Boolean.TRUE.equals(value);
      if (!reshufflable) {
        dummy.setReshuffleCommand("");
      }
    }
    else if (RESHUFFLE_COMMAND.equals(key)) {
      dummy.setReshuffleCommand((String) value);
    }
    else if (RESHUFFLE_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = HotKeyConfigurer.decode((String) value);
      }
      dummy.setReshuffleKey((KeyStroke) value);
    }
    else if (RESHUFFLE_TARGET.equals(key)) {
      dummy.setReshuffleTarget((String) value);
    }
    else if (RESHUFFLE_MESSAGE.equals(key)) {
      dummy.setReshuffleMsgFormat((String) value);
    }
    else if (REVERSE_REPORT_FORMAT.equals(key)) {
      dummy.setReverseMsgFormat((String) value);
    }
    else if (SHUFFLE_REPORT_FORMAT.equals(key)) {
      dummy.setShuffleMsgFormat((String) value);
    }
    else if (SHUFFLE_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = HotKeyConfigurer.decode((String) value);
      }
      dummy.setShuffleKey((KeyStroke) value);
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
        dummy.setHotkeyOnEmpty("true".equals(value));
      }
    }
    else if (EMPTY_HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = HotKeyConfigurer.decode((String) value);
      }
      dummy.setEmptyKey((KeyStroke) value);
    }
    else {
      super.setAttribute(key, value);
    }
  }


  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return colorVisibleCondition;
    }
    else if (RESHUFFLE_COMMAND.equals(name)
        || RESHUFFLE_MESSAGE.equals(name)
        || RESHUFFLE_TARGET.equals(name)
        || RESHUFFLE_HOTKEY.equals(name)) {
      return reshuffleVisibleCondition;
    }
    else if (FACE_DOWN_REPORT_FORMAT.equals(name)) {
      return faceDownFormatVisibleCondition;
    }
    else if (SHUFFLE_REPORT_FORMAT.equals(name) || SHUFFLE_HOTKEY.equals(name)) {
      return shuffleFormatVisibleCondition;
    }
    else if (REVERSE_REPORT_FORMAT.equals(name)) {
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
    else {
      return null;
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{CardSlot.class, DeckGlobalKeyCommand.class};
  }

  public Point getPosition() {
    Point p = new Point(pos);
    Board b = map.getBoardByName(owningBoardName);
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
    return map.placeOrMerge(p, myDeck == null ? getPosition() : myDeck.getPosition());
  }

  protected Stack initializeContents() {
    Stack s = super.initializeContents();
    myDeck = new Deck(getDeckType());
    for (Enumeration<GamePiece> e = s.getPieces(); e.hasMoreElements();) {
      myDeck.add(e.nextElement());
    }
    myDeck.setFaceDown(!Deck.NEVER.equals(dummy.getFaceDownOption()));
    return myDeck;
  }

  protected boolean placeNonStackingSeparately() {
    return false;
  }

  protected String getDeckType() {
    return dummy.getType();
  }
  
  public Dimension getSize() {
    return dummy.getSize();
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Deck.htm");
  }

  public static String getConfigureTypeName() {
    return "Deck";
  }
}
