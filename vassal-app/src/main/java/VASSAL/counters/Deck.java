/*
 *
 * Copyright (c) 2004-2012 by Rodney Kinney, Brent Easton
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Random;

import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.map.DeckGlobalKeyCommand;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.StackMetrics;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.ProblemDialog;

/**
 * A collection of pieces that behaves like a deck, i.e.: Doesn't move.
 * Can't be expanded. Can be shuffled. Can be turned face-up and face-down.
 */
public class Deck extends Stack implements PlayerRoster.SideChangeListener {
  public static final String ID = "deck;"; //$NON-NLS-1$
  public static final String ALWAYS = "Always"; // NON-NLS
  public static final String NEVER = "Never"; // NON-NLS
  public static final String USE_MENU = "Via right-click Menu"; // NON-NLS
  public static final String NO_USER = "nobody"; // Dummy user ID for turning // NON-NLS
  protected static final StackMetrics deckStackMetrics = new StackMetrics(false, 2, 2, 2, 2);
  // cards face down

  protected boolean drawOutline = true;
  protected Color outlineColor = Color.black;
  protected Dimension size = new Dimension(40, 40);
  protected boolean shuffle = true;
  protected String faceDownOption = ALWAYS;
  protected String shuffleOption = ALWAYS;
  protected String shuffleCommand = "";
  protected boolean allowMultipleDraw = false;
  protected boolean allowSelectDraw = false;
  protected boolean reversible = false;
  protected String reshuffleCommand = ""; //$NON-NLS-1$
  protected String reshuffleTarget;
  protected String reshuffleMsgFormat;
  protected NamedKeyStrokeListener reshuffleListener;
  protected NamedKeyStroke reshuffleKey;
  protected String reverseMsgFormat;
  protected String reverseCommand;
  protected NamedKeyStroke reverseKey;
  protected NamedKeyStrokeListener reverseListener;
  protected String shuffleMsgFormat;
  protected NamedKeyStrokeListener shuffleListener;
  protected NamedKeyStroke shuffleKey;
  protected String faceDownMsgFormat;
  protected boolean drawFaceUp;
  protected boolean persistable;
  protected FormattedString selectDisplayProperty = new FormattedString("$" + BasicPiece.BASIC_NAME + "$");
  protected String selectSortProperty = "";
  protected MutableProperty.Impl countProperty =
    new MutableProperty.Impl("", this);
  protected List<MutableProperty.Impl> expressionProperties = new ArrayList<>();

  protected String deckName;
  protected String localizedDeckName;

  protected boolean faceDown;
  protected int dragCount = 0;
  protected int maxStack = 10;
  protected CountExpression[] countExpressions = new CountExpression[0];
  protected boolean expressionCounting = false;
  protected List<GamePiece> nextDraw = null;
  protected KeyCommand[] commands;
  protected List<DeckGlobalKeyCommand> globalCommands = new ArrayList<>();
  protected boolean hotkeyOnEmpty;
  protected NamedKeyStroke emptyKey;
  protected boolean restrictOption;
  protected PropertyExpression restrictExpression = new PropertyExpression();
  protected PropertySource propertySource;

  /**
   * Special {@link CommandEncoder} to handle loading/saving Decks from files.
   */
  protected CommandEncoder commandEncoder = new CommandEncoder() {
    /**
     * Deserializes a Deck loaded from a file, turning it into a LoadDeckCommand.
     * @param command String contents of deck
     * @return {@link LoadDeckCommand} to put the specified contents in the deck
     */
    @Override
    public Command decode(String command) {
      if (!command.startsWith(LoadDeckCommand.PREFIX)) {
        return null;
      }
      return new LoadDeckCommand(Deck.this);
    }

    /**
     * Serializes a LoadDeckCommand
     * @param c LoadDeckCommand
     * @return string form of command
     */
    @Override
    public String encode(Command c) {
      if (!(c instanceof LoadDeckCommand)) {
        return null;
      }
      return LoadDeckCommand.PREFIX;
    }
  };

  private final GameModule gameModule;

  /**
   * Not for internal use, but required for initial build of module
   *
   * @deprecated use {@link #Deck(GameModule)}
   */
  @Deprecated
  public Deck() {
    this(GameModule.getGameModule());
  }

  /**
   * @deprecated use {@link #Deck(GameModule, String)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Deck(String type) {
    this(GameModule.getGameModule(), type);
    ProblemDialog.showDeprecated("2020-08-06");
  }

  /**
   * @deprecated use {@link #Deck(GameModule, String, PropertySource)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Deck(String type, PropertySource source) {
    this(GameModule.getGameModule(), type, source);
    ProblemDialog.showDeprecated("2020-08-06");
  }

  /**
   * Create an empty deck
   * @param gameModule The game module
   */
  public Deck(GameModule gameModule) {
    this(gameModule, ID);
  }

  /**
   * Creates an empty deck using specified type information
   * @param gameModule The game module
   * @param type Type information for the deck (the configuration information that does not change during the game)
   */
  public Deck(GameModule gameModule, String type) {
    this.gameModule = gameModule;
    mySetType(type);
  }

  /**
   * Creates an empty deck using specified type information
   * @param gameModule The game module
   * @param type Type information for the deck (the configuration information that does not change during the game)
   * @param source PropertySource
   */
  public Deck(GameModule gameModule, String type, PropertySource source) {
    this(gameModule, type);
    propertySource = source;
  }

  /**
   * Sets the Deck's property source
   * @param source PropertySource
   */
  public void setPropertySource(PropertySource source) {
    propertySource = source;
    if (globalCommands != null) {
      for (final DeckGlobalKeyCommand globalCommand : globalCommands) {
        globalCommand.setPropertySource(propertySource);
      }
    }
  }

  /**
   * Listener for when the local player changes side. Updates all of our counts of expressions of pieces configured to be counted.
   * @param oldSide local player's old side
   * @param newSide local player's new side
   */
  @Override
  public void sideChanged(String oldSide, String newSide) {
    updateCountsAll();
  }

  /**
   * Adds a Deck Global Key Command (DGKC).
   * @param globalCommand The command to add
   */
  public void addGlobalKeyCommand(DeckGlobalKeyCommand globalCommand) {
    globalCommands.add(globalCommand);
  }

  /**
   * Removes a Deck Global Key Command (DGKC).
   * @param globalCommand The command to remove
   */
  public void removeGlobalKeyCommand(DeckGlobalKeyCommand globalCommand) {
    globalCommands.remove(globalCommand);
  }

  /**
   * @return A list of Deck Global Key Commands for this deck, serialized by their encoder.
   */
  protected String[] getGlobalCommands() {
    final String[] commands = new String[globalCommands.size()];
    for (int i = 0; i < globalCommands.size(); i++) {
      commands[i] = globalCommands.get(i).encode();
    }
    return commands;
  }

  /**
   * Sets the list of Deck Global Key Commands for this deck
   * @param commands The list of commands to set (in serialized string form)
   */
  protected void setGlobalCommands(String[] commands) {
    globalCommands = new ArrayList<>(commands.length);
    for (final String command : commands) {
      globalCommands.add(new DeckGlobalKeyCommand(command, propertySource));
    }
  }

  /**
  * Update map-level count properties for all "expressions" of pieces that are configured
  * to be counted.  These are held in the String[] countExpressions.
  */
  private void updateCountsAll() {
    if (!doesExpressionCounting() || getMap() == null) {
      return;
    }
    //Clear out all of the registered count expressions
    for (int index = 0; index < countExpressions.length; index++) {
      expressionProperties.get(index).setPropertyValue("0"); //$NON-NLS-1$
    }
    //Increase all of the pieces with expressions specified in this deck
    asList().stream()
            .filter(Objects::nonNull)
            .forEach(p -> updateCounts(p, true));
  }

  /**
   * Update map-level count property for a piece located at index
   * @param index Index (within the Deck, counting from the top) of the piece whose count to update
   */
  private void updateCounts(int index) {
    if (!doesExpressionCounting()) {
      return;
    }
    if (index >= 0 && index < getPieceCount()) {
      final GamePiece p = getPieceAt(index);
      if (p == null) {
        //can't figure out the piece, do a full update
        updateCountsAll();
      }
      else {
        updateCounts(p, false);
      }
    }
    else {
      //can't figure out the piece, do a full update
      updateCountsAll();
    }
  }

  /**
   * Update map-level count property for a piece
   * @param p Game piece to update counts for
   * @param increase if true, increase the count; if false, decrease.
   */
  private void updateCounts(GamePiece p, boolean increase) {
    if (!doesExpressionCounting() || getMap() == null) {
      return;
    }
    //test all the expressions for this deck
    for (int index = 0; index < countExpressions.length; index++) {
      final MutableProperty.Impl prop = expressionProperties.get(index);
      final FormattedString formatted =
        new FormattedString(countExpressions[index].getExpression());
      final PieceFilter f = PropertiesPieceFilter.parse(formatted.getText());
      if (f.accept(p)) {
        final String mapProperty = prop.getPropertyValue();
        if (mapProperty != null) {
          int newValue = Integer.decode(mapProperty);
          if (increase) {
            newValue++;
          }
          else {
            newValue--;
          }
          prop.setPropertyValue(String.valueOf(newValue));
        }
      }
    }
  }

  /**
   * Set the <deckName>_numPieces property in the containing Map
   */
  protected void fireNumCardsProperty() {
    countProperty.setPropertyValue(String.valueOf(pieceCount));
  }

  /**
   * Inserts a piece into a specific position into the Deck (counting down from the top)
   * @param p Piece to insert
   * @param index "How many cards down into the Deck" to put it.
   */
  @Override
  protected void insertPieceAt(GamePiece p, int index) {
    super.insertPieceAt(p, index);
    updateCounts(p, true);
    fireNumCardsProperty();
  }

  /**
   * Removes a piece from a specific location in the deck
   * @param index Piece to remove, counting down from the top
   */
  @Override
  protected void removePieceAt(int index) {
    final int startCount = pieceCount;
    updateCounts(index);
    super.removePieceAt(index);
    fireNumCardsProperty();
    if (hotkeyOnEmpty && emptyKey != null && startCount > 0 && pieceCount == 0) {
      gameModule.fireKeyStroke(emptyKey);
    }
  }

  /**
   * Removes all pieces from the Deck.
   */
  @Override
  public void removeAll() {
    super.removeAll();
    updateCountsAll();
    fireNumCardsProperty();
  }

  /**
   * Sets the Map this Deck appears on
   * @param map Map to assign Deck to
   */
  @Override
  public void setMap(Map map) {
    if (map != getMap()) {
      countProperty.removeFromContainer();
      if (map != null) countProperty.addTo(map);
      for (final MutableProperty.Impl prop : expressionProperties) {
        prop.removeFromContainer();
        if (map != null) prop.addTo(map);
      }
    }
    super.setMap(map);
    updateCountsAll();
    fireNumCardsProperty();
  }


  public void addListeners() {
    if (shuffleListener == null) {
      shuffleListener = new NamedKeyStrokeListener(e -> {
        gameModule.sendAndLog(shuffle());
        repaintMap();
      });

      gameModule.addKeyStrokeListener(shuffleListener);
      shuffleListener.setKeyStroke(getShuffleKey());
    }

    if (reshuffleListener == null) {
      reshuffleListener = new NamedKeyStrokeListener(e -> {
        gameModule.sendAndLog(sendToDeck());
        repaintMap();
      });

      gameModule.addKeyStrokeListener(reshuffleListener);
      reshuffleListener.setKeyStroke(getReshuffleKey());
    }

    if (reverseListener == null) {
      reverseListener = new NamedKeyStrokeListener(e -> {
        gameModule.sendAndLog(reverse());
        repaintMap();
      });

      gameModule.addKeyStrokeListener(reverseListener);
      reverseListener.setKeyStroke(getReverseKey());
    }

    gameModule.addSideChangeListenerToPlayerRoster(this);
  }

  public void removeListeners() {
    if (shuffleListener != null) {
      gameModule.removeKeyStrokeListener(shuffleListener);
      shuffleListener = null;
    }

    if (reshuffleListener != null) {
      gameModule.removeKeyStrokeListener(reshuffleListener);
      reshuffleListener = null;
    }

    if (reverseListener != null) {
      gameModule.removeKeyStrokeListener(reverseListener);
      reverseListener = null;
    }

    gameModule.removeSideChangeListenerFromPlayerRoster(this);
  }


  /** Sets the information for this Deck.  See {@link Decorator#myGetType}
   *  @param type a serialized configuration string to
   *              set the "type information" of this Deck, which is
   *              information that doesn't change during the course of
   *              a single game (e.g. Image Files, Context Menu strings,
   *              etc, rules about when deck is shuffled, whether it is
   *              face-up or face down, etc).
   */
  protected void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    drawOutline = st.nextBoolean(true);
    outlineColor = ColorConfigurer.stringToColor(st.nextToken("0,0,0")); //$NON-NLS-1$
    size.setSize(st.nextInt(40), st.nextInt(40));
    faceDownOption = st.nextToken("Always"); //$NON-NLS-1$
    shuffleOption = st.nextToken("Always"); //$NON-NLS-1$
    allowMultipleDraw = st.nextBoolean(true);
    allowSelectDraw = st.nextBoolean(true);
    reversible = st.nextBoolean(true);
    reshuffleCommand = st.nextToken(""); //$NON-NLS-1$
    reshuffleTarget = st.nextToken(""); //$NON-NLS-1$
    reshuffleMsgFormat = st.nextToken(""); //$NON-NLS-1$
    setDeckName(st.nextToken(Resources.getString("Deck.deck")));
    shuffleMsgFormat = st.nextToken(""); //$NON-NLS-1$
    reverseMsgFormat = st.nextToken(""); //$NON-NLS-1$
    faceDownMsgFormat = st.nextToken(""); //$NON-NLS-1$
    drawFaceUp = st.nextBoolean(false);
    persistable = st.nextBoolean(false);
    shuffleKey = st.nextNamedKeyStroke(null);
    reshuffleKey = st.nextNamedKeyStroke(null);
    maxStack = st.nextInt(10);
    setCountExpressions(st.nextStringArray(0));
    expressionCounting = st.nextBoolean(false);
    setGlobalCommands(st.nextStringArray(0));
    hotkeyOnEmpty = st.nextBoolean(false);
    emptyKey = st.nextNamedKeyStroke(null);
    selectDisplayProperty.setFormat(st.nextToken("$" + BasicPiece.BASIC_NAME + "$"));
    selectSortProperty = st.nextToken("");
    restrictOption = st.nextBoolean(false);
    restrictExpression.setExpression(st.nextToken(""));
    shuffleCommand = st.nextToken(Resources.getString("Deck.shuffle"));
    reverseCommand = st.nextToken(Resources.getString("Deck.reverse"));
    reverseKey = st.nextNamedKeyStroke(null);

    final DrawPile myPile = DrawPile.findDrawPile(getDeckName());

    // If a New game/Load Game is starting, set this Deck into the matching DrawPile
    // If a Load Continuation is starting, ignore this Deck and let the DrawPile continue with the existing Deck.
    // Combined fix for bugs 4507, 10249 & 13461
    if (myPile != null && ! GameModule.getGameModule().getGameState().isGameStarted()) {
      myPile.setDeck(this);
    }
  }

  public String getFaceDownOption() {
    return faceDownOption;
  }

  /**
   * @return true if cards are turned face up when drawn from this deck
   */
  public boolean isDrawFaceUp() {
    return drawFaceUp;
  }

  public void setDrawFaceUp(boolean drawFaceUp) {
    this.drawFaceUp = drawFaceUp;
  }

  public void setFaceDownOption(String faceDownOption) {
    this.faceDownOption = faceDownOption;
    faceDown = !faceDownOption.equals(NEVER);
  }

  public Dimension getSize() {
    return size;
  }

  public void setSize(Dimension size) {
    this.size.setSize(size);
  }

  public String getShuffleOption() {
    return shuffleOption;
  }

  public void setShuffleOption(String shuffleOption) {
    this.shuffleOption = shuffleOption;
  }

  public boolean isShuffle() {
    return shuffle;
  }

  public int getMaxStack() {
    return maxStack;
  }

  @Override
  public int getMaximumVisiblePieceCount() {
    return Math.min(pieceCount, maxStack);
  }

  public String[] getCountExpressions() {
    final String[] fullstrings = new String[countExpressions.length];
    for (int index = 0; index < countExpressions.length; index++) {
      fullstrings[index] = countExpressions[index].getFullString();
    }
    return fullstrings;
  }

  public boolean doesExpressionCounting() {
    return expressionCounting;
  }

  public String getFaceDownMsgFormat() {
    return faceDownMsgFormat;
  }

  public void setFaceDownMsgFormat(String faceDownMsgFormat) {
    this.faceDownMsgFormat = faceDownMsgFormat;
  }

  public String getReverseMsgFormat() {
    return reverseMsgFormat;
  }

  public void setReverseMsgFormat(String reverseMsgFormat) {
    this.reverseMsgFormat = reverseMsgFormat;
  }

  public String getReverseCommand() {
    return reverseCommand;
  }

  public void setReverseCommand(String s) {
    reverseCommand = s;
  }

  public NamedKeyStroke getReverseKey() {
    return reverseKey;
  }

  public void setReverseKey(NamedKeyStroke reverseKey) {
    this.reverseKey = reverseKey;
  }

  public String getShuffleMsgFormat() {
    return shuffleMsgFormat;
  }

  public void setShuffleMsgFormat(String shuffleMsgFormat) {
    this.shuffleMsgFormat = shuffleMsgFormat;
  }

  public NamedKeyStroke getShuffleKey() {
    return shuffleKey;
  }

  public void setShuffleKey(NamedKeyStroke shuffleKey) {
    this.shuffleKey = shuffleKey;
  }

  public String getShuffleCommand() {
    return shuffleCommand;
  }

  public void setShuffleCommand(String s) {
    shuffleCommand = s;
  }

  public void setShuffle(boolean shuffle) {
    this.shuffle = shuffle;
  }

  public boolean isAllowMultipleDraw() {
    return allowMultipleDraw;
  }

  public void setAllowMultipleDraw(boolean allowMultipleDraw) {
    this.allowMultipleDraw = allowMultipleDraw;
  }

  public boolean isAllowSelectDraw() {
    return allowSelectDraw;
  }

  public void setMaxStack(int maxStack) {
    this.maxStack = maxStack;
  }

  public void setCountExpressions(String[] countExpressionsString) {
    final CountExpression[] c = new CountExpression[countExpressionsString.length];
    int goodExpressionCount = 0;
    for (int index = 0; index < countExpressionsString.length; index++) {
      final CountExpression n = new CountExpression(countExpressionsString[index]);
      if (n.getName() != null) {
        c[index] = n;
        goodExpressionCount++;
      }
    }

    this.countExpressions = Arrays.copyOf(c, goodExpressionCount);
    while (countExpressions.length > expressionProperties.size()) {
      expressionProperties.add(new MutableProperty.Impl("", this));
    }
    for (int i = 0; i < countExpressions.length; i++) {
      expressionProperties.get(i).setPropertyName(
        deckName + "_" + countExpressions[i].getName());
    }
  }

  public void setExpressionCounting(boolean expressionCounting) {
    this.expressionCounting = expressionCounting;
  }

  public void setAllowSelectDraw(boolean allowSelectDraw) {
    this.allowSelectDraw = allowSelectDraw;
  }

  public boolean isReversible() {
    return reversible;
  }

  public void setReversible(boolean reversible) {
    this.reversible = reversible;
  }

  public void setDeckName(String n) {
    if (Localization.getInstance().isTranslationInProgress()) {
      localizedDeckName = n;
    }
    else {
      deckName = n;
    }
    countProperty.setPropertyName(deckName + "_numPieces"); // NON-NLS
    for (int i = 0; i < countExpressions.length; ++i) {
      expressionProperties.get(i).setPropertyName(
        deckName + "_" + countExpressions[i].getName());
    }
  }

  public String getDeckName() {
    return deckName;
  }

  public String getLocalizedDeckName() {
    return localizedDeckName == null ? deckName : localizedDeckName;
  }

  /**
   * @return The popup menu text for the command that sends the entire deck to another deck
   */
  public String getReshuffleCommand() {
    return reshuffleCommand;
  }

  public void setReshuffleCommand(String reshuffleCommand) {
    this.reshuffleCommand = reshuffleCommand;
  }

  public NamedKeyStroke getReshuffleKey() {
    return reshuffleKey;
  }

  public void setReshuffleKey(NamedKeyStroke reshuffleKey) {
    this.reshuffleKey = reshuffleKey;
  }

  /**
   * The name of the {@link VASSAL.build.module.map.DrawPile} to which the
   * contents of this deck will be sent when the reshuffle command is selected
   */
  public String getReshuffleTarget() {
    return reshuffleTarget;
  }

  public void setReshuffleTarget(String reshuffleTarget) {
    this.reshuffleTarget = reshuffleTarget;
  }

  /**
   * @return The message to send to the chat window when the deck is reshuffled to another deck
   */
  public String getReshuffleMsgFormat() {
    return reshuffleMsgFormat;
  }

  public void setReshuffleMsgFormat(String reshuffleMsgFormat) {
    this.reshuffleMsgFormat = reshuffleMsgFormat;
  }

  public boolean isHotkeyOnEmpty() {
    return hotkeyOnEmpty;
  }

  public void setHotkeyOnEmpty(boolean b) {
    hotkeyOnEmpty = b;
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public KeyStroke getEmptyKey() {
    ProblemDialog.showDeprecated("2020-08-06");
    return emptyKey.getKeyStroke();
  }

  public NamedKeyStroke getNamedEmptyKey() {
    return emptyKey;
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void setEmptyKey(KeyStroke k) {
    ProblemDialog.showDeprecated("2020-08-06");
    emptyKey = NamedKeyStroke.of(k);
  }

  public void setEmptyKey(NamedKeyStroke k) {
    emptyKey = k;
  }

  public void setRestrictOption(boolean restrictOption) {
    this.restrictOption = restrictOption;
  }

  public boolean isRestrictOption() {
    return restrictOption;
  }

  public void setRestrictExpression(PropertyExpression restrictExpression) {
    this.restrictExpression = restrictExpression;
  }

  public PropertyExpression getRestrictExpression() {
    return restrictExpression;
  }

  /**
   * Does the specified GamePiece meet the rules to be contained
   * in this Deck.
   */
  public boolean mayContain(GamePiece piece) {
    if (! restrictOption || restrictExpression.isNull()) {
      return true;
    }
    else {
      return restrictExpression.accept(piece);
    }
  }

  @Override
  public String getType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(drawOutline)
      .append(ColorConfigurer.colorToString(outlineColor))
      .append(String.valueOf(size.width))
      .append(String.valueOf(size.height))
      .append(faceDownOption)
      .append(shuffleOption)
      .append(String.valueOf(allowMultipleDraw))
      .append(String.valueOf(allowSelectDraw))
      .append(String.valueOf(reversible))
      .append(reshuffleCommand)
      .append(reshuffleTarget)
      .append(reshuffleMsgFormat)
      .append(deckName)
      .append(shuffleMsgFormat)
      .append(reverseMsgFormat)
      .append(faceDownMsgFormat)
      .append(drawFaceUp)
      .append(persistable)
      .append(shuffleKey)
      .append(reshuffleKey)
      .append(String.valueOf(maxStack))
      .append(getCountExpressions())
      .append(expressionCounting)
      .append(getGlobalCommands())
      .append(hotkeyOnEmpty)
      .append(emptyKey)
      .append(selectDisplayProperty.getFormat())
      .append(selectSortProperty)
      .append(restrictOption)
      .append(restrictExpression)
      .append(shuffleCommand)
      .append(reverseCommand)
      .append(reverseKey);
    return ID + se.getValue();
  }

  /** Shuffle the contents of the Deck */
  public Command shuffle() {
    final GamePiece[] a = new GamePiece[pieceCount];
    System.arraycopy(contents, 0, a, 0, pieceCount);
    final List<GamePiece> l = Arrays.asList(a);
    DragBuffer.getBuffer().clear();
    Collections.shuffle(l, gameModule.getRNG());
    Command c = setContents(l);
    if (Map.isChangeReportingEnabled()) {
      c = c.append(reportCommand(shuffleMsgFormat, Resources.getString("Deck.shuffle"))); //$NON-NLS-1$
    }
    return c;
  }

  /**
   * Return a list if pieces in the Deck in Dealable order.
   * If this is an Always shuffle Deck, then shuffle the list of pieces, otherwise just
   * reverse the list order so that we deal from the top.
   *
   * @return List of pieces in Dealable order
   */
  public List<GamePiece> getOrderedPieces() {
    final List<GamePiece> pieces = asList();
    if (ALWAYS.equals(shuffleOption)) {
      Collections.shuffle(pieces, GameModule.getGameModule().getRNG());
    }
    else {
      Collections.reverse(pieces);
    }
    return pieces;
  }

  /**
   * Return an iterator of pieces to be drawn from the Deck. Normally, a random
   * piece will be drawn, but if the Deck supports it, the user may have
   * specified a particular set of pieces or a fixed number of pieces to select
   * with the next draw.
   */
  public PieceIterator drawCards() {
    final Iterator<GamePiece> it;
    if (nextDraw != null) {
      it = nextDraw.iterator();
    }
    else if (getPieceCount() == 0) {
      it = Collections.emptyIterator();
    }
    else {
      int count = Math.max(dragCount, Math.min(1, getPieceCount()));
      final ArrayList<GamePiece> pieces = new ArrayList<>();
      if (ALWAYS.equals(shuffleOption)) {
        // Instead of shuffling the entire deck, just pick <b>count</b> random elements
        final ArrayList<Integer> indices = new ArrayList<>();
        for (int i = 0; i < getPieceCount(); ++i) {
          indices.add(i);
        }

        final Random rng = gameModule.getRNG();

        while (count-- > 0 && !indices.isEmpty()) {
          final int i = rng.nextInt(indices.size());
          final int index = indices.get(i);
          indices.remove(i);
          final GamePiece p = getPieceAt(index);
          pieces.add(p);
        }
      }
      else {
        final Iterator<GamePiece> i = getPiecesReverseIterator();
        while (count-- > 0 && i.hasNext()) pieces.add(i.next());
      }
      it = pieces.iterator();
    }
    dragCount = 0;
    nextDraw = null;
    return new PieceIterator(it) {
      @Override
      public GamePiece nextPiece() {
        final GamePiece p = super.nextPiece();
        /*
         * Bug 12951 results in Cards going back into a Deck via Undo in an inconsistent state.
         * This statement is the culprit. As far as I can tell, it is not needed as Deck.pieceRemoved()
         * sets OBSCURED_BY if drawing a facedown card from a face down Deck which is the only case
         * where this would be needed.
         * Bug 13433
         * HOWEVER, the nextPiece() iterator is used to select cards to test against property match expressions
         * which historically assume that this has already been done. To maintain legacy support, we need to record
         * the original OBSCURED_BY, change it, then the consumers of nextPiece() must change it back.
         * Note use of scratch-pad props map in BasicPiece, no need to use persistent properties.
         */
        p.setProperty(Properties.OBSCURED_BY_PRE_DRAW, p.getProperty(Properties.OBSCURED_BY));
        if (faceDown) {
          p.setProperty(Properties.OBSCURED_BY, NO_USER);
        }
        return p;
      }
    };
  }

  /** Set the contents of this Deck to a Collection of GamePieces */
  protected Command setContents(Collection<GamePiece> c) {
    final ChangeTracker track = new ChangeTracker(this);
    removeAll();
    for (final GamePiece child : c) {
      insertChild(child, pieceCount);
    }
    return track.getChangeCommand();
  }

  /**
   * Set the contents of this Deck to an Iterator of GamePieces
   * @deprecated Use {@link #setContents(Collection)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected Command setContents(Iterator<GamePiece> it) {
    ProblemDialog.showDeprecated("2020-08-06");
    final ChangeTracker track = new ChangeTracker(this);
    removeAll();
    while (it.hasNext()) {
      final GamePiece child = it.next();
      insertChild(child, pieceCount);
    }
    return track.getChangeCommand();
  }

  @Override
  public String getState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(getMap() == null ? "null" : getMap().getIdentifier()).append(getPosition().x).append(getPosition().y); //$NON-NLS-1$
    se.append(faceDown);
    final SequenceEncoder se2 = new SequenceEncoder(',');
    asList().forEach(gamePiece -> se2.append(gamePiece.getId()));
    if (se2.getValue() != null) {
      se.append(se2.getValue());
    }
    return se.getValue();
  }

  @Override
  public void setState(String state) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(state, ';');
    final String mapId = st.nextToken();
    setPosition(new Point(st.nextInt(0), st.nextInt(0)));

    Map m = null;
    if (!"null".equals(mapId)) { //$NON-NLS-1$
      m = Map.getMapById(mapId);
      if (m == null) {
        ErrorDialog.dataWarning(new BadDataReport("No such map", mapId, null)); // NON-NLS
      }
    }

    if (m != getMap()) {
      if (m != null) {
        m.addPiece(this);
      }
      else {
        setMap(null);
      }
    }

    faceDown = "true".equals(st.nextToken()); //$NON-NLS-1$
    final ArrayList<GamePiece> l = new ArrayList<>();
    if (st.hasMoreTokens()) {
      final SequenceEncoder.Decoder st2 =
        new SequenceEncoder.Decoder(st.nextToken(), ',');
      while (st2.hasMoreTokens()) {
        final GamePiece p = gameModule.getGameState()
                                      .getPieceForId(st2.nextToken());
        if (p != null) {
          l.add(p);
        }
      }
    }
    setContents(l);
    commands = null; // Force rebuild of popup menu
  }

  public Command setContentsFaceDown(boolean value) {
    final ChangeTracker t = new ChangeTracker(this);
    Command c = new NullCommand();
    faceDown = value;
    if (Map.isChangeReportingEnabled()) {
      c = c.append(reportCommand(faceDownMsgFormat, value ? Resources.getString("Deck.face_down") : Resources.getString("Deck.face_up")));
    }
    return t.getChangeCommand().append(c);
  }

  /** Reverse the order of the contents of the Deck */
  public Command reverse() {
    final ArrayList<GamePiece> list = new ArrayList<>();
    for (final Iterator<GamePiece> i = getPiecesReverseIterator(); i.hasNext(); ) {
      list.add(i.next());
    }
    Command c = setContents(list);
    if (Map.isChangeReportingEnabled()) {
      c = c.append(reportCommand(reverseMsgFormat, Resources.getString("Deck.reverse")));
    }
    return c;
  }

  public boolean isDrawOutline() {
    return drawOutline;
  }

  public void setOutlineColor(Color outlineColor) {
    this.outlineColor = outlineColor;
  }

  public void setDrawOutline(boolean drawOutline) {
    this.drawOutline = drawOutline;
  }

  public Color getOutlineColor() {
    return outlineColor;
  }

  public boolean isFaceDown() {
    return faceDown;
  }

  @Override
  public Command pieceAdded(GamePiece p) {
    return null;
  }

  @Override
  public Command pieceRemoved(GamePiece p) {
    final ChangeTracker tracker = new ChangeTracker(p);
    p.setProperty(Properties.OBSCURED_TO_OTHERS, isFaceDown() && !isDrawFaceUp());
    return tracker.getChangeCommand();
  }

  public void setFaceDown(boolean faceDown) {
    this.faceDown = faceDown;
  }

  @Override
  public void draw(java.awt.Graphics g, int x, int y, Component obs, double zoom) {
    final int count = Math.min(getPieceCount(), maxStack);
    final GamePiece top = (nextDraw != null && !nextDraw.isEmpty()) ?
      nextDraw.get(0) : topPiece();

    if (top != null) {
      final Object owner = top.getProperty(Properties.OBSCURED_BY);
      top.setProperty(Properties.OBSCURED_BY, faceDown ? NO_USER : null);
      final Color blankColor = getBlankColor();
      final Rectangle r = top.getShape().getBounds();
      r.setLocation(x + (int) (zoom * (r.x)), y + (int) (zoom * (r.y)));
      r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
      for (int i = 0; i < count - 1; ++i) {
        if (blankColor != null) {
          g.setColor(blankColor);
          g.fillRect(r.x + (int) (zoom * 2 * i), r.y - (int) (zoom * 2 * i), r.width, r.height);
          g.setColor(Color.black);
          g.drawRect(r.x + (int) (zoom * 2 * i), r.y - (int) (zoom * 2 * i), r.width, r.height);
        }
        else if (faceDown) {
          top.draw(g, x + (int) (zoom * 2 * i), y - (int) (zoom * 2 * i), obs, zoom);
        }
        else {
          getPieceAt(count - i - 1).draw(g, x + (int) (zoom * 2 * i), y - (int) (zoom * 2 * i), obs, zoom);
        }
      }
      top.draw(g, x + (int) (zoom * 2 * (count - 1)), y - (int) (zoom * 2 * (count - 1)), obs, zoom);
      top.setProperty(Properties.OBSCURED_BY, owner);
    }
    else {
      if (drawOutline) {
        final Rectangle r = boundingBox();
        r.setLocation(x + (int) (zoom * r.x), y + (int) (zoom * r.y));
        r.setSize((int) (zoom * r.width), (int) (zoom * r.height));
        g.setColor(outlineColor);
        g.drawRect(r.x, r.y, r.width, r.height);
      }
    }
  }

  /**
   * The color used to draw boxes representing cards underneath the top one. If
   * null, then draw each card normally for face-up decks, and duplicate the top
   * card for face-down decks
   */
  protected Color getBlankColor() {
    Color c = Color.white;
    if (getMap() != null) {
      c = getMap().getStackMetrics().getBlankColor();
    }
    return c;
  }

  @Override
  public StackMetrics getStackMetrics() {
    return deckStackMetrics;
  }

  @Override
  public Rectangle boundingBox() {
    final GamePiece top = topPiece();
    final Dimension d = top == null ? size : top.getShape().getBounds().getSize();
    final Rectangle r = new Rectangle(new Point(), d);
    r.translate(-r.width / 2, -r.height / 2);
    final int n = getMaximumVisiblePieceCount();
    for (int i = 0; i < n; ++i) {
      r.y -= 2;
      r.height += 2;
      r.width += 2;
    }
    return r;
  }

  @Override
  public Shape getShape() {
    return boundingBox();
  }

  @Override
  public Object getProperty(Object key) {
    Object value = null;
    if (Properties.NO_STACK.equals(key)) {
      value = Boolean.TRUE;
    }
    else if (Properties.KEY_COMMANDS.equals(key)) {
      value = getKeyCommands();
    }
    return value;
  }

  protected KeyCommand[] getKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<>();
      KeyCommand c;
      if (USE_MENU.equals(shuffleOption)) {
        c = new KeyCommand(shuffleCommand, getShuffleKey(), this) {
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            gameModule.sendAndLog(shuffle());
            repaintMap();
          }
        };
        l.add(c);
      }
      if (reshuffleCommand.length() > 0) {
        c = new KeyCommand(reshuffleCommand, getReshuffleKey(), this) {
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent evt) {
            gameModule.sendAndLog(sendToDeck());
            repaintMap();
          }
        };
        l.add(c);
      }
      if (USE_MENU.equals(faceDownOption)) {
        final KeyCommand faceDownAction = new KeyCommand(faceDown ? Resources.getString("Deck.face_up") : Resources.getString("Deck.face_down"), NamedKeyStroke.NULL_KEYSTROKE, this) { //$NON-NLS-1$ //$NON-NLS-2$
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            final Command c = setContentsFaceDown(!faceDown);
            gameModule.sendAndLog(c);
            repaintMap();
          }
        };
        l.add(faceDownAction);
      }
      if (reversible) {
        c = new KeyCommand(reverseCommand, NamedKeyStroke.NULL_KEYSTROKE, this) { //$NON-NLS-1$
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            final Command c = reverse();
            gameModule.sendAndLog(c);
            repaintMap();
          }
        };
        l.add(c);
      }
      if (allowMultipleDraw) {
        c = new KeyCommand(Resources.getString("Deck.draw_multiple"), NamedKeyStroke.NULL_KEYSTROKE, this) { //$NON-NLS-1$
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            promptForDragCount();
          }
        };
        l.add(c);
      }
      if (allowSelectDraw) {
        c = new KeyCommand(Resources.getString("Deck.draw_specific"), NamedKeyStroke.NULL_KEYSTROKE, this) { //$NON-NLS-1$
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            promptForNextDraw();
            repaintMap();
          }
        };
        l.add(c);
      }
      if (persistable) {
        c = new KeyCommand(Resources.getString(Resources.SAVE), NamedKeyStroke.NULL_KEYSTROKE, this) {
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            gameModule.sendAndLog(saveDeck());
            repaintMap();
          }
        };
        l.add(c);
        c = new KeyCommand(Resources.getString(Resources.LOAD), NamedKeyStroke.NULL_KEYSTROKE, this) {
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            gameModule.sendAndLog(loadDeck());
            repaintMap();
          }
        };
        l.add(c);
      }

      for (final DeckGlobalKeyCommand cmd : globalCommands) {
        l.add(cmd.getKeyCommand(this));
      }

      commands = l.toArray(new KeyCommand[0]);
    }

    for (final KeyCommand command : commands) {
      if (Resources.getString("Deck.face_up").equals(command.getValue(Action.NAME)) && !faceDown) { //$NON-NLS-1$
        command.putValue(Action.NAME, Resources.getString("Deck.face_down")); //$NON-NLS-1$
      }
      else if (Resources.getString("Deck.face_down").equals(command.getValue(Action.NAME)) && faceDown) { //$NON-NLS-1$
        command.putValue(Action.NAME, Resources.getString("Deck.face_up")); //$NON-NLS-1$
      }
    }
    return commands;
  }

  /*
   * Format command report as per module designers setup.
   */
  protected Command reportCommand(String format, String commandName) {
    Command c = null;
    final FormattedString reportFormat = new FormattedString(format);
    reportFormat.setProperty(DrawPile.DECK_NAME, getLocalizedDeckName());
    reportFormat.setProperty(DrawPile.COMMAND_NAME, commandName);
    final String rep = reportFormat.getLocalizedText();
    if (rep.length() > 0) {
      c = new Chatter.DisplayText(gameModule.getChatter(), "* " + rep); //$NON-NLS-1$
      c.execute();
    }

    return c;
  }

  public void promptForDragCount() {
    while (true) {
      final String s = JOptionPane.showInputDialog(GameModule.getGameModule().getPlayerWindow(),
        Resources.getString("Deck.enter_the_number")); //$NON-NLS-1$
      if (s != null) {
        try {
          dragCount = Integer.parseInt(s);
          dragCount = Math.min(dragCount, getPieceCount());
          if (dragCount >= 0) break;
        }
        catch (NumberFormatException ex) {
          // Ignore if user doesn't enter a number
        }
      }
      else {
        break;
      }
    }
  }

  protected void promptForNextDraw() {
    final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, map.getView()), true);
    d.setTitle(Resources.getString("Deck.draw")); //$NON-NLS-1$
    d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));

    class AvailablePiece implements Comparable<AvailablePiece> {
      private final GamePiece piece;

      public AvailablePiece(GamePiece piece) {
        this.piece = piece;
      }

      @Override
      public int compareTo(AvailablePiece other) {
        if (other == null) return 1;

        final String otherProperty =
          (String) other.piece.getProperty(selectSortProperty);
        if (otherProperty == null) return 1;

        final String myProperty =
          (String) piece.getProperty(selectSortProperty);
        if (myProperty == null) return -1;

        return -otherProperty.compareTo(myProperty);
      }

      @Override
      public String toString() {
        return selectDisplayProperty.getText(piece);
      }

      @Override
      public boolean equals(Object o) {
        if (! (o instanceof AvailablePiece)) return false;
        return ((AvailablePiece)o).piece.equals(piece);
      }
    }

    final AvailablePiece[] pieces = new AvailablePiece[getPieceCount()];
    for (int i = 0; i < pieces.length; ++i) {
      pieces[pieces.length - i - 1] = new AvailablePiece(getPieceAt(i));
    }

    if (selectSortProperty != null && selectSortProperty.length() > 0) {
      Arrays.sort(pieces);
    }

    final JList<AvailablePiece> list = new JList<>(pieces);
    list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    d.add(new ScrollPane(list));
    d.add(new JLabel(Resources.getString("Deck.select_cards"))); //$NON-NLS-1$
    d.add(new JLabel(Resources.getString("Deck.then_click"))); //$NON-NLS-1$
    final Box box = Box.createHorizontalBox();
    JButton b = new JButton(Resources.getString(Resources.OK));
    b.addActionListener(e -> {
      final int[] selection = list.getSelectedIndices();
      if (selection.length > 0) {
        nextDraw = new ArrayList<>();
        for (final int value : selection) {
          nextDraw.add(pieces[value].piece);
        }
      }
      else {
        nextDraw = null;
      }
      d.dispose();
    });
    box.add(b);
    b = new JButton(Resources.getString(Resources.CANCEL));
    b.addActionListener(e -> d.dispose());
    box.add(b);
    d.add(box);
    d.pack();
    d.setLocationRelativeTo(d.getOwner());
    d.setVisible(true);
  }

  /**
   * Combine the contents of this Deck with the contents of the deck specified
   * by {@link #reshuffleTarget}
   */
  public Command sendToDeck() {
    Command c = null;
    nextDraw = null;
    final DrawPile target = DrawPile.findDrawPile(reshuffleTarget);
    if (target != null) {
      if ((reshuffleMsgFormat.length() > 0) && Map.isChangeReportingEnabled()) {
        c = reportCommand(reshuffleMsgFormat, reshuffleCommand);
        if (c == null) {
          c = new NullCommand();
        }
      }
      else {
        c = new NullCommand();
      }
      // move cards to deck
      final int cnt = getPieceCount() - 1;
      for (int i = cnt; i >= 0; i--) {
        c = c.append(target.addToContents(getPieceAt(i)));
      }
    }
    return c;
  }

  @Override
  public boolean isExpanded() {
    return false;
  }

  /** Return true if this deck can be saved to and loaded from a file on disk */
  public boolean isPersistable() {
    return persistable;
  }

  public void setPersistable(boolean persistable) {
    this.persistable = persistable;
  }

  private File getSaveFileName() {
    final FileChooser fc = gameModule.getFileChooser();
    final File sf = fc.getSelectedFile();
    if (sf != null) {
      String name = sf.getPath();
      final int index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index) + ".sav"; //$NON-NLS-1$
        fc.setSelectedFile(new File(name));
      }
    }

    if (fc.showSaveDialog(map.getView()) != FileChooser.APPROVE_OPTION)
      return null;

    File outputFile = fc.getSelectedFile();
    if (outputFile != null &&
        outputFile.exists() &&
        shouldConfirmOverwrite() &&
        JOptionPane.NO_OPTION ==
         JOptionPane.showConfirmDialog(gameModule.getPlayerWindow(),
          Resources.getString("Deck.overwrite", outputFile.getName()), Resources.getString("Deck.file_exists"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          JOptionPane.YES_NO_OPTION)) {
      outputFile = null;
    }

    return outputFile;
  }

  private boolean shouldConfirmOverwrite() {
    return System.getProperty("os.name").trim().equalsIgnoreCase("linux"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private Command saveDeck() {
    final Command c = new NullCommand();
    gameModule.warn(Resources.getString("Deck.saving_deck")); //$NON-NLS-1$

    final File saveFile = getSaveFileName();
    try {
      if (saveFile != null) {
        saveDeck(saveFile);
        gameModule.warn(Resources.getString("Deck.deck_saved")); //$NON-NLS-1$
      }
      else {
        gameModule.warn(Resources.getString("Deck.save_canceled")); //$NON-NLS-1$
      }
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, saveFile);
    }
    return c;
  }

  public void saveDeck(File f) throws IOException {
    Command comm = new LoadDeckCommand(null);
    for (final GamePiece p : asList()) {
      p.setMap(null);
      comm = comm.append(new AddPiece(p));
    }

    try (Writer w = Files.newBufferedWriter(f.toPath(), StandardCharsets.UTF_8)) {
      gameModule.addCommandEncoder(commandEncoder);
      w.write(gameModule.encode(comm));
      gameModule.removeCommandEncoder(commandEncoder);
    }
  }

  private File getLoadFileName() {
    final FileChooser fc = gameModule.getFileChooser();
    fc.selectDotSavFile();
    if (fc.showOpenDialog(map.getView()) != FileChooser.APPROVE_OPTION)
      return null;
    return fc.getSelectedFile();
  }

  private Command loadDeck() {
    Command c = new NullCommand();
    gameModule.warn(Resources.getString("Deck.loading_deck")); //$NON-NLS-1$

    final File loadFile = getLoadFileName();
    try {
      if (loadFile != null) {
        c = loadDeck(loadFile);
        gameModule.warn(Resources.getString("Deck.deck_loaded")); //$NON-NLS-1$
      }
      else {
        gameModule.warn(Resources.getString("Deck.load_canceled")); //$NON-NLS-1$
      }
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, loadFile);
    }

    return c;
  }

  public Command loadDeck(File f) throws IOException {
    final String ds = Files.readString(f.toPath(), StandardCharsets.UTF_8);

    gameModule.addCommandEncoder(commandEncoder);
    Command c = gameModule.decode(ds);
    gameModule.removeCommandEncoder(commandEncoder);
    if (c instanceof LoadDeckCommand) {
      /*
       * A LoadDeckCommand doesn't specify the deck to be changed (since the
       * saved deck can be loaded into any deck) so the Command we send to other
       * players is a ChangePiece command for this deck, which we need to place
       * after the AddPiece commands for the contents
       */
      final ChangeTracker t = new ChangeTracker(this);
      c.execute();
      final Command[] sub = c.getSubCommands();
      c = new NullCommand();
      for (final Command command : sub) {
        c.append(command);
      }
      c.append(t.getChangeCommand());
      updateCountsAll();
    }
    else {
      gameModule.warn(Resources.getString("Deck.not_a_saved_deck", f.getName())); //$NON-NLS-1$
      c = null;
    }
    return c;
  }

  /**
   * Command to set the contents of this deck from a saved file. The contents
   * are saved with whatever id's the pieces have in the game when the deck was
   * saved, but new copies are created when the deck is re-loaded.
   *
   * @author rkinney
   *
   */
  protected static class LoadDeckCommand extends Command {
    public static final String PREFIX = "DECK\t"; //$NON-NLS-1$
    private final Deck target;

    public LoadDeckCommand(Deck target) {
      this.target = target;
    }

    @Override
    protected void executeCommand() {
      target.removeAll();
      final Command[] sub = getSubCommands();
      for (final Command command : sub) {
        if (command instanceof AddPiece) {
          final GamePiece p = ((AddPiece) command).getTarget();
          // We set the id to null so that the piece will get a new id
          // when the AddPiece command executes
          p.setId(null);
          target.add(p);
        }
      }
    }

    public String getTargetId() {
      return target == null ? "" : target.getId(); //$NON-NLS-1$
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  /**
   * An object that parses expression strings from the config window
   */
  public static class CountExpression {
    private String fullstring;
    private String name;
    private String expression;
    public CountExpression(String expressionString) {
      final String[] split = expressionString.split("\\s*:\\s*", 2); //$NON-NLS-1$
      if (split.length == 2) {
        name       = split[0];
        expression = split[1];
        fullstring = expressionString;
      }
    }

    public String getName() {
      return name;
    }

    public String getExpression() {
      return expression;
    }

    public String getFullString() {
      return fullstring;
    }
  }

  /**
   * Return the number of cards to be returned by next call to
   * {@link #drawCards()}.
   */
  public int getDragCount() {
    return dragCount;
  }

  /**
   * Set the number of cards to be returned by next call to
   * {@link #drawCards()}.
   *
   * @param dragCount number of cards to be returned
   */
  public void setDragCount(int dragCount) {
    this.dragCount = dragCount;
  }

  public void setSelectDisplayProperty(String promptDisplayProperty) {
    this.selectDisplayProperty.setFormat(promptDisplayProperty);
  }

  public void setSelectSortProperty(String promptSortProperty) {
    this.selectSortProperty = promptSortProperty;
  }

  public String getSelectDisplayProperty() {
    return selectDisplayProperty.getFormat();
  }

  public String getSelectSortProperty() {
    return selectSortProperty;
  }

  protected void repaintMap() {
    if (map != null) {
      map.repaint();
    }
  }
}
