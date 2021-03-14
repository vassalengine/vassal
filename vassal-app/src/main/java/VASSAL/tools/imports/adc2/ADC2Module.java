/*
 *
 * Copyright (c) 2008 by Michael Kiefte
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

package VASSAL.tools.imports.adc2;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.InputEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.ChartWindow;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.MultiActionButton;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerHand;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.PrivateMap;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.ToolbarMenu;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.LOS_Thread;
import VASSAL.build.module.map.LayeredPieceCollection;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.turn.ListTurnLevel;
import VASSAL.build.module.turn.TurnTracker;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.Chart;
import VASSAL.build.widget.HtmlChart;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.PieceSlot;
import VASSAL.build.widget.TabWidget;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.Delete;
import VASSAL.counters.DynamicProperty;
import VASSAL.counters.Embellishment;
import VASSAL.counters.Footprint;
import VASSAL.counters.FreeRotator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Hideable;
import VASSAL.counters.Marker;
import VASSAL.counters.MovementMarkable;
import VASSAL.counters.Obscurable;
import VASSAL.counters.PropertySheet;
import VASSAL.counters.Replace;
import VASSAL.counters.ReturnToDeck;
import VASSAL.counters.UsePrototype;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.filechooser.ExtensionFileFilter;
import VASSAL.tools.imports.FileFormatException;
import VASSAL.tools.imports.Importer;
import VASSAL.tools.imports.adc2.SymbolSet.SymbolData;

public class ADC2Module extends Importer {

  private static final Logger log = LoggerFactory.getLogger(ADC2Module.class);

  private static final String FLIP_DEFINITIONS = "Flip Definitions";
  private static final String ADD_NEW_PIECES = "Add New Pieces";
  private static final String PC_NAME = "$pcName$";
  private static final String CHARTS = "Charts";
  private static final String TRAY = "Tray";
  private static final String FORCE_POOL_PNG = "forcePool.png";
  private static final String FORCE_POOL = "Force Pool";
  private static final String DECKS = "Decks";

  protected static class ForcePoolList extends ArrayList<Pool> {
    private static final long serialVersionUID = 1L;

    private class ForcePoolIterator implements Iterator<Pool> {
      private final Class<?> type;
      private int cursor = 0;

      private ForcePoolIterator(Class<?> type) {
        this.type = type;
        setNext();
      }

      private void setNext() {
        while (cursor < size()) {
          if (get(cursor).getClass() == type && get(cursor).isUseable())
            break;
          else
            ++cursor;
        }
      }

      @Override
      public boolean hasNext() {
        return cursor < size();
      }

      @Override
      public Pool next() {
        final Pool p = get(cursor);
        ++cursor;
        setNext();
        return p;
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }
    }

    public int count(Class<?> type) {
      int size = 0;
      final Iterator<Pool> iter = iterator(type);
      while (iter.hasNext()) {
        final Pool p = iter.next();
        if (p.getClass() == type && p.isUseable())
          ++size;
      }
      return size;
    }

    public Iterator<ADC2Module.Pool> iterator(Class<?> type) {
      return new ForcePoolIterator(type);
    }
  }

  public class Pool {
    public final String name;
    public final List<Piece> pieces;

    Pool(String name, List<Piece> pieces) {
      this.name = name;
      this.pieces = pieces;
    }

    List<Piece> getPieces() {
      if (pieces == null)
        return Collections.emptyList();
      else
        return Collections.unmodifiableList(pieces);
    }

    String getButtonName() {
      return name;
    }

    boolean isUseable() {
      return true;
    }
  }

  public class Cards extends Pool {
    protected Player owner;

    Cards(String name, List<Piece> pieces) {
      super(name, pieces);
      // cull non-cards
      if (pieces != null) {
        pieces.removeIf(piece -> !piece.isCard());
      }
    }

    public void setOwner(int owner) {
      if (owner == NO_PLAYERS)
        this.owner = Player.NO_PLAYERS;
      else if (owner >= ALL_PLAYERS)
        this.owner = Player.ALL_PLAYERS;
      else if (owner >= players.size())
        this.owner = Player.UNKNOWN;
      else
        this.owner = players.get(owner);
    }

    public Player getOwner() {
      return owner;
    }
  }

  public class DeckPool extends Cards {
    DeckPool(String name, List<Piece> pieces) {
      super(name, pieces);
    }

  }

  public class HandPool extends Cards {
    HandPool(String name, List<Piece> pieces) {
      super(name, pieces);
    }

    @Override
    String getButtonName() {
      return super.getButtonName() + " (" + getOwner().getName() + " Hand)";
    }
  }

  public class ForcePool extends Pool {
    @Override
    boolean isUseable() {
      if (getPieces().size() > 0)
        return true;
      if (name == null)
        return false;
      for (int i = 0; i < name.length(); ++i) {
        if (Character.isLetterOrDigit(name.charAt(i)))
          return true;
      }
      return false;
    }

    ForcePool(String name, List<Piece> pieces) {
      super(name, pieces);
    }
  }

  public class StatusDots {
    // type
    public static final int NOT_USED = 0;
    public static final int MOVED = 1;
    public static final int IN_COMBAT = 2;
    public static final int ATTACKED = 3;
    public static final int DEFENDED = 4;
    public static final int CLASS_VALUE = 5;
    public static final int PIECE_VALUE = 6;

    // position
    public static final int DO_NOT_DRAW = 0;
    public static final int TOP_LEFT = 1;
    public static final int TOP_CENTER = 2;
    public static final int TOP_RIGHT = 3;
    public static final int CENTER_LEFT = 4;
    public static final int CENTER_CENTER = 5;
    public static final int CENTER_RIGHT = 6;
    public static final int BOTTOM_LEFT = 7;
    public static final int BOTTOM_CENTER = 8;
    public static final int BOTTOM_RIGHT = 9;

    private final int type;
    private final int show;
    private final Color color;
    private final int position;
    private final int size;

    protected StatusDots(int type, int show, Color color, int position, int size) {
      this.type = type;
      this.show = show;
      this.color = color;
      this.position = position;
      this.size = size;
    }

    public Color getColor() {
      return color;
    }

    public int getPosition() {
      return position;
    }

    public int getShow() {
      return show;
    }

    public int getSize() {
      return size;
    }

    public int getType() {
      return type & 0xf;
    }

    public String getStatusPropertyName() {
      if (getType() == CLASS_VALUE)
        return classValues[type >>> 4];
      else if (getType() == PIECE_VALUE)
        return pieceValues[type >>> 4];
      else
        return null;
    }
  }

  private static final int FORCE_POOL_BLOCK_END = 30000;
  public static final String DRAW_ON_TOP_OF_OTHERS = "Draw on top of others?";
  public static final String PIECE = "Pieces";

  private static final double[] FACING_ANGLES = new double[46];

  static {
    for (int i = 0; i < 3; ++i) {
      FACING_ANGLES[i + 1] = -i * 90.0;
      FACING_ANGLES[i + 5] = -(i * 90.0 + 45.0);
    }

    for (int i = 0; i < 6; ++i) {
      FACING_ANGLES[i + 10] = -i * 60.0;
      FACING_ANGLES[i + 20] = -((i * 60.0 - 15.0) % 360.0);
      FACING_ANGLES[i + 30] = -(i * 60.0 + 30.0);
      FACING_ANGLES[i + 40] = -(i * 60.0 + 15.0);
    }
  }

  private final Set<String> uniquePieceNames = new HashSet<>();
  private static boolean usePieceNames = false;

  public class Piece {
    private static final String PIECE_PROPERTIES = "Piece Properties";
    public final PieceClass pieceClass;
    public final HideState hideState;
    private final int[] values = new int[8];
    private final ValueType[] types = new ValueType[8];
    private final String name;
    private final int flags;
    private final int facing;
    private GamePiece gamePiece;
    private PieceSlot pieceSlot;
    private final int position;
    private PropertySheet classPS = null;
    private PropertySheet piecePS = null;
    private Marker pieceNameMarker = null;

    public Piece(PieceClass cl) {
      this.name = null;
      this.pieceClass = cl;
      this.flags = 0;
      this.hideState = null;
      this.position = -1;
      facing = 0;
    }

    public Piece(int position, String name, PieceClass cl, HideState hidden, int flags, int facing) {
      if (name == null || name.equals(""))
        this.name = null;
      else
        this.name = name;
      this.position = position;
      this.pieceClass = cl;
      this.flags = flags;
      assert (hidden != null);
      this.hideState = hidden;
      this.facing = facing;

      final java.util.Map<Integer, List<Piece>> hash;
      if (inForcePool())
        hash = forcePoolHashMap;
      else
        hash = stacks;

      List<Piece> stack = hash.get(position);
      if (stack == null) {
        stack = new ArrayList<>();
        stack.add(this);
        hash.put(position, stack);
      }
      else {
        stack.add(0, this);
      }
    }

    public Pool getForcePool() {
      if (inForcePool()) {
        return forcePools.get(position);
      }
      else {
        return null;
      }
    }

    public boolean isCard() {
      return types[0] == ValueType.CARD;
    }

    @Override
    public boolean equals(Object obj) {
      if (!(obj instanceof Piece))
        return false;
      return getUniqueClassName().equals(((Piece) obj).getUniqueClassName()) &&
        pieceClass == ((Piece) obj).pieceClass;
    }

    @Override
    public int hashCode() {
      return getUniqueClassName().hashCode();
    }

    protected void setValue(int index, int value) {
      values[index] = value;
      types[index] = ValueType.NUMERIC;
    }

    protected void writeToArchive(SetupStack parent) throws IOException {
      final GamePiece gp = getGamePiece();
      if (gp == null)
        return;
      assert (pieceSlot == null);
      pieceSlot = new PieceSlot(gp);
      insertComponent(pieceSlot, parent);
    }

    protected void writeToArchive(DrawPile parent) throws IOException {
      final GamePiece gp = getGamePiece();
      if (gp == null)
        return;
      assert (pieceSlot == null);
      pieceSlot = new CardSlot();
      pieceSlot.setPiece(gp);
      insertComponent(pieceSlot, parent);
    }

    // TODO: create option whereby anyone can flip/hide a card.
    public Player getPlayer() {
      return pieceClass.getOwner();
    }

    public boolean inForcePool() {
      return (flags & 0x8) > 0;
    }

    protected GamePiece getGamePiece() throws IOException {

      if (gamePiece == null) {
        gamePiece = getBasicPiece();
        if (gamePiece == null)
          return null;
        // TODO: implement a YES_NO field type for PropertySheets
        // and a stack property viewer.
        // Piece values
        appendDecorator(getPieceNameMarker());
        appendDecorator(getDynamicProperty());
        appendDecorator(getPieceValueMask());
        appendDecorator(getMovementMarkable());
        appendDecorator(getDefendedEmbellishment());
        appendDecorator(getAttackedEmbellishment());
        appendDecorator(getFreeRotator());
        appendDecorator(getUsePrototype());
        appendDecorator(getPiecePropertySheet());
        appendDecorator(getReplaceWithPrevious());
        appendDecorator(getReplaceWithOther());
        appendDecorator(getClassPropertySheet());
        appendDecorator(getHidden());
      }

      return gamePiece;
    }

    private Decorator getReplaceWithPrevious() throws IOException {
      return pieceClass.getReplaceWithPreviousDecorator();
    }

    private Decorator getReplaceWithOther() throws IOException {
      return pieceClass.getReplaceWithOtherDecorator();
    }

    private Marker getPieceNameMarker() {
      if (pieceNameMarker == null) {
        if (name != null && name.length() > 0) {
          usePieceNames = true;
        }
        pieceNameMarker = new Marker(Marker.ID + "pcName", null);
        final SequenceEncoder se = new SequenceEncoder(',');
        se.append(name == null ? "" : name);
        pieceNameMarker.mySetState(se.getValue());
      }
      return pieceNameMarker;
    }

    private void appendDecorator(Decorator p) {
      if (p != null) {
        p.setInner(gamePiece);
        gamePiece = p;
      }
    }

    protected Decorator getHidden() throws IOException {
      final Decorator p = pieceClass.getHiddenDecorator();
      if (p != null && isHidden()) {
        final Player player = pieceClass.getPlayer(this);
        if (player != Player.ALL_PLAYERS && player != Player.NO_PLAYERS) {
          p.mySetState(player.getName());
        }
      }
      return p;
    }

    private boolean isHidden() {
      return pieceClass.checkHidden(this);
    }

    protected Obscurable getPieceValueMask() throws IOException {
      if (usePieceValues()) {
        final Obscurable p = pieceClass.getPieceValueMask();
        if (p != null) {
          if (hideState == HideState.INFO_HIDDEN)
            p.mySetState(getPlayer().getName());
        }
        return p;
      }
      else {
        return null;
      }
    }

    protected UsePrototype getUsePrototype() {
      return pieceClass.getUsePrototypeDecorator();
    }

    public double getFacingAngle() {
      if (facing >= FACING_ANGLES.length)
        return 0.0;
      else
        return FACING_ANGLES[facing];
    }

    protected Embellishment getDefendedEmbellishment() throws IOException {
      final Embellishment layer = pieceClass.getDefendedEmbellishmentDecorator();
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(hasDefended() ? 1 : -1).append("");
      layer.mySetState(se.getValue());
      return layer;
    }

    protected Embellishment getAttackedEmbellishment() throws IOException {
      final Embellishment layer = pieceClass.getAttackedEmbellishmentDecorator();
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(hasAttacked() ? 1 : -1).append("");
      layer.mySetState(se.getValue());
      return layer;
    }

    // TODO: provide angle phase offset for FreeRotator
    protected FreeRotator getFreeRotator() {
      final FreeRotator p = pieceClass.getFreeRotatorDecorator();
      if (p != null) {
        // for pieces that point from a corner, this will either be completely wrong
        // or completely right.  If we do nothing, it's guaranteed to be completely wrong
        p.setAngle(getFacingAngle());
      }
      return p;
    }

    protected MovementMarkable getMovementMarkable() throws IOException {
      final MovementMarkable p = pieceClass.getMovementMarkableDecorator();
      if (p != null) {
        p.setMoved(hasMoved());
      }
      return p;
    }

    //TODO:  add more math functions to MouseOverStackViewer including min(), max(), and mean().
    //     and antialiased characters in MouseOverStackViewer
    protected PropertySheet getPiecePropertySheet() {
      if (piecePS == null) {
        piecePS = new PropertySheet();
        SequenceEncoder se = new SequenceEncoder('~');
        final SequenceEncoder state = new SequenceEncoder('~');
        for (int i = 0; i < pieceValues.length; ++i) {
          if (pieceValues[i] != null && !pieceValues[i].equals("")) {
            se.append("0" + pieceValues[i]);
            final Object o = getValue(i);
            if (o instanceof String)
              state.append((String) o);
            else if (o instanceof Integer)
              state.append(o.toString());
            else if (o instanceof Boolean)
              state.append(o.equals(Boolean.TRUE) ? "yes" : "no");
            else
              state.append("");
          }
        }

        final String definition = se.getValue();

        String st = piecePS.myGetState();
        if (st == null) {
          st = state.getValue();
        }
        else if (state.getValue() != null) {
          st = piecePS.myGetState() + "~" + state.getValue();
        }

        if (definition != null && definition.length() > 0) {
          se = new SequenceEncoder(';'); // properties
          se.append(definition);
          se.append(PIECE_PROPERTIES); // menu name
          se.append('P'); // key
          se.append(0); // commit
          se.append("").append("").append(""); // colour
          piecePS.mySetType(PropertySheet.ID + se.getValue());
          piecePS.mySetState(st);
        }
        else {
          piecePS = null;
        }
      }

      return piecePS;
    }

    protected PropertySheet getClassPropertySheet() {
      if (classPS == null) {
        classPS = pieceClass.getPropertySheetDecorator();
      }
      return classPS;
    }

    protected DynamicProperty getDynamicProperty() {
      final DynamicProperty dp = pieceClass.getDynamicPropertyDecorator();
      dp.setInner(gamePiece); // so we can change the state below.
      dp.setValue(drawOnTopOfOthers() ? "1" : "0");
      return dp;
    }

    protected GamePiece getBasicPiece() throws IOException {
      final String fileName = pieceClass.getImageName();
      if (fileName == null)
        return null;
      final SequenceEncoder se = new SequenceEncoder(BasicPiece.ID, ';');
      se.append("").append("").append(fileName).append(getUniqueClassName());
      return new BasicPiece(se.getValue());
    }

    // hasAttacked() and hasDefended() will never be used.
    public boolean hasAttacked() {
      return (flags & 0x1) > 0;
    }

    public boolean hasDefended() {
      return !hasAttacked() && (flags & 0x2) > 0;
    }

    public boolean hasMoved() {
      return (flags & 0x4) > 0;
    }

    public boolean drawOnTopOfOthers() {
      return (flags & 0x10) > 0;
    }

    public String getUniqueClassName() {
      return pieceClass.getUniqueName();
    }

    public String getClassName() {
      return pieceClass.getName();
    }

    protected void setValue(int index, String value) {
      final byte[] b = value.getBytes(StandardCharsets.US_ASCII);
      int result = 0;
      for (int i = 0; i < 4; ++i)
        result = (result << 8) + (b[i] & 0xff);
      values[index] = result;
      types[index] = ValueType.TEXT;
    }

    protected void setValue(int index, boolean value) {
      if (value)
        values[index] = 1;
      else
        values[index] = 0;
      types[index] = ValueType.YESNO;
    }

    private int getValueAsInt(int index) {
      return values[index];
    }

    private String getValueAsString(int index) {
      final byte[] b = new byte[4];
      int mask = 0x7f000000;
      int length = 0;
      for (int i = 0; i < b.length; ++i) {
        b[i] = (byte) ((values[index] & mask) >> ((3 - i) * 8));
        if (b[i] < 0x20 || b[i] > 0x7e)
          break;
        ++length;
        mask >>= 8;
      }
      return new String(b, 0, length, StandardCharsets.US_ASCII);
    }

    private boolean getValueAsBoolean(int index) {
      return values[index] > 0;
    }

    public Object getValue(int index) {
      if (types[index] == null)
        return null;
      switch (types[index]) {
      case NUMERIC:
        return getValueAsInt(index);
      case TEXT:
        return getValueAsString(index);
      case YESNO:
        return getValueAsBoolean(index);
      default:
        return null;
      }
    }

    protected void writeToArchive(ListWidget list) throws IOException {
      final GamePiece gp = getGamePiece();
      if (gp == null)
        return;
      pieceSlot = new PieceSlot(gp);
      insertComponent(pieceSlot, list);
    }

    protected PieceSlot getPieceSlot() {
      return pieceSlot;
    }
  }

  public enum ValueType {
    NOT_USED, NUMERIC, TEXT, YESNO, CARD
  }

  public enum HideState {
    NOT_HIDDEN, INFO_HIDDEN, HIDDEN
  }

  public static class Player {

    public static final Player ALL_PLAYERS = new Player("All Players", null, 0);
    public static final Player NO_PLAYERS = new Player("No Player", null, 0);
    public static final Player UNKNOWN = new Player("Unknown", null, 0);
    private static int nPlayers = 0;
    private final String name;
    private final SymbolSet.SymbolData hiddenSymbol;
    private final int hiddenPieceOptions;
    private final int order;
    private final SortedSet<Player> allies = new TreeSet<>(Comparator.comparingInt(p -> p.order));

    public Player(String name, SymbolSet.SymbolData hiddenSymbol, int hiddenPieceOptions) {
      this.name = name;
      this.hiddenSymbol = hiddenSymbol;
      // this.searchRange = searchRange > 50 ? 50 : searchRange;
      this.hiddenPieceOptions = hiddenPieceOptions;
      order = nPlayers++;
      allies.add(this);
    }

    public boolean useHiddenPieces() {
      return (hiddenPieceOptions & 0x1) > 0;
    }

    public boolean hiddenWhenPlaced() {
      return (hiddenPieceOptions & 0x2) > 0;
    }

    // in ADC2 hiddenInForcePools and hiddenWhenPlaced are different concepts
    // as you only get to see a list of piece names when you look at the force
    // pools and those are hidden if hiddenInForcePools is in effect.
    // If hiddenWhenPlaced is in effect, all players can see the force pools
    // but units are hidden when they are placed on the board.
    // In VASSAL, we make them hidden in the force pool either way.
    public boolean hiddenInForcePools() {
      return (hiddenPieceOptions & 0x4) > 0 || hiddenWhenPlaced();
    }

    // TODO: add game master option to players
    public boolean isGameMaster() {
      return (hiddenPieceOptions & 0x8) > 0;
    }

    public SymbolSet.SymbolData getHiddenSymbol() {
      return hiddenSymbol;
    }

    public String getName() {
      final StringBuilder sb = new StringBuilder();
      for (final Player p : allies) {
        if (sb.length() > 0)
          sb.append('/');
        sb.append(p.name);
      }
      return sb.toString();
    }

    public void setAlly(Player player) {
      allies.add(player);
    }

    public boolean isAlly(Player player) {
      return allies.contains(player);
    }

    @Override
    public String toString() {
      return getName();
    }
  }

  private final java.util.Map<Integer, SymbolSet> cardDecks = new HashMap<>();

  public class CardClass extends PieceClass {
    private final int setIndex;
    private final int symbolIndex;

    public CardClass(String name, int symbolIndex, int setIndex) {
      super(name, null, ALL_PLAYERS, NO_HIDDEN_SYMBOL, 0);
      this.setIndex = setIndex;
      this.symbolIndex = symbolIndex;
    }

    @Override
    public String getHiddenName() {
      if (getOwner() == Player.NO_PLAYERS || getOwner() == Player.ALL_PLAYERS) {
        return "Unknown card";
      }
      else {
        return "Unknown " + getOwner().getName() + " card";
      }
    }

    @Override
    public SymbolData getHiddenSymbol() throws IOException {
      return getCardDeck(setIndex).getGamePiece(0);
    }

    @Override
    protected SymbolData getSymbol() throws IOException {
      if (symbol == null) {
        final SymbolSet set = getCardDeck(setIndex);
        symbol = set.getGamePiece(symbolIndex);
      }
      return symbol;
    }

    @Override
    protected void setValue(int index, boolean value) {
      assert (false);
    }

    @Override
    protected void setValue(int index, int value) {
      assert (false);
    }

    @Override
    protected void setValue(int index, String value) {
      assert (false);
    }

    @Override
    public FreeRotator getFreeRotatorDecorator() {
      return null;
    }

    @Override
    public Obscurable getPieceValueMask() throws IOException {
      return null;
    }

    @Override
    public boolean checkHidden(Piece piece) {
      if (piece.getForcePool() == null) {
        return piece.hideState == HideState.HIDDEN;
      }
      else if (piece.getForcePool() instanceof HandPool) {
        final Player player = getPlayer(piece);
        return player != Player.ALL_PLAYERS && player != Player.NO_PLAYERS;
      }
      else {
        return false;
      }
    }

    @Override
    public PropertySheet getPropertySheetDecorator() {
      return null;
    }

    @Override
    public Obscurable getHiddenDecorator() throws IOException {
      final Obscurable p;
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(NamedKeyStroke.of(KeyStroke.getKeyStroke('H', InputEvent.CTRL_DOWN_MASK))); // key command
      se.append(getHiddenSymbol().getFileName()); // hide image
      se.append("Hide Piece"); // menu name
      final BufferedImage image = getSymbol().getImage();
      se.append("G" + getFlagLayer(new Dimension(image.getWidth(), image.getHeight()), StateFlag.MARKER)); // display style
      se.append(getHiddenName()); // mask name
      if (getOwner() == Player.NO_PLAYERS || getOwner() == Player.ALL_PLAYERS) {
        se.append("side:");
      }
      else {
        se.append("sides:" + getOwner().getName()); // owning player
      }
      p = new Obscurable();
      p.mySetType(Obscurable.ID + se.getValue());
      return p;
    }

    @Override
    public Player getOwner() {
      return Player.ALL_PLAYERS;
    }

    @Override
    public Player getPlayer(Piece p) {
      if (p.inForcePool() && p.getForcePool() instanceof HandPool) {
        return ((HandPool) p.getForcePool()).getOwner();
      }
      else {
        return getOwner();
      }
    }
  }

  protected static final int ALL_PLAYERS = 200;
  protected static final int NO_PLAYERS = 201;

  /**
   * A general class for a game piece.  Typically all pieces that appear to be identical blong to the
   * same class.
   */
  public class PieceClass {

    public PieceClass backReplace;
    public static final String CLASS_PROPERTIES = "Class Properties";
    protected static final int NO_HIDDEN_SYMBOL = 30001;
    protected static final int PLAYER_DEFAULT_HIDDEN_SYMBOL = 30000;
    private final int[] values = new int[8];
    private final ValueType[] types = new ValueType[8];
    private final String name;
    protected SymbolSet.SymbolData symbol;
    protected int owner;
    private final int hiddenSymbol;
    private final int facing;
    private PieceClass flipClass;
    private Piece defaultPiece;
    private String uniqueName;
    private boolean flipClassAdded = false;
    private Piece flipDefinition;

    public PieceClass(String name, SymbolSet.SymbolData symbol, int owner, int hiddenSymbol, int facing) {
      this.name = name;
      this.symbol = symbol;
      this.owner = owner;
      this.hiddenSymbol = hiddenSymbol;
      this.facing = facing;
    }

    public Decorator getReplaceWithPreviousDecorator() throws IOException {
      final PieceClass flipClass = getBackFlipClass();
      if (flipClass == null)
        return null;
        // don't bother if there are only two counters that flip back and forth
      else if (getFlipClass() == flipClass)
        return null;

      final String path = flipClass.getFlipClassTreeConfigurePath();

      final SequenceEncoder se = new SequenceEncoder(path, ';');
      se.append("null").append(0).append(0).append(true).append((NamedKeyStroke) null).append("").append("").append(2).append(true);

      flipClass.writeFlipDefinition();

      return new Replace(Replace.ID + "Flip Back;B;" + se.getValue(), null);
    }

    // TODO: find a different way to do this so that we don't have to generate unique class names.
    private String getFlipClassTreeConfigurePath() {
      SequenceEncoder se2 = new SequenceEncoder(PieceWindow.class.getName(), ':');
      se2.append(FLIP_DEFINITIONS);
      final SequenceEncoder se = new SequenceEncoder(se2.getValue(), '/');
      se2 = new SequenceEncoder(ListWidget.class.getName(), ':');
      se.append(se2.getValue());
      se2 = new SequenceEncoder(PieceSlot.class.getName(), ':');
      se2.append(getUniqueName());
      se.append(se2.getValue());
      return se.getValue();
    }

    public Decorator getReplaceWithOtherDecorator() throws IOException {
      final PieceClass flipClass = getFlipClass();
      if (flipClass == null)
        return null;

      final SequenceEncoder se;
      final String path = flipClass.getFlipClassTreeConfigurePath();

      se = new SequenceEncoder(path, ';');
      se.append("null").append(0).append(0).append(true).append((NamedKeyStroke) null).append("").append("").append(2).append(true);

      flipClass.writeFlipDefinition();

      return new Replace(Replace.ID + "Flip;F;" + se.getValue(), null);
    }

    private void writeFlipDefinition() throws IOException {
      if (!flipClassAdded) {
        flipClassAdded = true;

        final ListWidget list = flipDefs.getAllDescendantComponentsOf(ListWidget.class).iterator().next();
        getFlipDefinition().writeToArchive(list);
      }
    }

    public PieceClass getBackFlipClass() {
      if (backReplace == this) { // this will probably never happen
        return null;
      }
      else {
        return backReplace;
      }
    }

    public DynamicProperty getDynamicPropertyDecorator() {
      final SequenceEncoder type = new SequenceEncoder(';');
      type.append("Layer");
      final SequenceEncoder constraints = new SequenceEncoder(',');
      constraints.append(true).append(0).append(1).append(true);
      type.append(constraints.getValue());
      final SequenceEncoder command = new SequenceEncoder(':');
      final KeyStroke stroke = KeyStroke.getKeyStroke('=', InputEvent.SHIFT_DOWN_MASK);
      final SequenceEncoder change = new SequenceEncoder(',');
      change.append('I').append(1);
      command.append("Draw on top").append(stroke.getKeyCode() + "," + stroke.getModifiers()).append(change.getValue());
      type.append(new SequenceEncoder(command.getValue(), ',').getValue());
      final DynamicProperty dp = new DynamicProperty();
      dp.mySetType(DynamicProperty.ID + type.getValue());
      return dp;
    }

    // need a unique name for the basic piece so that flip definitions will work
    public String getUniqueName() {
      if (uniqueName == null) {
        uniqueName = getName();
        int index = 1;
        while (uniquePieceNames.contains(uniqueName)) {
          uniqueName = getName() + " (" + (index++) + ")";
        }
        uniquePieceNames.add(uniqueName);
      }
      return uniqueName;
    }

    public boolean checkHidden(Piece piece) {
      return piece.hideState == HideState.HIDDEN || piece.inForcePool() && getOwner().hiddenInForcePools();
    }

    public PropertySheet getPropertySheetDecorator() {
      final SequenceEncoder type = new SequenceEncoder('~');
      final SequenceEncoder state = new SequenceEncoder('~');
      for (int i = 0; i < classValues.length; ++i) {
        if (classValues[i] != null && !classValues[i].equals("")) {
          type.append("0" + classValues[i]);
          final Object o = getValue(i);
          if (o instanceof String)
            state.append((String) o);
          else if (o instanceof Integer)
            state.append(o.toString());
          else if (o instanceof Boolean)
            state.append(o.equals(Boolean.TRUE) ? "yes" : "no");
          else
            state.append("");
        }
      }

      PropertySheet p = null;
      if (type.getValue() != null && type.getValue().length() > 0) {
        p = new PropertySheet();
        final SequenceEncoder se = new SequenceEncoder(';'); // properties
        se.append(type.getValue() == null ? "" : type.getValue());
        se.append(CLASS_PROPERTIES); // menu name
        se.append('C'); // key
        se.append(0); // commit
        se.append("").append("").append(""); // colour
        p.mySetType(PropertySheet.ID + se.getValue());
        p.mySetState(state.getValue());
      }

      return p;
    }

    public UsePrototype getUsePrototypeDecorator() {
      final SequenceEncoder se = new SequenceEncoder(UsePrototype.ID.replaceAll(";", ""), ';');
      se.append(COMMON_PROPERTIES);
      final UsePrototype p = new UsePrototype();
      p.mySetType(se.getValue());
      return p;
    }

    public Embellishment getAttackedEmbellishmentDecorator() throws IOException {
      return getCombatEmbellishmentDecorator("Mark Attacked", "A", StateFlag.ATTACK);
    }

    public Embellishment getDefendedEmbellishmentDecorator() throws IOException {
      return getCombatEmbellishmentDecorator("Mark Defended", "D", StateFlag.DEFEND);
    }

    private Embellishment getCombatEmbellishmentDecorator(String command, String key, StateFlag flag) throws IOException {
      final BufferedImage image = getSymbol().getImage();
      final int xOffset = (image.getWidth() + 1) / 2 + 5;
      final int yOffset = 0;
      final String imageName = getFlagTab(image.getHeight(), flag);
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(command)                   // Activate command
        .append(InputEvent.CTRL_DOWN_MASK) // Activate modifiers
        .append(key)                       // Activate key
        .append("")                        // Up command
        .append(0)                         // Up modifiers
        .append("")                        // Up key
        .append("")                        // Down command
        .append(0)                         // Down modifiers
        .append("")                        // Down key
        .append("")                        // Reset command
        .append("")                        // Reset key
        .append("")                        // Reset level
        .append(false)                     // Draw underneath when selected
        .append(xOffset)                   // x offset
        .append(yOffset)                   // y offset
        .append(StringArrayConfigurer.arrayToString(new String[] {imageName})) // Image name
        .append(StringArrayConfigurer.arrayToString(new String[] {""}))
        .append(false)                     // loop levels
        .append(command)                   // name
        .append((NamedKeyStroke) null)     // Random key
        .append("")                        // Random text
        .append(false)                     // Follow property
        .append("")                        // Property name
        .append(1);                        // First level value
      final Embellishment layer = new Embellishment();
      layer.mySetType(Embellishment.ID + se.getValue());
      return layer;
    }

    // TODO: permit offset to mask image.
    public Obscurable getPieceValueMask() throws IOException {
      if (getOwner().useHiddenPieces()) {
        final SequenceEncoder se = new SequenceEncoder(';');
        se.append(NamedKeyStroke.of(KeyStroke.getKeyStroke('I', InputEvent.CTRL_DOWN_MASK))); // key command
        se.append(getImageName()); // hide image
        se.append("Hide Info"); // menu name
        final BufferedImage image = getSymbol().getImage();
        se.append("G" + getFlagLayer(new Dimension(image.getWidth(), image.getHeight()), StateFlag.INFO)); // display style
        if (name == null)
          se.append(getName());
        else
          se.append("Unknown Piece"); // mask name
        se.append("sides:" + getOwner().getName()); // owning player
        final Obscurable p = new Obscurable();
        p.mySetType(Obscurable.ID + se.getValue());
        return p;
      }
      else {
        return null;
      }
    }

    public MovementMarkable getMovementMarkableDecorator() throws IOException {
      final SequenceEncoder se = new SequenceEncoder(';');
      final BufferedImage img = getSymbol().getImage();
      final int xOffset = (img.getWidth() + 1) / 2;
      final int yOffset = -img.getHeight() / 2;
      final String movedIcon = getFlagTab(img.getHeight(), StateFlag.MOVE);
      se.append(movedIcon).append(xOffset).append(yOffset);
      final MovementMarkable p = new MovementMarkable();
      p.mySetType(MovementMarkable.ID + se.getValue());
      return p;
    }

    public Decorator getHiddenDecorator() throws IOException {
      if (getOwner().useHiddenPieces()) {
        final Decorator p;
        final String sides;
        if (getOwner() == Player.ALL_PLAYERS || getOwner() == Player.NO_PLAYERS) {
          sides = "side:";
        }
        else {
          sides = "sides:" + getOwner().getName();
        }
        final SequenceEncoder se = new SequenceEncoder(';');
        se.append(NamedKeyStroke.of(KeyStroke.getKeyStroke('H', InputEvent.CTRL_DOWN_MASK))); // key command
        if (getHiddenSymbol() == null) {
          // TODO Add transparency to background color as well as alpha for unit.
          se.append("Hide Piece"); // command
          se.append(new Color(255, 255, 255)); // background colour
          se.append(sides); // owning player
          p = new Hideable();
          ((Hideable) p).mySetType(Hideable.ID + se.getValue());
        }
        else {
          se.append(getHiddenSymbol().getFileName()); // hide image
          se.append("Hide Piece"); // menu name
          final BufferedImage image = getSymbol().getImage();
          se.append("G" + getFlagLayer(new Dimension(image.getWidth(), image.getHeight()), StateFlag.MARKER)); // display style
          se.append(getHiddenName()); // mask name
          se.append(sides); // owning player
          p = new Obscurable();
          ((Obscurable) p).mySetType(Obscurable.ID + se.getValue());
        }
        return p;
      }
      else {
        return null;
      }
    }

    public FreeRotator getFreeRotatorDecorator() {
      final int nsides = getMap().getNFaces();
      final int nfacings;
      switch (getAllowedFacings()) {
      case NONE:
        return null;
      case FLAT_SIDES:
        nfacings = nsides == 4 ? 4 : 12;
        break;
      default:
        nfacings = nsides == 4 ? 8 : 24;
      }
      final String type = FreeRotator.ID + nfacings + ";];[;Rotate CW;Rotate CCW;;;;";
      final FreeRotator p = new FreeRotator();
      p.mySetType(type);
      return p;
    }

    public String getHiddenName() {
      return "Unknown Piece";
    }

    protected void setValue(int index, int value) {
      values[index] = value;
      types[index] = ValueType.NUMERIC;
    }

    public FacingDirection getAllowedFacings() {
      if (allowedFacings == null)
        return FacingDirection.NONE;
      else if (facing >= allowedFacings.length)
        return FacingDirection.NONE;
      else
        return allowedFacings[facing];
    }

    public SymbolSet.SymbolData getHiddenSymbol() throws IOException {
      if (hiddenSymbol == PLAYER_DEFAULT_HIDDEN_SYMBOL)
        return getOwner().getHiddenSymbol();
      else if (hiddenSymbol == NO_HIDDEN_SYMBOL)
        return null;
      else return getSet().getGamePiece(hiddenSymbol);
    }

    public String getImageName() throws IOException {
      if (getSymbol() == null)
        return null;
      else
        return symbol.getFileName();
    }

    public Player getOwner() {
      if (owner == NO_PLAYERS)
        return Player.NO_PLAYERS;
      else if (owner >= players.size())
        return Player.ALL_PLAYERS;
      else
        return players.get(owner);
    }

    public Player getPlayer(Piece p) {
      return getOwner();
    }

    protected void setFlipClass(int to) {
      if (to >= 0 && to < pieceClasses.size())
        flipClass = pieceClasses.get(to);
    }

    protected void setBackFlipClass(int from) {
      backReplace = pieceClasses.get(from);
      assert (backReplace.getFlipClass() == this);
    }

    public PieceClass getFlipClass() {
      if (flipClass == this) { // if the flip class is this, then it doesn't count
        return null;
      }
      else {
        return flipClass;
      }
    }

    public String getName() {
      return name;
    }

    protected void setValue(int index, String value) {
      final byte[] b = value.getBytes(StandardCharsets.US_ASCII);
      int result = 0;
      for (int i = 0; i < 4; ++i)
        result = (result << 8) + (b[i] & 0xff);
      values[index] = result;
      types[index] = ValueType.TEXT;
    }

    protected void setValue(int index, boolean value) {
      if (value)
        values[index] = 1;
      else
        values[index] = 0;
      types[index] = ValueType.YESNO;
    }

    public int getValueAsInt(int index) {
      return values[index];
    }

    public String getValueAsString(int index) {
      final byte[] b = new byte[4];
      int length = 0;
      int mask = 0x7f000000;
      for (int i = 0; i < b.length; ++i) {
        b[i] = (byte) ((values[index] & mask) >> ((3 - i) * 8));
        if (b[i] < 0x20 || b[i] > 0x7e)
          break;
        ++length;
        mask >>= 8;
      }
      return new String(b, 0, length, StandardCharsets.US_ASCII);
    }

    public int getNValues() {
      int total = 0;
      for (final ValueType t : types)
        if (t != ValueType.NOT_USED)
          ++total;
      return total;
    }

    public boolean getValueAsBoolean(int index) {
      return values[index] > 0;
    }

    public Object getValue(int index) {
      if (types[index] == null)
        return null;
      switch (types[index]) {
      case NUMERIC:
        return getValueAsInt(index);
      case TEXT:
        return getValueAsString(index);
      case YESNO:
        return getValueAsBoolean(index);
      default:
        return null;
      }
    }

    protected void writeToArchive(ListWidget list) throws IOException {
      getDefaultPiece().writeToArchive(list);
    }

    protected Piece getDefaultPiece() {
      if (defaultPiece == null)
        defaultPiece = new Piece(this);
      return defaultPiece;
    }

    protected Piece getFlipDefinition() {
      if (flipDefinition == null) {
        flipDefinition = new Piece(this);
      }
      return flipDefinition;
    }

    protected SymbolSet.SymbolData getSymbol() throws IOException {
      return symbol;
    }
  }

  public static final String COMMON_PROPERTIES = "Common Properties";
  private String name;
  private MapBoard map = null;
  private final List<PieceClass> pieceClasses = new ArrayList<>();
  private final List<Piece> pieces = new ArrayList<>();
  private final List<Player> players = new ArrayList<>();
  private final java.util.Map<Integer, List<Piece>> stacks = new HashMap<>();
  private final java.util.Map<Integer, List<Piece>> forcePoolHashMap = new HashMap<>();
  private final ForcePoolList forcePools = new ForcePoolList();
  private final String[] classValues = new String[8];
  private final String[] pieceValues = new String[8];
  private FacingDirection[] allowedFacings;

  protected PieceClass getClassFromIndex(int index) {
    if (index < 0 || index >= pieceClasses.size())
      return null;
    return pieceClasses.get(index);
  }

  protected SymbolSet getCardDeck(int deck) throws IOException {
    SymbolSet set = cardDecks.get(deck);
    if (set == null) {
      final File f = action.getCaseInsensitiveFile(new File(deckName + "-c" + (deck + 1) + ".set"), file, true,
        new ExtensionFileFilter(ADC2Utils.SET_DESCRIPTION, new String[] {ADC2Utils.SET_EXTENSION}));
      if (f == null)
        throw new FileNotFoundException("Unable to locate deck symbol set.");
      set = new SymbolSet();
      set.importCardSet(action, f);
      cardDecks.put(deck, set);
    }
    return set;
  }

  private java.util.Map<StateFlag, java.util.Map<Dimension, String>> hiddenFlagImages;
  private int version;
  private int classCombatSummaryValues;
  private int pieceCombatSummaryValues;
  private final StatusDots[] statusDots = new StatusDots[6]; //NOPMD
  private final List<String> turnNames = new ArrayList<>();
  private boolean useLOS;
  private String deckName;
  private int nCardSets;
  private final String[] infoPages = new String[10];
  private String infoPageName;

  public static final Color FLAG_BACKGROUND = new Color(1.0f, 1.0f, 0.8f, 0.8f);
  public static final Color FLAG_FOREGROUND = new Color(0.5f, 0.0f, 0.5f, 1.0f);
  //  public static final Color FLAG_BACKGROUND = Color.BLACK;
//  public static final Color FLAG_FOREGROUND = Color.WHITE;
  private int nFlipDefs = 0;
  private PieceWindow flipDefs;
  private PieceWindow pieceWin;

  public static class StateFlag {
    public static final StateFlag MOVE = new StateFlag("M", FLAG_BACKGROUND, FLAG_FOREGROUND, 0);
    public static final StateFlag ATTACK = new StateFlag("A", FLAG_BACKGROUND, FLAG_FOREGROUND, 1);
    public static final StateFlag DEFEND = new StateFlag("D", FLAG_BACKGROUND, FLAG_FOREGROUND, 1);
    public static final StateFlag INFO = new StateFlag("h", FLAG_BACKGROUND, FLAG_FOREGROUND, 2);
    public static final StateFlag MARKER = new StateFlag("H", FLAG_BACKGROUND, FLAG_FOREGROUND, 2);
    public static final StateFlag COMBAT = new StateFlag("C", FLAG_BACKGROUND, FLAG_FOREGROUND, 1);

    private final String name;
    private final Color background;
    private final Color foreground;
    private final int tab;
    private String imageName;
    // TODO contents of statusDots are updated but never used
    private final List<StatusDots> statusDots = new ArrayList<>();

    public StateFlag(String flag, Color background, Color foreground, int tab) {
      this.name = flag;
      this.background = background;
      this.foreground = foreground;
      this.tab = tab;
    }

    public String getStatusIconName() throws IOException {
      if (imageName == null) {
        final BufferedImage icon =
          new BufferedImage(10, 15, BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g = icon.createGraphics();
        drawFlagImage(g);
        imageName = getUniqueImageFileName(name, ".png");

        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        ImageIO.write(icon, "png", out);
        final byte[] imageDataArray = out.toByteArray();
        GameModule.getGameModule().getArchiveWriter().addImage(imageName, imageDataArray);
      }
      return imageName;
    }

    public void addStatusDots(StatusDots dots) {
      statusDots.add(dots);
    }

    public void drawFlagImage(Graphics2D g) {
      final int tabHeight = 15;
      final int tabWidth = 10;
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
      g.setColor(background);
      g.fillRoundRect(-tabWidth, 0, 2 * tabWidth, tabHeight, 6, 6);
      g.setColor(foreground);
      g.drawRoundRect(-tabWidth, 0, 2 * tabWidth - 1, tabHeight - 1, 6, 6);
      g.setFont(new Font("Dialog", Font.PLAIN, 9));
      final Rectangle2D r = g.getFontMetrics().getStringBounds(name, g);
      g.drawString(name, tabWidth / 2 - (int) (r.getWidth() / 2.0) - 1, 11);
      g.setBackground(new Color(0, 0, 0, 0));
      g.clearRect(-tabWidth, 0, tabWidth, tabHeight);
    }
  }

  private String getFlagTab(int height, StateFlag flag) throws IOException {
    if (hiddenFlagImages == null)
      hiddenFlagImages = new HashMap<>();

    final java.util.Map<Dimension, String> map = hiddenFlagImages.computeIfAbsent(flag, k -> new HashMap<>());

    final Dimension d = new Dimension(0, height);
    String imageName = map.get(d);
    if (imageName == null) {
      final int tabHeight = 15;
      final int tabSpace = height < 43 ? (height - tabHeight) / 2 : tabHeight - 1;
      final int tabWidth = 10;
      final BufferedImage icon =
        new BufferedImage(tabWidth, height, BufferedImage.TYPE_INT_ARGB);
      final Graphics2D g = icon.createGraphics();
      g.translate(0, tabSpace * flag.tab);
      flag.drawFlagImage(g);

      imageName = getUniqueImageFileName(flag.name + 0 + "x" + height);
      map.put(d, imageName);

      final ByteArrayOutputStream out = new ByteArrayOutputStream();
      ImageIO.write(icon, "png", out);
      final byte[] imageDataArray = out.toByteArray();
      GameModule.getGameModule().getArchiveWriter().addImage(imageName, imageDataArray);
    }

    return imageName;
  }

  private String getFlagLayer(Dimension d, StateFlag flag) throws IOException {
    if (hiddenFlagImages == null)
      hiddenFlagImages = new HashMap<>();

    final java.util.Map<Dimension, String> map = hiddenFlagImages.computeIfAbsent(flag, k -> new HashMap<>());

    String imageName = map.get(d);
    if (imageName == null) {
      final int tabHeight = 15;
      final int tabSpace = d.height < 43 ? (d.height - tabHeight) / 2 : tabHeight - 1;
      final int tabWidth = 10;
      final BufferedImage icon = new BufferedImage(
        d.width + 2 * tabWidth, d.height, BufferedImage.TYPE_INT_ARGB);
      final Graphics2D g = icon.createGraphics();
      g.translate(d.width + tabWidth, tabSpace * flag.tab);
      flag.drawFlagImage(g);

      imageName = getUniqueImageFileName(flag.name + d.width + "x" + d.height);
      map.put(d, imageName);

      final ByteArrayOutputStream out = new ByteArrayOutputStream();
      ImageIO.write(icon, "png", out);
      final byte[] imageDataArray = out.toByteArray();
      GameModule.getGameModule().getArchiveWriter().addImage(imageName, imageDataArray);
    }

    return imageName;
  }

  public boolean usePieceValues() {
    for (final String pieceValue : pieceValues) {
      if (pieceValue != null && !pieceValue.equals(""))
        return true;
    }
    return false;
  }

  @Override
  protected void load(File f) throws IOException {
    super.load(f);

    try (InputStream fin = Files.newInputStream(f.toPath());
         InputStream bin = new BufferedInputStream(fin);
         DataInputStream in = new DataInputStream(bin)) {
      name = stripExtension(f.getName());

      final int header = in.readByte();
      if (header != -3 && header != -2)
        throw new FileFormatException("Invalid Game Module Header");

      // TODO: figure out version-specific formats for older versions.
      version = in.readUnsignedShort();

      final String s = readWindowsFileName(in);
      final String mapFileName = forceExtension(s, "map");
      map = new MapBoard();
      final File mapFile = action.getCaseInsensitiveFile(new File(mapFileName), f, true,
        new ExtensionFileFilter(ADC2Utils.MAP_DESCRIPTION, new String[]{ADC2Utils.MAP_EXTENSION}));
      if (mapFile == null)
        throw new FileNotFoundException("Unable to locate map file.");
      map.importFile(action, mapFile);

      // TODO: each block has an ideosyncratic way of terminating itself.
      // this has to be tested extensively.
      try {
        readGameTurnBlock(in);
        readClassBlock(in);
        readClassValueBlock(in);
        readPieceBlock(in);
        readPieceValueBlock(in);
        readPlayerBlock(in);
        readReplayBlock(in);
        readPoolBlock(in);
        readStackBlock(in);
        readCombatSummaryBlock(in);
        readFacingBlock(in);
        readSoundSettingBlock(in);
        readFlipDefinitionBlock(in);
        readPieceStatusDotsBlock(in);
        readDiceBlock(in);
        readTurnNameBlock(in);
        readLOSBlock(in);
        readLOSFlagBlock(in);
        readDeckNameBlock(in);
        readPoolOwnerBlock(in);
        readAutoRevealWhenMovingLOSFlagBlock(in);
        readCombatRevealFlagBlock(in);
        readInfoPageBlock(in);
        readInfoSizeBlock(in);
        readAllianceBlock(in);
        readDrawOptionsBlock(in);
        readPieceStatusDotsBlock(in); // read this in again!
      }
      catch (ADC2Utils.NoMoreBlocksException e) {
        log.error("Error during import", e);
      }
    }
  }

  // TODO: what happens when this conflicts with the draw options in the map file itself?
  private void readDrawOptionsBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Draw Options");

    @SuppressWarnings("unused") final boolean showHexSides = in.readByte() != 0;
    @SuppressWarnings("unused") final boolean showHexLines = in.readByte() != 0;
    @SuppressWarnings("unused") final boolean showPlaceNames = in.readByte() != 0;
    final int pieceOptionFlags = in.readUnsignedByte();
    @SuppressWarnings("unused") final boolean showPieces = (pieceOptionFlags & 0x1) > 0;
    @SuppressWarnings("unused") final boolean showMarkers = (pieceOptionFlags & 0x2) == 0;

    /*
     * First three bytes give symbols per hex for the three zoomlevels.
     * 1 = 1 (inside hex)
     * 4 = 4 per hex
     * 101 = 1 (inside hex) & overlay stack on pieces
     * 104 = 4 per hex & overlay stack on pieces
     * 200 = 1 (centered)
     * 201 = 1 (centered) & overlay stack on pieces
     * all other values are completely invalid.
     *
     * The purpose of the last byte in this block is unknown.
     */
    in.readFully(new byte[4]);
  }

  // TODO: allow multiple players to see hidden units.
  protected void readAllianceBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Alliances");

    /* int nAlliances = */ ADC2Utils.readBase250Word(in); // ignored
    for (final Player p1 : players) {
      in.readUnsignedShort(); // unknown
      for (final Player p2 : players) {
        if (in.readUnsignedShort() > 0) {
          p1.setAlly(p2);
        }
      }
    }
  }

  protected void readInfoSizeBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Info Size");
    in.readFully(new byte[4]);
  }

  protected void readInfoPageBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Info Page");

    infoPageName = readWindowsFileName(in);
    if (infoPageName.length() > 0) {
      final File ipx = action.getCaseInsensitiveFile(new File(forceExtension(infoPageName, "ipx")), file, true,
        new ExtensionFileFilter("Info page file (*.ipx;*.IPX)", new String[] {".ipx"}));
      if (ipx != null) {

        try (InputStream fin = Files.newInputStream(ipx.toPath());
             InputStream bin = new BufferedInputStream(fin);
             DataInputStream input = new DataInputStream(bin)) {
          try {
            while (true) { // loop until EOF
              while (input.readUnsignedByte() != 0x3b) {
                // skip
              }
              final int idx = input.readUnsignedByte();
              final int len = input.readUnsignedByte();
              final byte[] dimensions = new byte[8];
              input.readFully(dimensions);
              final byte[] buf = new byte[len];
              input.readFully(buf);
              final String name = new String(buf, StandardCharsets.US_ASCII);
              if (idx < 10 && infoPages[idx] == null) {
                infoPages[idx] = name;
              }
            }
          }
          catch (EOFException e) {
            // do nothing
          }
        }
      }
      else {
        infoPageName = null;
      }
    }
  }

  protected void readCombatRevealFlagBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Combat Reveal Option Flag");

    in.readByte();
  }

  protected void readAutoRevealWhenMovingLOSFlagBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Auto Reveal When Moving (LOS) Flag");

    in.readByte();
  }

  protected void readPoolOwnerBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Pool Owner");

    final Iterator<Pool> iter = forcePools.iterator();
    for (int i = 0; i < forcePools.size(); ++i) {
      final Pool p = iter.next();
      final int owner = in.readUnsignedByte();
      if (p instanceof Cards) {
        ((Cards) p).setOwner(owner);
      }
    }
  }

  protected void readDeckNameBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Deck Name");
    deckName = stripExtension(readWindowsFileName(in));
  }

  protected void readLOSFlagBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "LOS Flags");

    /* boolean useElevation = */ in.readByte();
    final int rightMouseButton = in.readByte();
    /* 0 = normal
       1 = move pieces
       2 = zoom in out
       3 = single LOS
       4 = area LOS
       5 = redraw map
       6 = place pieces ? */
    if (rightMouseButton == 3 || rightMouseButton == 4)
      useLOS = true;
  }

  protected void readLOSBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "LOS");

    in.readFully(new byte[18]); // unknown
    /* int maxRange = */ in.readUnsignedShort();
    /* int degradeLOS = */ in.readByte();
    /* int pointsToBlockLOS = */ ADC2Utils.readBase250Word(in);
    /* String levelHeight = */ readNullTerminatedString(in, 20);
    /* int useSmoothing = */ in.readByte();
    /* int cliffElevation = */ ADC2Utils.readBase250Word(in);
    if (version > 0x0206) {
      /* int hexSize = */ ADC2Utils.readBase250Word(in);
      final byte[] units = new byte[10];
      in.readFully(units);
    }

    final int nBlocks = ADC2Utils.readBase250Word(in);
    for (int i = 0; i < nBlocks; ++i) {
      /* int blockPoints = */ ADC2Utils.readBase250Word(in);
      /* int baseElevation = */ ADC2Utils.readBase250Word(in);
      /* int aboveGroundLevel = */ ADC2Utils.readBase250Word(in);
      /* int whenSpotting = */ ADC2Utils.readBase250Word(in);
      /* int whenTarget = */ ADC2Utils.readBase250Word(in);
      final byte[] color = new byte[3];
      in.readFully(color);
    }
  }

  protected void readTurnNameBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Turn Names");

    final int nNames = ADC2Utils.readBase250Word(in);
    boolean terminate = false;

    for (int i = 0; i < nNames; ++i) {
      final String name = readNullTerminatedString(in, 50);
      if (name.equals(""))
        terminate = true;
      else if (!terminate)
        turnNames.add(name);
    }
  }

  // makers of ADC2 modules never seem to make use of this.
  protected void readDiceBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Dice");

    /* boolean autoRevealWhenMoving = */ in.readByte();
    in.readByte(); // unknown
    /* int ndice = */ in.readUnsignedByte();
    /* int nsides = */ in.readUnsignedByte();
  }

  // All of this information appears to be ignored by ADC2.  This information
  // is read again later in the file.
  protected void readPieceStatusDotsBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Piece Status Dots");

    final byte[] size = new byte[3];
    for (int i = 0; i < 6; ++i) {
      final int type = in.readByte();
      final int show = ADC2Utils.readBase250Word(in);
      final int color = in.readUnsignedByte();
      final int position = in.readByte();
      in.readFully(size);

      statusDots[i] = new StatusDots(type, show, ADC2Utils.getColorFromIndex(color), position, size[2]);
    }
  }

  protected void readFlipDefinitionBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Flip Definition");

    in.readUnsignedByte(); // unknown byte

    nFlipDefs = ADC2Utils.readBase250Word(in);
    for (int i = 0; i < nFlipDefs; ++i) {
      final int from = ADC2Utils.readBase250Word(in);
      final int to = ADC2Utils.readBase250Word(in);
      if (from >= 0 && from < pieceClasses.size() && to >= 0 && to < pieceClasses.size()) {
        pieceClasses.get(from).setFlipClass(to);
        pieceClasses.get(to).setBackFlipClass(from);
      }
    }
  }

  protected void readSoundSettingBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Sound Settings");

    for (int i = 0; i < 3; ++i)
      /* scrollJumpSize[i] = */ in.readUnsignedByte();
    in.readFully(new byte[3]); // unknown
    /* soundOn = */  in.readUnsignedByte();
  }

  public enum FacingDirection {
    FLAT_SIDES, VERTEX, BOTH, NONE
  }

  protected void readFacingBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Facing");

    final int nFacing = in.readUnsignedByte();
    allowedFacings = new FacingDirection[nFacing + 1];
    allowedFacings[0] = FacingDirection.NONE;

    for (int i = 0; i < nFacing; ++i) {
      /* String styleName = */ readNullTerminatedString(in);

      final int direction = in.readUnsignedByte();
      // one invalid facing struct will invalidate all later ones.
      if (i == 0 || allowedFacings[i] != FacingDirection.NONE) {
        switch (direction) {
        case 2:
          allowedFacings[i + 1] = FacingDirection.VERTEX;
          break;
        case 3:
          allowedFacings[i + 1] = FacingDirection.BOTH;
          break;
        default:
          allowedFacings[i + 1] = FacingDirection.FLAT_SIDES;
        }
      }
      else {
        allowedFacings[i + 1] = FacingDirection.NONE;
      }

      // this describes how the arrow is drawn in ADC2
      /* int display = */ in.readUnsignedByte();
      /* int fillColor = */ in.readUnsignedByte();
      /* int outlineColor = */ in.readUnsignedByte();
      // zoom sizes
      in.readFully(new byte[3]);
    }
  }

  protected void readCombatSummaryBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Fast Zoom");

    /* int fastZoom = */ in.readUnsignedByte();
    classCombatSummaryValues = in.readUnsignedByte();
    pieceCombatSummaryValues = in.readUnsignedByte();
    /* int fastDraw = */ in.readUnsignedByte();
  }

  // None of this is either doable or appropriate in VASSAL.
  protected void readStackBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Stack");

    final int nStackDefs = ADC2Utils.readBase250Word(in);
    for (int i = 0; i < nStackDefs; ++i) {
      // SymbolSet.SymbolData symbol = getSet().getGamePiece(
      ADC2Utils.readBase250Word(in); // );
      // int mustContain =
      ADC2Utils.readBase250Word(in); // class index
      for (int j = 0; j < 3; ++j) {
        // one per zoom level
        // int atLeastNPieces =
        in.readUnsignedByte();
      }
      // int owningPlayer =
      ADC2Utils.readBase250Word(in);
    }
  }

  // TODO: this is a big job to implement and may not even be worth doing at all.
  protected void readReplayBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Replay");

    final int nBytes;
    if (version > 0x0203)
      nBytes = ADC2Utils.readBase250Integer(in);
    else
      nBytes = ADC2Utils.readBase250Word(in);
    in.readFully(new byte[nBytes]);
  }

  protected void readPoolBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, FORCE_POOL);

    final int nForcePools = ADC2Utils.readBase250Word(in);
    for (int i = 0; i < nForcePools; ++i) {
      final String n = readNullTerminatedString(in, 25);
      final int type = in.readByte();
      in.readFully(new byte[2]); // not sure what these do
      final int nunits = ADC2Utils.readBase250Word(in); // ignored
      if (nunits != FORCE_POOL_BLOCK_END) {
        switch (type) {
        case 2:
          forcePools.add(new HandPool(n, forcePoolHashMap.get(i)));
          break;
        case 3:
          forcePools.add(new DeckPool(n, forcePoolHashMap.get(i)));
          break;
        default:
          forcePools.add(new ForcePool(n, forcePoolHashMap.get(i)));
        }
      }
      else
        break;
    }
  }

  protected void readPlayerBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Player");

    // file format doesn't actually care how many players it says there is.
    /* int nplayers = */ ADC2Utils.readBase250Word(in);

    // The last player is typically an empty string which indicates the end of
    // the list despite the fact that the number of players is clearly
    // indicated.
    String name;
    do {
      name = readNullTerminatedString(in, 25);
      // someday we may try to crack this, but since it doesn't actually encrypt anything
      // there's no real point.
      // byte[] password = new byte[20];
      in.readFully(new byte[20]);
      /* int startingZoomLevel = */ in.readByte();
      /* int startingPosition = */ ADC2Utils.readBase250Word(in);
      final SymbolSet.SymbolData hiddenSymbol = getSet().getGamePiece(ADC2Utils.readBase250Word(in));
      /* String picture = */ readNullTerminatedString(in);

      // we don't do anything with this.
      /* int searchRange = */ in.readUnsignedByte();

      final int hiddenPieceOptions = in.readUnsignedByte();
      in.readByte(); // padding

      if (name.length() > 0) {
        final Player player = new Player(name, hiddenSymbol, hiddenPieceOptions);
        players.add(player);
      }
    } while (name.length() > 0);
  }

  @SuppressWarnings("fallthrough")
  protected void readPieceBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, PIECE);

    final int nPieces = ADC2Utils.readBase250Word(in);
    for (int i = 0; i < nPieces; ++i) {
      String name = readNullTerminatedString(in, 25);

      final PieceClass cl = getClassFromIndex(ADC2Utils.readBase250Word(in));
      if (cl == null)
        throw new FileFormatException("Invalid Class Index");

      // prevent duplication
      if (name.equals(cl.getName()))
        name = "";

      final int[] values = new int[8];
      for (int j = 0; j < values.length; ++j)
        values[j] = in.readInt();

      final ValueType[] types = new ValueType[8];
      for (int j = 0; j < types.length; ++j) {
        switch (in.readUnsignedByte()) {
        case 1:
          types[j] = ValueType.NUMERIC;
          break;
        case 2:
          types[j] = ValueType.TEXT;
          break;
        case 3:
          types[j] = ValueType.YESNO;
          break;
        case 10:
          if (j == 0) {
            types[j] = ValueType.CARD;
            break;
          } // else fall through
        default:
          types[j] = ValueType.NOT_USED;
          break;
        }
      }

      final HideState hidden;
      switch (in.readUnsignedByte()) {
      case 0:
        hidden = HideState.NOT_HIDDEN;
        break;
      case 1:
        hidden = HideState.INFO_HIDDEN;
        break;
      default:
        hidden = HideState.HIDDEN;
        break;
      }

      in.readFully(new byte[2]); // don't know what these do

      final int position = ADC2Utils.readBase250Word(in);
      final int flags = in.readUnsignedByte();

      int facing = in.readUnsignedByte();
      if (facing > FACING_ANGLES.length)
        facing = 0;

      final Piece p = new Piece(position, name, cl, hidden, flags, facing);
      for (int j = 0; j < values.length; ++j) {
        p.setValue(j, values[j]);
        p.types[j] = types[j];
      }
      pieces.add(p);
    }
  }

  protected void readClassValueBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Class Value");

    for (int i = 0; i < classValues.length; ++i)
      classValues[i] = readNullTerminatedString(in, 15);
  }

  protected void readPieceValueBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Piece Value");

    for (int i = 0; i < pieceValues.length; ++i)
      pieceValues[i] = readNullTerminatedString(in, 15);
  }

  protected void readClassBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Class");

    final int nClasses = ADC2Utils.readBase250Word(in);

    for (int i = 0; i < nClasses; ++i) {
      final int symbolIndex = ADC2Utils.readBase250Word(in);
      final String name = readNullTerminatedString(in, 25);

      final int[] values = new int[8];
      for (int j = 0; j < values.length; ++j)
        values[j] = in.readInt();

      boolean isCard = false;
      int setIndex = 0;
      final ValueType[] types = new ValueType[8];
      for (int j = 0; j < types.length; ++j) {
        final int t = in.readUnsignedByte();
        if (j == 0 && t == 10)
          isCard = true;
        if (isCard) {
          if (j == 1)
            setIndex = t;
          if (setIndex >= nCardSets)
            nCardSets = setIndex + 1;
        }
        else {
          switch (t) {
          case 1:
            types[j] = ValueType.NUMERIC;
            break;
          case 2:
            types[j] = ValueType.TEXT;
            break;
          case 3:
            types[j] = ValueType.YESNO;
            break;
          default:
            types[j] = ValueType.NOT_USED;
          }
        }
      }

      final int owner = in.readUnsignedByte();
      final int hiddenSymbol = ADC2Utils.readBase250Word(in);

      // 0 = not used.
      final int facing = in.readUnsignedByte();

      final PieceClass cl;
      if (isCard) {
        cl = new CardClass(name, symbolIndex, setIndex);
      }
      else {
        cl = new PieceClass(name, getSet().getGamePiece(symbolIndex), owner, hiddenSymbol, facing);

        for (int j = 0; j < values.length; ++j) {
          cl.setValue(j, values[j]);
          cl.types[j] = types[j];
        }
      }

      pieceClasses.add(cl);
    }
  }

  protected void readGameTurnBlock(DataInputStream in) throws IOException {
    ADC2Utils.readBlockHeader(in, "Game Turn");
    @SuppressWarnings("unused") final int gameTurn = ADC2Utils.readBase250Word(in);
  }

  protected void writePrototypesToArchive(GameModule gameModule) {
    final PrototypesContainer container = gameModule.getAllDescendantComponentsOf(PrototypesContainer.class).iterator().next();
    final PrototypeDefinition def = new PrototypeDefinition();
    insertComponent(def, container);
    def.setConfigureName(COMMON_PROPERTIES);

    // set common properties
    GamePiece gp = new BasicPiece();

    final Delete del = new Delete();
    SequenceEncoder se = new SequenceEncoder(';');
    se.append("Delete").append(NamedKeyStroke.of(KeyStroke.getKeyStroke("DELETE")));
    del.mySetType(Delete.ID + se.getValue());
    del.setInner(gp);
    gp = del;

    if (forcePools.count(ForcePool.class) > 0)
      gp = new ReturnToDeck(ReturnToDeck.ID + "Return to Force Pool;R;;Select Force Pool", gp);

    se = new SequenceEncoder(';');
    se.append(NamedKeyStroke.of(KeyStroke.getKeyStroke('T', InputEvent.CTRL_DOWN_MASK)))
      .append("Movement Trail")
      .append(false)
      .append(false)
      .append(10)
      .append(Color.WHITE)
      .append(Color.BLACK)
      .append(100)
      .append(0);
    gp = new Footprint(Footprint.ID + se.getValue(), gp);
    se = new SequenceEncoder(',');
    se.append(ADC2Utils.TYPE);

    gp = new Marker(Marker.ID + se.getValue(), gp);
    gp.setProperty(ADC2Utils.TYPE, PIECE);

    def.setPiece(gp);
  }

  @Override
  public void writeToArchive() throws IOException {
    final GameModule gameModule = GameModule.getGameModule();
    gameModule.setAttribute(GameModule.MODULE_NAME, name);

    writePrototypesToArchive(gameModule);
    getMap().writeToArchive();
    configureStatusFlagButtons();
    configureMapLayers();
    pieceWin = gameModule.getAllDescendantComponentsOf(PieceWindow.class).iterator().next();
    configureFlipDefinitions(gameModule);
    writeClassesToArchive(gameModule);
    writeForcePoolsToArchive(gameModule);
    writeDecksToArchive(gameModule);
    writeHandsToArchive(gameModule);
    writeInfoPagesToArchive(gameModule);
    writeToolbarMenuToArchive(gameModule);
    writeSetupStacksToArchive(gameModule);
    writePlayersToArchive(gameModule);
    configureMouseOverStackViewer(gameModule);
    configureMainMap();
    configureDiceRoller(gameModule);
    if (turnNames.size() > 1)  // must have at least two turns
      configureTurnCounter(gameModule);
    if (useLOS)
      insertComponent(new LOS_Thread(), gameModule);
  }

  private void configureFlipDefinitions(GameModule gameModule) {
    if (nFlipDefs > 0) {
      flipDefs = new PieceWindow();
      insertComponent(flipDefs, gameModule);
      flipDefs.setAttribute(PieceWindow.NAME, FLIP_DEFINITIONS);
      flipDefs.setAttribute(PieceWindow.HIDDEN, Boolean.TRUE);
      flipDefs.setAttribute(PieceWindow.BUTTON_TEXT, "");
      flipDefs.setAttribute(PieceWindow.TOOLTIP, "");

      final ListWidget list = new ListWidget();
      insertComponent(list, flipDefs);
    }
  }

  private void configureMainMap() throws IOException {
    final Map mainMap = getMainMap();
    mainMap.setAttribute(Map.MARK_UNMOVED_ICON, StateFlag.MOVE.getStatusIconName());
//  if (usePieceNames) {
//    mainMap.setAttribute(Map.MOVE_WITHIN_FORMAT, "$" + Map.PIECE_NAME + "$" + "/" + PC_NAME + " moves $" + Map.OLD_LOCATION + "$ -> $" + Map.LOCATION + "$ *");
//    mainMap.setAttribute(Map.MOVE_TO_FORMAT, "$" + Map.PIECE_NAME + "$" + "/" + PC_NAME + " moves $" + Map.OLD_LOCATION + "$ -> $" + Map.LOCATION + "$ *");
//    mainMap.setAttribute(Map.CREATE_FORMAT, "$" + Map.PIECE_NAME + "$/" + PC_NAME + " created in $" + Map.LOCATION + "$");
//  }
  }

  @SuppressWarnings("removal")
  private void configureStatusFlagButtons() throws IOException {
    String imageName;
    MassKeyCommand command;

    imageName = StateFlag.ATTACK.getStatusIconName();
    command = new MassKeyCommand();
    insertComponent(command, getMainMap());
    command.setAttribute(MassKeyCommand.TOOLTIP, "Clear attacked status");
    command.setAttribute(MassKeyCommand.BUTTON_TEXT, "Attacked");
    command.setAttribute(MassKeyCommand.HOTKEY, null);
    command.setAttribute(MassKeyCommand.ICON, imageName);
    command.setAttribute(MassKeyCommand.NAME, "Attacked");
    command.setAttribute(MassKeyCommand.KEY_COMMAND, NamedKeyStroke.of(KeyStroke.getKeyStroke('A', InputEvent.CTRL_DOWN_MASK)));
    command.setAttribute(MassKeyCommand.PROPERTIES_FILTER, "Mark Attacked_Active = true");
    command.setAttribute(MassKeyCommand.DECK_COUNT, -1);
    command.setAttribute(MassKeyCommand.REPORT_SINGLE, Boolean.TRUE);
    command.setAttribute(MassKeyCommand.REPORT_FORMAT, "");

    imageName = StateFlag.DEFEND.getStatusIconName();
    command = new MassKeyCommand();
    insertComponent(command, getMainMap());
    command.setAttribute(MassKeyCommand.TOOLTIP, "Clear defended status");
    command.setAttribute(MassKeyCommand.BUTTON_TEXT, "Defended");
    command.setAttribute(MassKeyCommand.HOTKEY, null);
    command.setAttribute(MassKeyCommand.ICON, imageName);
    command.setAttribute(MassKeyCommand.NAME, "Defended");
    command.setAttribute(MassKeyCommand.KEY_COMMAND, NamedKeyStroke.of(KeyStroke.getKeyStroke('D', InputEvent.CTRL_DOWN_MASK)));
    command.setAttribute(MassKeyCommand.PROPERTIES_FILTER, "Mark Defended_Active = true");
    command.setAttribute(MassKeyCommand.DECK_COUNT, -1);
    command.setAttribute(MassKeyCommand.REPORT_SINGLE, Boolean.TRUE);
    command.setAttribute(MassKeyCommand.REPORT_FORMAT, "");

    final MultiActionButton button = new MultiActionButton();
    insertComponent(button, getMainMap());
    button.setAttribute(MultiActionButton.BUTTON_TEXT, "");
    button.setAttribute(MultiActionButton.TOOLTIP, "Clear combat status flags.");
    button.setAttribute(MultiActionButton.ICON, StateFlag.COMBAT.getStatusIconName());
    button.setAttribute(MultiActionButton.HOTKEY, KeyStroke.getKeyStroke('C', InputEvent.CTRL_DOWN_MASK));
    button.setAttribute(MultiActionButton.MENU_ITEMS, StringArrayConfigurer.arrayToString(new String[] {"Attacked", "Defended"}));
  }

  protected void writeInfoPagesToArchive(GameModule gameModule) throws IOException {
    if (infoPageName != null && !infoPageName.equals("")) {
      final ChartWindow charts = new ChartWindow();
      insertComponent(charts, gameModule);
      charts.setAttribute(ChartWindow.NAME, CHARTS);
      charts.setAttribute(ChartWindow.BUTTON_TEXT, CHARTS);
      charts.setAttribute(ChartWindow.TOOLTIP, CHARTS);
      charts.setAttribute(ChartWindow.HOTKEY, NamedKeyStroke.of(KeyStroke.getKeyStroke('C', InputEvent.CTRL_DOWN_MASK)));

      final TabWidget tab = new TabWidget();
      insertComponent(tab, charts);

      for (int i = 0; i < infoPages.length; ++i) {
        File f = action.getCaseInsensitiveFile(new File(forceExtension(infoPageName, "b" + i)), file, false, null);
        if (f == null) {
          f = action.getCaseInsensitiveFile(new File(forceExtension(infoPageName, "t" + i)), file, false, null);
        }
        if (f != null) {
          final boolean isChart = Character.toLowerCase(getExtension(f.getName()).charAt(0)) == 'b';
          final Widget w;
          if (isChart) {
            w = new Chart();
            insertComponent(w, tab);
            w.setAttribute(Chart.NAME, infoPages[i]);
            gameModule.getArchiveWriter().addImage(f.getPath(), f.getName());
            w.setAttribute(Chart.FILE, f);
          }
          else {
            w = new HtmlChart();
            insertComponent(w, tab);
            w.setAttribute(HtmlChart.NAME, infoPages[i]);

            final StringBuilder sb = new StringBuilder();
            sb.append("<html><body>");

            try (BufferedReader input = Files.newBufferedReader(f.toPath(), StandardCharsets.US_ASCII)) {
              String line;
              do {
                line = input.readLine();
                if (StringUtils.isNotEmpty(line)) {
                  line = line
                    .replaceAll(" (?: )", "&nbsp;")
                    .replaceAll("(?<=&nbsp;) ", "&nbsp;")
                    .replaceFirst("^ ", "&nbsp;");
                  sb.append("<p>").append(line).append("</p>");
                }
              } while (line != null);

              sb.append("</body></html>");
              gameModule.getArchiveWriter().addFile(f.getName(), sb.toString().getBytes(StandardCharsets.UTF_8));
              w.setAttribute(HtmlChart.FILE, f.getName());
            }
          }
          tab.propertyChange(new PropertyChangeEvent(w, Configurable.NAME_PROPERTY, "", infoPages[i]));
        }
      }
    }
  }

  protected void configureMapLayers() {
    // add game piece layers to map
    final LayeredPieceCollection layer = getLayeredPieceCollection();
    String order = layer.getAttributeValueString(LayeredPieceCollection.LAYER_ORDER);
    if (order.equals("")) {
      order = "0,1";
    }
    else {
      order = order + ",0,1";
    }
    layer.setAttribute(LayeredPieceCollection.LAYER_ORDER, order);
  }

  protected void configureTurnCounter(GameModule gameModule) {
    final TurnTracker tracker = new TurnTracker();
    insertComponent(tracker, gameModule);
    tracker.setAttribute(TurnTracker.TURN_FORMAT, "$level1$");
    final ListTurnLevel list = new ListTurnLevel();
    insertComponent(list, tracker);
    list.setAttribute("property", "currentTurn");
    final String[] names = new String[turnNames.size()];
    list.setAttribute("list", StringArrayConfigurer.arrayToString(turnNames.toArray(names)));
    // TODO: set current turn
  }

  @SuppressWarnings("removal")
  protected void configureDiceRoller(GameModule gameModule) {
    final DiceButton dice = new DiceButton();
    insertComponent(dice, gameModule);
    dice.setAttribute(DiceButton.NAME, "Roll");
    dice.setAttribute(DiceButton.PROMPT_ALWAYS, Boolean.TRUE);
    dice.setAttribute(DiceButton.TOOLTIP, "Roll the dice");
    dice.setAttribute(DiceButton.BUTTON_TEXT, "Roll");
    dice.setAttribute(DiceButton.REPORT_FORMAT, "** $name$ $nDice$d$nSides$ (+$plus$ each) = $result$ *** &lt;$playerName$&gt;");
  }

  protected void configureMouseOverStackViewer(GameModule gameModule) {
    final CounterDetailViewer viewer = gameModule.getAllDescendantComponentsOf(CounterDetailViewer.class).iterator().next();
    viewer.setAttribute(CounterDetailViewer.DISPLAY, CounterDetailViewer.FILTER);
    viewer.setAttribute(CounterDetailViewer.PROPERTY_FILTER, ADC2Utils.TYPE + " = " + PIECE);

    final StringBuilder sb = new StringBuilder();
    int mask = 0x1;
    for (final String classValue : classValues) {
      if (classValue != null && !classValue.equals("")) {
        if ((classCombatSummaryValues & mask) > 0) {
          if (sb.length() > 0)
            sb.append('-');
          sb.append("$sum(").append(classValue).append(")$");
        }
      }
      mask <<= 1;
    }
    mask = 0x1;
    for (final String pieceValue : pieceValues) {
      if (pieceValue != null && !pieceValue.equals("")) {
        if ((pieceCombatSummaryValues & mask) > 0) {
          if (sb.length() > 0)
            sb.append('-');
          sb.append("$sum(").append(pieceValue).append(")$");
        }
      }
      mask <<= 1;
    }
    viewer.setAttribute(CounterDetailViewer.SHOW_TEXT, Boolean.TRUE);
    if (sb.length() > 0)
      sb.append(' ');
    viewer.setAttribute(CounterDetailViewer.MINIMUM_DISPLAYABLE, "1");
    viewer.setAttribute(CounterDetailViewer.SUMMARY_REPORT_FORMAT, sb + "($LocationName$)");
    if (usePieceNames) {
      viewer.setAttribute(CounterDetailViewer.COUNTER_REPORT_FORMAT, PC_NAME);
    }
    viewer.setAttribute(CounterDetailViewer.UNROTATE_PIECES, Boolean.TRUE);
    viewer.setAttribute(CounterDetailViewer.BG_COLOR, Color.WHITE);
  }

  protected void writeClassesToArchive(GameModule gameModule) throws IOException {
    pieceWin.setAttribute(PieceWindow.NAME, ADD_NEW_PIECES);

    final ListWidget list = new ListWidget();
    insertComponent(list, pieceWin);

    for (final PieceClass c : pieceClasses) {
      c.writeToArchive(list);
    }
  }

  protected void writePlayersToArchive(GameModule gameModule) {
    final PlayerRoster roster = gameModule.getAllDescendantComponentsOf(PlayerRoster.class).iterator().next();
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final Player player : players) {
      if (player.allies.first() == player) // only write out if it's the first in an alliance
        se.append(player.getName());
    }
    for (int i = 0; i < 2; ++i)
      roster.setAttribute(PlayerRoster.SIDES, se.getValue());
  }

  // TODO make a select all cards in hand option
  protected void writeHandsToArchive(GameModule module) throws IOException {
    final int nHands = forcePools.count(HandPool.class);
    if (nHands == 0)
      return;
    for (final Iterator<Pool> iter = forcePools.iterator(HandPool.class); iter.hasNext(); ) {
      final HandPool pool = (HandPool) iter.next();
      final PlayerHand hand = new PlayerHand();
      insertComponent(hand, module);
      if (pool.getOwner() == Player.ALL_PLAYERS) {
        final String[] sides = new String[players.size()];
        for (int i = 0; i < players.size(); ++i) {
          sides[i] = players.get(i).getName();
        }
        hand.setAttribute(PrivateMap.SIDE, StringArrayConfigurer.arrayToString(sides));
      }
      else {
        hand.setAttribute(PrivateMap.SIDE, pool.getOwner().getName());
      }
      hand.setAttribute(PrivateMap.VISIBLE, Boolean.TRUE);
      hand.setAttribute(PrivateMap.NAME, pool.name);
      hand.setAttribute(PrivateMap.MARK_MOVED, GlobalOptions.NEVER);
      hand.setAttribute(PrivateMap.USE_LAUNCH_BUTTON, Boolean.TRUE);
      hand.setAttribute(PrivateMap.BUTTON_NAME, pool.getButtonName());

      final BoardPicker picker = hand.getBoardPicker();
      final Board board = new Board();
      insertComponent(board, picker);
      board.setConfigureName(pool.name);

      final List<Piece> s = pool.getPieces();
      if (!pieces.isEmpty()) {
        final SetupStack stack = new SetupStack();
        insertComponent(stack, hand);

        final Dimension d = getMaxDeckSize();
        final Point p = new Point(d.width / 2 + 10, d.height / 2 + 10);
        stack.setAttribute(SetupStack.NAME, pool.name);
        stack.setAttribute(SetupStack.OWNING_BOARD, board.getConfigureName());
        stack.setAttribute(SetupStack.X_POSITION, Integer.toString(p.x));
        stack.setAttribute(SetupStack.Y_POSITION, Integer.toString(p.y));
        for (final Piece pc : s) {
          pc.writeToArchive(stack);
        }
      }
    }
  }

  protected void writeDecksToArchive(GameModule gameModule) throws IOException {
    final int nDecks = forcePools.count(DeckPool.class);
    if (nDecks == 0)
      return;

    final Map deckMap = new Map();
    insertComponent(deckMap, gameModule);

    deckMap.setMapName(DECKS);
    deckMap.setAttribute(Map.MARK_MOVED, GlobalOptions.NEVER);
    deckMap.setAttribute(Map.USE_LAUNCH_BUTTON, Boolean.TRUE);
    deckMap.setAttribute(Map.BUTTON_NAME, DECKS);
    deckMap.setAttribute(Map.HOTKEY, NamedKeyStroke.of(KeyStroke.getKeyStroke('D', InputEvent.CTRL_DOWN_MASK)));

    final BoardPicker boardPicker = deckMap.getBoardPicker();

    // write force pool board
    final Dimension maxSize = getMaxDeckSize();
    final boolean vertical = maxSize.width > maxSize.height;
    final JPanel panel = new JPanel();
    final JPanel[] deckPanels = new JPanel[nDecks];
    final GridBagConstraints c = new GridBagConstraints();
    c.insets = new Insets(5, 5, 5, 5);
    c.fill = GridBagConstraints.BOTH;
    c.anchor = GridBagConstraints.CENTER;
    panel.setLayout(new GridBagLayout());
    panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    Iterator<Pool> iter = forcePools.iterator(DeckPool.class);
    for (int i = 0; i < nDecks; ++i) {
      final Pool pool = iter.next();
      deckPanels[i] = new JPanel();
      deckPanels[i].setPreferredSize(maxSize);
      deckPanels[i].setMaximumSize(maxSize);
      deckPanels[i].setBorder(BorderFactory.createLoweredBevelBorder());
      if (vertical) {
        c.gridy = i * 2;
        c.gridx = 1;
      }
      else {
        c.gridy = 1;
        c.gridx = i;
      }
      c.insets.bottom = 2;
      c.insets.top = 5;
      panel.add(deckPanels[i], c);
      final String name;
      if (((Cards) pool).getOwner() == Player.ALL_PLAYERS) {
        name = pool.name;
      }
      else if (((Cards) pool).getOwner() == Player.NO_PLAYERS) {
        name = pool.name;
      }
      else {
        name = pool.name + " (" + ((Cards) pool).getOwner().getName() + ")";
      }
      final JLabel label = new JLabel(name);
      label.setHorizontalAlignment(SwingConstants.CENTER);
      c.gridy += 1;
      c.insets.top = 2;
      c.insets.bottom = 5;
      panel.add(label, c);
    }
    final Dimension d = panel.getPreferredSize();
    panel.setSize(d);
    panel.doLayout();
    final BufferedImage poolImage =
      new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_RGB);
    final Graphics2D g = poolImage.createGraphics();
    panel.printAll(g);

    // write the map image
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    ImageIO.write(poolImage, "png", out);
    final byte[] imageDataArray = out.toByteArray();
    final String deckImageName = "decks.png";
    gameModule.getArchiveWriter().addImage(deckImageName, imageDataArray);

    final Board board = new Board();
    insertComponent(board, boardPicker);
    board.setConfigureName(DECKS);
    board.setAttribute(Board.IMAGE, deckImageName);

    // create decks
    final Rectangle rv = new Rectangle();
    iter = forcePools.iterator(DeckPool.class);
    for (int i = 0; i < nDecks; ++i) {
      final Pool pool = iter.next();
      final DrawPile pile = new DrawPile();
      insertComponent(pile, deckMap);

      final JPanel p = deckPanels[i];
      p.getBounds(rv);
      pile.setAttribute(DrawPile.OWNING_BOARD, DECKS);
      pile.setAttribute(DrawPile.X_POSITION, rv.x + rv.width / 2);
      pile.setAttribute(DrawPile.Y_POSITION, rv.y + rv.height / 2);
      pile.setAttribute(DrawPile.WIDTH, rv.width);
      pile.setAttribute(DrawPile.HEIGHT, rv.height);
      pile.setAttribute(DrawPile.FACE_DOWN, Deck.ALWAYS);
      pile.setAttribute(DrawPile.DRAW_FACE_UP, Boolean.FALSE);
      pile.setAttribute(DrawPile.SHUFFLE, Deck.NEVER);
      pile.setAttribute(DrawPile.REVERSIBLE, Boolean.FALSE);
      pile.setAttribute(DrawPile.ALLOW_MULTIPLE, Boolean.TRUE);
      pile.setAttribute(DrawPile.ALLOW_SELECT, ((Cards) pool).getOwner() == Player.ALL_PLAYERS);
      pile.setAttribute(DrawPile.RESHUFFLABLE, Boolean.FALSE);
      pile.setAttribute(DrawPile.NAME, pool.name);
      pile.setAttribute(DrawPile.SHUFFLE, DrawPile.USE_MENU);
      pile.setAttribute(DrawPile.SHUFFLE_REPORT_FORMAT, "$playerName$ reshuffles $deckName$");
      pile.setAttribute(DrawPile.SHUFFLE_HOTKEY, NamedKeyStroke.of(KeyStroke.getKeyStroke('S', InputEvent.CTRL_DOWN_MASK)));

      for (final Piece pc : pool.getPieces()) {
        pc.writeToArchive(pile);
      }
    }
  }

  @SuppressWarnings("removal")
  protected void writeToolbarMenuToArchive(GameModule gameModule) {
    final int nHands = forcePools.count(HandPool.class);
    if (nHands == 0)
      return;
    final ToolbarMenu menu = new ToolbarMenu();
    insertComponent(menu, gameModule);
    menu.setAttribute(ToolbarMenu.BUTTON_TEXT, "Windows");
    menu.setAttribute(ToolbarMenu.TOOLTIP, "Open trays, decks, charts, and hands.");
    final String[] items = new String[nHands + 3];
    items[0] = TRAY;
    items[1] = DECKS;
    int start = 2;
    if (infoPageName != null) {
      items[2] = CHARTS;
      start = 3;
    }
    final Iterator<Pool> iter = forcePools.iterator(HandPool.class);
    for (int i = 0; i < nHands; ++i) {
      items[i + start] = iter.next().getButtonName();
    }
    menu.setAttribute(ToolbarMenu.MENU_ITEMS, StringArrayConfigurer.arrayToString(items));
  }

  private Dimension getMaxDeckSize() throws IOException {
    final Dimension d = new Dimension(0, 0);
    for (int i = 0; i < nCardSets; ++i)
      getCardDeck(i).getMaxSize(d);
    return d;
  }

  /**
   * Creates a board with deck stacks in which force pools are kept.
   *
   * @throws IOException
   */
  // TODO: cards should not be accessible if they are invisible. Can still draw
  // invisible cards right now.
  protected void writeForcePoolsToArchive(GameModule gameModule) throws IOException {
    final int nForcePools = forcePools.count(ForcePool.class);
    if (nForcePools == 0)
      return;

    final GameModule module = GameModule.getGameModule();
    final Map forcePoolMap = new Map();
    insertComponent(forcePoolMap, module);

    forcePoolMap.setMapName(TRAY);
    forcePoolMap.setAttribute(Map.MARK_MOVED, GlobalOptions.NEVER);
    forcePoolMap.setAttribute(Map.USE_LAUNCH_BUTTON, Boolean.TRUE);
    forcePoolMap.setAttribute(Map.BUTTON_NAME, TRAY);
    forcePoolMap.setAttribute(Map.HOTKEY, NamedKeyStroke.of(KeyStroke.getKeyStroke('T', InputEvent.CTRL_DOWN_MASK)));

    final BoardPicker boardPicker = forcePoolMap.getBoardPicker();

    // write force pool board
    final Dimension modalSize = getSet().getModalSize();
    modalSize.height = modalSize.height * 3 / 2;
    modalSize.width = modalSize.width * 3 / 2;
    final JPanel panel = new JPanel();
    final JPanel[] deckPanels = new JPanel[nForcePools];
    final GridBagConstraints c = new GridBagConstraints();
    c.insets = new Insets(5, 5, 5, 5);
    c.fill = GridBagConstraints.BOTH;
    c.anchor = GridBagConstraints.CENTER;
    panel.setLayout(new GridBagLayout());
    panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    final int nColumns = (int) Math.sqrt(nForcePools);
    Iterator<Pool> iter = forcePools.iterator(ForcePool.class);
    for (int i = 0; i < nForcePools; ++i) {
      final ForcePool fp = (ForcePool) iter.next();
      deckPanels[i] = new JPanel();
      deckPanels[i].setBorder(BorderFactory.createLoweredBevelBorder());
      c.gridy = (i / nColumns) * 2;
      c.gridx = i % nColumns;
      c.insets.bottom = 2;
      c.insets.top = 5;
      panel.add(deckPanels[i], c);
      final JLabel label = new JLabel(fp.name);
      label.setHorizontalAlignment(SwingConstants.CENTER);
      c.gridy += 1;
      c.insets.top = 2;
      c.insets.bottom = 5;
      panel.add(label, c);
      final Dimension d = label.getPreferredSize();
      if (d.width > modalSize.width)
        modalSize.width = d.width;
    }

    for (final JPanel p : deckPanels)
      p.setPreferredSize(modalSize);

    final Dimension d = panel.getPreferredSize();
    panel.setSize(d);
    panel.doLayout();
    final BufferedImage forcePool =
      new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_RGB);
    final Graphics2D g = forcePool.createGraphics();
    panel.printAll(g);

    // write the map image
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    ImageIO.write(forcePool, "png", out);
    final byte[] imageDataArray = out.toByteArray();
    module.getArchiveWriter().addImage(FORCE_POOL_PNG, imageDataArray);

    final Board board = new Board();
    insertComponent(board, boardPicker);
    board.setConfigureName(TRAY);
    board.setAttribute(Board.IMAGE, FORCE_POOL_PNG);

    // create decks
    final Rectangle rv = new Rectangle();
    iter = forcePools.iterator(ForcePool.class);
    for (int i = 0; i < nForcePools; ++i) {
      final ForcePool fp = (ForcePool) iter.next();
      final DrawPile pile = new DrawPile();
      insertComponent(pile, forcePoolMap);

      final JPanel p = deckPanels[i];
      p.getBounds(rv);
      pile.setAttribute(DrawPile.OWNING_BOARD, TRAY);
      pile.setAttribute(DrawPile.X_POSITION, rv.x + rv.width / 2);
      pile.setAttribute(DrawPile.Y_POSITION, rv.y + rv.height / 2);
      pile.setAttribute(DrawPile.WIDTH, rv.width);
      pile.setAttribute(DrawPile.HEIGHT, rv.height);
      pile.setAttribute(DrawPile.FACE_DOWN, Deck.NEVER);
      pile.setAttribute(DrawPile.DRAW_FACE_UP, Boolean.TRUE);
      pile.setAttribute(DrawPile.SHUFFLE, Deck.NEVER);
      pile.setAttribute(DrawPile.REVERSIBLE, Boolean.FALSE);
      pile.setAttribute(DrawPile.ALLOW_MULTIPLE, Boolean.FALSE);
      pile.setAttribute(DrawPile.RESHUFFLABLE, Boolean.FALSE);
      pile.setAttribute(DrawPile.NAME, fp.name);

      for (final Piece pc : fp.getPieces()) {
        pc.writeToArchive(pile);
      }
    }
  }

  // add option to show only top piece just like decks.
  protected void writeSetupStacksToArchive(GameModule gameModule)
    throws IOException {
    final Map mainMap = getMainMap();

    final Point offset = getMap().getCenterOffset();
    for (final java.util.Map.Entry<Integer, List<Piece>> en : stacks.entrySet()) {
      final int hex = en.getKey();
      final Point p = getMap().indexToPosition(hex);
      if (p == null) continue;

      final SetupStack stack = new SetupStack();
      insertComponent(stack, mainMap);

      p.translate(offset.x, offset.y);
      final String location = mainMap.locationName(p);
      stack.setAttribute(SetupStack.NAME, location);
      final Board board = getMap().getBoard();
      stack.setAttribute(SetupStack.OWNING_BOARD, board.getConfigureName());

      final MapGrid mg = board.getGrid();
      Zone z = null;
      if (mg instanceof ZonedGrid) {
        z = ((ZonedGrid) mg).findZone(p);
      }
      stack.setAttribute(SetupStack.X_POSITION, Integer.toString(p.x));
      stack.setAttribute(SetupStack.Y_POSITION, Integer.toString(p.y));
      if (z != null) {
        try {
          if (mg.getLocation(location) != null) {
            if ((!mg.locationName(mg.getLocation(location)).equals(location))) {
              throw new AssertionError("Bad location");
            }
            stack.setAttribute(SetupStack.USE_GRID_LOCATION, true);
            stack.setAttribute(SetupStack.LOCATION, location);
          }
        }
        catch (BadCoords e) {
          log.error("Error while writing setup stacks", e);
        }
      }
      for (final Piece pc : en.getValue()) {
        pc.writeToArchive(stack);
      }
    }
  }

  protected MapBoard getMap() {
    return map;
  }

  protected SymbolSet getSet() {
    return getMap().getSet();
  }

  @Override
  public boolean isValidImportFile(File f) throws IOException {
    try (InputStream fin = Files.newInputStream(f.toPath());
         DataInputStream in = new DataInputStream(fin)) {
      final int header = in.readByte();
      return header == -3 || header == -2;
    }
  }
}
