/*
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
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
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
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.turn.ListTurnLevel;
import VASSAL.build.module.turn.TurnTracker;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.PieceSlot;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.Delete;
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
import VASSAL.tools.ExtensionFileFilter;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imports.FileFormatException;
import VASSAL.tools.imports.Importer;
import VASSAL.tools.imports.adc2.SymbolSet.SymbolData;

public class ADC2Module extends Importer {
	
	private static final String TRAY = "Tray";
	private static final String FORCE_POOL_PNG = "forcePool.png";
	private static final String FORCE_POOL = "Force Pool";
	private static final String DECKS = "Decks";

	protected class ForcePoolList extends ArrayList<Pool> {
		private static final long serialVersionUID = 1L;	
		
		private class ForcePoolIterator implements Iterator<Pool> {			
			private final Class type;
			private int cursor = 0;

			private ForcePoolIterator(Class type) {
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
			
			public boolean hasNext() {
				return cursor < size();
			}

			public Pool next() {
				Pool p = get(cursor);
				++cursor;
				setNext();
				return p;
			}

			public void remove() {
				throw new IllegalStateException();
			}			
		}
		
		public int count(Class type) {
			int size = 0;
			Iterator<Pool> iter = iterator(type);
			while (iter.hasNext()) {
				Pool p = iter.next();
				if (p.getClass() == type && p.isUseable())
					++size;
			}
			return size;
		}
		
		public Iterator<ADC2Module.Pool> iterator(Class type) {
			return new ForcePoolIterator(type);
		}
	}

	public class Pool {
		public final String name;
		public final ArrayList<Piece> pieces;
		
		Pool(String name, ArrayList<Piece> pieces) {
			this.name = name;
			this.pieces = pieces;			
		}
		
		@SuppressWarnings("unchecked")
		List<Piece> getPieces() {
			if (pieces == null)
				return Collections.EMPTY_LIST;
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
		
		Cards(String name, ArrayList<Piece> pieces) {
			super(name, pieces);
			// cull non-cards
			if (pieces != null) {
				for (Iterator<Piece> iter = pieces.iterator(); iter.hasNext(); ) {
					if (!iter.next().isCard()) {
						iter.remove();
					}
				}
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
		DeckPool(String name, ArrayList<Piece> pieces) {
			super(name, pieces);
		}
		
	}
	
	public class HandPool extends Cards {
		HandPool(String name, ArrayList<Piece> pieces) {
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

		ForcePool(String name, ArrayList<Piece> pieces) {
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
		private final int color;
		private final int position;
		private final int size;

		protected StatusDots(int type, int show, int color, int position, int size) {
			this.type = type;
			this.show = show;
			this.color = color;
			this.position = position;
			this.size = size;
		}

		public int getColor() {
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
				return classValues[type >> 4];
			else if (getType() == PIECE_VALUE)
				return pieceValues[type >> 4];
			else
				return null;
		}

	}

	private static final int FORCE_POOL_BLOCK_END = 30000;
	public static final String DRAW_ON_TOP_OF_OTHERS = "Draw on top of others?";
	public static final String PIECE = "Pieces";

	public static final double[] FACING_ANGLES = new double[46];
	
	static {
		for (int i = 0; i < 3; ++i) {
			FACING_ANGLES[i+1] = -i*90.0;
			FACING_ANGLES[i+5] = -(i*90.0 + 45.0);
		}
		
		for (int i = 0; i < 6; ++i) {
			FACING_ANGLES[i+10] = -i*60.0;
			FACING_ANGLES[i+20] = -((i*60.0 - 15.0) % 360.0);
			FACING_ANGLES[i+30] = -(i*60.0 + 30.0);
			FACING_ANGLES[i+40] = -(i*60.0 + 15.0);
		}
	}
	
	private final HashSet<String> uniquePieceNames = new HashSet<String>();
	
	public class Piece {
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
			assert(hidden != null);
			this.hideState = hidden;
			this.facing = facing;
			
			final HashMap<Integer, ArrayList<Piece>> hash;
			if (inForcePool())
				hash = forcePoolHashMap;
			else
				hash = stacks;
			
			ArrayList<Piece> stack = hash.get(position);
			if (stack == null) {
				stack = new ArrayList<Piece>();
				stack.add(this);
				hash.put(position, stack);
			} else {
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
			if (getName().equals(((Piece) obj).getName()) && pieceClass == ((Piece) obj).pieceClass)
				return true;
			else
				return false;
		}

		protected void setValue(int index, int value) {
			values[index] = value;
			types[index] = ValueType.NUMERIC;
		}
		
		protected void writeToArchive(SetupStack parent) throws IOException {
			final GamePiece gp = getGamePiece();
			if (gp == null)
				return;
			assert(pieceSlot == null);
			pieceSlot = new PieceSlot(gp);
			InsertComponent(pieceSlot, parent);
		}
		
		protected void writeToArchive(DrawPile parent) throws IOException {
			GamePiece gp = getGamePiece();
			if (gp == null)
				return;
			assert(pieceSlot == null);
			pieceSlot = new CardSlot();
			pieceSlot.setPiece(gp);
			InsertComponent(pieceSlot, parent);
		}

		public Player getPlayer() {
			return pieceClass.getOwner();
		}
		
		public boolean inForcePool() {
			return (flags & 0x8) > 0;
		}
		
		protected void setReplace() {
			if (pieceClass.getFlipClass() == null)
				return;

			// get tree configure path
			// TODO: find a different way to do this so that we don't have to generate unique class names.
			PieceWindow pw = GameModule.getGameModule().getAllDescendantComponentsOf(PieceWindow.class).iterator().next();
			ListWidget lw = pw.getAllDescendantComponentsOf(ListWidget.class).iterator().next();
			Piece p = pieceClass.getFlipClass().getDefaultPiece();			
			PieceSlot ps = p.getPieceSlot();
			SequenceEncoder se2 = new SequenceEncoder(pw.getClass().getName(), ':');
			if (pw.getConfigureName() != null)
				se2.append(pw.getConfigureName());
			SequenceEncoder se = new SequenceEncoder(se2.getValue(), '/');
			se2 = new SequenceEncoder(lw.getClass().getName(), ':');
			if (lw.getConfigureName() != null)
				se2.append(lw.getConfigureName());
			se.append(se2.getValue());
			se2 = new SequenceEncoder(ps.getClass().getName(), ':');
			if (ps.getConfigureName() != null)
				se2.append(ps.getConfigureName());
			se.append(se2.getValue());
			String path = se.getValue();
			
			// find insertion point
			GamePiece outer;
			if (gamePiece.getClass() == Hideable.class || gamePiece.getClass() == Obscurable.class) {
				outer = gamePiece; 
				gamePiece = ((Decorator) gamePiece).getInner();
			}
			else
				outer = null;
			
			// set replacement
			se = new SequenceEncoder(path, ';');
			se.append("null").append(0).append(0).append(true);
			gamePiece = new Replace(Replace.ID + "Flip;F;" + se.getValue(), gamePiece);
			if (outer != null) {
				((Decorator) outer).setInner(gamePiece);
				gamePiece = outer;
			}
			getPieceSlot().setPiece(gamePiece);
		}
		
		protected GamePiece getGamePiece() throws IOException {
			
			if (gamePiece == null) {
				gamePiece = getBasicPiece();
				if (gamePiece == null)
					return null;
				// TODO: implement a YES_NO field type for PropertySheets
				// and a stack property viewer.
				// Piece values
				appendDecorator(getPropertySheet());
				appendDecorator(getPieceValueMask());
				appendDecorator(getMarker());
				appendDecorator(getMovementMarkable());
				appendDecorator(getFreeRotator());
				appendDecorator(getUsePrototype());
				appendDecorator(getHidden());
			}
			
			return gamePiece;
		}

		private void appendDecorator(Decorator p) {
			if (p != null) {
				p.setInner(gamePiece);
				gamePiece = p;
			}
		}

		protected Decorator getHidden() throws IOException {
			Decorator p = pieceClass.getHiddenDecorator();
			if (p != null && isHidden()) {
				Player player = pieceClass.getPlayer(this);
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
				Obscurable p = pieceClass.getPieceValueMask();
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
		
		// TODO: provide angle phase offset for FreeRotator
		protected FreeRotator getFreeRotator() {
			FreeRotator p = pieceClass.getFreeRotatorDecorator();
			if (p != null) {
				// for pieces that point from a corner, this will either be completely wrong
				// or completely right.  If we do nothing, it's guaranteed to be completely wrong
				p.setAngle(getFacingAngle());
			}
			return p;
		}

		protected MovementMarkable getMovementMarkable() throws IOException {
			MovementMarkable p = pieceClass.getMovementMarkableDecorator();
			if (p != null) {
				p.setMoved(hasMoved());
			}
			return p;
		}

		 //TODO:  add more math functions to MouseOverStackViewer including min(), max(), and mean().
		//		 and antialiased characters in MouseOverStackViewer
		protected PropertySheet getPropertySheet() {
			PropertySheet p = pieceClass.getPropertySheetDecorator();
			if (p != null) {
				SequenceEncoder se = new SequenceEncoder('~');
				SequenceEncoder state = new SequenceEncoder('~');
				for(int i = 0; i < pieceValues.length; ++i) {
					if (pieceValues[i] != null && !pieceValues[i].equals("")) {
						se.append("0" + pieceValues[i]);
						if (pieceValues[i] != null && !pieceValues[i].equals("")) {
							Object o = getValue(i);
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
				}
				se.append("0" + DRAW_ON_TOP_OF_OTHERS);
				state.append(drawOnTopOfOthers() ? "yes" : "no");
				se = new SequenceEncoder(se.getValue(), ';'); // properties
				se.append("Properties"); // menu name
				se.append('P'); // key
				se.append(0); // commit
				se.append("").append("").append(""); // colour
				p.mySetType(PropertySheet.ID + se.getValue());
				p.mySetState(state.getValue());

				return p;
			}
			else {
				return null;
			}
		}

		protected Marker getMarker() {
			return pieceClass.getMarkerDecorator();
		}

		protected GamePiece getBasicPiece() throws IOException {
			String fileName = pieceClass.getImageName();
			if (fileName == null)
				return null;
			SequenceEncoder se = new SequenceEncoder(BasicPiece.ID, ';');
			se.append("").append("").append(fileName).append(getName());
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

		public String getName() {
			if (name == null)
				return pieceClass.getUniqueName();
			else
				return pieceClass.getUniqueName() + " " + name;
		}
		
		protected void setValue(int index, String value) {
			byte[] b = value.getBytes();
			int result = 0;
			for (int i = 0; i < 4; ++i)
				result = (result<<8) + b[i];
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
			byte[] b = new byte[4];
			int mask = 0x7f000000;
			int length = 0;
			for (int i = 0; i < b.length; ++i) {
				b[i] = (byte) ((values[index] & mask) >> ((3-i)*8));
				if (b[i] == 0)
					break;
				++length;
				mask >>= 8;
			}
			return new String(b, 0, length);
		}
		
		private boolean getValueAsBoolean(int index) {
			return values[index] > 0 ? true : false;
		}
		
		public Object getValue(int index) {
			if (types[index] == null)
				return null;
			switch(types[index]) {
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
			GamePiece gp = getGamePiece();
			if (gp == null)
				return;
			pieceSlot = new PieceSlot(gp);
			InsertComponent(pieceSlot, list);
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
		private final String name;
		private final SymbolSet.SymbolData hiddenSymbol;
		private final int hiddenPieceOptions;

		public Player(String name, SymbolSet.SymbolData hiddenSymbol, int hiddenPieceOptions) {
			this.name = name;
			this.hiddenSymbol = hiddenSymbol;
			// this.searchRange = searchRange > 50 ? 50 : searchRange;
			this.hiddenPieceOptions = hiddenPieceOptions;
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
		
		public SymbolSet.SymbolData getHiddenSymbol() {
			return hiddenSymbol;
		}

		public String getName() {
			return name;
		}
	}

	private HashMap<Integer,SymbolSet> cardDecks = new HashMap<Integer,SymbolSet>();

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
				SymbolSet set = getCardDeck(setIndex);
				symbol = set.getGamePiece(symbolIndex);
			}
			return symbol;
		}

		@Override
		protected void setValue(int index, boolean value) {
			assert(false);
		}

		@Override
		protected void setValue(int index, int value) {
			assert(false);
		}

		@Override
		protected void setValue(int index, String value) {
			assert(false);
		}

		@Override
		public FreeRotator getFreeRotatorDecorator() {
			return null;
		}

		@Override
		public Marker getMarkerDecorator() {
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
				Player player = getPlayer(piece);
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
			Obscurable p;
			SequenceEncoder se = new SequenceEncoder(';');
			se.append(KeyStroke.getKeyStroke('H', InputEvent.CTRL_MASK)); // key command
			se.append(getHiddenSymbol().getFileName()); // hide image
			se.append("Hide Piece"); // menu name
			BufferedImage image = getSymbol().getImage();
			se.append("G" + getHiddenImage(new Dimension(image.getWidth(), image.getHeight()), HiddenFlag.MARKER)); // display style
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
		
		public PieceClass(String name, SymbolSet.SymbolData symbol, int owner, int hiddenSymbol, int facing) {
			this.name = name;
			this.symbol = symbol;
			this.owner = owner;
			this.hiddenSymbol = hiddenSymbol;
			this.facing = facing;
		}

		public String getUniqueName() {
			if (uniqueName == null) {
				uniqueName = getName();
				int index = 0;
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
			return new PropertySheet();
		}

		public UsePrototype getUsePrototypeDecorator() {
			SequenceEncoder se = new SequenceEncoder(UsePrototype.ID.replaceAll(";", ""), ';');
			se.append(COMMON_PROPERTIES);
			UsePrototype p = new UsePrototype();
			p.mySetType(se.getValue());
			return p;
		}

		public Obscurable getPieceValueMask() throws IOException {
			if (getOwner().useHiddenPieces()) {
				SequenceEncoder se = new SequenceEncoder(';');
				se.append(KeyStroke.getKeyStroke('I', InputEvent.CTRL_MASK)); // key command
				se.append(getImageName()); // hide image
				se.append("Hide Info"); // menu name
				BufferedImage image = getSymbol().getImage();
				se.append("G" + getHiddenImage(new Dimension(image.getWidth(), image.getHeight()), HiddenFlag.INFO)); // display style
				if (name == null)
					se.append(getName());
				else
					se.append("Unknown Piece"); // mask name
				se.append("sides:" + getOwner().getName()); // owning player
				Obscurable p = new Obscurable();			
				p.mySetType(Obscurable.ID + se.getValue());
				return p;
			}
			else {
				return null;
			}
		}

		public MovementMarkable getMovementMarkableDecorator() throws IOException {
			SequenceEncoder se = new SequenceEncoder(';');
			BufferedImage img = getSymbol().getImage();
			int xOffset = img.getWidth()/2 + 1;
			int yOffset = -img.getHeight()/2 - 2;
			String movedIcon = "/images/moved.gif";
			se.append(movedIcon).append(xOffset).append(yOffset);
			MovementMarkable p = new MovementMarkable();
			p.mySetType(MovementMarkable.ID + se.getValue());
			return p;
		}

		public Marker getMarkerDecorator() {
			SequenceEncoder se;
			boolean useMarker = false;
			se = new SequenceEncoder(',');
			for (int i = 0; i < classValues.length; ++i) {
				if (classValues[i] != null && !classValues[i].equals("") && getValue(i) != null) {
					se.append(classValues[i]);
					useMarker = true;
				}
			}
			if (useMarker) {
				Marker p = new Marker();
				p.mySetType(Marker.ID + se.getValue());
				for (int i = 0; i < classValues.length; ++i) {
					if (classValues[i] == null || classValues[i].equals(""))
						continue;
					Object o = getValue(i);
					if (o instanceof Boolean)
						p.setProperty(classValues[i], o.equals(Boolean.TRUE) ? "yes" : "no");
					else if (o instanceof String)
						p.setProperty(classValues[i], (String) o);
					else if (o instanceof Integer)
						p.setProperty(classValues[i], o.toString());
				}
				return p;
			}
			else {
				return null;
			}
		}

		public Decorator getHiddenDecorator() throws IOException {
			if (getOwner().useHiddenPieces()) {
				Decorator p;
				String sides;
				if (getOwner() == Player.ALL_PLAYERS || getOwner() == Player.NO_PLAYERS) {
					sides = "side:";
				}
				else {
					sides = "sides:" + getOwner().getName();
				}
				SequenceEncoder se = new SequenceEncoder(';');
				se.append(KeyStroke.getKeyStroke('H', InputEvent.CTRL_MASK)); // key command
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
					BufferedImage image = getSymbol().getImage();
					se.append("G" + getHiddenImage(new Dimension(image.getWidth(), image.getHeight()), HiddenFlag.MARKER)); // display style
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
			int nsides = getMap().getNFaces();
			int nfacings;
			switch (getAllowedFacings()) {
			case NONE:
				return null;
			case FLAT_SIDES:
				nfacings = nsides == 4 ? 4 : 12;
				break;				
			default:
				nfacings = nsides == 4 ? 8 : 24;
			}
			String type = FreeRotator.ID + nfacings + ";];[;Rotate CW;Rotate CCW;;;;";
			FreeRotator p = new FreeRotator();
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
		
		public PieceClass getFlipClass() {
			return flipClass;
		}

		public String getName() {
			return name;
		}
		
		protected void setValue(int index, String value) {
			byte[] b = value.getBytes();
			int result = 0;
			for (int i = 0; i < 4; ++i)
				result = (result<<8) + b[i];
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
			byte[] b = new byte[4];
			int length = 0;
			int mask = 0x7f000000;
			for (int i = 0; i < b.length; ++i) {
				b[i] = (byte) ((values[index] & mask) >> ((3-i)*8));
				if (b[i] == 0)
					break;
				++length;
				mask >>= 8;
			}
			return new String(b, 0, length);
		}
		
		public int getNValues() {
			int total = 0;
			for (ValueType t : types)
				if (t != ValueType.NOT_USED)
					++total;
			return total;
		}
		
		public boolean getValueAsBoolean(int index) {
			return values[index] > 0 ? true : false;
		}
		
		public Object getValue(int index) {
			if (types[index] == null)
				return null;
			switch(types[index]) {
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

		protected SymbolSet.SymbolData getSymbol() throws IOException {
			return symbol;
		}		
	}

	public static final String COMMON_PROPERTIES = "Common Properties";
	private String name;
	private MapBoard map = null;
	@SuppressWarnings("unused")
	private int gameTurn = -1;
	private final ArrayList<PieceClass> pieceClasses = new ArrayList<PieceClass>();
	private final ArrayList<Piece> pieces = new ArrayList<Piece>();
	private final ArrayList<Player> players = new ArrayList<Player>();
	private final HashMap<Integer,ArrayList<Piece>> stacks = new HashMap<Integer,ArrayList<Piece>>();
	private final HashMap<Integer, ArrayList<Piece>> forcePoolHashMap = new HashMap<Integer,ArrayList<Piece>>();
	private ForcePoolList forcePools = new ForcePoolList();
	private final String[] classValues = new String[8];
	private final String[] pieceValues = new String[8];
	private FacingDirection allowedFacings[]; 
	
	protected PieceClass getClassFromIndex(int index) {
		if (index < 0 || index >= pieceClasses.size())
			return null;
		return pieceClasses.get(index);
	}

	protected SymbolSet getCardDeck(int deck) throws IOException {
		SymbolSet set = cardDecks.get(deck);
		if (set == null) {
			File f = action.getCaseInsensitiveFile(new File(deckName + "-c" + (deck+1) + ".set"), file, true, 
					new ExtensionFileFilter(ADC2Utils.SET_DESCRIPTION, new String[] {ADC2Utils.SET_EXTENSION}));
			if (f == null)
				throw new FileNotFoundException("Unable to locate deck symbol set.");
			set = new SymbolSet();
			set.importCardSet(action, f);
			cardDecks.put(deck, set);
		}
		return set;
	}

	private HashMap<HiddenFlag, HashMap<Dimension, String>> hiddenFlagImages;
	private int version;
	private int classCombatSummaryValues;
	private int pieceCombatSummaryValues;
	private final StatusDots[] statusDots = new StatusDots[6];
	private final ArrayList<String> turnNames = new ArrayList<String>();
	private boolean useLOS;
	private String deckName;
	private int nCardSets;
	
	public enum HiddenFlag {
		INFO("i", new Color(0.2f, 1.0f, 1.0f, 0.4f), new Dimension(10, 15)),
		MARKER("H", new Color(1.0f, 1.0f, 0.2f, 0.4f), new Dimension(15, 10));
		
		public final String flag;
		public final Color color;
		public final Dimension size;

		HiddenFlag(String flag, Color c, Dimension d) {
			this.flag = flag;
			this.color = c;
			this.size = d;
		}
	}
	
	protected String getHiddenImage(Dimension d, HiddenFlag flag) throws IOException {
		if (hiddenFlagImages == null)
			hiddenFlagImages = new HashMap<HiddenFlag, HashMap<Dimension,String>>();
		
		HashMap<Dimension,String> map = hiddenFlagImages.get(flag);		
		if (map == null) {
			map = new HashMap<Dimension,String>();
			hiddenFlagImages.put(flag, map);
		}
		
		String imageName = map.get(d);
		if (imageName == null) {
			BufferedImage icon = new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_ARGB);
			Graphics2D g = icon.createGraphics();
			
			g.setColor(flag.color);
			g.fillRect(0, 0, flag.size.width, flag.size.height);
			
			imageName = getUniqueImageFileName(flag.flag + d.width + "x" + d.height);
			map.put(d, imageName);
			
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			ImageIO.write(icon, "png", out);
			byte[] imageDataArray = out.toByteArray();
			GameModule.getGameModule().getArchiveWriter().addImage(imageName, imageDataArray);
		}
		
		return imageName;
	}

	public boolean usePieceValues() {
		for (int i = 0; i < pieceValues.length; ++i) {
			if (pieceValues[i] != null && !pieceValues[i].equals(""))
				return true;
		}
		return false;
	}

	@Override
	protected void load(File f) throws IOException {
		super.load(f);
		
		DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(f)));
		
		name = stripExtension(f.getName());

		int header = in.readByte();
		if (header != -3 && header != -2)
			throw new FileFormatException("Invalid Game Module Header");
		
		// TODO: figure out version-specific formats for older versions.
		version = in.readUnsignedShort();
		
		String s = readWindowsFileName(in);
		String mapFileName = forceExtension(s, "map");
		map = new MapBoard();
		File mapFile = action.getCaseInsensitiveFile(new File(mapFileName), f, true, 
				new ExtensionFileFilter(ADC2Utils.MAP_DESCRIPTION, new String[] {ADC2Utils.MAP_EXTENSION}));
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
		}
		catch(ADC2Utils.NoMoreBlocksException e) { }
	}
	
	protected void readPoolOwnerBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Pool Owner");
		
		Iterator<Pool> iter = forcePools.iterator();
		for (int i = 0; i < forcePools.size(); ++i) {
			Pool p = iter.next();
			int owner = in.readUnsignedByte();
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
		int rightMouseButton = in.readByte();
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
		
		in.read(new byte[18]); // unknown
		/* int maxRange = */ in.readUnsignedShort();
		/* boolean degradeLOS = */ in.readByte();
		/* int pointsToBlockLOS = */ ADC2Utils.readBase250Word(in);
		/* String levelHeight = */ readNullTerminatedString(in, 20);
		/* boolean useSmoothing = */ in.readByte();
		/* int cliffElevation = */ ADC2Utils.readBase250Word(in);
		if (version > 0x0203) {
			/* int hexSize = */ ADC2Utils.readBase250Word(in);
			/* String units = */ in.read(new byte[10]);
		}
		
		int nBlocks = ADC2Utils.readBase250Word(in);
		for (int i = 0; i < nBlocks; ++i) {
			/* int blockPoints = */ ADC2Utils.readBase250Word(in);
			/* int baseElevation = */ ADC2Utils.readBase250Word(in);
			/* int aboveGroundLevel = */ ADC2Utils.readBase250Word(in);
			/* int whenSpotting = */ ADC2Utils.readBase250Word(in);
			/* int whenTarget = */ ADC2Utils.readBase250Word(in);
			/* Color color = */ in.read(new byte[3]);
		}
	}

	protected void readTurnNameBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Turn Names");
		
		int nNames = ADC2Utils.readBase250Word(in);
		boolean terminate = false;
		
		for (int i = 0; i < nNames; ++i) {
			String name = readNullTerminatedString(in, 50);
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
		
		byte[] size = new byte[3];
		for (int i = 0; i < 6; ++i) {
			int type = in.readByte();
			int show = ADC2Utils.readBase250Word(in);
			int color = in.readUnsignedByte();
			int position = in.readByte();
			in.read(size);
			statusDots[i] = new StatusDots(type, show, color, position, size[2]);
		}
	}

	protected void readFlipDefinitionBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Flip Definition");
				
		in.readUnsignedByte(); // unknown byte
		
		final int nFlipDefs = ADC2Utils.readBase250Word(in);
		for (int i = 0; i < nFlipDefs; ++i) {
			int from = ADC2Utils.readBase250Word(in);
			int to = ADC2Utils.readBase250Word(in);
			if (from >= 0 && from < pieceClasses.size() && to >= 0 && to < pieceClasses.size())
				pieceClasses.get(from).setFlipClass(to);
		}
	}

	protected void readSoundSettingBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Sound Settings");
		
		for (int i = 0; i < 3; ++i)
			/* scrollJumpSize[i] = */ in.readUnsignedByte();
		in.read(new byte[3]); // unknown
		/* soundOn = */	in.readUnsignedByte();
	}

	public enum FacingDirection {
		FLAT_SIDES, VERTEX, BOTH, NONE
	}
	
	protected void readFacingBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Facing");
		
		final int nFacing = in.readUnsignedByte();
		allowedFacings = new FacingDirection[nFacing+1];
		allowedFacings[0] = FacingDirection.NONE;
		
		for (int i = 0; i < nFacing; ++i) {
			/* String styleName = */ readNullTerminatedString(in);
			
			int direction = in.readUnsignedByte();
			// one invalid facing struct will invalidate all later ones.
			if (i == 0 || allowedFacings[i] != FacingDirection.NONE) {
				switch (direction) {
				case 2:
					allowedFacings[i+1] = FacingDirection.VERTEX;
					break;
				case 3:
					allowedFacings[i+1] = FacingDirection.BOTH;
					break;
				default:
					allowedFacings[i+1] = FacingDirection.FLAT_SIDES;	
				}
			}
			else {
				allowedFacings[i+1] = FacingDirection.NONE;
			}
			
			// this describes how the arrow is drawn in ADC2
			/* int display = */ in.readUnsignedByte();
			/* int fillColor = */ in.readUnsignedByte();
			/* int outlineColor = */ in.readUnsignedByte();
			// zoom sizes
			in.read(new byte[3]);
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
			for (int j = 0; j < 3; ++j) {// one per zoom level
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
		
		int nBytes;
		if (version > 0x0203)
			nBytes = ADC2Utils.readBase250Integer(in);
		else
			nBytes = ADC2Utils.readBase250Word(in);
		in.read(new byte[nBytes]);
	}

	protected void readPoolBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, FORCE_POOL);
		
		final int nForcePools = ADC2Utils.readBase250Word(in);
		for (int i = 0; i < nForcePools; ++i) {
			String n = readNullTerminatedString(in, 25);
			int type = in.readByte();
			in.read(new byte[2]); // not sure what these do
			int nunits = ADC2Utils.readBase250Word(in); // ignored
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
			in.read(new byte[20]);
			/* int startingZoomLevel = */ in.readByte();
			/* int startingPosition = */ ADC2Utils.readBase250Word(in);
			SymbolSet.SymbolData hiddenSymbol = getSet().getGamePiece(ADC2Utils.readBase250Word(in));
			/* String picture = */ readNullTerminatedString(in);
			
			// we don't do anything with this.
			/* int searchRange = */ in.readUnsignedByte();
			
			int hiddenPieceOptions = in.readUnsignedByte();
			in.readByte(); // padding
			
			if (name.length() > 0) {
				Player player = new Player(name, hiddenSymbol, hiddenPieceOptions);
				players.add(player);
			}
		} while (name.length() > 0);
	}

	protected void readPieceBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, PIECE);
		
		int nPieces = ADC2Utils.readBase250Word(in);
		for (int i = 0; i < nPieces; ++i) {
			String name = readNullTerminatedString(in, 25);
			
			PieceClass cl = getClassFromIndex(ADC2Utils.readBase250Word(in));
			if (cl == null)
				throw new FileFormatException("Invalid Class Index");
			
			// prevent duplication
			if (name.equals(cl.getName()))
				name = "";
			
			int values[] = new int[8];
			for (int j = 0; j < values.length; ++j)
				values[j] = in.readInt();
			
			ValueType types[] = new ValueType[8];
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
			
			HideState hidden;
			switch(in.readUnsignedByte()) {
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
			
			in.read(new byte[2]); // don't know what these do
			
			int position = ADC2Utils.readBase250Word(in);			
			int flags = in.readUnsignedByte();
			
			int facing = in.readUnsignedByte();
			if (facing > FACING_ANGLES.length)
				facing = 0;
			
			Piece p = new Piece(position, name, cl, hidden, flags, facing);			
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
		
		for (int i = 0; i < pieceValues .length; ++i)
			pieceValues[i] = readNullTerminatedString(in, 15);
	}
	
	protected void readClassBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Class");
		
		int nClasses = ADC2Utils.readBase250Word(in);
		
		for (int i = 0; i < nClasses; ++i) {
			int symbolIndex = ADC2Utils.readBase250Word(in);
			String name = readNullTerminatedString(in, 25);
			
			int[] values = new int[8];
			for (int j = 0; j < values.length; ++j)
				values[j] = in.readInt();
			
			boolean isCard = false;
			int setIndex = 0;
			ValueType[] types = new ValueType[8];
			for (int j = 0; j < types.length; ++j) {
				int t = in.readUnsignedByte();
				if (j == 0 && t == 10)
					isCard = true;				
				if (isCard) {
					if (j == 1)
						setIndex = t;
					if (setIndex >= nCardSets)
						nCardSets = setIndex+1;
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
			
			int owner = in.readUnsignedByte();
			int hiddenSymbol = ADC2Utils.readBase250Word(in);
		
			// 0 = not used.
			int facing = in.readUnsignedByte();
			
			PieceClass cl;
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
		gameTurn = ADC2Utils.readBase250Word(in);
	}

	protected void writePrototypesToArchive(GameModule gameModule) {
		PrototypesContainer container = gameModule.getAllDescendantComponentsOf(PrototypesContainer.class).iterator().next();
		PrototypeDefinition def = new PrototypeDefinition();
		InsertComponent(def, container);
		def.setConfigureName(COMMON_PROPERTIES);

		// set common properties
		GamePiece gp = new BasicPiece();
		
		gp = new Delete(Delete.ID + "Delete;D", gp);
		
		if (forcePools.count(ForcePool.class) > 0)
			gp = new ReturnToDeck(ReturnToDeck.ID + "Return to Force Pool;R;;Select Force Pool", gp);
		
		SequenceEncoder se = new SequenceEncoder(';');
		se.append(KeyStroke.getKeyStroke('T', InputEvent.CTRL_MASK))
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
		GameModule gameModule = GameModule.getGameModule();
		gameModule.setAttribute(GameModule.MODULE_NAME, name);

		configureMapLayers();		
		configureMouseOverStackViewer(gameModule);
		writePrototypesToArchive(gameModule);
		getMap().writeToArchive();	
		writeClassesToArchive(gameModule);
		writeForcePoolsToArchive(gameModule);
		writeDecksToArchive(gameModule);
		writeHandsToArchive(gameModule);
		writeToolbarMenuToArchive(gameModule);
		writeSetupStacksToArchive(gameModule);		
		writePlayersToArchive(gameModule);		
		configureDiceRoller(gameModule);
		if (turnNames.size() > 1)  // must have at least two turns
			configureTurnCounter(gameModule);
		if (useLOS)
			InsertComponent(new LOS_Thread(), gameModule);
	}

	protected void configureMapLayers() {
		// add game piece layers to map
		Map map = getMap().getMainMap();
		LayeredPieceCollection layer = new LayeredPieceCollection();
		InsertComponent(layer, map);
		layer.setAttribute(LayeredPieceCollection.PROPERTY_NAME, DRAW_ON_TOP_OF_OTHERS);
		layer.setAttribute(LayeredPieceCollection.LAYER_ORDER, StringArrayConfigurer.arrayToString(new String[] {"no", "yes"}));
	}

	protected void configureTurnCounter(GameModule gameModule) {
		TurnTracker tracker = new TurnTracker();
		InsertComponent(tracker, gameModule);
		tracker.setAttribute(TurnTracker.TURN_FORMAT, "$level1$");
		ListTurnLevel list = new ListTurnLevel();
		InsertComponent(list, tracker);
		list.setAttribute("property", "currentTurn");
		String[] names = new String[turnNames.size()];
		list.setAttribute("list", StringArrayConfigurer.arrayToString(turnNames.toArray(names)));
		// TODO: set current turn
	}

	protected void configureDiceRoller(GameModule gameModule) {
		DiceButton dice = new DiceButton();
		InsertComponent(dice, gameModule);
		dice.setAttribute(DiceButton.NAME, "Roll");
		dice.setAttribute(DiceButton.PROMPT_ALWAYS, Boolean.TRUE);
		dice.setAttribute(DiceButton.TOOLTIP, "Roll the dice");
		dice.setAttribute(DiceButton.BUTTON_TEXT, "Roll");
	}

	protected void configureMouseOverStackViewer(GameModule gameModule) {
		CounterDetailViewer viewer = gameModule.getAllDescendantComponentsOf(CounterDetailViewer.class).iterator().next();
		viewer.setAttribute(CounterDetailViewer.DISPLAY, CounterDetailViewer.FILTER);
		viewer.setAttribute(CounterDetailViewer.PROPERTY_FILTER, ADC2Utils.TYPE + " = " + PIECE);
		
		StringBuilder sb = new StringBuilder();
		int mask = 0x1;
		for (int i = 0; i < classValues.length; ++i) {
			if (classValues[i] != null && !classValues[i].equals("")) {
				if ((classCombatSummaryValues & mask) > 0) {
					if (sb.length() > 0)
						sb.append('-');
					sb.append("$sum(" + classValues[i] + ")$");
				}
			}
			mask <<= 1;
		}
		mask = 0x1;
		for (int i = 0; i < pieceValues.length; ++i) {
			if (pieceValues[i] != null && !pieceValues[i].equals("")) {
				if ((pieceCombatSummaryValues & mask) > 0) {
					if (sb.length() > 0)
						sb.append('-');
					sb.append("$sum(" + pieceValues[i] + ")$");
				}
			}
			mask <<= 1;
		}
		viewer.setAttribute(CounterDetailViewer.SHOW_TEXT, Boolean.TRUE);
		if (sb.length() > 0)
			sb.append(' ');
		viewer.setAttribute(CounterDetailViewer.SUMMARY_REPORT_FORMAT, sb.toString() + "($LocationName$)");
		viewer.setAttribute(CounterDetailViewer.UNROTATE_PIECES, Boolean.TRUE);
		viewer.setAttribute(CounterDetailViewer.BG_COLOR, Color.WHITE);
	}

	protected void writeClassesToArchive(GameModule gameModule) throws IOException {
		PieceWindow win = gameModule.getAllDescendantComponentsOf(PieceWindow.class).iterator().next();

		win.setAttribute(PieceWindow.NAME, "Add New Pieces");
		
		ListWidget list = new ListWidget();
		InsertComponent(list, win);
		
		for (PieceClass c : pieceClasses)
			c.writeToArchive(list);
		
		for (PieceClass c : pieceClasses)
			c.getDefaultPiece().setReplace();
	}

	protected void writePlayersToArchive(GameModule gameModule) {
		final PlayerRoster roster = gameModule.getAllDescendantComponentsOf(PlayerRoster.class).iterator().next();
		final SequenceEncoder se = new SequenceEncoder(',');
		for (Player player : players)
			se.append(player.getName());
		for (int i = 0; i < 2; ++i)
			roster.setAttribute(PlayerRoster.SIDES, se.getValue());		
	}

	// TODO make a select all cards in hand option
	protected void writeHandsToArchive(GameModule module) throws IOException {
		final int nHands = forcePools.count(HandPool.class);
		if (nHands == 0)
			return;		
		for (Iterator<Pool> iter = forcePools.iterator(HandPool.class); iter.hasNext(); ) {
			HandPool pool = (HandPool) iter.next();
			PlayerHand hand = new PlayerHand();
			InsertComponent(hand, module);
			if (pool.getOwner() == Player.ALL_PLAYERS) {
				String sides[] = new String[players.size()];
				for (int i = 0; i < players.size(); ++i) {
					sides[i] = players.get(i).getName();
				}
				hand.setAttribute(PrivateMap.SIDE, StringArrayConfigurer.arrayToString(sides));
			}
			else {
				hand.setAttribute(PrivateMap.SIDE, pool.getOwner().getName());
			}
			hand.setAttribute(PrivateMap.VISIBLE, Boolean.TRUE);
			hand.setAttribute(PrivateMap.MAP_NAME, pool.name);
			hand.setAttribute(PrivateMap.MARK_MOVED, GlobalOptions.NEVER);
			hand.setAttribute(PrivateMap.USE_LAUNCH_BUTTON, Boolean.TRUE);
			hand.setAttribute(PrivateMap.BUTTON_NAME, pool.getButtonName());
			
			BoardPicker picker = hand.getBoardPicker();
			final Board board = new Board();
			InsertComponent(board, picker);
			board.setConfigureName(pool.name);

			List<Piece> s = pool.getPieces();
			if (pieces.size() > 0) {
				SetupStack stack = new SetupStack();
				InsertComponent(stack, hand);

				Dimension d = getMaxDeckSize();
				Point p = new Point(d.width/2 + 10, d.height/2 + 10);
				stack.setAttribute(SetupStack.NAME, pool.name);
				stack.setAttribute(SetupStack.OWNING_BOARD, board.getConfigureName());
				stack.setAttribute(SetupStack.X_POSITION, Integer.toString(p.x));
				stack.setAttribute(SetupStack.Y_POSITION, Integer.toString(p.y));
				for (Piece pc : s) {
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
		InsertComponent(deckMap, gameModule);
		
		deckMap.setMapName(DECKS);
		deckMap.setAttribute(Map.MARK_MOVED, GlobalOptions.NEVER);
		deckMap.setAttribute(Map.USE_LAUNCH_BUTTON, Boolean.TRUE);
		deckMap.setAttribute(Map.BUTTON_NAME, DECKS);
		deckMap.setAttribute(Map.HOTKEY, KeyStroke.getKeyStroke('D', InputEvent.CTRL_DOWN_MASK));
		
		final BoardPicker boardPicker = deckMap.getBoardPicker();
		
		// write force pool board
		final Dimension maxSize = getMaxDeckSize();
		boolean vertical = maxSize.width > maxSize.height;
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
			Pool pool = iter.next();
			deckPanels[i] = new JPanel();
			deckPanels[i].setPreferredSize(maxSize);
			deckPanels[i].setMaximumSize(maxSize);
			deckPanels[i].setBorder(BorderFactory.createLoweredBevelBorder());
			if (vertical) {
				c.gridy = i*2;
				c.gridx = 1;
			}
			else {
				c.gridy = 1;
				c.gridx = i;
			}
			c.insets.bottom = 2;
			c.insets.top = 5;
			panel.add(deckPanels[i], c);
			String name;
			if (((Cards) pool).getOwner() == Player.ALL_PLAYERS) {
				name = pool.name;
			}
			else if (((Cards) pool).getOwner() == Player.NO_PLAYERS) {
				name = pool.name;
			}
			else {
				name = pool.name + " (" + ((Cards) pool).getOwner() + ")";
			}
			JLabel label = new JLabel(name);
			label.setHorizontalAlignment(SwingConstants.CENTER);
			c.gridy += 1;
			c.insets.top = 2;
			c.insets.bottom = 5;
			panel.add(label, c);
		}		
		final Dimension d = panel.getPreferredSize();		
		panel.setSize(d);
		panel.doLayout();
		final BufferedImage poolImage = new BufferedImage(d.width, d.height, BufferedImage.TYPE_3BYTE_BGR);
		final Graphics2D g = poolImage.createGraphics();
		panel.printAll(g);
		
		// write the map image
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		ImageIO.write(poolImage, "png", out);
		final byte[] imageDataArray = out.toByteArray();
		final String deckImageName = "decks.png";
		gameModule.getArchiveWriter().addImage(deckImageName, imageDataArray);
		
		final Board board = new Board();
		InsertComponent(board, boardPicker);
		board.setConfigureName(DECKS);
		board.setAttribute(Board.IMAGE, deckImageName);
		
		// create decks
		final Rectangle rv = new Rectangle();
		iter = forcePools.iterator(DeckPool.class);
		for (int i = 0; i < nDecks; ++i) {
			Pool pool = iter.next();
			DrawPile pile = new DrawPile();
			InsertComponent(pile, deckMap);
			
			JPanel p = deckPanels[i];
			p.getBounds(rv);
			pile.setAttribute(DrawPile.OWNING_BOARD, DECKS);
			pile.setAttribute(DrawPile.X_POSITION, rv.x + rv.width/2);
			pile.setAttribute(DrawPile.Y_POSITION, rv.y + rv.height/2);			
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
			pile.setAttribute(DrawPile.SHUFFLE_HOTKEY, KeyStroke.getKeyStroke('S', InputEvent.CTRL_DOWN_MASK));
			
			for (Piece pc : pool.getPieces()) {
				pc.writeToArchive(pile);
			}
		}
	}

	protected void writeToolbarMenuToArchive(GameModule gameModule) {
		final int nHands = forcePools.count(HandPool.class);
		if (nHands == 0)
			return;
		ToolbarMenu menu = new ToolbarMenu();
		InsertComponent(menu, gameModule);
		menu.setAttribute(ToolbarMenu.BUTTON_TEXT, "Windows");
		menu.setAttribute(ToolbarMenu.TOOLTIP, "Open trays, decks, and hands.");
		String items[] = new String[nHands+2];
		items[0] = TRAY;
		items[1] = DECKS;
		Iterator<Pool> iter = forcePools.iterator(HandPool.class);
		for (int i = 0; i < nHands; ++i) {
			items[i+2] = iter.next().getButtonName();
		}
		menu.setAttribute(ToolbarMenu.MENU_ITEMS, StringArrayConfigurer.arrayToString(items));
	}
	
	private Dimension getMaxDeckSize() throws IOException {
		Dimension d = new Dimension(0,0);
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
		int nForcePools = forcePools.count(ForcePool.class);
		if (nForcePools == 0)
			return;
		
		final GameModule module = GameModule.getGameModule();
		final Map forcePoolMap = new Map();
		InsertComponent(forcePoolMap, module);
		
		forcePoolMap.setMapName(TRAY);
		forcePoolMap.setAttribute(Map.MARK_MOVED, GlobalOptions.NEVER);
		forcePoolMap.setAttribute(Map.USE_LAUNCH_BUTTON, Boolean.TRUE);
		forcePoolMap.setAttribute(Map.BUTTON_NAME, TRAY);
		forcePoolMap.setAttribute(Map.HOTKEY, KeyStroke.getKeyStroke('T', InputEvent.CTRL_DOWN_MASK));
		
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
		final int nColumns = (int) Math.sqrt((double) nForcePools);
		Iterator<Pool> iter = forcePools.iterator(ForcePool.class);
		for (int i = 0; i < nForcePools; ++i) {
			ForcePool fp = (ForcePool) iter.next();
			deckPanels[i] = new JPanel();
			deckPanels[i].setBorder(BorderFactory.createLoweredBevelBorder());
			c.gridy = (i/nColumns)*2;
			c.gridx = i%nColumns;
			c.insets.bottom = 2;
			c.insets.top = 5;
			panel.add(deckPanels[i], c);
			JLabel label = new JLabel(fp.name);
			label.setHorizontalAlignment(SwingConstants.CENTER);
			c.gridy += 1;
			c.insets.top = 2;
			c.insets.bottom = 5;
			panel.add(label, c);
			Dimension d = label.getPreferredSize();
			if (d.width > modalSize.width)
				modalSize.width = d.width;
		}
		
		for (JPanel p : deckPanels)
			p.setPreferredSize(modalSize);
		
		final Dimension d = panel.getPreferredSize();		
		panel.setSize(d);
		panel.doLayout();
		final BufferedImage forcePool = new BufferedImage(d.width, d.height, BufferedImage.TYPE_3BYTE_BGR);
		final Graphics2D g = forcePool.createGraphics();
		panel.printAll(g);
		
		// write the map image
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		ImageIO.write(forcePool, "png", out);
		final byte[] imageDataArray = out.toByteArray();
		module.getArchiveWriter().addImage(FORCE_POOL_PNG, imageDataArray);
		
		final Board board = new Board();
		InsertComponent(board, boardPicker);
		board.setConfigureName(TRAY);
		board.setAttribute(Board.IMAGE, FORCE_POOL_PNG);
		
		// create decks
		final Rectangle rv = new Rectangle();
		iter = forcePools.iterator(ForcePool.class);
		for (int i = 0; i < nForcePools; ++i) {
			ForcePool fp = (ForcePool) iter.next();
			DrawPile pile = new DrawPile();
			InsertComponent(pile, forcePoolMap);
			
			JPanel p = deckPanels[i];
			p.getBounds(rv);
			pile.setAttribute(DrawPile.OWNING_BOARD, TRAY);
			pile.setAttribute(DrawPile.X_POSITION, rv.x + rv.width/2);
			pile.setAttribute(DrawPile.Y_POSITION, rv.y + rv.height/2);			
			pile.setAttribute(DrawPile.WIDTH, rv.width);
			pile.setAttribute(DrawPile.HEIGHT, rv.height);
			pile.setAttribute(DrawPile.FACE_DOWN, Deck.NEVER);
			pile.setAttribute(DrawPile.DRAW_FACE_UP, Boolean.TRUE);
			pile.setAttribute(DrawPile.SHUFFLE, Deck.NEVER);
			pile.setAttribute(DrawPile.REVERSIBLE, Boolean.FALSE);
			pile.setAttribute(DrawPile.ALLOW_MULTIPLE, Boolean.FALSE);
			pile.setAttribute(DrawPile.RESHUFFLABLE, Boolean.FALSE);
			pile.setAttribute(DrawPile.NAME, fp.name);
			
			for (Piece pc : fp.getPieces()) {
				pc.writeToArchive(pile);
				pc.setReplace();
			}
		}
	}
	
	protected void writeSetupStacksToArchive(GameModule gameModule) throws IOException {
		final Map mainMap = getMap().getMainMap();
		
		final Point offset = getMap().getCenterOffset();
		for (int hex : stacks.keySet()) {
			Point p = getMap().indexToPosition(hex);
			if (p == null)
				continue;
			ArrayList<Piece> s = stacks.get(hex);
			SetupStack stack = new SetupStack();
			InsertComponent(stack, mainMap);

			p.translate(offset.x, offset.y);
			String location = mainMap.locationName(p);
			stack.setAttribute(SetupStack.NAME, location);
			Board board = getMap().getBoard();
			stack.setAttribute(SetupStack.OWNING_BOARD, board.getConfigureName());
			
			MapGrid mg = board.getGrid();
			Zone z = null;
			if (mg instanceof ZonedGrid)
				z = ((ZonedGrid) mg).findZone(p);				
			stack.setAttribute(SetupStack.X_POSITION, Integer.toString(p.x));
			stack.setAttribute(SetupStack.Y_POSITION, Integer.toString(p.y));
			if (z != null) {
				try {
					if (mg.getLocation(location) != null) {						
						assert(mg.locationName(mg.getLocation(location)).equals(location));
						stack.setAttribute(SetupStack.USE_GRID_LOCATION, true);
						stack.setAttribute(SetupStack.LOCATION, location);
					}
				}
				catch(BadCoords e) {}
			}
			for (Piece pc : s) {
				pc.writeToArchive(stack);
				pc.setReplace();
			}
		}
	}
	
	protected MapBoard getMap() {
		return map;
	}
	
	protected SymbolSet getSet() {
		return getMap().getSet();
	}
}