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
import java.awt.Font;
import java.awt.FontMetrics;
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
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.LayeredPieceCollection;
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
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

public class ADC2Module extends Importer {
	
	private static final int FORCE_POOL_BLOCK_END = 30000;
	private static final String DRAW_ON_TOP_OF_OTHERS = "Draw on top of others?";
	private static final String PIECE = "Pieces";

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
	
	protected class Piece {
		private final int[] values = new int[8];
		private final ValueType[] types = new ValueType[8];
		private final String name;
		private final PieceClass cl;
		private final HideState hidden;
		private final int flags;
		private GamePiece gamePiece;
		private PieceSlot pieceSlot;
		@SuppressWarnings("unused")
		private int facing;
	
		public Piece(PieceClass cl) {
			this.name = null;
			this.cl = cl;
			this.flags = 0;
			this.hidden = null;
		}
		
		public Piece(int position, String name, PieceClass cl, HideState hidden, int flags, int facing) {
			if (name == null || name.equals(""))
				this.name = null;
			else
				this.name = name;
			this.cl = cl;
			this.flags = flags;
			assert(hidden != null);
			this.hidden = hidden;
			this.facing = facing;
			
			final HashMap<Integer, ArrayList<Piece>> hash;
			if (inForcePool())
				hash = forcePools;
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
	
		public boolean isInfoHidden() {
			return hidden == HideState.INFO_HIDDEN;
		}
		
		public boolean isHidden() {
			return hidden == HideState.HIDDEN;
		}
		
		public PieceClass getPieceClass() {
			return cl;
		}
		
		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof Piece))
				return false;
			if (getName().equals(((Piece) obj).getName()) && cl == ((Piece) obj).cl)
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
			pieceSlot.build(null);
			pieceSlot.updateGpId(GameModule.getGameModule());
			pieceSlot.addTo(parent);
			parent.add(pieceSlot);			
		}
		
		protected void writeToArchive(DrawPile parent) throws IOException {
			GamePiece gp = getGamePiece();
			if (gp == null)
				return;
			assert(pieceSlot == null);
			pieceSlot = new CardSlot();
			pieceSlot.setPiece(gp);
			pieceSlot.build(null);
			pieceSlot.updateGpId(GameModule.getGameModule());
			pieceSlot.addTo(parent);
			parent.add(pieceSlot);						
		}

		public Player getPlayer() {
			return cl.getPlayer();
		}
		
		private boolean inForcePool() {
			return (flags & 0x8) > 0;
		}
		
		protected void setReplace() {
			if (getPieceClass().getFlipClass() == null)
				return;

			// get tree configure path
			PieceWindow pw = GameModule.getGameModule().getAllDescendantComponentsOf(PieceWindow.class).iterator().next();
			ListWidget lw = pw.getAllDescendantComponentsOf(ListWidget.class).iterator().next();
			Piece p = getPieceClass().getFlipClass().getDefaultPiece();			
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
		
		GamePiece getGamePiece() throws IOException {
			
			if (gamePiece == null) {
				gamePiece = getBasicPiece();
				if (gamePiece == null)
					return null;
				// TODO: implement a YES_NO field type for PropertySheets
				// and a stack property viewer.
				// Piece values
				gamePiece = addPropertySheet(gamePiece);
				
				if (getPlayer().useHiddenPieces() && usePieceValues())
					gamePiece = addPieceValueMask(gamePiece);
				
				gamePiece = addMarker(gamePiece);				
				gamePiece = addMovementMarkable(gamePiece);
				if (getPieceClass().getAllowedFacings() != FacingDirection.NONE)
					gamePiece = addFreeRotator(gamePiece);		
				gamePiece = addUsePrototype(gamePiece);
				
				if (getPieceClass().getPlayer().useHiddenPieces()) {
					if (getPieceClass().getHiddenSymbol() == null)
						gamePiece = addHideable(gamePiece);
					else
						gamePiece = addPieceMask(gamePiece);
				}
				
			}
			
			return gamePiece;
		}

		private GamePiece addPieceMask(GamePiece gamePiece) throws IOException {
			SequenceEncoder se = new SequenceEncoder(';');
			se.append(KeyStroke.getKeyStroke('H', InputEvent.CTRL_MASK)); // key command
			se.append(getPieceClass().getHiddenSymbol().getFileName()); // hide image
			se.append("Hide Piece"); // menu name
			BufferedImage image = getPieceClass().getSymbol().getImage();
			se.append("G" + getHiddenInfoImage(new Dimension(image.getWidth(), image.getHeight()), HIDDEN_FLAG)); // display style
			se.append("Unknown Piece"); // mask name
			se.append("sides:" + getPieceClass().getPlayer().getName()); // owning player
			gamePiece = new Obscurable(Obscurable.ID + se.getValue(), gamePiece);
			if (isHidden() || inForcePool() && getPieceClass().getPlayer().hiddenInForcePools())
				((Decorator) gamePiece).mySetState(getPieceClass().getPlayer().getName());
			
			return gamePiece;
		}

		private GamePiece addHideable(GamePiece gamePiece) {
			// TODO Add transparency to background color as well as alpha for unit.
		    SequenceEncoder se = new SequenceEncoder(';');
		    se.append(KeyStroke.getKeyStroke('H', InputEvent.CTRL_MASK)); // key command
		    se.append("Hide Piece"); // command
		    se.append(new Color(255, 255, 255)); // background colour
		    se.append("sides:" + getPieceClass().getPlayer().getName()); // controlling player
		    gamePiece = new Hideable(Hideable.ID + se.getValue(), gamePiece);
		    if (isHidden() || inForcePool() && getPieceClass().getPlayer().hiddenInForcePools())
		    	((Hideable) gamePiece).mySetState(getPieceClass().getPlayer().getName());
		    
			return gamePiece;
		}

		private GamePiece addPieceValueMask(GamePiece gamePiece) throws IOException {
			SequenceEncoder se = new SequenceEncoder(';');
			se.append(KeyStroke.getKeyStroke('I', InputEvent.CTRL_MASK)); // key command
			se.append(getPieceClass().getImageName()); // hide image
			se.append("Hide Info"); // menu name
			BufferedImage image = getPieceClass().getSymbol().getImage();
			se.append("G" + getHiddenInfoImage(new Dimension(image.getWidth(), image.getHeight()), HIDDEN_INFO_FLAG)); // display style
			if (name == null)
				se.append(getName());
			else
				se.append("Unknown Piece"); // mask name
			se.append("sides:" + getPieceClass().getPlayer().getName()); // owning player
			gamePiece = new Obscurable(Obscurable.ID + se.getValue(), gamePiece);
			if (isInfoHidden())
				((Decorator) gamePiece).mySetState(getPieceClass().getPlayer().getName());
			
			return gamePiece;
		}

		private GamePiece addUsePrototype(GamePiece gamePiece) {
			SequenceEncoder se = new SequenceEncoder(UsePrototype.ID.replaceAll(";", ""), ';');
			se.append(COMMON_PROPERTIES);
			gamePiece = new UsePrototype(se.getValue(), gamePiece);
			return gamePiece;
		}

		// TODO: provide angle phase offset for FreeRotator
		private GamePiece addFreeRotator(GamePiece gamePiece) {
			int nsides = getMap().getNFaces();
			int nfacings;
			switch (getPieceClass().getAllowedFacings()) {
			case FLAT_SIDES:
				nfacings = nsides == 4 ? 4 : 12;
				break;
			default:
				nfacings = nsides == 4 ? 4 : 24;
			}
			String type = FreeRotator.ID + nfacings + ";];[;Rotate CW;Rotate CCW;;;;";
			gamePiece = new FreeRotator(type, gamePiece);
			((FreeRotator) gamePiece).setAngle(FACING_ANGLES[facing]);
			
			return gamePiece;
		}

		private GamePiece addMovementMarkable(GamePiece gamePiece) {
			SequenceEncoder se;
			se = new SequenceEncoder(';');
			BufferedImage img = getPieceClass().getSymbol().getImage();
			int xOffset = img.getWidth()/2 + 1;
			int yOffset = -img.getHeight()/2 - 2;
			String movedIcon = "/images/moved.gif";
			se.append(movedIcon).append(xOffset).append(yOffset);
			gamePiece = new MovementMarkable(MovementMarkable.ID + se.getValue(), gamePiece);
			((MovementMarkable) gamePiece).setMoved(hasMoved());
			return gamePiece;
		}

		 //TODO:  add more math functions to MouseOverStackViewer including min(), max(), and mean().
		//		 and antialiased characters in MouseOverStackViewer
		private GamePiece addPropertySheet(GamePiece gamePiece) {
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
			gamePiece = new PropertySheet(PropertySheet.ID + se.getValue(), gamePiece);
			((PropertySheet) gamePiece).mySetState(state.getValue());
			
			return gamePiece;
		}

		private GamePiece addMarker(GamePiece gamePiece) {
			PieceClass c = getPieceClass();
			SequenceEncoder se;
			boolean useMarker = false;
			se = new SequenceEncoder(',');
			for (int i = 0; i < classValues.length; ++i) {
				if (classValues[i] != null && !classValues[i].equals("") && c.getValue(i) != null) {
					se.append(classValues[i]);
					useMarker = true;
				}
			}
			if (useMarker) {
				gamePiece = new Marker(Marker.ID + se.getValue(), gamePiece);
				for (int i = 0; i < classValues.length; ++i) {
					if (classValues[i] == null || classValues[i].equals(""))
						continue;
					Object o = c.getValue(i);
					if (o instanceof Boolean)
						gamePiece.setProperty(classValues[i], o.equals(Boolean.TRUE) ? "yes" : "no");
					else if (o instanceof String)
						gamePiece.setProperty(classValues[i], (String) o);
					else if (o instanceof Integer)
						gamePiece.setProperty(classValues[i], o.toString());
				}
			}
			return gamePiece;
		}

		private GamePiece getBasicPiece() throws IOException {
			String fileName = getPieceClass().getImageName();
			if (fileName == null)
				return null;
			SequenceEncoder se = new SequenceEncoder(BasicPiece.ID, ';');
			se.append("").append("").append(fileName).append(getName());
			return new BasicPiece(se.getValue());
		}

		// hasAttacked() and hasDefended() will never be used.
		@SuppressWarnings("unused")
		private boolean hasAttacked() {
			return (flags & 0x1) > 0;
		}
		
		@SuppressWarnings("unused")
		private boolean hasDefended() {
			return !hasAttacked() && (flags & 0x2) > 0;
		}
		
		private boolean hasMoved() {
			return (flags & 0x4) > 0;
		}
		
		private boolean drawOnTopOfOthers() {
			return (flags & 0x10) > 0;
		}

		protected String getName() {
			if (name == null)
				return getPieceClass().getName();
			else
				return getPieceClass().getName() + " " + name;
		}

		void setValue(int index, String value) {
			byte[] b = value.getBytes();
			int result = 0;
			for (int i = 0; i < 4; ++i)
				result = (result<<8) + b[i];
			values[index] = result;
			types[index] = ValueType.TEXT;
		}
		
		void setValue(int index, boolean value) {
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
		
		Object getValue(int index) {
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

		void writeToArchive(ListWidget list) throws IOException {
			GamePiece gp = getGamePiece();
			if (gp == null)
				return;
			pieceSlot = new PieceSlot(gp);
			pieceSlot.build(null);
			pieceSlot.updateGpId(GameModule.getGameModule());
			pieceSlot.addTo(list);
			list.add(pieceSlot);			
		}

		PieceSlot getPieceSlot() {
			return pieceSlot;
		}		
	}

	enum ValueType {
		NOT_USED, NUMERIC, TEXT, YESNO
	}
	
	enum HideState {
		NOT_HIDDEN, INFO_HIDDEN, HIDDEN
	}

	protected static class Player {

		static final Player ALL_PLAYERS = new Player("All Players", null, 0);
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

	/**
	 * A general class for a game piece.  Typically all pieces that appear to be identical blong to the
	 * same class.
	 */
	protected class PieceClass {
		
		private final int[] values = new int[8];
		private final ValueType[] types = new ValueType[8];
		private final String name;
		private final SymbolSet.SymbolData symbol;
		private final int owner;
		private final int hiddenSymbol;
		private final int facing;
		private PieceClass flipClass;
		private Piece defaultPiece;
		
		public PieceClass(String name, SymbolSet.SymbolData symbol, int owner, int hiddenSymbol, int facing) {
			this.name = name;
			this.symbol = symbol;
			this.owner = owner;
			this.hiddenSymbol = hiddenSymbol;
			this.facing = facing;
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

		public SymbolSet.SymbolData getHiddenSymbol() {
			if (hiddenSymbol == 30000)
				return getPlayer().getHiddenSymbol();
			else if (hiddenSymbol == 30001)
				return null;
			else return getSet().getGamePiece(hiddenSymbol);
		}

		public String getImageName() throws IOException {
			if (symbol == null)
				return null;
			else
				return symbol.getFileName();
		}
		
		public Player getPlayer() {
			if (owner >= players.size())
				return Player.ALL_PLAYERS;
			else
				return players.get(owner);			
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

		SymbolSet.SymbolData getSymbol() {
			return symbol;
		}		
	}

	private static final String COMMON_PROPERTIES = "Common Properties";
	private String name;
	private MapBoard map = null;
	@SuppressWarnings("unused")
  private int gameTurn = -1;
	private final ArrayList<PieceClass> pieceClasses = new ArrayList<PieceClass>();
	private final ArrayList<Piece> pieces = new ArrayList<Piece>();
	private final ArrayList<Player> players = new ArrayList<Player>();
	private final HashMap<Integer,ArrayList<Piece>> stacks = new HashMap<Integer,ArrayList<Piece>>();
	private final HashMap<Integer, ArrayList<Piece>> forcePools = new HashMap<Integer,ArrayList<Piece>>();
	private String[] forcePoolNames;
	private final String[] classValues = new String[8];
	private final String[] pieceValues = new String[8];
	private FacingDirection allowedFacings[]; 
	
	protected PieceClass getClassFromIndex(int index) {
		if (index < 0 || index >= pieceClasses.size())
			return null;
		return pieceClasses.get(index);
	}

	private HashMap<String, HashMap<Dimension, String>> hiddenFlagImages;
	private int version;
	private static final String HIDDEN_INFO_FLAG = "i";
	private static final String HIDDEN_FLAG = "";
	private String getHiddenInfoImage(Dimension d, String flag) throws IOException {
		if (hiddenFlagImages == null)
			hiddenFlagImages = new HashMap<String, HashMap<Dimension,String>>();
		
		HashMap<Dimension,String> map = hiddenFlagImages.get(flag);		
		if (map == null) {
			map = new HashMap<Dimension,String>();
			hiddenFlagImages.put(flag, map);
		}
		
		String imageName = map.get(d);
		if (imageName == null) {
			BufferedImage icon = new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_ARGB);
			Graphics2D g = icon.createGraphics();
			
			g.setBackground(new Color(0.0f, 0.0f, 0.0f, 0.0f));
			g.clearRect(0, 0, d.width, d.height);
			
			g.setFont(new Font("Serif", Font.BOLD, 12));
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			FontMetrics fm = g.getFontMetrics();
			
			g.setColor(new Color(1.0f, 1.0f, 0.2f, 0.4f));
			Rectangle2D rect = fm.getStringBounds("M", g);
			Rectangle2D rect2 = fm.getStringBounds(flag, g);
			g.fillRect(0, 0, (int) rect.getWidth() + 2, (int) rect.getHeight() + 2);
			
			g.setColor(new Color(0.8f, 0.0f, 0.0f, 0.4f));
			g.drawString(flag, (int) ((rect.getWidth() - rect2.getWidth())/2.0) + 1, fm.getAscent() + 1);
			
			imageName = getUniqueImageFileName(flag + d.width + "x" + d.height);
			map.put(d, imageName);
			
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			ImageIO.write(icon, "png", out);
			byte[] imageDataArray = out.toByteArray();
			GameModule.getGameModule().getArchiveWriter().addImage(imageName, imageDataArray);
		}
		
		return imageName;
	}

	protected boolean usePieceValues() {
		for (int i = 0; i < pieceValues.length; ++i) {
			if (pieceValues[i] != null && !pieceValues[i].equals(""))
				return true;
		}
		return false;
	}

	@Override
	protected void load(File f) throws IOException {
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
		map.importFile(action, action.getCaseInsensitiveFile(new File(mapFileName), f, true, 
				new ExtensionFileFilter(ADC2Utils.MAP_DESCRIPTION, new String[] {ADC2Utils.MAP_EXTENSION})));
		
		// bail if any of these block reads fall of the end of the file, but preserve all of the information to that point.
		try {
			readGameTurnBlock(in);
			readClassBlock(in);
			readClassValueBlock(in);
			readPieceBlock(in);
			readPieceValueBlock(in);
			readPlayerBlock(in);
			readReplayBlock(in);
			readForcePoolBlock(in);
			readStackBlock(in);
			readFastZoomBlock(in);
			readFacingBlock(in);
			readSoundSettingBlock(in);
			readFlipDefinitionBlock(in);
		}
		catch(EOFException e) { }
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

	protected enum FacingDirection {
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
			
			/* int display = */ in.readUnsignedByte();
			/* int fillColor = */ in.readUnsignedByte();
			/* int outlineColor = */ in.readUnsignedByte();
			// zoom sizes
			in.read(new byte[3]);
		}
	}

	// not applicable to VASSAL.
	protected void readFastZoomBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Fast Zoom");
		
		/* int fastZoom = */ in.readUnsignedByte();
		/* int classCombatSummaryValues = */ in.readUnsignedByte();
		/* int pieceCombatSummaryValues = */ in.readUnsignedByte();
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

	protected void readForcePoolBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Force Pool");
		
		final int nForcePools = ADC2Utils.readBase250Word(in);
		final ArrayList<String> fp = new ArrayList<String>(nForcePools);
		for (int i = 0; i < nForcePools; ++i) {
			String n = readNullTerminatedString(in, 25);
			in.read(new byte[3]); // not sure what these do
			if (ADC2Utils.readBase250Word(in) != FORCE_POOL_BLOCK_END)
				fp.add(n);
			else
				break;
		}
		forcePoolNames = fp.toArray(new String[0]);
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
			
			int values[] = new int[8];
			for (int j = 0; j < values.length; ++j)
				values[j] = in.readInt();
			
			ValueType types[] = new ValueType[8];
			for (int j = 0; j < types.length; ++j) {
				int t = in.readUnsignedByte();
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
			
			in.read(new byte[2]);
			
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
			SymbolSet.SymbolData symbol = getSet().getGamePiece(ADC2Utils.readBase250Word(in));
			
			String name = readNullTerminatedString(in, 25);
			
			int[] values = new int[8];
			for (int j = 0; j < values.length; ++j)
				values[j] = in.readInt();
			
			ValueType[] types = new ValueType[8];
			for (int j = 0; j < types.length; ++j) {
				int t = in.readUnsignedByte();
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
					break;
				}
			}
			
			int owner = in.readUnsignedByte();
			int hiddenSymbol = ADC2Utils.readBase250Word(in);
		
			// 0 = not used.
			int facing = in.readUnsignedByte();
			
			PieceClass cl = new PieceClass(name, symbol, owner, hiddenSymbol, facing);
			for (int j = 0; j < values.length; ++j) {
				cl.setValue(j, values[j]);
				cl.types[j] = types[j];
			}
			
			pieceClasses.add(cl);
		}			
	}

	protected void readGameTurnBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Game Turn");
		gameTurn = ADC2Utils.readBase250Word(in);
	}

	protected void writePrototypesToArchive() {
		GameModule module = GameModule.getGameModule();

		PrototypesContainer container = module.getAllDescendantComponentsOf(PrototypesContainer.class).iterator().next();
		PrototypeDefinition def = new PrototypeDefinition();
		def.build(null);
		def.addTo(container);
		container.add(def);
		def.setConfigureName(COMMON_PROPERTIES);

		// set common properties
		GamePiece gp = new BasicPiece();
		
		gp = new Delete(Delete.ID + "Delete;D", gp);
		
		if (forcePoolNames != null && forcePoolNames.length > 0)
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

		// add game piece layers to map
		Map map = getMap().getMainMap();
		LayeredPieceCollection layer = new LayeredPieceCollection();
		layer.build(null);
		layer.addTo(map);
		map.add(layer);
		layer.setAttribute(LayeredPieceCollection.PROPERTY_NAME, DRAW_ON_TOP_OF_OTHERS);
		layer.setAttribute(LayeredPieceCollection.LAYER_ORDER, StringArrayConfigurer.arrayToString(new String[] {"no", "yes"}));
		
		// prototype definitions
		writePrototypesToArchive();
		getMap().writeToArchive();	
		writeClassesToArchive();
		if (forcePoolNames != null && forcePoolNames.length != 0)
			writeForcePoolsToArchive();
		writeSetupStacksToArchive();		
		writePlayersToArchive();
		
		// dice
		DiceButton dice = new DiceButton();
		dice.build(null);
		dice.setAttribute(DiceButton.NAME, "Roll");
		dice.setAttribute(DiceButton.PROMPT_ALWAYS, Boolean.TRUE);
		dice.setAttribute(DiceButton.TOOLTIP, "Roll the dice");
		dice.setAttribute(DiceButton.BUTTON_TEXT, "Roll");
		dice.addTo(gameModule);
		gameModule.add(dice);
	}

	protected void writeClassesToArchive() throws IOException {
		GameModule module = GameModule.getGameModule();
		PieceWindow win = module.getAllDescendantComponentsOf(PieceWindow.class).iterator().next();

		win.setAttribute(PieceWindow.NAME, "Add New Pieces");
		
		ListWidget list = new ListWidget();
		list.build(null);
		list.addTo(win);
		win.add(list);
		
		for (PieceClass c : pieceClasses)
			c.writeToArchive(list);
		
		for (PieceClass c : pieceClasses)
			c.getDefaultPiece().setReplace();
	}

	protected void writePlayersToArchive() {
		final PlayerRoster roster = GameModule.getGameModule().getAllDescendantComponentsOf(PlayerRoster.class).iterator().next();
		final SequenceEncoder se = new SequenceEncoder(',');
		for (Player player : players)
			se.append(player.getName());
		for (int i = 0; i < 2; ++i)
			roster.setAttribute(PlayerRoster.SIDES, se.getValue());		
	}

	/**
	 * Creates a board with deck stacks in which force pools are kept.
	 * 
	 * @throws IOException
	 */
	// TODO: cards should not be accessible if they are invisible. Can still draw
	// invisible cards right now.
	protected void writeForcePoolsToArchive() throws IOException {
		if (forcePoolNames == null || forcePoolNames.length == 0)
			return;
		
		final GameModule module = GameModule.getGameModule();
		final Map forcePoolMap = new Map();
		forcePoolMap.build(null);
		forcePoolMap.addTo(module);
		module.add(forcePoolMap);
		
		forcePoolMap.setMapName("Force Pool");
		forcePoolMap.setAttribute(Map.MARK_MOVED, GlobalOptions.NEVER);
		forcePoolMap.setAttribute(Map.USE_LAUNCH_BUTTON, Boolean.TRUE);
		forcePoolMap.setAttribute(Map.BUTTON_NAME, "Counter Tray");
		
		final BoardPicker boardPicker = forcePoolMap.getBoardPicker();
		
		// write force pool board
		final Dimension modalSize = getSet().getModalSize();
		final JPanel panel = new JPanel();
		final JPanel[] deckPanels = new JPanel[forcePoolNames.length];
		final GridBagConstraints c = new GridBagConstraints();
		c.insets = new Insets(5, 5, 5, 5);
		panel.setLayout(new GridBagLayout());
		panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		final int nForcePools = forcePoolNames.length;
		final int nRows = (nForcePools+3)/4;
		for (int i = 0; i < nForcePools; ++i) {
			deckPanels[i] = new JPanel();
			deckPanels[i].setPreferredSize(modalSize);
			deckPanels[i].setMaximumSize(modalSize);
			deckPanels[i].setBackground(getMap().getTableColor());
			deckPanels[i].setBorder(BorderFactory.createLoweredBevelBorder());
			c.gridx = i/nRows;
			c.gridy = (i%nRows)*2;
			c.insets.bottom = 2;
			c.insets.top = 5;
			panel.add(deckPanels[i], c);
			JLabel label = new JLabel(forcePoolNames[i]);
			label.setMaximumSize(label.getPreferredSize());
			c.gridy += 1;
			c.insets.top = 2;
			c.insets.bottom = 5;
			panel.add(label, c);
		}		
		final Dimension d = panel.getPreferredSize();		
		panel.setSize(d);
		panel.doLayout();
		final BufferedImage forcePool = new BufferedImage(d.width, d.height, BufferedImage.TYPE_3BYTE_BGR);
		final Graphics2D g = forcePool.createGraphics();
		panel.setBackground(getMap().getTableColor());
		panel.printAll(g);
		
		// write the map image
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		ImageIO.write(forcePool, "png", out);
		final byte[] imageDataArray = out.toByteArray();
		final String forcePoolImageName = "forcePool.png";
		module.getArchiveWriter().addImage(forcePoolImageName, imageDataArray);
		
		final Board board = new Board();
		board.build(null);
		board.addTo(boardPicker);
		boardPicker.add(board);
		board.setConfigureName("Force Pools");
		board.setAttribute(Board.IMAGE, forcePoolImageName);
		
		// create decks
		final Rectangle rv = new Rectangle();
		for (int i = 0; i < forcePoolNames.length; ++i) {
			DrawPile pile = new DrawPile();
			pile.build(null);
			pile.addTo(forcePoolMap);
			forcePoolMap.add(pile);			
			
			JPanel p = deckPanels[i];
			p.getBounds(rv);
			pile.setAttribute(DrawPile.OWNING_BOARD, "Force Pools");
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
			pile.setAttribute(DrawPile.NAME, forcePoolNames[i]);
			
			if (forcePools.get(i) != null)
				for (Piece pc : forcePools.get(i)) {
					pc.writeToArchive(pile);
					pc.setReplace();
				}
		}
	}
	
	protected void writeSetupStacksToArchive() throws IOException {
		final Map mainMap = getMap().getMainMap();
		
		// change mouse over stack viewer
		CounterDetailViewer viewer = GameModule.getGameModule().getAllDescendantComponentsOf(CounterDetailViewer.class).iterator().next();
		viewer.setAttribute(CounterDetailViewer.DISPLAY, CounterDetailViewer.FILTER);
		viewer.setAttribute(CounterDetailViewer.PROPERTY_FILTER, ADC2Utils.TYPE + " = " + PIECE);
		
		final Point offset = getMap().getCenterOffset();
		for (int hex : stacks.keySet()) {
			Point p = getMap().indexToPosition(hex);
			if (p == null)
				continue;
			ArrayList<Piece> s = stacks.get(hex);
			SetupStack stack = new SetupStack();
			stack.build(null);
			stack.addTo(mainMap);

			mainMap.add(stack);
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