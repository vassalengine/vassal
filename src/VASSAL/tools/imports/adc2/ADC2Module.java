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
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Delete;
import VASSAL.counters.Footprint;
import VASSAL.counters.FreeRotator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Marker;
import VASSAL.counters.MovementMarkable;
import VASSAL.counters.Replace;
import VASSAL.counters.ReturnToDeck;
import VASSAL.counters.UsePrototype;
import VASSAL.tools.ExtensionFileFilter;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imports.FileFormatException;
import VASSAL.tools.imports.Importer;

public class ADC2Module extends Importer {
	
	private static final String PIECE = "Pieces";

	protected class Piece {
		private final int[] values = new int[8];
		private final ValueType[] types = new ValueType[8];
		private final String name;
		private final Class cl;
		@SuppressWarnings("unused")
    private final HideState hidden;
		private final int flags;
		private GamePiece gamePiece;
		private PieceSlot pieceSlot;
	
		public Piece(Class cl) {
			this.name = null;
			this.cl = cl;
			this.flags = 0;
			this.hidden = null;
		}
		
		public Piece(int position, String name, Class cl, HideState hidden, int flags) {
			if (name == null || name.equals(""))
				this.name = null;
			else
				this.name = name;
			this.cl = cl;
			this.flags = flags;
			this.hidden = hidden;
			
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
		
		public Class getPieceClass() {
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
			
			// set replacement
			se = new SequenceEncoder(path, ';');
			se.append("null").append(0).append(0).append(true);
			gamePiece = new Replace(Replace.ID + "Flip;F;" + se.getValue(), gamePiece);
			getPieceSlot().setPiece(gamePiece);
		}
		
		GamePiece getGamePiece() throws IOException {
			if (gamePiece == null) {
				String fileName = getPieceClass().getImageName();
				if (fileName == null)
					return null;
				BasicPiece bp = new BasicPiece();
				SequenceEncoder se = new SequenceEncoder(BasicPiece.ID, ';');
				se.append("").append("").append(fileName).append(getName());
				bp.mySetType(se.getValue());			

				// facing
				if (getPieceClass().getAllowedFacings() > 1) {
					String type = FreeRotator.ID + getPieceClass().getAllowedFacings() + ";];[;Rotate CW;Rotate CCW;;;;";
					gamePiece = new FreeRotator(type, bp);				
				}				
				else
					gamePiece = bp;

				// common properties
				se = new SequenceEncoder(UsePrototype.ID.replaceAll(";", ""), ';');
				se.append(COMMON_PROPERTIES);
				gamePiece = new UsePrototype(se.getValue(), gamePiece);
			}
			return gamePiece;
		}
		
		private boolean hasAttacked() {
			return (flags & 0x1) > 0;
		}
		
		@SuppressWarnings("unused")
    private boolean hasDefended() {
			return !hasAttacked() && (flags & 0x2) > 0;
		}
		
		@SuppressWarnings("unused")
    private boolean hasMoved() {
			return (flags & 0x4) > 0;
		}
		
		@SuppressWarnings("unused")
    private boolean drawOnTopOfOthers() {
			return (flags & 0x10) > 0;
		}

		protected String getName() {
			if (name == null)
				return getPieceClass().getName();
			else
				return getPieceClass().getName() + " " + name;
		}

		@SuppressWarnings("unused")
    private boolean anyUsedProperties() {
			for (int i = 0; i < 8; ++i)
				if (getValue(i) != null)
					return true;
			return false;
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
			for (int i = 0; i < b.length; ++i) {
				b[i] = (byte) ((values[index] & mask) >> ((3-i)*8));
				mask >>= 8;
			}
			return new String(b);
		}
		
		private boolean getValueAsBoolean(int index) {
			return values[index] > 0 ? true : false;
		}
		
		Object getValue(int index) {
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

		static final Player ALL_PLAYERS = new Player("All Players", null, 0, 0);
		private final String name;
		private final SymbolSet.SymbolData hiddenSymbol;
		@SuppressWarnings("unused")
    private final int searchRange;
		private final int hiddenPieceOptions;

		public Player(String name, SymbolSet.SymbolData hiddenSymbol, int searchRange, int hiddenPieceOptions) {
			this.name = name;
			this.hiddenSymbol = hiddenSymbol;
			this.searchRange = searchRange > 50 ? 50 : searchRange;
			this.hiddenPieceOptions = hiddenPieceOptions;
		}
		
		public boolean useHiddenPieces() {
			return (hiddenPieceOptions & 0x1) > 0;
		}
		
		public boolean hiddenWhenPlaced() {
			return (hiddenPieceOptions & 0x2) > 0;
		}
				
		public boolean hiddenInForcePools() {
			return (hiddenPieceOptions & 0x4) > 0;
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
	protected class Class {
		
		private final int[] values = new int[8];
		private final ValueType[] types = new ValueType[8];
		private final String name;
		private final SymbolSet.SymbolData symbol;
		private final int owner;
		private final int hiddenSymbol;
		private final int facing;
		private Class flipClass;
		private Piece defaultPiece;
		
		public Class(String name, SymbolSet.SymbolData symbol, int owner, int hiddenSymbol, int facing) {
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
		
		public int getAllowedFacings() {
			if (allowedFacings == null)
				return 1;
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
		
		// TODO: why is there both getPlayer() and getOwner()?
		public Player getPlayer() {
			return players.get(owner);
		}
		
		public Player getOwner() {
			if (owner == 200)
				return Player.ALL_PLAYERS;
			else
				return players.get(owner);
		}

		protected void setFlipClass(int to) {
			if (to >= 0 && to < classes.size())
				flipClass = classes.get(to);
		}
		
		public Class getFlipClass() {
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
			int mask = 0x7f000000;
			for (int i = 0; i < b.length; ++i) {
				b[i] = (byte) ((values[index] & mask) >> ((3-i)*8));
				mask >>= 8;
			}
			return new String(b);
		}
		
		public boolean getValueAsBoolean(int index) {
			return values[index] > 0 ? true : false;
		}
		
		public Object getValue(int index) {
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
	}

	private static final String COMMON_PROPERTIES = "Common Properties";
	private String name;
	private MapBoard map = null;
	@SuppressWarnings("unused")
  private int gameTurn = -1;
	private final ArrayList<Class> classes = new ArrayList<Class>();
	private final ArrayList<Piece> pieces = new ArrayList<Piece>();
	private final ArrayList<Player> players = new ArrayList<Player>();
	private final HashMap<Integer,ArrayList<Piece>> stacks = new HashMap<Integer,ArrayList<Piece>>();
	private final HashMap<Integer, ArrayList<Piece>> forcePools = new HashMap<Integer,ArrayList<Piece>>();
	private String[] forcePoolNames;
	private final String[] classValues = new String[8];
	private final String[] pieceValues = new String[8];
	private int allowedFacings[]; 
	
	protected Class getClassFromIndex(int index) {
		if (index < 0 || index >= classes.size())
			return null;
		return classes.get(index);
	}
	
	@Override
	protected void load(File f) throws IOException {
		DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(f)));
		
		name = stripExtension(f.getName());

		int header = in.readByte();
		if (header != -3 && header != -2)
			throw new FileFormatException("Invalid Game Module Header");
				
		// version information doesn't seem to do anything
		in.read(new byte[2]);
		
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
		catch(EOFException e) {
			
		}
	}
	
	protected void readFlipDefinitionBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Flip Definition");
				
		in.readUnsignedByte(); // unknown byte
		
		final int nFlipDefs = ADC2Utils.readBase250Word(in);
		for (int i = 0; i < nFlipDefs; ++i) {
			int from = ADC2Utils.readBase250Word(in);
			int to = ADC2Utils.readBase250Word(in);
			if (from >= 0 && from < classes.size() && to >= 0 && to < classes.size())
				classes.get(from).setFlipClass(to);
		}
	}

	protected void readSoundSettingBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Sound Settings");
		
		for (int i = 0; i < 3; ++i)
			/* scrollJumpSize[i] = */ in.readUnsignedByte();
		in.read(new byte[3]); // unknown
		/* soundOn = */	in.readUnsignedByte();
	}

	// ADC2 uses a very boroque method for indicating facing. Most of this is not applicable to VASSAL.
	protected void readFacingBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Facing");
		
		final int nFacing = in.readUnsignedByte();
		allowedFacings = new int[nFacing+1];
		allowedFacings[0] = 1;
		
		for (int i = 0; i < nFacing; ++i) {
			/* String styleName = */ readNullTerminatedString(in);			
			/* int direction = */ in.readUnsignedByte();
			allowedFacings[i+1] = 2 * getMap().getNFaces();
			
			/* int display = */ in.readUnsignedByte();
			/* int fillColor = */ in.readUnsignedByte();
			/* int outlineColor = */ in.readUnsignedByte();
			// zoom sizes
			in.read(new byte[3]);
		}
	}

	// mostly not applicable to VASSAL.
	protected void readFastZoomBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Fast Zoom");
		
		/* int fastZoom = */ in.readUnsignedByte();
		
		@SuppressWarnings("unused")
    int classCombatSummaryValues = in.readUnsignedByte();
		@SuppressWarnings("unused")
    int pieceCombatSummaryValues = in.readUnsignedByte();
		/* int fastDraw = */ in.readUnsignedByte();
	}

	// I'm not even clear on what these do in ADC2.
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
	
	// TODO: this is a big job to implement.
	protected void readReplayBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Replay");
		
		int nBytes = ADC2Utils.readBase250Integer(in);
		in.read(new byte[nBytes]);
	}

	protected void readForcePoolBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, "Force Pool");
		
		final int nForcePools = ADC2Utils.readBase250Word(in);
		forcePoolNames = new String[nForcePools];
		for (int i = 0; i < nForcePools; ++i) {
			forcePoolNames[i] = readNullTerminatedString(in, 25);
			in.read(new byte[3]); // not sure what these do
			/* int nUnits = */ ADC2Utils.readBase250Word(in);
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
			
			// we don't do anything with this yet.
			int searchRange = in.readUnsignedByte();
			
			int hiddenPieceOptions = in.readUnsignedByte();
			in.readByte(); // padding
			
			if (name.length() > 0) {
				Player player = new Player(name, hiddenSymbol, searchRange, hiddenPieceOptions);
				players.add(player);
			}
		} while (name.length() > 0);
	}

	protected void readPieceBlock(DataInputStream in) throws IOException {
		ADC2Utils.readBlockHeader(in, PIECE);
		
		int nPieces = ADC2Utils.readBase250Word(in);
		for (int i = 0; i < nPieces; ++i) {
			String name = readNullTerminatedString(in, 25);
			
			Class cl = getClassFromIndex(ADC2Utils.readBase250Word(in));
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
			
			in.readUnsignedByte(); // ignored?
			
			Piece p = new Piece(position, name, cl, hidden, flags);			
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
			// TODO: if symbol is null, then there's a problem.
			// Check what ADC2 does.
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
			
			// TODO: if the owner doesn't exist, then the class is treated as
			// belonging to all players (owner = 200)
			int owner = in.readUnsignedByte();
		
			int hiddenSymbol = ADC2Utils.readBase250Word(in);
		
			// 0 = not used. Any value appears valid even if it's out of range
			int facing = in.readUnsignedByte();
			
			Class cl = new Class(name, symbol, owner, hiddenSymbol, facing);
			for (int j = 0; j < values.length; ++j) {
				cl.setValue(j, values[j]);
				cl.types[j] = types[j];
			}
			
			classes.add(cl);
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
		def.addTo(container);
		container.add(def);
		def.setConfigureName(COMMON_PROPERTIES);

		// set common properties
		GamePiece gp = new BasicPiece();		
		gp = new Delete(Delete.ID + "Delete;D", gp);
		gp = new ReturnToDeck(ReturnToDeck.ID + "Return to Force Pool;R;;Select Force Pool", gp);
		SequenceEncoder se = new SequenceEncoder(';');
		Dimension modalSize = getSet().getModalSize();
		int xOffset = modalSize.width/2;
		int yOffset = -modalSize.height/2;
		String movedIcon = "/images/moved.gif";
		se.append(movedIcon).append(xOffset).append(yOffset);
		gp = new MovementMarkable(MovementMarkable.ID + se.getValue(), gp);
		se = new SequenceEncoder(';');
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
		GameModule.getGameModule().setAttribute(GameModule.MODULE_NAME, name);

		// prototype definitions
		writePrototypesToArchive();
		getMap().writeToArchive();	
		writeClassesToArchive();
		if (!forcePools.isEmpty())
			writeForcePoolsToArchive();
		writeSetupStacksToArchive();		
		writePlayersToArchive();
	}

	protected void writeClassesToArchive() throws IOException {
		GameModule module = GameModule.getGameModule();
		PieceWindow win = module.getAllDescendantComponentsOf(PieceWindow.class).iterator().next();

		win.setAttribute(PieceWindow.NAME, "Add New Pieces");
		
		ListWidget list = new ListWidget();
		list.addTo(win);
		win.add(list);
		
		for (Class c : classes)
			c.writeToArchive(list);
		
		for (Class c : classes)
			c.getDefaultPiece().setReplace();
	}

	protected void writePlayersToArchive() {
		final PlayerRoster roster = GameModule.getGameModule().getAllDescendantComponentsOf(PlayerRoster.class).iterator().next();
		final SequenceEncoder se = new SequenceEncoder(',');
		for (Player player : players)
			se.append(player.getName());
		roster.setAttribute(PlayerRoster.SIDES, se.getValue());
	}

	/**
	 * Creates a board with deck stacks in which force pools are kept.
	 * 
	 * @throws IOException
	 */
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
		board.addTo(boardPicker);
		boardPicker.add(board);
		board.setConfigureName("Force Pools");
		board.setAttribute(Board.IMAGE, forcePoolImageName);
		
		// create decks
		final Rectangle rv = new Rectangle();
		for (int i = 0; i < forcePoolNames.length; ++i) {
			DrawPile pile = new DrawPile();
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
		final Point offset = getMap().getCenterOffset();
		for (int hex : stacks.keySet()) {
			Point p = getMap().indexToPosition(hex);
			if (p == null)
				continue;
			ArrayList<Piece> s = stacks.get(hex);
			SetupStack stack = new SetupStack();
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