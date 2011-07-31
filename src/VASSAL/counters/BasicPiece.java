/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertyNameSource;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.i18n.Localization;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.ScaledImagePainter;

/**
 * Basic class for representing a physical component of the game Can be a counter, a card, or an overlay
 */
public class BasicPiece implements TranslatablePiece, StateMergeable, PropertyNameSource {
  public static final String ID = "piece;";
  private static Highlighter highlighter;
  /**
   * Return information about the current location of the piece through getProperty():
   *
   * LocationName - Current Location Name of piece as displayed in Chat Window CurrentX - Current X position CurrentY -
   * Current Y position CurrentMap - Current Map name or "" if not on a map CurrentBoard - Current Board name or "" if
   * not on a map CurrentZone - If the current map has a multi-zoned grid, then return the name of the Zone the piece is
   * in, or "" if the piece is not in any zone, or not on a map
   */
  public static final String LOCATION_NAME = "LocationName";
  public static final String CURRENT_MAP = "CurrentMap";
  public static final String CURRENT_BOARD = "CurrentBoard";
  public static final String CURRENT_ZONE = "CurrentZone";
  public static final String CURRENT_X = "CurrentX";
  public static final String CURRENT_Y = "CurrentY";
  public static final String OLD_LOCATION_NAME = "OldLocationName";
  public static final String OLD_MAP = "OldMap";
  public static final String OLD_BOARD = "OldBoard";
  public static final String OLD_ZONE = "OldZone";
  public static final String OLD_X = "OldX";
  public static final String OLD_Y = "OldY";
  public static final String BASIC_NAME = "BasicName";
  public static final String PIECE_NAME = "PieceName";
  public static final String DECK_NAME = "DeckName";
  public static final String DECK_POSITION = "DeckPosition";
  public static Font POPUP_MENU_FONT = new Font("Dialog", 0, 11);
  protected JPopupMenu popup;
  protected Rectangle imageBounds;
  protected ScaledImagePainter imagePainter = new ScaledImagePainter();
  private Map map;
  private KeyCommand[] commands;
  private Stack parent;
  private Point pos = new Point(0, 0);
  private String id;
  private java.util.Map<Object, Object> props;
  /** @deprecated Moved into own traits, retained for backward compatibility */
  @Deprecated
  private char cloneKey;
  /** @deprecated Moved into own traits, retained for backward compatibility */
  @Deprecated
  private char deleteKey;
  /**
   * @deprecated Replaced by
   * @{link #srcOp}.
   */
  @Deprecated
  protected Image image;
  protected String imageName;
  private String commonName;

  public BasicPiece() {
    this(ID + ";;;;");
  }

  public BasicPiece(String type) {
    mySetType(type);
  }

  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    cloneKey = st.nextChar('\0');
    deleteKey = st.nextChar('\0');
    imageName = st.nextToken();
    commonName = st.nextToken();
    imagePainter.setImageName(imageName);
    commands = null;
  }

  public String getType() {
    final SequenceEncoder se =
      new SequenceEncoder(cloneKey > 0 ? String.valueOf(cloneKey) : "", ';');
    return ID + se.append(deleteKey > 0 ? String.valueOf(deleteKey) : "")
                  .append(imageName)
                  .append(commonName).getValue();
  }

  public void setMap(Map map) {
    if (map != this.map) {
      commands = null;
      this.map = map;
    }
  }

  public Map getMap() {
    return getParent() == null ? map : getParent().getMap();
  }

  public Object getProperty(Object key) {
    if (BASIC_NAME.equals(key)) {
      return getName();
    }
    else
        return getPublicProperty(key);
  }

  /*
   * Properties visible in a masked unit
   */
  public Object getPublicProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getKeyCommands();
    }
    else if (LOCATION_NAME.equals(key)) {
      return getMap() == null ? "" : getMap().locationName(getPosition());
    }
    else if (PIECE_NAME.equals(key)) {
      return Decorator.getOutermost(this).getName();
    }
    else if (CURRENT_MAP.equals(key)) {
      return getMap() == null ? "" : getMap().getConfigureName();
    }
    else if (DECK_NAME.equals(key)) {
      return getParent() instanceof Deck ? ((Deck) getParent()).getDeckName() : "";
    }
    else if (DECK_POSITION.equals(key)) {
      if (getParent() instanceof Deck) {
        final Deck deck = (Deck) getParent();
        final int size = deck.getPieceCount();
        final int pos = deck.indexOf(Decorator.getOutermost(this));
        return String.valueOf(size - pos + 1);
      }
      else {
        return "0";
      }
     }
    else if (CURRENT_BOARD.equals(key)) {
      if (getMap() != null) {
        final Board b = getMap().findBoard(getPosition());
        if (b != null) {
          return b.getName();
        }
      }
      return "";
    }
    else if (CURRENT_ZONE.equals(key)) {
      if (getMap() != null) {
        final Zone z = getMap().findZone(getPosition());
        if (z != null) {
          return z.getName();
        }
      }
      return "";
    }
    else if (CURRENT_X.equals(key)) {
      return String.valueOf(getPosition().x);
    }
    else if (CURRENT_Y.equals(key)) {
      return String.valueOf(getPosition().y);
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return "";
    }
    Object prop = props == null ? null : props.get(key);
    if (prop == null) {
      final Map map = getMap();
      final Zone zone = (map == null ? null : map.findZone(getPosition()));
      if (zone != null) {
        prop = zone.getProperty(key);
      }
      else if (map != null) {
        prop = map.getProperty(key);
      }
      else {
        prop = GameModule.getGameModule().getProperty(key);
      }
    }
    return prop;
  }

  public Object getLocalizedProperty(Object key) {
    if (BASIC_NAME.equals(key)) {
      return getLocalizedName();
    }
    else {
      return getLocalizedPublicProperty(key);
    }
  }

  /*
   * Properties visible in a masked unit
   */
  public Object getLocalizedPublicProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getProperty(key);
    }
    else if (LOCATION_NAME.equals(key)) {
      return getMap() == null ? "" : getMap().localizedLocationName(getPosition());
    }
    else if (PIECE_NAME.equals(key)) {
      return Decorator.getOutermost(this).getName();
    }
    else if (BASIC_NAME.equals(key)) {
      return getLocalizedName();
    }
    else if (CURRENT_MAP.equals(key)) {
      return getMap() == null ? "" : getMap().getLocalizedConfigureName();
    }
    else if (DECK_NAME.equals(key)) {
      return getProperty(key);
    }
    else if (DECK_POSITION.equals(key)) {
      if (getParent() instanceof Deck) {
        final Deck deck = (Deck) getParent();
        final int size = deck.getPieceCount();
        final int pos = deck.indexOf(Decorator.getOutermost(this));
        return String.valueOf(size - pos);
      }
      else {
        return "0";
      }
    }
    else if (CURRENT_BOARD.equals(key)) {
      if (getMap() != null) {
        final Board b = getMap().findBoard(getPosition());
        if (b != null) {
          return b.getLocalizedName();
        }
      }
      return "";
    }
    else if (CURRENT_ZONE.equals(key)) {
      if (getMap() != null) {
        final Zone z = getMap().findZone(getPosition());
        if (z != null) {
          return z.getLocalizedName();
        }
      }
      return "";
    }
    else if (CURRENT_X.equals(key)) {
      return getProperty(key);
    }
    else if (CURRENT_Y.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return getProperty(key);
    }
    Object prop = props == null ? null : props.get(key);
    if (prop == null) {
      final Map map = getMap();
      final Zone zone = (map == null ? null : map.findZone(getPosition()));
      if (zone != null) {
        prop = zone.getLocalizedProperty(key);
      }
      else if (map != null) {
        prop = map.getLocalizedProperty(key);
      }
      else {
        prop = GameModule.getGameModule().getLocalizedProperty(key);
      }
    }
    return prop;
  }

  public void setProperty(Object key, Object val) {
    if (props == null) {
      props = new HashMap<Object, Object>();
    }
    if (val == null) {
      props.remove(key);
    }
    else {
      props.put(key, val);
    }
  }

  protected Object prefsValue(String s) {
    return GameModule.getGameModule().getPrefs().getValue(s);
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (imageBounds == null) {
      imageBounds = boundingBox();
    }
    imagePainter.draw(g, x + (int) (zoom * imageBounds.x), y + (int) (zoom * imageBounds.y), zoom, obs);
  }

  protected KeyCommand[] getKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<KeyCommand>();
      final GamePiece target = Decorator.getOutermost(this);
      if (cloneKey > 0) {
        l.add(new KeyCommand("Clone", KeyStroke.getKeyStroke(cloneKey, InputEvent.CTRL_MASK), target));
      }
      if (deleteKey > 0) {
        l.add(new KeyCommand("Delete", KeyStroke.getKeyStroke(deleteKey, InputEvent.CTRL_MASK), target));
      }
      commands = l.toArray(new KeyCommand[l.size()]);
    }
    final GamePiece outer = Decorator.getOutermost(this);
    boolean canAdjustPosition = outer.getMap() != null && outer.getParent() != null && outer.getParent().topPiece() != getParent().bottomPiece();
    enableCommand("Move up", canAdjustPosition);
    enableCommand("Move down", canAdjustPosition);
    enableCommand("Move to top", canAdjustPosition);
    enableCommand("Move to bottom", canAdjustPosition);
    enableCommand("Clone", outer.getMap() != null);
    enableCommand("Delete", outer.getMap() != null);
    return commands;
  }

  private void enableCommand(String name, boolean enable) {
    for (int i = 0; i < commands.length; ++i) {
      if (name.equals(commands[i].getName())) {
        commands[i].setEnabled(enable);
      }
    }
  }

  private boolean isEnabled(KeyStroke stroke) {
    if (stroke == null) {
      return false;
    }
    for (int i = 0; i < commands.length; ++i) {
      if (stroke.equals(commands[i].getKeyStroke())) {
        return commands[i].isEnabled();
      }
    }
    return true;
  }

  public Point getPosition() {
    return getParent() == null ? new Point(pos) : getParent().getPosition();
  }

  public void setPosition(Point p) {
    if (getMap() != null && getParent() == null) {
      getMap().repaint(getMap().boundingBoxOf(Decorator.getOutermost(this)));
    }
    pos = p;
    if (getMap() != null && getParent() == null) {
      getMap().repaint(getMap().boundingBoxOf(Decorator.getOutermost(this)));
    }
  }

  public Stack getParent() {
    return parent;
  }

  public void setParent(Stack s) {
    parent = s;
  }

  public Rectangle boundingBox() {
    if (imageBounds == null) {
      imageBounds = ImageUtils.getBounds(imagePainter.getImageSize());
    }
    return new Rectangle(imageBounds);
  }

  public Shape getShape() {
    return boundingBox();
  }

  public boolean equals(GamePiece c) {
    return c == this;
  }

  public String getName() {
    return commonName;
  }

  public String getLocalizedName() {
    final String key = TranslatablePiece.PREFIX + getName();
    return Localization.getInstance().translate(key, getName());
  }

  public Command keyEvent(KeyStroke stroke) {
    getKeyCommands();
    if (!isEnabled(stroke)) {
      return null;
    }
    Command comm = null;
    final GamePiece outer = Decorator.getOutermost(this);
    if (KeyStroke.getKeyStroke(cloneKey, InputEvent.CTRL_MASK).equals(stroke)) {
      final GamePiece newPiece = ((AddPiece) GameModule.getGameModule().decode(GameModule.getGameModule().encode(new AddPiece(outer)))).getTarget();
      newPiece.setId(null);
      GameModule.getGameModule().getGameState().addPiece(newPiece);
      newPiece.setState(outer.getState());
      comm = new AddPiece(newPiece);
      if (getMap() != null) {
        comm.append(getMap().placeOrMerge(newPiece, getPosition()));
        KeyBuffer.getBuffer().remove(outer);
        KeyBuffer.getBuffer().add(newPiece);
        if (GlobalOptions.getInstance().autoReportEnabled() && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
          String s = "* " + outer.getLocalizedName();
          final String loc = getMap().locationName(outer.getPosition());
          if (loc != null) {
            s += " cloned in " + loc + " * ";
          }
          else {
            s += "cloned *";
          }
          final Command report = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), s);
          report.execute();
          comm = comm.append(report);
        }
      }
    }
    else if (KeyStroke.getKeyStroke(deleteKey, InputEvent.CTRL_MASK).equals(stroke)) {
      comm = new RemovePiece(outer);
      if (getMap() != null && GlobalOptions.getInstance().autoReportEnabled() && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
        String s = "* " + outer.getLocalizedName();
        final String loc = getMap().locationName(outer.getPosition());
        if (loc != null) {
          s += " deleted from " + loc + " * ";
        }
        else {
          s += " deleted *";
        }
        final Command report = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), s);
        comm = comm.append(report);
      }
      comm.execute();
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveUpKey())) {
      if (parent != null) {
        final String oldState = parent.getState();
        final int index = parent.indexOf(outer);
        if (index < parent.getPieceCount() - 1) {
          parent.insert(outer, index + 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToFront(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToFront(outer);
      }
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveDownKey())) {
      if (parent != null) {
        final String oldState = parent.getState();
        final int index = parent.indexOf(outer);
        if (index > 0) {
          parent.insert(outer, index - 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToBack(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToBack(outer);
      }
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveTopKey())) {
      parent = outer.getParent();
      if (parent != null) {
        final String oldState = parent.getState();
        if (parent.indexOf(outer) < parent.getPieceCount() - 1) {
          parent.insert(outer, parent.getPieceCount() - 1);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToFront(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToFront(outer);
      }
    }
    else if (getMap() != null && stroke.equals(getMap().getStackMetrics().getMoveBottomKey())) {
      parent = getParent();
      if (parent != null) {
        final String oldState = parent.getState();
        if (parent.indexOf(outer) > 0) {
          parent.insert(outer, 0);
          comm = new ChangePiece(parent.getId(), oldState, parent.getState());
        }
        else {
          getMap().getPieceCollection().moveToBack(parent);
        }
      }
      else {
        getMap().getPieceCollection().moveToBack(outer);
      }
    }
    return comm;
  }

  public String getState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    final String mapName = map == null ? "null" : map.getIdentifier();
    se.append(mapName);
    final Point p = getPosition();
    se.append(p.x).append(p.y);
    se.append(getGpId());
    return se.getValue();
  }

  public void setState(String s) {
    final GamePiece outer = Decorator.getOutermost(this);
    final Map oldMap = getMap();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    final String mapId = st.nextToken();
    Map newMap = null;
    if (!"null".equals(mapId)) {
      newMap = Map.getMapById(mapId);
      if (newMap == null) {
        Decorator.reportDataError(this, Resources.getString("Error.not_found", "Map"), "mapId="+mapId);
      }
    }
    final Point newPos = new Point(st.nextInt(0), st.nextInt(0));
    setPosition(newPos);
    if (newMap != oldMap) {
      if (newMap != null) {
        // This will remove from oldMap
        // and set the map to newMap
        newMap.addPiece(outer);
      }
      else if (oldMap != null) {
        oldMap.removePiece(outer);
        setMap(null);
      }
      else {
        setMap(null);
      }
    }
    setGpId(st.nextToken(""));
  }

  public void mergeState(String newState, String oldState) {
    if (!newState.equals(oldState)) {
      setState(newState);
    }
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  /**
   * Get the Highlighter instance for drawing selected pieces. Note that since this is a static method, all pieces in a
   * module will always use the same Highlighter
   */
  public static Highlighter getHighlighter() {
    if (highlighter == null) {
      highlighter = new ColoredBorder();
    }
    return highlighter;
  }

  /**
   * Set the Highlighter for all pieces
   */
  public static void setHighlighter(Highlighter h) {
    highlighter = h;
  }

  public String getDescription() {
    return "Basic Piece";
  }

  public String getGpId() {
    String id = (String) getProperty(Properties.PIECE_ID);
    return id == null ? "" : id;
  }

  public void setGpId(String id) {
    setProperty(Properties.PIECE_ID, id == null ? "" : id);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("BasicPiece.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  private static class Ed implements PieceEditor {
    private JPanel panel;
    private KeySpecifier cloneKeyInput;
    private KeySpecifier deleteKeyInput;
    private JTextField pieceName;
    private ImagePicker picker;
    private String state;

    private Ed(BasicPiece p) {
      state = p.getState();
      initComponents(p);
    }

    private void initComponents(BasicPiece p) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      picker = new ImagePicker();
      picker.setImageName(p.imageName);
      panel.add(picker);
      cloneKeyInput = new KeySpecifier(p.cloneKey);
      deleteKeyInput = new KeySpecifier(p.deleteKey);
      pieceName = new JTextField(12);
      pieceName.setText(p.commonName);
      pieceName.setMaximumSize(pieceName.getPreferredSize());
      Box col = Box.createVerticalBox();
      Box row = Box.createHorizontalBox();
      row.add(new JLabel("Name:  "));
      row.add(pieceName);
      col.add(row);
      if (p.cloneKey != 0) {
        row = Box.createHorizontalBox();
        row.add(new JLabel("To Clone:  "));
        row.add(cloneKeyInput);
        col.add(row);
      }
      if (p.deleteKey != 0) {
        row = Box.createHorizontalBox();
        row.add(new JLabel("To Delete:  "));
        row.add(deleteKeyInput);
        col.add(row);
      }
      panel.add(col);
    }

    public void reset(BasicPiece p) {
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return state;
    }

    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(cloneKeyInput.getKey(), ';');
      final String type = se.append(deleteKeyInput.getKey()).append(picker.getImageName()).append(pieceName.getText()).getValue();
      return BasicPiece.ID + type;
    }
  }

  public String toString() {
    return super.toString() + "[name=" + getName() + ",type=" + getType() + ",state=" + getState() + "]";
  }

  public PieceI18nData getI18nData() {
    final PieceI18nData data = new PieceI18nData(this);
    data.add(commonName, "Basic piece name");
    return data;
  }

  /**
   * Return Property names exposed by this trait
   */
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<String>();
    l.add(LOCATION_NAME);
    l.add(CURRENT_MAP);
    l.add(CURRENT_BOARD);
    l.add(CURRENT_ZONE);
    l.add(CURRENT_X);
    l.add(CURRENT_Y);
    l.add(OLD_LOCATION_NAME);
    l.add(OLD_MAP);
    l.add(OLD_BOARD);
    l.add(OLD_ZONE);
    l.add(OLD_X);
    l.add(OLD_Y);
    l.add(BASIC_NAME);
    l.add(PIECE_NAME);
    l.add(DECK_NAME);
    return l;
  }
}
