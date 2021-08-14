/*
 *
 * Copyright (c) 2003-2012 by Rodney Kinney, Brent Easton
 * GridLocation modifications copyright (c) 2010-2011 by Pieter Geerkens
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

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.Region;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.AuditTrail;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import net.miginfocom.swing.MigLayout;

/**
 * This trait adds a command that sends a piece to another location. Options for the
 * target location are:
 * <li>Specified x,y co-ords on a named map/board</li>
 * <li>The centre of a named Zone on a named map</li>
 * <li>A named Region on a named map</li>
 * <li>The location of another counter selected by a Property Match String</li>
 * <li>A specified grid-location on a given board & map </li>
 * <p>Once the target location is identified, it can be further offset in the X and Y directions
 * by a set of multipliers.
 * All Input Fields may use $...$ variable names
 */
public class SendToLocation extends Decorator implements TranslatablePiece {
  public static final String ID = "sendto;"; // NON-NLS
  private static final String _0 = "0";
  public static final String BACK_MAP = "backMap"; // NON-NLS
  public static final String BACK_POINT = "backPoint"; // NON-NLS

  // Translated Destination descriptions
  protected static final String DEST_GRIDLOCATION = "G"; // NON-NLS
  protected static final String DEST_LOCATION = "L"; // NON-NLS
  protected static final String DEST_ZONE = "Z"; // NON-NLS
  protected static final String DEST_REGION = "R"; // NON-NLS
  protected static final String DEST_COUNTER = "A"; // NON-NLS
  protected static final String[] DEST_OPTIONS = {
    DEST_GRIDLOCATION, DEST_LOCATION, DEST_ZONE, DEST_REGION, DEST_COUNTER
  };
  // Actual valued recorded for Destination option
  protected static final String[] DEST_KEYS = {
    Resources.getString("Editor.SendToLocation.grid_location_on_selected_map"),
    Resources.getString("Editor.SendToLocation.location_on_selected_map"),
    Resources.getString("Editor.SendToLocation.zone_on_selected_map"),
    Resources.getString("Editor.SendToLocation.region_on_selected_map"),
    Resources.getString("Editor.SendToLocation.another_counter_selected_by_properties")
  };

  protected KeyCommand[] command;
  protected String commandName;
  protected String backCommandName;
  protected NamedKeyStroke key;
  protected NamedKeyStroke backKey;
  protected FormattedString mapId = new FormattedString("");
  protected FormattedString boardName = new FormattedString("");
  protected FormattedString x = new FormattedString("");
  protected FormattedString xIndex = new FormattedString("");
  protected FormattedString xOffset = new FormattedString("");
  protected FormattedString y = new FormattedString("");
  protected FormattedString yIndex = new FormattedString("");
  protected FormattedString yOffset = new FormattedString("");
  protected FormattedString gridLocation = new FormattedString("");
  protected KeyCommand sendCommand;
  protected KeyCommand backCommand;
  protected String description;
  protected String destination;
  protected FormattedString zone = new FormattedString("");
  protected FormattedString region = new FormattedString("");
  protected PropertyExpression propertyFilter = new PropertyExpression("");
  private Map map;

  public SendToLocation() {
    this(ID, null);
  }

  public SendToLocation(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("");
    key = st.nextNamedKeyStroke(null);
    mapId.setFormat(st.nextToken(""));
    boardName.setFormat(st.nextToken(""));
    x.setFormat(st.nextToken("0"));
    y.setFormat(st.nextToken("0"));
    backCommandName = st.nextToken("");
    backKey = st.nextNamedKeyStroke(null);
    xIndex.setFormat(st.nextToken("0"));
    yIndex.setFormat(st.nextToken("0"));
    xOffset.setFormat(st.nextToken("0"));
    yOffset.setFormat(st.nextToken("0"));
    description = st.nextToken("");
    destination = st.nextToken(DEST_LOCATION.substring(0, 1));
    if (destination.length() == 0) {
      destination = DEST_LOCATION.substring(0, 1);
    }
    zone.setFormat(st.nextToken(""));
    region.setFormat(st.nextToken(""));
    propertyFilter.setExpression(st.nextToken(""));
    gridLocation.setFormat(st.nextToken(""));
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName)
      .append(key)
      .append(mapId.getFormat())
      .append(boardName.getFormat())
      .append(x.getFormat())
      .append(y.getFormat())
      .append(backCommandName)
      .append(backKey)
      .append(xIndex.getFormat())
      .append(yIndex.getFormat())
      .append(xOffset.getFormat())
      .append(yOffset.getFormat())
      .append(description)
      .append(destination)
      .append(zone.getFormat())
      .append(region.getFormat())
      .append(propertyFilter.getExpression())
      .append(gridLocation.getFormat());
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      sendCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), this);
      backCommand = new KeyCommand(backCommandName, backKey, Decorator.getOutermost(this), this);
      final ArrayList<KeyCommand> l = new ArrayList<>();
      if (commandName.length() > 0 && key != null && !key.isNull()) {
        l.add(sendCommand);
      }
      if (backCommandName.length() > 0 && backKey != null && !backKey.isNull()) {
        l.add(backCommand);
      }
      command = l.toArray(new KeyCommand[0]);
    }

    for (final KeyCommand c : command) {
      if (c.getName().equals(backCommandName)) {
        c.setEnabled(getMap() != null &&
                     getProperty(BACK_MAP) != null &&
                     getProperty(BACK_POINT) != null);
      }
      else {
        final Point p = getSendLocation();
        c.setEnabled(getMap() != null && p != null &&
            (map != getMap() || !p.equals(getPosition())));
      }
    }
    return command;
  }

  private void LogBadGridLocation(Point p) {
    String s = "* " + Decorator.getOutermost(this).getName();
    if (getMap() == null) {
      s += "getMap is null"; // NON-NLS
    }
    else if (p == null) {
      s += "p is null"; // NON-NLS
    }
    else {
      s += "getMap: " + getMap().getMapName() + // NON-NLS
           "; p: (" + p.x + "," + p.y + // NON-NLS
           "; Position: (" + getPosition().x + "," + getPosition().y + // NON-NLS
           "); map: " + map.getMapName() + ";"; // NON-NLS
    }
    new Chatter.DisplayText(
        GameModule.getGameModule().getChatter(), s).execute();
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    final Map backMap = (Map)getProperty(BACK_MAP);
    if (backMap != null) {
      se.append(backMap.getIdentifier());
    }
    else {
      se.append("");
    }
    final Point backPoint = (Point)getProperty(BACK_POINT);
    if (backPoint != null) {
      se.append(backPoint.x).append(backPoint.y);
    }
    else {
      se.append("").append("");
    }
    return se.getValue();
  }

  private Point getSendLocation() {
    final GamePiece outer = Decorator.getOutermost(this);
    map = null;
    Point dest = null;
    // Home in on a counter
    if (destination.equals(DEST_COUNTER.substring(0, 1))) {
      GamePiece target = null;
      // Find first counter matching the properties
      for (final GamePiece piece :
           GameModule.getGameModule().getGameState().getAllPieces()) {
        if (piece instanceof Stack) {
          final Stack s = (Stack) piece;
          for (final GamePiece gamePiece : s.asList()) {
            if (propertyFilter.accept(outer, gamePiece)) {
              target = gamePiece;
              if (target != null) break;
            }
          }
        }
        else {
          if (propertyFilter.accept(outer, piece)) {
            target = piece;
          }
        }
        if (target != null) break;
      }
      // Determine target's position
      if (target != null) {
        map = target.getMap();
        if (map != null) {
          dest = target.getPosition();
        }
      }
    }
    // Location/Zone/Region processing all use specified map
    else {
      map = Map.getMapById(mapId.getText(outer, this, "Editor.SendToLocation.map"));
      if (map == null) {
        map = getMap();
      }
      if (map != null) {
        final Board b;
        switch (destination.charAt(0)) {
        case 'G':
          b = map.getBoardByName(boardName.getText(outer, this, "Editor.SendToLocation.board"));
          if (b != null) {
            try {
              final MapGrid g = b.getGrid();
              if (g != null) { // Board may not have a grid assigned.
                dest = g.getLocation(gridLocation.getText(outer, this, "Editor.SendToLocation.grid_location"));
                if (dest != null)  dest.translate(b.bounds().x, b.bounds().y);
              }
              else {
                reportDataError(this, Resources.getString("Error.no_grid_assigned"), map.getMapName());
              }
            }
            catch (BadCoords e) {
              LogBadGridLocation(dest);
              reportDataError(this, Resources.getString(
                "Error.not_found", Resources.getString("Editor.SendToLocation.grid_location")), map.getMapName());
              // ignore SendTo request.
            }
          }
          break;
        case 'L':
          final int xValue = x.getTextAsInt(outer, Resources.getString("Editor.x_position"), this);
          final int yValue = y.getTextAsInt(outer, Resources.getString("Editor.y_position"), this);

          dest = new Point(xValue, yValue);

          b = map.getBoardByName(boardName.getText(outer, this, "Editor.SendToLocation.board"));
          if (b != null) {
            dest.translate(b.bounds().x, b.bounds().y);
          }
          break;

        case 'Z':
          final String zoneName = zone.getText(outer, this, "Editor.SendToLocation.zone_name");
          final Zone z = map.findZone(zoneName);
          if (z == null) {
            reportDataError(this, Resources.getString("Error.not_found", "Zone"), zone.debugInfo(zoneName, "Zone")); // NON-NLS
          }
          else {
            final Rectangle r = z.getBounds();
            final Rectangle r2 = z.getBoard().bounds();
            dest = new Point(r2.x + r.x + r.width / 2, r2.y + r.y + r.height / 2);
          }
          break;

        case 'R':
          final String regionName = region.getText(outer, this, "Editor.SendToLocation.region_name");
          final Region r = map.findRegion(regionName);
          if (r == null) {
            reportDataError(this, Resources.getString("Error.not_found", "Region"), region.debugInfo(regionName, "Region")); // NON-NLS
          }
          else {
            final Rectangle r2 = r.getBoard().bounds();
            dest = new Point(r.getOrigin().x + r2.x, r.getOrigin().y + r2.y);
          }
          break;
        }
      }
    }

    // Offset destination by Advanced Options offsets
    if ((dest != null) && (destination.charAt(0) != 'G')) {
      dest = offsetDestination(dest.x, dest.y, outer);
    }

    return dest;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();

    // Get the non-matching keystrokes out of here early so that we can have some common Mat code for the remainder
    final boolean send = sendCommand.matches(stroke);
    if (!send && !backCommand.matches(stroke)) {
      return null;
    }

    final GamePiece outer = Decorator.getOutermost(this);
    final Mat mat;
    List<GamePiece> contents = null;
    List<Point> offsets = null;
    Command c = null;
    Point dest = null;

    // If we're about to move a Mat, establish the initial relative positions of all its "contents"
    if (GameModule.getGameModule().isMatSupport()) {
      final String matName = (String)outer.getProperty(Mat.MAT_NAME);
      if (matName != null && !"".equals(matName)) {
        mat = (Mat) Decorator.getDecorator(outer, Mat.class);
        if (mat != null) {
          final Point basePt = outer.getPosition();
          contents = mat.getContents();
          offsets = mat.getOffsets(basePt.x, basePt.y);
        }
      }
    }

    if (send) {
      final Stack parent = outer.getParent();
      dest = getSendLocation();
      if (map != null && dest != null) {
        if (map == getMap() && dest.equals(getPosition())) {
          // don't do anything if we're already there.
          return null;
        }
        final ChangeTracker tracker = new ChangeTracker(this);
        setProperty(BACK_MAP, getMap());
        setProperty(BACK_POINT, getPosition());

        // Mark moved and generate movement trail if compatibility pref turned on
        if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(GlobalOptions.SEND_TO_LOCATION_MOVEMENT_TRAILS))) {
          outer.setProperty(Properties.MOVED, Boolean.TRUE);
        }

        c = tracker.getChangeCommand();
        c = c.append(putOldProperties(this));
        if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
          dest = map.snapTo(dest);
        }
        c = c.append(map.placeOrMerge(outer, dest));
        // Apply Auto-move key
        if (map.getMoveKey() != null) {
          c = c.append(outer.keyEvent(map.getMoveKey()));
        }
        if (parent != null) {
          c = c.append(parent.pieceRemoved(outer));
        }

      }
    }
    else {
      final Map backMap = (Map) getProperty(BACK_MAP);
      final Point backPoint = (Point) getProperty(BACK_POINT);
      final ChangeTracker tracker = new ChangeTracker(this);
      setProperty(BACK_MAP, null);
      setProperty(BACK_POINT, null);

      // Mark moved and generate movement trail if compatibility pref turned on
      if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(GlobalOptions.SEND_TO_LOCATION_MOVEMENT_TRAILS))) {
        outer.setProperty(Properties.MOVED, Boolean.TRUE);
      }

      c = tracker.getChangeCommand();

      if (backMap != null && backPoint != null) {
        c = c.append(putOldProperties(this));
        c = c.append(backMap.placeOrMerge(outer, backPoint));
        dest = backPoint;

        // Apply Auto-move key
        if (backMap.getMoveKey() != null) {
          c = c.append(outer.keyEvent(backMap.getMoveKey()));
        }
      }
    }

    // Mat support
    if ((c != null) && GameModule.getGameModule().isMatSupport()) {

      // If a cargo piece has been "sent", find it a new Mat if needed.
      if (Boolean.TRUE.equals(outer.getProperty(MatCargo.IS_CARGO))) { //NON-NLS
        final MatCargo cargo = (MatCargo) Decorator.getDecorator(outer, MatCargo.class);
        if (cargo != null) {
          c = c.append(cargo.findNewMat());
        }
      }

      // If a Mat has been sent, send all its contents, at an appropriate offset.
      if ((offsets != null) && dest != null) {
        for (int i = 0; i < contents.size(); i++) {
          final GamePiece piece = contents.get(i);
          final Stack pieceParent = piece.getParent();
          final MatCargo cargo = (MatCargo) Decorator.getDecorator(piece, MatCargo.class);
          if (cargo != null) {

            // Get Cargo's pre-move offset from the Mat
            final Point pt = new Point(dest);
            pt.x += offsets.get(i).x;
            pt.y += offsets.get(i).y;

            // From here down we're just duplicating a send-to-location command for the Cargo piece

            final ChangeTracker tracker = new ChangeTracker(piece);
            // Mark moved and generate movement trail if compatibility pref turned on
            if (Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(GlobalOptions.SEND_TO_LOCATION_MOVEMENT_TRAILS))) {
              piece.setProperty(Properties.MOVED, Boolean.TRUE);
            }

            c = tracker.getChangeCommand();
            c = c.append(putOldProperties(piece));

            //BR// I sort of think cargo shouldn't snap when moving in lockstep with its mat.
            //BR// This may lead to the eventual conclusion that cargo pieces shouldn't snap
            //BR// even when dragged, IF they land on an eligible Mat.
            //if (!Boolean.TRUE.equals(piece.getProperty(Properties.IGNORE_GRID))) {
            //  dest = map.snapTo(dest);
            //}

            c = c.append(map.placeOrMerge(piece, pt));
            // Apply Auto-move key
            if (map.getMoveKey() != null) {
              c = c.append(piece.keyEvent(map.getMoveKey()));
            }
            if (pieceParent != null) {
              c = c.append(pieceParent.pieceRemoved(piece));
            }
          }
        }
      }
    }

    return c;
  }

  /*
   * Offset the destination by the Advanced Options offset
   */
  protected Point offsetDestination(int x, int y, GamePiece outer) {
    final int xPos = x + parse("xIndex", xIndex, outer) * parse("xOffset", xOffset, outer); // NON-NLS
    final int yPos = y + parse("yIndex", yIndex, outer) * parse("yOffset", yOffset, outer); // NON-NLS
    return new Point(xPos, yPos);
  }

  private int parse(String desc, FormattedString s, GamePiece outer) {
    int i = 0;
    // Explicitly create an AuditTrail as we have a description, not a message key (both Strings)
    final String val = s.getText(outer, _0, this, AuditTrail.create(this, s.getFormat(), desc));
    try {
      i = Integer.parseInt(val);
    }
    catch (NumberFormatException e) {
      reportDataError(this, Resources.getString("Error.non_number_error"), s.debugInfo(val, desc), e);
    }
    return i;
  }

  @Override
  public void mySetState(String newState) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, ';');
    final String mapId = st.nextToken("");
    if (mapId.length() > 0) {
      setProperty(BACK_MAP, Map.getMapById(mapId));
    }
    final String x = st.nextToken("");
    final String y = st.nextToken("");
    if (x.length() > 0 && y.length() > 0) {
      try {
        setProperty(BACK_POINT, new Point(Integer.parseInt(x), Integer.parseInt(y)));
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"),
          "Back Point=(" + x + "," + y + ")", e); // NON-NLS
      }
    }
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public String getDescription() {
    String d = Resources.getString("Editor.SendToLocation.trait_description");
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SendToLocation.html"); // NON-NLS
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {commandName, backCommandName},
                       new String[] {getCommandDescription(description, Resources.getString("Editor.SendToLocation.send_command_description")),
                                     getCommandDescription(description, Resources.getString("Editor.SendToLocation.back_command_description"))});
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof SendToLocation)) return false;
    final SendToLocation c = (SendToLocation) o;

    if (! Objects.equals(commandName, c.commandName)) return false;
    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(mapId, c.mapId)) return false;
    if (! Objects.equals(boardName, c.boardName)) return false;
    if (! Objects.equals(x, c.x)) return false;
    if (! Objects.equals(y, c.y)) return false;
    if (! Objects.equals(backCommandName, c.backCommandName)) return false;
    if (! Objects.equals(backKey, c.backKey)) return false;
    if (! Objects.equals(xIndex, c.xIndex)) return false;
    if (! Objects.equals(yIndex, c.yIndex)) return false;
    if (! Objects.equals(xOffset, c.xOffset)) return false;
    if (! Objects.equals(yOffset, c.yOffset)) return false;
    if (! Objects.equals(description, c.description)) return false;
    if (! Objects.equals(destination, c.destination)) return false;
    if (! Objects.equals(zone, c.zone)) return false;
    if (! Objects.equals(region, c.region)) return false;
    if (! Objects.equals(propertyFilter, c.propertyFilter)) return false;
    if (! Objects.equals(gridLocation, c.gridLocation)) return false;

    final Map backMap = (Map) getProperty(BACK_MAP);
    final String id1 = backMap == null ? "" : backMap.getIdentifier();
    final Map backMapc = (Map) c.getProperty(BACK_MAP);
    final String id2 = backMapc == null ? "" : backMapc.getIdentifier();
    if (! Objects.equals(id1, id2)) return false;

    final Point bp1 = (Point) getProperty(BACK_POINT);
    final Point bp2 = (Point) c.getProperty(BACK_POINT);

    return Objects.equals(bp1, bp2);

  }

  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected StringConfigurer backNameInput;
    protected NamedHotKeyConfigurer keyInput;
    protected NamedHotKeyConfigurer backKeyInput;
    protected FormattedStringConfigurer mapIdInput;
    protected FormattedStringConfigurer boardNameInput;
    protected JLabel xInputLabel;
    protected FormattedStringConfigurer xInput;
    protected JLabel yInputLabel;
    protected FormattedStringConfigurer yInput;
    @Deprecated(since = "2020-12-11", forRemoval = true)
    protected JLabel advancedLabel;
    @Deprecated(since = "2020-12-11", forRemoval = true)
    protected BooleanConfigurer advancedInput;
    protected JLabel xAdvancedLabel;
    protected JLabel yAdvancedLabel;
    protected FormattedStringConfigurer xIndexInput;
    protected FormattedStringConfigurer xOffsetInput;
    protected FormattedStringConfigurer yIndexInput;
    protected FormattedStringConfigurer yOffsetInput;
    protected StringConfigurer descInput;
    protected StringEnumConfigurer destInput;
    protected TranslatingStringEnumConfigurer newDestInput;
    protected JLabel propertyLabel;
    protected PropertyExpressionConfigurer propertyInput;
    protected JLabel zoneLabel;
    protected FormattedStringConfigurer zoneInput;
    protected JLabel regionLabel;
    protected FormattedStringConfigurer regionInput;
    protected TraitConfigPanel controls;
    protected JLabel mapLabel;
    protected JPanel mapControls;
    protected JLabel boardLabel;
    protected JPanel boardControls;
    protected JPanel advancedControls;
    protected JPanel yAdvancedControls;
    protected JLabel gridLabel;
    protected StringConfigurer gridLocationInput;

    public Ed(SendToLocation p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      nameInput = new StringConfigurer(p.commandName);
      nameInput.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", nameInput);

      keyInput = new NamedHotKeyConfigurer(p.key);
      controls.add("Editor.keyboard_command", keyInput);

      backNameInput = new StringConfigurer(p.backCommandName);
      backNameInput.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.SendToLocation.send_back_command_name", backNameInput);

      backKeyInput = new NamedHotKeyConfigurer(p.backKey);
      controls.add("Editor.SendToLocation.send_back_keyboard_command", backKeyInput);

      newDestInput = new TranslatingStringEnumConfigurer(DEST_OPTIONS, DEST_KEYS);
      newDestInput.setValue(DEST_LOCATION);
      for (final String destOption : DEST_OPTIONS) {
        if (destOption.substring(0, 1).equals(p.destination)) {
          newDestInput.setValue(destOption);
        }
      }
      newDestInput.addPropertyChangeListener(arg0 -> updateVisibility());
      controls.add("Editor.SendToLocation.destination", newDestInput);

      mapLabel = new JLabel(Resources.getString("Editor.SendToLocation.map"));
      mapControls = new JPanel(new MigLayout("ins 0", "[grow,fill]rel[]rel[]")); // NON-NLS
      final JPanel mapPanel = new JPanel(new MigLayout("ins 0", "[grow]", "push[]push")); // NON-NLS
      mapIdInput = new FormattedExpressionConfigurer(p.mapId.getFormat(), p);
      mapIdInput.setHintKey("Editor.SendToLocation.map_hint");
      mapPanel.add(mapIdInput.getControls(), "grow"); // NON-NLS
      mapControls.add(mapPanel, "grow"); // NON-NLS
      JButton select = new JButton(Resources.getString("Editor.select"));
      select.addActionListener(e -> selectMap());
      mapControls.add(select);
      JButton clear = new JButton(Resources.getString("Editor.clear"));
      clear.addActionListener(e -> clearMap());
      mapControls.add(clear);
      controls.add(mapLabel, mapControls);

      boardLabel = new JLabel(Resources.getString("Editor.SendToLocation.board"));
      boardControls = new JPanel(new MigLayout("ins 0", "[grow,fill]rel[]rel[]")); // NON-NLS
      boardNameInput = new FormattedExpressionConfigurer(p.boardName.getFormat(), p);
      boardNameInput.setHintKey("Editor.SendToLocation.board_hint");
      final JPanel boardPanel = new JPanel(new MigLayout("ins 0", "[grow]", "push[]push")); // NON-NLS
      boardPanel.add(boardNameInput.getControls(), "grow"); // NON-NLS
      boardControls.add(boardPanel, "grow"); // NON-NLS
      select = new JButton(Resources.getString("Editor.select"));
      select.addActionListener(e -> selectBoard());
      clear = new JButton(Resources.getString("Editor.clear"));
      clear.addActionListener(e -> clearBoard());
      boardControls.add(select);
      boardControls.add(clear);
      controls.add(boardLabel, boardControls);

      xInputLabel = new JLabel(Resources.getString("Editor.x_position"));
      xInput = new FormattedExpressionConfigurer(p.x.getFormat(), p);
      controls.add(xInputLabel, xInput);

      yInputLabel = new JLabel(Resources.getString("Editor.y_position"));
      yInput = new FormattedExpressionConfigurer(p.y.getFormat(), p);
      controls.add(yInputLabel, yInput);

      zoneLabel = new JLabel(Resources.getString("Editor.SendToLocation.zone_name"));
      zoneInput = new FormattedExpressionConfigurer(p.zone.getFormat(), p);
      zoneInput.setHintKey("Editor.SendToLocation.zone_hint");
      controls.add(zoneLabel, zoneInput);

      regionLabel = new JLabel(Resources.getString("Editor.SendToLocation.region_name"));
      regionInput = new FormattedExpressionConfigurer(p.region.getFormat(), p);
      regionInput.setHintKey("Editor.SendToLocation.region_hint");
      controls.add(regionLabel, regionInput);

      propertyLabel = new JLabel(Resources.getString("Editor.property_match_label"));
      propertyInput = new PropertyExpressionConfigurer(p.propertyFilter);
      controls.add(propertyLabel, propertyInput);

      gridLabel = new JLabel(Resources.getString("Editor.SendToLocation.grid_location"));
      gridLocationInput = new StringConfigurer(p.gridLocation.getFormat());
      gridLocationInput.setHintKey("Editor.SendToLocation.grid_location_hint");
      controls.add(gridLabel, gridLocationInput);

      xAdvancedLabel = new JLabel(Resources.getString("Editor.SendToLocation.additional_x_offset"));
      advancedControls = new JPanel(new MigLayout("ins 0", "[grow,fill]rel[]rel[grow,fill]")); // NON-NLS
      xIndexInput = new FormattedExpressionConfigurer(p.xIndex.getFormat(), p);
      advancedControls.add(xIndexInput.getControls(), "grow"); // NON-NLS
      advancedControls.add(new JLabel(Resources.getString("Editor.SendToLocation.times")));
      xOffsetInput = new FormattedExpressionConfigurer(p.xOffset.getFormat(), p);
      advancedControls.add(xOffsetInput.getControls(), "grow"); // NON-NLS
      controls.add(xAdvancedLabel, advancedControls);

      yAdvancedLabel = new JLabel(Resources.getString("Editor.SendToLocation.additional_y_offset"));
      yAdvancedControls = new JPanel(new MigLayout("ins 0", "[grow,fill]rel[]rel[grow,fill]")); // NON-NLS
      yIndexInput = new FormattedExpressionConfigurer(p.yIndex.getFormat(), p);
      yAdvancedControls.add(yIndexInput.getControls(), "grow"); // NON-NLS
      yAdvancedControls.add(new JLabel(Resources.getString("Editor.SendToLocation.times")));
      yOffsetInput = new FormattedExpressionConfigurer(p.yOffset.getFormat(), p);
      yAdvancedControls.add(yOffsetInput.getControls(), "grow"); // NON-NLS
      controls.add(yAdvancedLabel, yAdvancedControls);

      updateVisibility();
    }

    private void updateVisibility() {

      final String destOption = newDestInput.getValueString();

      xInput.getControls().setVisible(destOption.equals(DEST_LOCATION));
      xInputLabel.setVisible(destOption.equals(DEST_LOCATION));
      yInput.getControls().setVisible(destOption.equals(DEST_LOCATION));
      yInputLabel.setVisible(destOption.equals(DEST_LOCATION));

      mapControls.setVisible(!destOption.equals(DEST_COUNTER));
      mapLabel.setVisible(!destOption.equals(DEST_COUNTER));

      boardControls.setVisible(destOption.equals(DEST_LOCATION) || destOption.equals(DEST_GRIDLOCATION));
      boardLabel.setVisible(destOption.equals(DEST_LOCATION) || destOption.equals(DEST_GRIDLOCATION));

      zoneInput.getControls().setVisible(destOption.equals(DEST_ZONE));
      zoneLabel.setVisible(destOption.equals(DEST_ZONE));

      regionInput.getControls().setVisible(destOption.equals(DEST_REGION));
      regionLabel.setVisible(destOption.equals(DEST_REGION));

      propertyInput.getControls().setVisible(destOption.equals(DEST_COUNTER));
      propertyLabel.setVisible(destOption.equals(DEST_COUNTER));

      gridLocationInput.getControls().setVisible(destOption.equals(DEST_GRIDLOCATION));
      gridLabel.setVisible(destOption.equals(DEST_GRIDLOCATION));

      repack(controls);
    }

    private void clearBoard() {
      boardNameInput.setValue("");
    }

    private void clearMap() {
      //map = null;
      mapIdInput.setValue("");
    }

    private void selectBoard() {
      final ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), Board.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        final Board b = (Board) d.getTarget();
        boardNameInput.setValue(b.getName());
      }
    }

    private void selectMap() {
      final ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), Map.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        final Map map = (Map) d.getTarget();
        mapIdInput.setValue(map.getMapName());
      }
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
        .append(keyInput.getValueString())
        .append(mapIdInput.getValueString())
        .append(boardNameInput.getValueString())
        .append(xInput.getValueString())
        .append(yInput.getValueString())
        .append(backNameInput.getValueString())
        .append(backKeyInput.getValueString())
        .append(xIndexInput.getValueString())
        .append(yIndexInput.getValueString())
        .append(xOffsetInput.getValueString())
        .append(yOffsetInput.getValueString())
        .append(descInput.getValueString())
        .append(newDestInput.getValueString().charAt(0))
        .append(zoneInput.getValueString())
        .append(regionInput.getValueString())
        .append(propertyInput.getValueString())
        .append(gridLocationInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }


  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final ArrayList<String> l = new ArrayList<>();

    if (destination.equals(DEST_COUNTER.substring(0, 1))) {
      l.add(propertyFilter.getExpression());
    }
    else {
      l.add(mapId.getFormat());
      switch (destination.charAt(0)) {
      case 'G':
        l.add(boardName.getFormat());
        l.add(gridLocation.getFormat());
        break;
      case 'L':
        l.add(boardName.getFormat());
        l.add(x.getFormat());
        l.add(y.getFormat());
        break;
      case 'Z':
        l.add(zone.getFormat());
        break;
      case 'R':
        l.add(region.getFormat());
        break;
      }
    }
    return l;
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.singletonList(key);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(commandName);
  }
}
