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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.build.module.map.boardPicker.board.Region;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.PropertySource;
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
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.AuditableException;
import VASSAL.script.expression.FormattedStringExpression;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import net.miginfocom.swing.MigLayout;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
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

/**
 * This trait adds a command that sends a piece to another location. Options for the
 * target location are:
 * <li>Specified x,y co-ords on a named map/board</li>
 * <li>The centre of a named Zone on a named map</li>
 * <li>A named Region on a named map</li>
 * <li>The location of another counter selected by a Property Match String</li>
 * <li>A specified grid-location on a given board and map </li>
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
  public static final String DEST_GRIDLOCATION = "G"; // NON-NLS
  public static final String DEST_LOCATION = "L"; // NON-NLS
  public static final String DEST_ZONE = "Z"; // NON-NLS
  public static final String DEST_REGION = "R"; // NON-NLS
  public static final String DEST_COUNTER = "A"; // NON-NLS
  public static final String DEST_COUNTER_CYCLE = "C"; //NON-NLS
  public static final String DEST_COUNTER_NEAREST = "N"; //NON-NLS
  public static final String[] DEST_OPTIONS = {
    DEST_GRIDLOCATION, DEST_LOCATION, DEST_ZONE, DEST_REGION, DEST_COUNTER, DEST_COUNTER_CYCLE, DEST_COUNTER_NEAREST
  };
  // Actual valued recorded for Destination option
  public static final String[] DEST_KEYS = {
    "Editor.SendToLocation.grid_location_on_selected_map",
    "Editor.SendToLocation.location_on_selected_map",
    "Editor.SendToLocation.zone_on_selected_map",
    "Editor.SendToLocation.region_on_selected_map",
    "Editor.SendToLocation.another_counter_selected_by_properties_any",
    "Editor.SendToLocation.another_counter_selected_by_properties_cycle",
    "Editor.SendToLocation.another_counter_selected_by_properties_nearest"
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
    xOffset.setFormat(st.nextToken("1")); //BR// Better defaults
    yOffset.setFormat(st.nextToken("1"));
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
      command = l.toArray(KeyCommand.NONE);
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

  public static class Destination {
    public Map map;
    public Point point;

    Destination(Map map, Point point) {
      this.map = map;
      this.point = point;
    }
  }

  /**
   * Compute a destination from the parameters
   * @param source The property source for any expressions or property filters (e.g. the outer piece in the case of a Send To Location, the map in the case of a MoveCameraButton)
   * @param auditSource Our audit type in case something goes wrong (e.g. this specific STL trait, or the specific MCB component)
   * @param destination Destination type (send-to-XYlocation? send-to-zone? send-to-grid-location? send-to property matched counter? etc)
   * @param mapId Map Id, or null to use sourceMap
   * @param boardName BoardName expression
   * @param zone Zone expression
   * @param region Region expression
   * @param gridLocation gridLocation expression
   * @param x X Position expression
   * @param y Y Position expression
   * @param propertyFilter Property filter to be used if we're selecting a destination counter based on one
   * @param sourceMap The source map if we're matching a counter by property filter expression (and used as the map for all purposes if mapId is null)
   * @param startPosition Starting position to base any "nearest piece" calculation from
   * @return Destination object (includes map and point)
   */
  public static Destination getSendLocation(PropertySource source, Auditable auditSource, String destination, FormattedString mapId, FormattedString boardName, FormattedString zone, FormattedString region, FormattedString gridLocation, FormattedString x, FormattedString y, PropertyExpression propertyFilter, Map sourceMap, Point startPosition) {
    Map map = null;
    Point dest = null;
    // Home in on a counter
    if (destination.equals(DEST_COUNTER.substring(0, 1)) || destination.equals(DEST_COUNTER_CYCLE.substring(0, 1)) || destination.equals(DEST_COUNTER_NEAREST.substring(0, 1))) {
      final List<GamePiece> targets = new ArrayList<>();
      // Find first counters matching the properties
      for (final GamePiece piece :
           GameModule.getGameModule().getGameState().getAllPieces()) {
        if (piece instanceof Stack) {
          final Stack s = (Stack) piece;
          for (final GamePiece gamePiece : s.asList()) {
            if (propertyFilter.accept(source, gamePiece) && !targets.contains(gamePiece)) {
              targets.add(gamePiece);
              if (destination.equals(DEST_COUNTER.substring(0, 1))) {
                break; // "Any match" version just takes first hit
              }
            }
          }
          if (destination.equals(DEST_COUNTER.substring(0, 1)) && !targets.isEmpty()) {
            break; // "Any match" version just takes first hit
          }
        }
        else {
          if (propertyFilter.accept(source, piece) && !targets.contains(piece)) {
            targets.add(piece);
            if (destination.equals(DEST_COUNTER.substring(0, 1))) {
              break; // "Any match" version just takes first hit
            }
          }
        }
      }

      if (!targets.isEmpty()) {
        GamePiece target = null;

        if (destination.equals(DEST_COUNTER.substring(0, 1))) {
          // "Any Match" version just takes the first hit
          target = targets.get(0);
        }
        else {
          // Now figure out which one we're currently nearest to
          double dist = Double.MAX_VALUE;
          int startIndex = 0;
          for (int index = 0; index < targets.size(); index++) {
            if (targets.get(index).getMap() != sourceMap) {
              continue;
            }
            final double myDist = targets.get(index).getPosition().distance(startPosition);
            if (myDist >= dist) {
              continue;
            }
            dist = myDist;
            startIndex = index;
          }

          if (destination.equals(DEST_COUNTER_CYCLE.substring(0, 1))) {
            // Cycling Match version - search through our list of targets starting with the *next* one after the
            // one we're nearest to, but looping through them all. Pick the first target in the list. This means
            // that if there are multiple targets, successively executing this command will cycle us to each of
            // the valid targets.
            for (int counter = 0; counter < targets.size(); counter++) {
              final int index = (startIndex + counter + 1) % targets.size();
              if ((targets.get(index).getMap() != sourceMap) ||
                (!startPosition.equals(targets.get(index).getPosition()))) {
                target = targets.get(index);
                break;
              }
            }
          }
          else {
            // Nearest Match version - we just take the one that was nearest
            target = targets.get(startIndex);
          }
        }

        // Determine target's position
        if (target != null) {
          map = target.getMap();
          if (map != null) {
            dest = target.getPosition();
          }
        }
      }
    }
    // Location/Zone/Region processing all use specified map
    else {
      if (mapId != null) {
        map = Map.getMapById(mapId.getText(source, auditSource, "Editor.SendToLocation.map"));
      }
      if (map == null) {
        map = sourceMap;
      }
      if (map != null) {
        final Board b;
        switch (destination.charAt(0)) {
        case 'G':
          b = map.getBoardByName(boardName.getText(source, auditSource, "Editor.SendToLocation.board"));
          if (b != null) {
            final AuditTrail gridAudit = AuditTrail.create(auditSource, gridLocation, Resources.getString("Editor.SendToLocation.grid_location"));
            try {
              final MapGrid g = b.getGrid();
              if (g != null) { // Board may not have a grid assigned.
                dest = g.getLocation(gridLocation.getText(source, auditSource, gridAudit));
                if (dest != null)  dest.translate(b.bounds().x, b.bounds().y);
              }
              else {
                if (auditSource instanceof EditablePiece) {
                  reportDataError((EditablePiece)auditSource, Resources.getString("Error.no_grid_assigned"), map.getMapName(), new AuditableException(auditSource, gridAudit));
                }
                else if (auditSource instanceof AbstractConfigurable) {
                  ErrorDialog.dataWarning(new BadDataReport((AbstractConfigurable)auditSource, Resources.getString("Error.no_grid_assigned"), map.getMapName(), new AuditableException(auditSource, gridAudit)));
                }
              }
            }
            catch (BadCoords e) {
              if (auditSource instanceof EditablePiece) {
                reportDataError((EditablePiece)auditSource, Resources.getString(
                  "Error.not_found", Resources.getString("Editor.SendToLocation.grid_location")), map.getMapName(), new AuditableException(auditSource, gridAudit));
              }
              else if (auditSource instanceof AbstractConfigurable) {
                ErrorDialog.dataWarning(new BadDataReport(Resources.getString(
                  "Error.not_found", Resources.getString("Editor.SendToLocation.grid_location")), map.getMapName(), new AuditableException(auditSource, gridAudit)));
              }
              // ignore SendTo request.
            }
          }
          break;
        case 'L':
          final int xValue;
          final int yValue;
          if (auditSource instanceof EditablePiece) {
            xValue = x.getTextAsInt(source, Resources.getString("Editor.x_position"), (EditablePiece)auditSource);
            yValue = y.getTextAsInt(source, Resources.getString("Editor.y_position"), (EditablePiece)auditSource);
          }
          else if (auditSource instanceof AbstractConfigurable) {
            xValue = x.getTextAsInt(source, Resources.getString("Editor.x_position"), (AbstractConfigurable) auditSource);
            yValue = y.getTextAsInt(source, Resources.getString("Editor.y_position"), (AbstractConfigurable) auditSource);
          }
          else {
            xValue = 0;
            yValue = 0;
          }

          dest = new Point(xValue, yValue);

          b = map.getBoardByName(boardName.getText(source, auditSource, "Editor.SendToLocation.board"));
          if (b != null) {
            dest.translate(b.bounds().x, b.bounds().y);
          }
          break;

        case 'Z':
          final AuditTrail zoneAudit = AuditTrail.create(auditSource, zone, Resources.getString("Editor.SendToLocation.zone_name"));
          final String zoneName = zone.getText(source, auditSource, zoneAudit);
          final Zone z = map.findZone(zoneName);
          if (z == null) {
            if (auditSource instanceof EditablePiece) {
              reportDataError((EditablePiece)auditSource, Resources.getString("Error.not_found", "Zone"), zone.debugInfo(zoneName, "Zone"), new AuditableException(auditSource, zoneAudit)); // NON-NLS
            }
            else if (auditSource instanceof AbstractConfigurable) {
              ErrorDialog.dataWarning(new BadDataReport((AbstractConfigurable)auditSource, Resources.getString("Error.not_found", "Zone"), zone.debugInfo(zoneName, "Zone"), new AuditableException(auditSource, zoneAudit))); // NON-NLS
            }
          }
          else {
            final Rectangle r = z.getBounds();
            final Rectangle r2 = z.getBoard().bounds();
            dest = new Point(r2.x + r.x + r.width / 2, r2.y + r.y + r.height / 2);
          }
          break;

        case 'R':
          final AuditTrail regionAudit = AuditTrail.create(auditSource, region, Resources.getString("Editor.SendToLocation.region_name"));
          final String regionName = region.getText(source, auditSource, regionAudit);
          final Region r = map.findRegion(regionName);
          if (r == null) {
            if (auditSource instanceof EditablePiece) {
              reportDataError((EditablePiece) auditSource, Resources.getString("Error.not_found", "Region"), region.debugInfo(regionName, "Region"), new AuditableException(auditSource, regionAudit)); // NON-NLS
            }
            else if (auditSource instanceof AbstractConfigurable) {
              ErrorDialog.dataWarning(new BadDataReport((AbstractConfigurable)auditSource, Resources.getString("Error.not_found", "Zone"), region.debugInfo(regionName, "Zone"), new AuditableException(auditSource, regionAudit))); // NON-NLS
            }
          }
          else {
            final Rectangle r2 = r.getBoard().bounds();
            dest = new Point(r.getOrigin().x + r2.x, r.getOrigin().y + r2.y);
          }
          break;
        }
      }
    }

    return new Destination(map, dest);
  }


  private Point getSendLocation() {
    final GamePiece outer = Decorator.getOutermost(this);

    // Do pre-evaluation of $...$ expressions for source/target matching
    final FormattedStringExpression props = new FormattedStringExpression(propertyFilter.getExpression());
    final PropertyExpression pf = new PropertyExpression(props.tryEvaluate(outer, this, "Editor.SendToLocation.getSendLocation")); //NON-NLS

    final Destination dest = getSendLocation(outer, this, destination, mapId, boardName, zone, region, gridLocation, x, y, pf, getMap(), getPosition());
    map = dest.map;

    // Offset destination by Advanced Options offsets
    if (dest.point != null) {
      dest.point = offsetDestination(dest.point.x, dest.point.y, outer);
    }

    return dest.point;
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
    final Map oldMap = outer.getMap();

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
      dest = getSendLocation();
      if (map != null && dest != null) {
        if (map == getMap() && dest.equals(getPosition())) {
          // don't do anything if we're already there.
          return null;
        }
        final ChangeTracker tracker = new ChangeTracker(this);
        setProperty(BACK_MAP, getMap());
        setProperty(BACK_POINT, getPosition());
        c = tracker.getChangeCommand();

        // Prepare piece for move, setting "old location" properties. Mark moved and generate movement trail if global options setting is on
        c = prepareMove(c, GlobalOptions.getInstance().isSendToLocationMoveTrails());

        if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
          dest = map.snapTo(dest);
        }
        c = c.append(map.placeOrMerge(outer, dest));

        // The map field might change or even become null through the apply-key processes below, so we schedule our repaints now based on our current information.
        if (oldMap != null && oldMap != map) {
          oldMap.repaint();
        }
        map.repaint();

        // Complete the move, finding new mat if needed and applying map afterburner key
        c = finishMove(c, true, true, GlobalOptions.getInstance().isSendToLocationMoveTrails());
      }
    }
    else {
      final Map backMap = (Map) getProperty(BACK_MAP);
      final Point backPoint = (Point) getProperty(BACK_POINT);
      final ChangeTracker tracker = new ChangeTracker(this);
      setProperty(BACK_MAP, null);
      setProperty(BACK_POINT, null);
      c = tracker.getChangeCommand();

      if (backMap != null && backPoint != null) {
        // Prepare piece for move, setting "old location" properties. Mark moved and generate movement trail if global options setting is on
        c = prepareMove(c, GlobalOptions.getInstance().isSendToLocationMoveTrails());

        c = c.append(backMap.placeOrMerge(outer, backPoint));
        dest = backPoint;

        // Complete the move, finding new mat if needed and applying map afterburner key
        c = finishMove(c, true, true, GlobalOptions.getInstance().isSendToLocationMoveTrails());

        if (oldMap != null && oldMap != backMap) {
          oldMap.repaint();
        }
        backMap.repaint();
      }
    }

    // Mat support
    if ((c != null) && GameModule.getGameModule().isMatSupport()) {
      // If a Mat has been sent, send all its contents, at an appropriate offset.
      if ((offsets != null) && dest != null) {
        for (int i = 0; i < contents.size(); i++) {
          final GamePiece piece = contents.get(i);
          final MatCargo cargo = (MatCargo) Decorator.getDecorator(piece, MatCargo.class);
          if (cargo != null) {

            // Get Cargo's pre-move offset from the Mat
            final Point pt = new Point(dest);
            pt.x += offsets.get(i).x;
            pt.y += offsets.get(i).y;

            // From here down we're basically just duplicating a send-to-location command for the Cargo piece

            // Prepare piece for move, setting "old location" properties. Mark moved and generate movement trail if global options setting is on
            c = piece.prepareMove(c, GlobalOptions.getInstance().isSendToLocationMoveTrails());

            //BR// I sort of think cargo shouldn't snap when moving in lockstep with its mat.
            //BR// This may lead to the eventual conclusion that cargo pieces shouldn't snap
            //BR// even when dragged, IF they land on an eligible Mat.
            //if (!Boolean.TRUE.equals(piece.getProperty(Properties.IGNORE_GRID))) {
            //  dest = map.snapTo(dest);
            //}

            c = c.append(map.placeOrMerge(piece, pt));

            // Complete the move, applying map afterburner key
            c = piece.finishMove(c, true, false, GlobalOptions.getInstance().isSendToLocationMoveTrails());
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
    d += getCommandDesc(commandName, key);
    return d;
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.SendToLocation.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return description;
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
      propertyInput = new PropertyExpressionConfigurer(p.propertyFilter, p);
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

      final boolean destCounter = (destOption.equals(DEST_COUNTER) || destOption.equals(DEST_COUNTER_CYCLE) || destOption.equals(DEST_COUNTER_NEAREST));
      mapControls.setVisible(!destCounter);
      mapLabel.setVisible(!destCounter);

      boardControls.setVisible(destOption.equals(DEST_LOCATION) || destOption.equals(DEST_GRIDLOCATION));
      boardLabel.setVisible(destOption.equals(DEST_LOCATION) || destOption.equals(DEST_GRIDLOCATION));

      zoneInput.getControls().setVisible(destOption.equals(DEST_ZONE));
      zoneLabel.setVisible(destOption.equals(DEST_ZONE));

      regionInput.getControls().setVisible(destOption.equals(DEST_REGION));
      regionLabel.setVisible(destOption.equals(DEST_REGION));

      propertyInput.getControls().setVisible(destCounter);
      propertyLabel.setVisible(destCounter);

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

    if (destination.equals(DEST_COUNTER.substring(0, 1)) || destination.equals(DEST_COUNTER_CYCLE.substring(0, 1)) || destination.equals(DEST_COUNTER_NEAREST.substring(0, 1))) {
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
