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

import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
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
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
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
  public static final String ID = "sendto;";
  private static final String _0 = "0";
  public static final String BACK_MAP = "backMap";
  public static final String BACK_POINT = "backPoint";
  protected static final String DEST_GRIDLOCATION = "Grid location on selected Map";
  protected static final String DEST_LOCATION = "Location on selected Map";
  protected static final String DEST_ZONE = "Zone on selected Map";
  protected static final String DEST_REGION = "Region on selected Map";
  protected static final String DEST_COUNTER = "Another counter, selected by properties";
  protected static final String[] DEST_OPTIONS = {
    DEST_GRIDLOCATION, DEST_LOCATION, DEST_ZONE, DEST_REGION, DEST_COUNTER
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
 // private Point dest;

  public SendToLocation() {
    this(ID + ";;;;0;0;;;", null);
  }

  public SendToLocation(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
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
    destination = st.nextToken(DEST_LOCATION.substring(0,1));
    if (destination.length() == 0) {
      destination = DEST_LOCATION.substring(0,1);
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
      ArrayList<KeyCommand> l = new ArrayList<>();
      if (commandName.length() > 0 && key != null && !key.isNull()) {
        l.add(sendCommand);
      }
      if (backCommandName.length() > 0 && backKey != null && !backKey.isNull()) {
        l.add(backCommand);
      }
      command = l.toArray(new KeyCommand[0]);
    }

    for (KeyCommand c : command) {
      if (c.getName().equals(backCommandName)) {
        c.setEnabled(getMap() != null &&
                     getProperty(BACK_MAP) != null &&
                     getProperty(BACK_POINT) != null);
      }
      else {
        Point p = getSendLocation();
        c.setEnabled(getMap() != null && p != null &&
            (map != getMap() || !p.equals(getPosition())) );
      }
    }
    return command;
  }

  private void LogBadGridLocation(Point p) {
    String s = "* " + Decorator.getOutermost(this).getName();
    if (getMap() == null) {
      s += "getMap is null";
    }
    else if ( p == null) {
      s += "p is null";
    }
    else {
      s += "getMap: " + getMap().getMapName() +
           "; p: (" + p.x + "," + p.y +
           "; Position: (" + getPosition().x + "," + getPosition().y +
           "); map: " + map.getMapName() + ";";
    }
    new Chatter.DisplayText(
        GameModule.getGameModule().getChatter(),s).execute();
  }

  @Override
  public String myGetState() {
    SequenceEncoder se = new SequenceEncoder(';');
    Map backMap = (Map)getProperty(BACK_MAP);
    if (backMap != null) {
      se.append(backMap.getIdentifier());
    }
    else {
      se.append("");
    }
    Point backPoint = (Point)getProperty(BACK_POINT);
    if (backPoint != null) {
      se.append(backPoint.x).append(backPoint.y);
    }
    else {
      se.append("").append("");
    }
    return se.getValue();
  }

  private Point getSendLocation() {
    GamePiece outer = Decorator.getOutermost(this);
    map = null;
    Point dest = null;
    // Home in on a counter
    if (destination.equals(DEST_COUNTER.substring(0, 1))) {
      GamePiece target = null;
      // Find first counter matching the properties
      for (GamePiece piece :
           GameModule.getGameModule().getGameState().getAllPieces()) {
        if (piece instanceof Stack) {
          Stack s = (Stack) piece;
          for (GamePiece gamePiece : s.asList()) {
            if (propertyFilter.accept(this, gamePiece)) {
              target = gamePiece;
              if (target != null) break;
            }
          }
        }
        else {
          if (propertyFilter.accept(this, piece)) {
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
      map = Map.getMapById(mapId.getText(outer));
      if (map == null) {
        map = getMap();
      }
      if (map != null) {
        Board b;
        switch (destination.charAt(0)) {
        case 'G':
          b = map.getBoardByName(boardName.getText(outer));
          if (b != null ) {
            try {
              dest = b.getGrid().getLocation(gridLocation.getText(outer));
              if (dest != null)  dest.translate(b.bounds().x, b.bounds().y);
            }
            catch (BadCoords e) {
              LogBadGridLocation(dest);
              reportDataError(this, Resources.getString(
                "Error.not_found", "Grid Location"),map.getMapName());
              ; // ignore SendTo request.
            }
          }
          break;
        case 'L':
          final int xValue = x.getTextAsInt(outer, "Xlocation", this);
          final int yValue = y.getTextAsInt(outer, "YLocation", this);

          dest = new Point(xValue,yValue);

          b = map.getBoardByName(boardName.getText(outer));
          if (b != null && dest != null) {
            dest.translate(b.bounds().x, b.bounds().y);
          }
          break;

        case 'Z':
          final String zoneName = zone.getText(outer);
          Zone z = map.findZone(zoneName);
          if (z == null) {
            reportDataError(this, Resources.getString("Error.not_found", "Zone"), zone.debugInfo(zoneName, "Zone"));
          }
          else {
            Rectangle r = z.getBounds();
            Rectangle r2 = z.getBoard().bounds();
            dest = new Point(r2.x + r.x + r.width/2, r2.y + r.y + r.height/2);
          }
          break;

        case 'R':
          final String regionName = region.getText(outer);
          Region r = map.findRegion(regionName);
          if (r == null) {
            reportDataError(this, Resources.getString("Error.not_found", "Region"), region.debugInfo(regionName, "Region"));
          }
          else {
            Rectangle r2 = r.getBoard().bounds();
            if (r != null) {
              dest = new Point(r.getOrigin().x + r2.x, r.getOrigin().y + r2.y);
            }
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
    Command c = null;
    myGetKeyCommands();
    if (sendCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      Stack parent = outer.getParent();
      Point dest = getSendLocation();
      if (map != null && dest != null) {
        if (map == getMap() && dest.equals(getPosition())) {
          // don't do anything if we're already there.
          return null;
        }
        final ChangeTracker tracker = new ChangeTracker(this);
        setProperty(BACK_MAP, getMap());
        setProperty(BACK_POINT, getPosition());
        c = tracker.getChangeCommand();
        c = c.append(setOldProperties(this));
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
    else if (backCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      Map backMap = (Map) getProperty(BACK_MAP);
      Point backPoint = (Point) getProperty(BACK_POINT);
      final ChangeTracker tracker = new ChangeTracker(this);
      setProperty(BACK_MAP, null);
      setProperty(BACK_POINT, null);
      c = tracker.getChangeCommand();

      if (backMap != null && backPoint != null) {
        c = c.append(setOldProperties(this));
        c = c.append(backMap.placeOrMerge(outer, backPoint));

        // Apply Auto-move key
        if (backMap.getMoveKey() != null) {
          c = c.append(outer.keyEvent(backMap.getMoveKey()));
        }
      }
    }
    return c;
  }

  /*
   * Offset the destination by the Advanced Options offset
   */
  protected Point offsetDestination(int x, int y, GamePiece outer) {
    int xPos = x + parse("xIndex", xIndex, outer) * parse("xOffset", xOffset, outer);
    int yPos = y + parse("yIndex", yIndex, outer) * parse("yOffset", yOffset, outer);
    return new Point(xPos, yPos);
  }

  private int parse(String desc, FormattedString s, GamePiece outer) {
    int i = 0;
    String val = s.getText(outer, _0);
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
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState,';');
    String mapId = st.nextToken("");
    if (mapId.length() > 0) {
      setProperty(BACK_MAP,Map.getMapById(mapId));
    }
    String x = st.nextToken("");
    String y = st.nextToken("");
    if (x.length() > 0 && y.length()> 0) {
      try {
        setProperty(BACK_POINT, new Point(Integer.parseInt(x), Integer.parseInt(y)));
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"), "Back Point=("+x+","+y+")", e);
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
    String d = "Send to Location";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SendToLocation.htm");
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {commandName, backCommandName},
                       new String[] {getCommandDescription(description, "Send command"), getCommandDescription(description, "Back command")});
  }

  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected StringConfigurer backNameInput;
    protected NamedHotKeyConfigurer keyInput;
    protected NamedHotKeyConfigurer backKeyInput;
    protected FormattedStringConfigurer mapIdInput;
    protected FormattedStringConfigurer boardNameInput;
    protected FormattedStringConfigurer xInput;
    protected FormattedStringConfigurer yInput;
    protected BooleanConfigurer advancedInput;
    protected FormattedStringConfigurer xIndexInput;
    protected FormattedStringConfigurer xOffsetInput;
    protected FormattedStringConfigurer yIndexInput;
    protected FormattedStringConfigurer yOffsetInput;
    protected StringConfigurer descInput;
    protected StringEnumConfigurer destInput;
    protected PropertyExpressionConfigurer propertyInput;
    protected FormattedStringConfigurer zoneInput;
    protected FormattedStringConfigurer regionInput;
    //protected Map map;
    protected JPanel controls;
    protected Box mapControls;
    protected Box boardControls;
    protected Box advancedControls;
    protected StringConfigurer gridLocationInput;

    public Ed(SendToLocation p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      descInput = new StringConfigurer(null, "Description:  ", p.description);
      controls.add(descInput.getControls());

      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new NamedHotKeyConfigurer(null,"Keyboard Command:  ",p.key);
      controls.add(keyInput.getControls());

      backNameInput = new StringConfigurer(null, "Send Back Command name:  ", p.backCommandName);
      controls.add(backNameInput.getControls());

      backKeyInput = new NamedHotKeyConfigurer(null,"Send Back Keyboard Command:  ",p.backKey);
      controls.add(backKeyInput.getControls());

      destInput = new StringEnumConfigurer(null, "Destination:  ", DEST_OPTIONS);
      destInput.setValue(DEST_LOCATION);
      for (String destOption : DEST_OPTIONS) {
        if (destOption.substring(0, 1).equals(p.destination)) {
          destInput.setValue(destOption);
        }
      }
      destInput.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent arg0) {
          updateVisibility();
        }});
      controls.add(destInput.getControls());

      mapControls = Box.createHorizontalBox();
      mapIdInput = new FormattedExpressionConfigurer(null, "Map:  ", p.mapId.getFormat(), p);
      mapControls.add(mapIdInput.getControls());
      JButton select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          selectMap();
        }
      });
      mapControls.add(select);
      JButton clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          clearMap();
        }
      });
      mapControls.add(clear);
      controls.add(mapControls);

      boardControls = Box.createHorizontalBox();
      boardNameInput = new FormattedExpressionConfigurer(null, "Board:  ", p.boardName.getFormat(), p);
      boardControls.add(boardNameInput.getControls());
      select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          selectBoard();
        }
      });
      clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          clearBoard();
        }
      });
      boardControls.add(select);
      boardControls.add(clear);
      controls.add(boardControls);


      xInput = new FormattedExpressionConfigurer(null, "X Position:  ", p.x.getFormat(), p);
      controls.add(xInput.getControls());

      yInput = new FormattedExpressionConfigurer(null, "Y Position:  ", p.y.getFormat(), p);
      controls.add(yInput.getControls());

      zoneInput = new FormattedExpressionConfigurer(null, "Zone Name:  ", p.zone.getFormat(), p);
      controls.add(zoneInput.getControls());

      regionInput = new FormattedExpressionConfigurer(null, "Region Name:  ", p.region.getFormat(), p);
      controls.add(regionInput.getControls());

      propertyInput = new PropertyExpressionConfigurer(null, "Property Match:  ", p.propertyFilter);
      controls.add(propertyInput.getControls());

      gridLocationInput = new StringConfigurer(null, "Grid Location:  ", p.gridLocation.getFormat());
      controls.add(gridLocationInput.getControls());

      advancedInput = new BooleanConfigurer(null, "Advanced Options", false);
      advancedInput.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent arg0) {
          updateVisibility();
        }});
      controls.add(advancedInput.getControls());

      advancedControls = Box.createHorizontalBox();
      xIndexInput = new FormattedExpressionConfigurer(null, "Additional X offset:  ", p.xIndex.getFormat(), p);
      advancedControls.add(xIndexInput.getControls());
      xOffsetInput = new FormattedExpressionConfigurer(null, " times ", p.xOffset.getFormat(), p);
      advancedControls.add(xOffsetInput.getControls());
      controls.add(advancedControls);

      advancedControls = Box.createHorizontalBox();
      yIndexInput = new FormattedExpressionConfigurer(null, "Additional Y offset:  ", p.yIndex.getFormat(), p);
      advancedControls.add(yIndexInput.getControls());
      yOffsetInput = new FormattedExpressionConfigurer(null, " times ", p.yOffset.getFormat(), p);
      advancedControls.add(yOffsetInput.getControls());
      controls.add(advancedControls);

      updateVisibility();
    }

    private void updateVisibility() {
//      boolean advancedVisible = advancedInput.booleanValue();
      advancedInput.getControls().setVisible(!destInput.getValue().equals(DEST_GRIDLOCATION));
      boolean advancedVisible = advancedInput.booleanValue()
        && advancedInput.getControls().isVisible();
      xIndexInput.getControls().setVisible(advancedVisible);
      xOffsetInput.getControls().setVisible(advancedVisible);
      yIndexInput.getControls().setVisible(advancedVisible);
      yOffsetInput.getControls().setVisible(advancedVisible);

      String destOption = destInput.getValueString();
      xInput.getControls().setVisible(destOption.equals(DEST_LOCATION));
      yInput.getControls().setVisible(destOption.equals(DEST_LOCATION));
      mapControls.setVisible(!destOption.equals(DEST_COUNTER));
      boardControls.setVisible(destOption.equals(DEST_LOCATION)
        || destOption.equals(DEST_GRIDLOCATION));
      zoneInput.getControls().setVisible(destOption.equals(DEST_ZONE));
      regionInput.getControls().setVisible(destOption.equals(DEST_REGION));
      propertyInput.getControls().setVisible(destOption.equals(DEST_COUNTER));
      gridLocationInput.getControls().setVisible(destOption.equals(DEST_GRIDLOCATION));

      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }

    private void clearBoard() {
      boardNameInput.setValue("");
    }

    private void clearMap() {
      //map = null;
      mapIdInput.setValue("");
    }

    private void selectBoard() {
      ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), Board.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        Board b = (Board) d.getTarget();
        boardNameInput.setValue(b.getName());
      }
    }

    private void selectMap() {
      ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), Map.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        Map map = (Map) d.getTarget();
        mapIdInput.setValue(map.getMapName());
      }
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
        .append(keyInput.getValueString())
        //.append(map == null ? "" : map.getIdentifier())
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
        .append(destInput.getValueString().charAt(0))
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
}
