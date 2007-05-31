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
import java.util.Enumeration;
import java.util.List;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.Region;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * This trait adds a command that sends a piece to another location. Options for the
 * target location are:
 *   1. Specified x,y co-ords on a named map/board
 *   2. The centre of a named Zone on a named map
 *   3. A named Region on a named map
 *   4. The location of another counter selected by a Propert Match String
 * Once the target locaiton is identified, it can be further offset in the X and Y directions
 * by a set of multipliers.
 * All Input Fields may use $...$ variable names 
 */
public class SendToLocation extends Decorator implements TranslatablePiece {
  public static final String ID = "sendto;";
  public static final String BACK_MAP = "backMap";
  public static final String BACK_POINT = "backPoint";
  protected static final String DEST_LOCATION = "Location on selected Map";
  protected static final String DEST_ZONE = "Zone on selected Map";
  protected static final String DEST_REGION = "Region on selected Map";
  protected static final String DEST_COUNTER = "Another counter, selected by properties";
  protected static final String[] DEST_OPTIONS = new String[] {DEST_LOCATION, DEST_ZONE, DEST_REGION, DEST_COUNTER};
  protected KeyCommand[] command;
  protected String commandName;
  protected String backCommandName;
  protected KeyStroke key;
  protected KeyStroke backKey;
  protected FormattedString mapId = new FormattedString("");
  protected FormattedString boardName = new FormattedString("");
  protected FormattedString x = new FormattedString("");
  protected FormattedString xIndex = new FormattedString("");
  protected FormattedString xOffset = new FormattedString("");
  protected FormattedString y = new FormattedString("");
  protected FormattedString yIndex = new FormattedString("");
  protected FormattedString yOffset = new FormattedString("");
  protected KeyCommand sendCommand;
  protected KeyCommand backCommand;
  protected String description;
  protected String destination;
  protected FormattedString zone = new FormattedString("");
  protected FormattedString region = new FormattedString("");
  protected PropertyExpression propertyFilter = new PropertyExpression("");

  public SendToLocation() {
    this(ID + ";;;;0;0;;", null);
  }

  public SendToLocation(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    commandName = st.nextToken("");
    key = st.nextKeyStroke(null);
    mapId.setFormat(st.nextToken(""));
    boardName.setFormat(st.nextToken(""));
    x.setFormat(st.nextToken("0"));
    y.setFormat(st.nextToken("0"));
    backCommandName = st.nextToken("");
    backKey = st.nextKeyStroke(null);
    xIndex.setFormat(st.nextToken("0"));
    yIndex.setFormat(st.nextToken("0"));
    xOffset.setFormat(st.nextToken("0"));
    yOffset.setFormat(st.nextToken("0"));
    description = st.nextToken("");
    destination = st.nextToken("");
    zone.setFormat(st.nextToken(""));
    region.setFormat(st.nextToken(""));
    propertyFilter.setExpression(st.nextToken(""));
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
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
        .append(propertyFilter.getExpression());
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      sendCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this), getI18nData());
      backCommand = new KeyCommand(backCommandName, backKey, Decorator.getOutermost(this), getI18nData());
      List l = new ArrayList();
      if (commandName.length() > 0 && key != null) {
        l.add(sendCommand);
      }
      if (backCommandName.length() > 0 && backKey != null) {
        l.add(backCommand);
      }
      command = (KeyCommand[]) l.toArray(new KeyCommand[l.size()]);
    }
    for (int i = 0; i < command.length; i++) {
      if (command[i].getName().equals(backCommandName)) {
        command[i].setEnabled(getMap() != null && getProperty(BACK_MAP) != null && getProperty(BACK_POINT) != null);
      }
      else {
        command[i].setEnabled(getMap() != null);
      }
    }
    return command;
  }

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

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (sendCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      Stack parent = outer.getParent();
      Map map = null;
      Point dest = null;
      
      // Home in on a counter
      if (destination.equals(DEST_COUNTER.substring(0, 1))) {
        GamePiece target = null;
        // Find first counter matching the properties
        for (Enumeration e = GameModule.getGameModule().getGameState().getPieces(); e.hasMoreElements() && target==null; ) {
          GamePiece piece = (GamePiece) e.nextElement();
          if (piece instanceof Stack) {
            Stack s = (Stack) piece;
            for (int i=0; i < s.getPieceCount() && target == null; i++) {
              if (propertyFilter.accept(s.getPieceAt(i))) {
                target = s.getPieceAt(i);
              }
            }
          }
          else {
            if (propertyFilter.accept(piece)) {
              target = piece;
            }
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
      // Location/Zone/Region processing all use specified map
      else {
        map = Map.getMapById(mapId.getText(outer));
        if (map == null) {
          map = getMap();
        }
        if (map != null) {
          switch (destination.charAt(0)) {
          case 'L':
            try {
              dest = offsetDestination(Integer.parseInt(x.getText(outer)), Integer.parseInt(y.getText(outer)), outer);
            }
            catch (Exception e) {
              ;
            }
            Board b = map.getBoardByName(boardName.getText(outer));
            if (b != null && dest != null) {
              dest.translate(b.bounds().x, b.bounds().y);
            }
            break;
            
          case 'Z':
            Zone z = map.findZone(zone.getText(outer));
            if (z != null) {
              Rectangle r = z.getBounds();
              Rectangle r2 = z.getBoard().bounds();
              dest = new Point(r2.x + r.x + r.width/2, r2.y + r.y + r.height/2);
             }
            break;
            
          case 'R':
            Region r = map.findRegion(region.getText(outer));
            Rectangle r2 = r.getBoard().bounds();
            if (r != null) {
              dest = new Point(r.getOrigin().x + r2.x, r.getOrigin().y + r2.y);
            }
            break;
          }
        }
      }

      // Offset destination by Advanced Options offsets
      if (dest != null) {
        dest = offsetDestination(dest.x, dest.y, outer);
      }
      
      if (map != null && dest != null) {
        setProperty(BACK_MAP, getMap());
        setProperty(BACK_POINT, getPosition());
        if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
          dest = map.snapTo(dest);
        }
        c = map.placeOrMerge(outer, dest);
        // Apply Auto-move key
        if (map.getMoveKey() != null) {
          c.append(outer.keyEvent(map.getMoveKey()));
        }
        if (parent != null) {
          c.append(parent.pieceRemoved(outer));
        }
      }
    }
    else if (backCommand.matches(stroke)) {
      GamePiece outer = Decorator.getOutermost(this);
      Map backMap = (Map) getProperty(BACK_MAP);
      Point backPoint = (Point) getProperty(BACK_POINT);
      if (backMap != null && backPoint != null) {
         c = backMap.placeOrMerge(outer, backPoint);
         // Apply Auto-move key
         if (backMap.getMoveKey() != null) {
           c.append(outer.keyEvent(backMap.getMoveKey()));
         }         
      }
      setProperty(BACK_MAP, null);
      setProperty(BACK_POINT, null);
    }
    return c;
  }
   
  /*
   * Offset the destination by the Advanced Options offset 
   */
  protected Point offsetDestination(int x, int y, GamePiece outer) {
    int xPos = x + Integer.parseInt(xIndex.getText(outer)) * Integer.parseInt(xOffset.getText(outer));
    int yPos = y + Integer.parseInt(yIndex.getText(outer)) * Integer.parseInt(yOffset.getText(outer));
    return new Point(xPos, yPos);
  }
  
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
        // Ignore
      }
    }
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public String getDescription() {
    String d = "Send to Location";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SendToLocation.htm");
  }
  
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {commandName, backCommandName}, 
                       new String[] {getCommandDescription(description, "Send command"), getCommandDescription(description, "Back command")});
  }
  
  public static class Ed implements PieceEditor {
    protected StringConfigurer nameInput;
    protected StringConfigurer backNameInput;
    protected HotKeyConfigurer keyInput;
    protected HotKeyConfigurer backKeyInput;
    protected JTextField mapIdInput;
    protected JTextField boardNameInput;
    protected StringConfigurer xInput;
    protected StringConfigurer yInput;
    protected BooleanConfigurer advancedInput;
    protected StringConfigurer xIndexInput;
    protected StringConfigurer xOffsetInput;
    protected StringConfigurer yIndexInput;
    protected StringConfigurer yOffsetInput;
    protected StringConfigurer descInput;
    protected StringEnumConfigurer destInput;
    protected StringConfigurer propertyInput;
    protected StringConfigurer zoneInput;
    protected StringConfigurer regionInput;
    //protected Map map;
    protected JPanel controls;
    protected Box mapControls;
    protected Box boardControls;
    protected Box advancedControls;

    public Ed(SendToLocation p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      descInput = new StringConfigurer(null, "Description:  ", p.description);
      controls.add(descInput.getControls());
      
      nameInput = new StringConfigurer(null, "Command name:  ", p.commandName);
      controls.add(nameInput.getControls());

      keyInput = new HotKeyConfigurer(null,"Keyboard Command:  ",p.key);
      controls.add(keyInput.getControls());

      backNameInput = new StringConfigurer(null, "Send Back Command name:  ", p.backCommandName);
      controls.add(backNameInput.getControls());

      backKeyInput = new HotKeyConfigurer(null,"Send Back Keyboard Command:  ",p.backKey);
      controls.add(backKeyInput.getControls());
      
      destInput = new StringEnumConfigurer(null, "Destination:  ", DEST_OPTIONS);
      destInput.setValue(DEST_LOCATION);
      for (int i=0; i < DEST_OPTIONS.length; i++) {
        if (DEST_OPTIONS[i].substring(0,1).equals(p.destination)) {
          destInput.setValue(DEST_OPTIONS[i]);
        }
      }
      destInput.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent arg0) {
          updateVisibility();
        }});
      controls.add(destInput.getControls());
      
      mapControls = Box.createHorizontalBox();
      mapIdInput = new JTextField(12);
      mapIdInput.setText(p.mapId.getFormat());
      //map = Map.getMapById(p.mapId.getFormat());
      //if (map != null) {
      //  mapIdInput.setText(map.getMapName());
      //}
      mapIdInput.setEditable(true);
      mapControls.add(new JLabel("Map:  "));
      mapControls.add(mapIdInput);
      JButton select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectMap();
        }
      });
      mapControls.add(select);
      JButton clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          clearMap();
        }
      });
      mapControls.add(clear);
      controls.add(mapControls);

      boardControls = Box.createHorizontalBox();
      boardNameInput = new JTextField(12);
      boardNameInput.setText(p.boardName.getFormat());
      boardNameInput.setEditable(true);
      boardControls.add(new JLabel("Board:  "));
      boardControls.add(boardNameInput);
      select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectBoard();
        }
      });
      clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          clearBoard();
        }
      });
      boardControls.add(select);
      boardControls.add(clear);
      controls.add(boardControls);
      
      
      xInput = new StringConfigurer(null, "X Position:  ", p.x.getFormat());
      controls.add(xInput.getControls());

      yInput = new StringConfigurer(null, "Y Position:  ", p.y.getFormat());
      controls.add(yInput.getControls());
      
      zoneInput = new StringConfigurer(null, "Zone Name:  ", p.zone.getFormat());
      controls.add(zoneInput.getControls());

      regionInput = new StringConfigurer(null, "Region Name:  ", p.region.getFormat());
      controls.add(regionInput.getControls());
      
      propertyInput = new PropertyExpressionConfigurer(null, "Property Match:  ", p.propertyFilter);
      controls.add(propertyInput.getControls());
      
      advancedInput = new BooleanConfigurer(null, "Advanced Options", false);
      advancedInput.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent arg0) {
          updateVisibility();
        }});
      controls.add(advancedInput.getControls());
      
      advancedControls = Box.createHorizontalBox();
      xIndexInput = new StringConfigurer(null, "Additional X offset:  ", p.xIndex.getFormat());
      advancedControls.add(xIndexInput.getControls());
      xOffsetInput = new StringConfigurer(null, " times ", p.xOffset.getFormat());
      advancedControls.add(xOffsetInput.getControls());
      controls.add(advancedControls);
      
      advancedControls = Box.createHorizontalBox();
      yIndexInput = new StringConfigurer(null, "Additional Y offset:  ", p.yIndex.getFormat());
      advancedControls.add(yIndexInput.getControls());
      yOffsetInput = new StringConfigurer(null, " times ", p.yOffset.getFormat());
      advancedControls.add(yOffsetInput.getControls());
      controls.add(advancedControls);
      
      updateVisibility();
    }
    
    private void updateVisibility() {
      boolean advancedVisible = advancedInput.booleanValue().booleanValue();
      xIndexInput.getControls().setVisible(advancedVisible);
      xOffsetInput.getControls().setVisible(advancedVisible);
      yIndexInput.getControls().setVisible(advancedVisible);
      yOffsetInput.getControls().setVisible(advancedVisible);
      
      String destOption = destInput.getValueString();
      xInput.getControls().setVisible(destOption.equals(DEST_LOCATION));
      yInput.getControls().setVisible(destOption.equals(DEST_LOCATION));
      mapControls.setVisible(!destOption.equals(DEST_COUNTER));
      boardControls.setVisible(destOption.equals(DEST_LOCATION));
      zoneInput.getControls().setVisible(destOption.equals(DEST_ZONE));
      regionInput.getControls().setVisible(destOption.equals(DEST_REGION));
      propertyInput.getControls().setVisible(destOption.equals(DEST_COUNTER));
      
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }
    
    private void clearBoard() {
      boardNameInput.setText("");
    }

    private void clearMap() {
      //map = null;
      mapIdInput.setText("");
    }

    private void selectBoard() {
      ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), Board.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        Board b = (Board) d.getTarget();
        boardNameInput.setText(b.getName());
      }
    }

    private void selectMap() {
      ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), Map.class);
      d.setVisible(true);
      if (d.getTarget() != null) {
        Map map = (Map) d.getTarget();
        mapIdInput.setText(map.getMapName());
      }
    }

    public Component getControls() {
      return controls;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameInput.getValueString())
          .append((KeyStroke)keyInput.getValue())
          //.append(map == null ? "" : map.getIdentifier())
          .append(mapIdInput.getText())
          .append(boardNameInput.getText())
          .append(xInput.getValueString())
          .append(yInput.getValueString())
          .append(backNameInput.getValueString())
          .append((KeyStroke)backKeyInput.getValue())
          .append(xIndexInput.getValueString())
          .append(yIndexInput.getValueString())
          .append(xOffsetInput.getValueString())
          .append(yOffsetInput.getValueString())
          .append(descInput.getValueString())
          .append(destInput.getValueString().charAt(0))
          .append(zoneInput.getValueString())
          .append(regionInput.getValueString())
          .append(propertyInput.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
