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
import java.util.List;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
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
 * This traitadds a command that sends a piece to a particular location ona particular
 * board of a particular Map.
 */
public class SendToLocation extends Decorator implements EditablePiece {
  public static final String ID = "sendto;";
  public static final String BACK_MAP = "backMap";
  public static final String BACK_POINT = "backPoint";
  protected KeyCommand[] command;
  protected String commandName;
  protected String backCommandName;
  protected KeyStroke key;
  protected KeyStroke backKey;
  protected String mapId;
  protected String boardName;
  protected FormattedString x = new FormattedString("");
  protected FormattedString xIndex = new FormattedString("");
  protected FormattedString xOffset = new FormattedString("");
  protected FormattedString y = new FormattedString("");
  protected FormattedString yIndex = new FormattedString("");
  protected FormattedString yOffset = new FormattedString("");
  protected KeyCommand sendCommand;
  protected KeyCommand backCommand;
  protected String description;

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
    commandName = st.nextToken();
    key = st.nextKeyStroke(null);
    mapId = st.nextToken();
    boardName = st.nextToken();
    x.setFormat(st.nextToken("0"));
    y.setFormat(st.nextToken("0"));
    backCommandName = st.nextToken("");
    backKey = st.nextKeyStroke(null);
    xIndex.setFormat(st.nextToken("0"));
    yIndex.setFormat(st.nextToken("0"));
    xOffset.setFormat(st.nextToken("0"));
    yOffset.setFormat(st.nextToken("0"));
    description = st.nextToken("");
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName)
        .append(key)
        .append(mapId)
        .append(boardName)
        .append(x.getFormat())
        .append(y.getFormat())
        .append(backCommandName)
        .append(backKey)
        .append(xIndex.getFormat())
        .append(yIndex.getFormat())
        .append(xOffset.getFormat())
        .append(yOffset.getFormat())
        .append(description);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command == null) {
      sendCommand = new KeyCommand(commandName, key, Decorator.getOutermost(this));
      backCommand = new KeyCommand(backCommandName, backKey, Decorator.getOutermost(this));
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
      Map m = Map.getMapById(mapId);
      if (m == null) {
        m = getMap();
      }
      if (m != null) {
        Point dest;
        try {
          dest = getDestination();
        }
        catch (Exception e) {
          return null;
        }
        Board b = m.getBoardByName(boardName);
        if (b != null) {
          dest.translate(b.bounds().x, b.bounds().y);
        }
        setProperty(BACK_MAP, getMap());
        setProperty(BACK_POINT, getPosition());
        if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
          dest = m.snapTo(dest);
        }
        c = m.placeOrMerge(outer, dest);
        // Apply Auto-move key
        if (m.getMoveKey() != null) {
          c.append(outer.keyEvent(m.getMoveKey()));
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
  
  protected Point getDestination() {
    GamePiece outer = Decorator.getOutermost(this);
    int xPos = Integer.parseInt(x.getText(outer)) + Integer.parseInt(xIndex.getText(outer)) * Integer.parseInt(xOffset.getText(outer));
    int yPos = Integer.parseInt(y.getText(outer)) + Integer.parseInt(yIndex.getText(outer)) * Integer.parseInt(yOffset.getText(outer));
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
  
  public static class Ed implements PieceEditor {
    private StringConfigurer nameInput;
    private StringConfigurer backNameInput;
    private HotKeyConfigurer keyInput;
    private HotKeyConfigurer backKeyInput;
    private JTextField mapIdInput;
    private JTextField boardNameInput;
    private StringConfigurer xInput;
    private StringConfigurer yInput;
    protected BooleanConfigurer advancedInput;
    protected StringConfigurer xIndexInput;
    protected StringConfigurer xOffsetInput;
    protected StringConfigurer yIndexInput;
    protected StringConfigurer yOffsetInput;
    protected StringConfigurer descInput;
    private Map map;
    private JPanel controls;

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
      
      Box b = Box.createHorizontalBox();
      mapIdInput = new JTextField(12);
      map = Map.getMapById(p.mapId);
      if (map != null) {
        mapIdInput.setText(map.getMapName());
      }
      mapIdInput.setEditable(false);
      b.add(new JLabel("Map:  "));
      b.add(mapIdInput);
      JButton select = new JButton("Select");
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectMap();
        }
      });
      b.add(select);
      JButton clear = new JButton("Clear");
      clear.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          clearMap();
        }
      });
      b.add(clear);
      controls.add(b);

      b = Box.createHorizontalBox();
      boardNameInput = new JTextField(12);
      boardNameInput.setText(p.boardName);
      boardNameInput.setEditable(false);
      b.add(new JLabel("Board:  "));
      b.add(boardNameInput);
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
      b.add(select);
      b.add(clear);
      controls.add(b);
      
      xInput = new StringConfigurer(null, "X Position:  ", p.x.getFormat());
      controls.add(xInput.getControls());

      yInput = new StringConfigurer(null, "Y Position:  ", p.y.getFormat());
      controls.add(yInput.getControls());
      
      advancedInput = new BooleanConfigurer(null, "Advanced Options", false);
      advancedInput.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent arg0) {
          updateAdvancedVisibility();
        }});
      controls.add(advancedInput.getControls());
      
      b = Box.createHorizontalBox();
      xIndexInput = new StringConfigurer(null, "Additional X offset:  ", p.xIndex.getFormat());
      b.add(xIndexInput.getControls());
      xOffsetInput = new StringConfigurer(null, " times ", p.xOffset.getFormat());
      b.add(xOffsetInput.getControls());
      controls.add(b);
      
      b = Box.createHorizontalBox();
      yIndexInput = new StringConfigurer(null, "Additional Y offset:  ", p.yIndex.getFormat());
      b.add(yIndexInput.getControls());
      yOffsetInput = new StringConfigurer(null, " times ", p.yOffset.getFormat());
      b.add(yOffsetInput.getControls());
      controls.add(b);
      
      updateAdvancedVisibility();
    }
    
    private void updateAdvancedVisibility() {
      boolean visible = advancedInput.booleanValue().booleanValue();
      xIndexInput.getControls().setVisible(visible);
      xOffsetInput.getControls().setVisible(visible);
      yIndexInput.getControls().setVisible(visible);
      yOffsetInput.getControls().setVisible(visible);
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }
    
    private void clearBoard() {
      boardNameInput.setText("");
    }

    private void clearMap() {
      map = null;
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
        map = (Map) d.getTarget();
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
          .append(map == null ? "" : map.getIdentifier())
          .append(boardNameInput.getText())
          .append(xInput.getValueString())
          .append(yInput.getValueString())
          .append(backNameInput.getValueString())
          .append((KeyStroke)backKeyInput.getValue())
          .append(xIndexInput.getValueString())
          .append(yIndexInput.getValueString())
          .append(xOffsetInput.getValueString())
          .append(yOffsetInput.getValueString())
          .append(descInput.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
