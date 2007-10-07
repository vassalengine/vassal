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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 14, 2002
 * Time: 4:25:21 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentPathDialog;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.ComponentPathBuilder;
import VASSAL.tools.SequenceEncoder;

/**
 * This Decorator defines a key command to places another counter on top of this one.
 */
public class PlaceMarker extends Decorator implements TranslatablePiece {
  public static final String ID = "placemark;";
  protected KeyCommand command;
  protected KeyStroke key;
  protected String markerSpec;
  protected String markerText = "";
  protected int xOffset=0;
  protected int yOffset=0;
  protected boolean matchRotation=false;
  protected KeyCommand[] commands;
  protected KeyStroke afterBurnerKey;
  protected String description = "";

  public PlaceMarker() {
    this(ID + "Place Marker;M;null;null;null", null);
  }

  public PlaceMarker(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
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

  protected KeyCommand[] myGetKeyCommands() {
    command.setEnabled(getMap() != null
                       && markerSpec != null);
    return commands;
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(command.getName());
    se.append(key);
    se.append(markerSpec == null ? "null" : markerSpec);
    se.append(markerText == null ? "null" : markerText);
    se.append(xOffset).append(yOffset);
    se.append(matchRotation);
    se.append(afterBurnerKey);
    se.append(description);
    return ID + se.getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (command.matches(stroke)) {
      return placeMarker();
    }
    else {
      return null;
    }
  }

  protected Command placeMarker() {
    GamePiece marker = createMarker();
    Command c = null;
    if (marker != null) {
      GamePiece outer = getOutermost(this);
      Point p = getPosition();
      p.translate(xOffset,-yOffset);
      if (matchRotation) {
        FreeRotator myRotation = (FreeRotator) Decorator.getDecorator(outer,FreeRotator.class);
        FreeRotator markerRotation = (FreeRotator) Decorator.getDecorator(marker,FreeRotator.class);
        if (myRotation != null
          && markerRotation != null) {
          markerRotation.setAngle(myRotation.getAngle());
          Point2D myPosition = getPosition().getLocation();
          Point2D markerPosition = p.getLocation();
          markerPosition = AffineTransform.getRotateInstance(myRotation.getAngleInRadians(),myPosition.getX(), myPosition.getY()).transform(markerPosition,null);
          p = new Point((int)markerPosition.getX(),(int)markerPosition.getY());
        }
      }
      if (!Boolean.TRUE.equals(marker.getProperty(Properties.IGNORE_GRID))) {
        p = getMap().snapTo(p);
      }
      c = getMap().placeOrMerge(marker,p);
      if (afterBurnerKey != null) {
        marker.setProperty(Properties.SNAPSHOT, PieceCloner.getInstance().clonePiece(marker));
        c.append(marker.keyEvent(afterBurnerKey));
      }
      if (marker.getProperty(Properties.SELECT_EVENT_FILTER) == null) {
        if (marker.getParent() == null || !marker.getParent().equals(getParent())) {
          KeyBuffer.getBuffer().clear();
        }
        KeyBuffer.getBuffer().add(marker);
      }
      if (markerText != null && getMap() != null) {
        if (!Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_OTHERS))
            && !Boolean.TRUE.equals(outer.getProperty(Properties.OBSCURED_TO_ME))
            && !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
          String location = getMap().locationName(getPosition());
          if (location != null) {
            Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), " * " + location + ":  " + outer.getName() + " " + markerText + " * ");
            display.execute();
            c = c == null ? display : c.append(display);
          }
        }
      }
    }
    return c;
  }

  /**
   * The marker, with prototypes fully expanded
   * @return
   */
  protected GamePiece createMarker() {
    GamePiece piece = createBaseMarker();
    if (piece == null) {
      piece = new BasicPiece();
    }
    else {
      piece = PieceCloner.getInstance().clonePiece(piece);
    }
    return piece;
  }

  /**
   * The marker, with prototypes unexpanded
   * @return
   */
  public GamePiece createBaseMarker() {
    if (markerSpec == null) {
      return null;
    }
    GamePiece piece = null;
    if (isMarkerStandalone()) {
      AddPiece comm = (AddPiece) GameModule.getGameModule().decode(markerSpec);
      piece = comm.getTarget();
      piece.setState(comm.getState());
    }
    else {
      try {
        Configurable[] c = ComponentPathBuilder.getInstance().getPath(markerSpec);
        if (c[c.length - 1] instanceof PieceSlot) {
          piece = ((PieceSlot) c[c.length - 1]).getPiece();
        }
      }
      catch (ComponentPathBuilder.PathFormatException e) {
      }
    }
    return piece;
  }

  /**
   * @return true if the marker is defined from scratch.  Return false if the marker is defined as a component in the Game Piece Palette
   */
  public boolean isMarkerStandalone() {
    return markerSpec.startsWith(BasicCommandEncoder.ADD);
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    String d = "Place Marker";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Marker.htm");
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    String name = st.nextToken();
    key = st.nextKeyStroke(null);
    command = new KeyCommand(name, key, this, this);
    if (name.length() > 0 && key != null) {
      commands = new KeyCommand[]{command};
    }
    else {
      commands = new KeyCommand[0];
    }
    markerSpec = st.nextToken();
    if ("null".equals(markerSpec)) {
      markerSpec = null;
    }
    markerText = st.nextToken("null");
    if ("null".equals(markerText)) {
      markerText = null;
    }
    xOffset = st.nextInt(0);
    yOffset = st.nextInt(0);
    matchRotation = st.nextBoolean(false);
    afterBurnerKey = st.nextKeyStroke(null);
    description = st.nextToken("");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  
  public PieceI18nData getI18nData() {
    return getI18nData(command.getName(), getCommandDescription(description, "Place Marker command"));
  }

  protected static class Ed implements PieceEditor {
    private HotKeyConfigurer keyInput;
    private StringConfigurer commandInput;
    private PieceSlot pieceInput;
    private JPanel p = new JPanel();
    private String markerSlotPath;
    protected JButton defineButton = new JButton("Define Marker");
    protected JButton selectButton = new JButton("Select");
    protected IntConfigurer xOffsetConfig = new IntConfigurer(null,"Horizontal offset:  ");
    protected IntConfigurer yOffsetConfig = new IntConfigurer(null,"Vertical offset:  ");
    protected BooleanConfigurer matchRotationConfig;
    protected HotKeyConfigurer afterBurner;
    protected StringConfigurer descConfig;

    protected Ed(PlaceMarker piece) {
      matchRotationConfig = createMatchRotationConfig();
      descConfig = new StringConfigurer(null, "Description:  ", piece.description);
      keyInput = new HotKeyConfigurer(null,"Keyboard Command:  ",piece.key);
      afterBurner = new HotKeyConfigurer(null, "Keystroke to apply after placement:  ", piece.afterBurnerKey);
      commandInput = new StringConfigurer(null, "Command:  ", piece.command.getName());
      GamePiece marker = piece.createBaseMarker();
      pieceInput = new PieceSlot(marker);

      markerSlotPath = piece.markerSpec;

      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      p.add(descConfig.getControls());
      p.add(commandInput.getControls());
      p.add(keyInput.getControls());
      Box b = Box.createHorizontalBox();
      b.add(pieceInput.getComponent());
      defineButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          markerSlotPath = null;
          new ConfigurerWindow(pieceInput.getConfigurer()).setVisible(true);
        }
      });
      b.add(defineButton);
      selectButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          ChoosePieceDialog d = new ChoosePieceDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, p), PieceSlot.class);
          d.setVisible(true);
          if (d.getTarget() instanceof PieceSlot) {
            pieceInput.setPiece(((PieceSlot) d.getTarget()).getPiece());
          }
          if (d.getPath() != null) {
            markerSlotPath = ComponentPathBuilder.getInstance().getId(d.getPath());
          }
          else {
            markerSlotPath = null;
          }
        }
      });
      b.add(selectButton);
      p.add(b);
      xOffsetConfig.setValue(new Integer(piece.xOffset));
      p.add(xOffsetConfig.getControls());
      yOffsetConfig.setValue(new Integer(piece.yOffset));
      p.add(yOffsetConfig.getControls());
      matchRotationConfig.setValue(Boolean.valueOf(piece.matchRotation));
      p.add(matchRotationConfig.getControls());
      p.add(afterBurner.getControls());
    }

    protected BooleanConfigurer createMatchRotationConfig() {
      return new BooleanConfigurer(null,"Match Rotation?");
    }

    public Component getControls() {
      return p;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(commandInput.getValueString());
      se.append((KeyStroke)keyInput.getValue());
      if (pieceInput.getPiece() == null) {
        se.append("null");
      }
      else if (markerSlotPath != null) {
        se.append(markerSlotPath);
      }
      else {
        String spec = GameModule.getGameModule().encode(new AddPiece(pieceInput.getPiece()));
        se.append(spec);
      }
      se.append("null"); // Older versions specified a text message to echo.  Now performed by the ReportState trait, but we remain backward-compatible.
      se.append(xOffsetConfig.getValueString());
      se.append(yOffsetConfig.getValueString());
      se.append(matchRotationConfig.getValueString());
      se.append((KeyStroke)afterBurner.getValue());
      se.append(descConfig.getValueString());
      return ID + se.getValue();
    }

    public static class ChoosePieceDialog extends ChooseComponentPathDialog {
      private static final long serialVersionUID = 1L;

      public ChoosePieceDialog(Frame owner, Class targetClass) {
        super(owner, targetClass);
      }

      protected boolean isValidTarget(Object selected) {
        return super.isValidTarget(selected) || CardSlot.class.isInstance(selected);
      }
    }
  }
}
