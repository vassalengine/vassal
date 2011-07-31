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
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.CardSlot;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentPathDialog;
import VASSAL.configure.ConfigurerWindow;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.ComponentPathBuilder;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * This Decorator defines a key command to places another counter on top of this one.
 */
public class PlaceMarker extends Decorator implements TranslatablePiece {
  public static final String ID = "placemark;";
  protected KeyCommand command;
  protected NamedKeyStroke key;
  protected String markerSpec;
  protected String markerText = "";
  protected int xOffset = 0;
  protected int yOffset = 0;
  protected boolean matchRotation = false;
  protected KeyCommand[] commands;
  protected NamedKeyStroke afterBurnerKey;
  protected String description = "";
  protected String gpId = "";
  protected String newGpId;
  protected GpIdSupport gpidSupport; // The component that generates unique Slot Id's for us
  protected static final int STACK_TOP = 0;
  protected static final int STACK_BOTTOM = 1;
  protected static final int ABOVE = 2;
  protected static final int BELOW = 3;
  protected int placement = STACK_TOP;
  protected boolean above;

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
    command.setEnabled(getMap() != null && markerSpec != null);
    return commands;
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(command.getName())
      .append(key)
      .append(markerSpec == null ? "null" : markerSpec)
      .append(markerText == null ? "null" : markerText)
      .append(xOffset).append(yOffset)
      .append(matchRotation)
      .append(afterBurnerKey)
      .append(description)
      .append(gpId)
      .append(placement)
      .append(above);
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
    final Map m = getMap();
    if (m == null) return null;

    final GamePiece marker = createMarker();
    if (marker == null) return null;

    Command c = null;
    final GamePiece outer = getOutermost(this);
    Point p = getPosition();
    p.translate(xOffset, -yOffset);
    if (matchRotation) {
      final FreeRotator myRotation =
        (FreeRotator) Decorator.getDecorator(outer, FreeRotator.class);
      final FreeRotator markerRotation =
        (FreeRotator) Decorator.getDecorator(marker, FreeRotator.class);
      if (myRotation != null && markerRotation != null) {
        markerRotation.setAngle(myRotation.getAngle());
        final Point2D myPosition = getPosition().getLocation();
        Point2D markerPosition = p.getLocation();
        markerPosition = AffineTransform.getRotateInstance(
            myRotation.getAngleInRadians(),
            myPosition.getX(), myPosition.getY()
          ).transform(markerPosition, null);
        p = new Point((int) markerPosition.getX(), (int) markerPosition.getY());
      }
    }

    if (!Boolean.TRUE.equals(marker.getProperty(Properties.IGNORE_GRID))) {
      p = getMap().snapTo(p);
    }

    if (m.getStackMetrics().isStackingEnabled() &&
        !Boolean.TRUE.equals(marker.getProperty(Properties.NO_STACK)) &&
        !Boolean.TRUE.equals(outer.getProperty(Properties.NO_STACK)) &&
        m.getPieceCollection().canMerge(outer, marker)) {
      final Stack parent = getParent();
      GamePiece target = outer;
      int index = -1;
      switch (placement) {
      case ABOVE:
        target = outer;
        break;
      case BELOW:
        index = parent == null ? 0 : parent.indexOf(outer);
        break;
      case STACK_BOTTOM:
        index = 0;
        break;
      case STACK_TOP:
        target = parent == null ? outer : parent;
      }
      c = m.getStackMetrics().merge(target, marker);
      if (index >= 0) {
        final ChangeTracker ct = new ChangeTracker(parent);
        parent.insert(marker,index);
        c = c.append(ct.getChangeCommand());
      }
    }
    else {
      c = m.placeAt(marker, p);
    }

    if (afterBurnerKey != null && !afterBurnerKey.isNull()) {
      marker.setProperty(Properties.SNAPSHOT,
                         PieceCloner.getInstance().clonePiece(marker));
      c.append(marker.keyEvent(afterBurnerKey.getKeyStroke()));
    }

    if (getProperty(Properties.SELECTED) == Boolean.TRUE)
      selectMarker(marker);

    if (markerText != null) {
      if (!Boolean.TRUE.equals(
            outer.getProperty(Properties.OBSCURED_TO_OTHERS)) &&
          !Boolean.TRUE.equals(
            outer.getProperty(Properties.OBSCURED_TO_ME)) &&
          !Boolean.TRUE.equals(
            outer.getProperty(Properties.INVISIBLE_TO_OTHERS))) {
        final String location = m.locationName(getPosition());
        if (location != null) {
          Command display = new Chatter.DisplayText(
            GameModule.getGameModule().getChatter(),
            " * " + location + ":  " + outer.getName() +
              " " + markerText + " * ");
          display.execute();
          c = c == null ? display : c.append(display);
        }
      }
    }

    return c;
  }

  protected void selectMarker(GamePiece marker) {
    if (marker.getProperty(Properties.SELECT_EVENT_FILTER) == null) {
      if (marker.getParent() != null && marker.getParent().equals(getParent())) {
        KeyBuffer.getBuffer().add(marker);
      }
    }
  }

  /**
   * The marker, with prototypes fully expanded
   *
   * @return
   */
  public GamePiece createMarker() {
    GamePiece piece = createBaseMarker();
    if (piece == null) {
      piece = new BasicPiece();
      newGpId = getGpId();
    }
    else {
      piece = PieceCloner.getInstance().clonePiece(piece);
    }
    piece.setProperty(Properties.PIECE_ID, newGpId);
    return piece;
  }

  /**
   * The marker, with prototypes unexpanded
   *
   * @return
   */
  public GamePiece createBaseMarker() {
    if (markerSpec == null) {
      return null;
    }
    GamePiece piece = null;
    if (isMarkerStandalone()) {
      final AddPiece comm =
        (AddPiece) GameModule.getGameModule().decode(markerSpec);
      piece = comm.getTarget();
      piece.setState(comm.getState());
      newGpId = getGpId();
    }
    else {
      try {
        final Configurable[] c =
          ComponentPathBuilder.getInstance().getPath(markerSpec);
        final Configurable conf = c[c.length-1];

        if (conf instanceof PieceSlot) {
          piece = ((PieceSlot) conf).getPiece();
          newGpId = ((PieceSlot) conf).getGpId();
        }
      }
      catch (ComponentPathBuilder.PathFormatException e) {
        reportDataError(this, Resources.getString("Resources.place_error"), e.getMessage()+" markerSpec="+markerSpec, e);
      }
    }
    return piece;
  }

  /**
   * @return true if the marker is defined from scratch. Return false if the marker is defined as a component in the
   *         Game Piece Palette
   */
  public boolean isMarkerStandalone() {
    return markerSpec != null && markerSpec.startsWith(BasicCommandEncoder.ADD);
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
    key = st.nextNamedKeyStroke(null);
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
    afterBurnerKey = st.nextNamedKeyStroke(null);
    description = st.nextToken("");
    setGpId(st.nextToken(""));
    placement = st.nextInt(STACK_TOP);
    above = st.nextBoolean(false);
    gpidSupport = GameModule.getGameModule().getGpIdSupport();
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public PieceI18nData getI18nData() {
    return getI18nData(command.getName(), getCommandDescription(description, "Place Marker command"));
  }

  public String getGpId() {
    return gpId;
  }

  public void setGpId(String s) {
    gpId = s;
  }

  public void updateGpId(GpIdSupport s) {
    gpidSupport = s;
    updateGpId();
  }

  public void updateGpId() {
    setGpId(gpidSupport.generateGpId());
  }

  protected static class Ed implements PieceEditor {
    private NamedHotKeyConfigurer keyInput;
    private StringConfigurer commandInput;
    private PieceSlot pieceInput;
    private JPanel p = new JPanel();
    private String markerSlotPath;
    protected JButton defineButton = new JButton("Define Marker");
    protected JButton selectButton = new JButton("Select");
    protected IntConfigurer xOffsetConfig = new IntConfigurer(null, "Horizontal offset:  ");
    protected IntConfigurer yOffsetConfig = new IntConfigurer(null, "Vertical offset:  ");
    protected BooleanConfigurer matchRotationConfig;
    protected BooleanConfigurer aboveConfig;
    protected JComboBox placementConfig;
    protected NamedHotKeyConfigurer afterBurner;
    protected StringConfigurer descConfig;
    private String slotId;

    protected Ed(PlaceMarker piece) {
      matchRotationConfig = createMatchRotationConfig();
      aboveConfig = createAboveConfig();
      descConfig = new StringConfigurer(null, "Description:  ", piece.description);
      keyInput = new NamedHotKeyConfigurer(null, "Keyboard Command:  ", piece.key);
      afterBurner = new NamedHotKeyConfigurer(null, "Keystroke to apply after placement:  ", piece.afterBurnerKey);
      commandInput = new StringConfigurer(null, "Command:  ", piece.command.getName());
      GamePiece marker = piece.createBaseMarker();
      pieceInput = new PieceSlot(marker);
      pieceInput.updateGpId(piece.gpidSupport);
      pieceInput.setGpId(piece.getGpId());
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
            slotId = "";
          }
          else {
            markerSlotPath = null;
          }
        }
      });
      b.add(selectButton);
      p.add(b);
      xOffsetConfig.setValue(piece.xOffset);
      p.add(xOffsetConfig.getControls());
      yOffsetConfig.setValue(piece.yOffset);
      p.add(yOffsetConfig.getControls());
      matchRotationConfig.setValue(Boolean.valueOf(piece.matchRotation));
      p.add(matchRotationConfig.getControls());
      if (aboveConfig != null) {
        aboveConfig.setValue(Boolean.valueOf(piece.above));
        p.add(aboveConfig.getControls());
        ((JCheckBox) matchRotationConfig.getControls()).addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            aboveConfig.getControls().setVisible(((JCheckBox) matchRotationConfig.getControls()).isSelected());
          }
        });
        aboveConfig.getControls().setVisible(Boolean.valueOf(piece.matchRotation));
      }
      placementConfig = new JComboBox(new String[]{"On top of stack","On bottom of stack","Above this piece","Below this piece"});
      placementConfig.setSelectedIndex(piece.placement);
      Box placementBox = Box.createHorizontalBox();
      placementBox.add(new JLabel("Place marker:  "));
      placementBox.add(placementConfig);
      p.add(placementBox);
      p.add(afterBurner.getControls());
      slotId = piece.getGpId();
    }

    protected BooleanConfigurer createMatchRotationConfig() {
      return new BooleanConfigurer(null, "Match Rotation?");
    }

    protected BooleanConfigurer createAboveConfig() {
      return null;
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
      se.append(keyInput.getValueString());
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
      se.append("null"); // Older versions specified a text message to echo. Now performed by the ReportState trait,
                          // but we remain backward-compatible.
      se.append(xOffsetConfig.getValueString());
      se.append(yOffsetConfig.getValueString());
      se.append(matchRotationConfig.getValueString());
      se.append(afterBurner.getValueString());
      se.append(descConfig.getValueString());
      se.append(slotId);
      se.append(placementConfig.getSelectedIndex());
      se.append(aboveConfig == null ? "false" : aboveConfig.getValueString());
      return ID + se.getValue();
    }
    public static class ChoosePieceDialog extends ChooseComponentPathDialog {
      private static final long serialVersionUID = 1L;

      public ChoosePieceDialog(Frame owner, Class<PieceSlot> targetClass) {
        super(owner, targetClass);
      }

      protected boolean isValidTarget(Object selected) {
        return super.isValidTarget(selected) || CardSlot.class.isInstance(selected);
      }
    }
  }
}
