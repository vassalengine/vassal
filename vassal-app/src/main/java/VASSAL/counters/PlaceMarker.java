/*
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
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertiesWindow;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.search.ImageSearchTarget;
import VASSAL.tools.ComponentPathBuilder;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

/**
 * This Decorator defines a key command to places another counter on top of this one.
 */
public class PlaceMarker extends Decorator implements TranslatablePiece, RecursionLimiter.Loopable {
  public static final String ID = "placemark;"; // NON-NLS
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
    this(ID + Resources.getString("Editor.PlaceMarker.default_command") + ";M;null;null;null", null); // NON-NLS
  }

  public PlaceMarker(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
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
  protected KeyCommand[] myGetKeyCommands() {
    command.setEnabled(getMap() != null && markerSpec != null);
    return commands;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(command.getName())
      .append(key)
      .append(markerSpec == null ? "null" : markerSpec) // NON-NLS
      .append(markerText == null ? "null" : markerText) // NON-NLS
      .append(xOffset).append(yOffset)
      .append(matchRotation)
      .append(afterBurnerKey)
      .append(description)
      .append(gpId)
      .append(placement)
      .append(above);
    return ID + se.getValue();
  }

  @Override
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

    Command c;
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
      GamePiece target = outer;
      int index = -1;
      Stack parent = getParent();

      if (parent == null) {
        // we're not in a stack now, but we will be _after_ the merge
        if (placement == STACK_BOTTOM || placement == BELOW) {
          index = 0;
        }
      }
      else {
        // we're in a stack already
        switch (placement) {
        case STACK_TOP:
          target = parent;
          break;
        case STACK_BOTTOM:
          index = 0;
          break;
        case ABOVE:
          break;
        case BELOW:
          index = parent.indexOf(outer);
        }
      }

      c = m.getStackMetrics().merge(target, marker);

      if (index >= 0) {
        // we need to adjust the marker's position in the Stack
        if (parent == null) {
          // get the newly formed Stack if there hadn't been one before
          parent = target.getParent();
        }
        final ChangeTracker ct = new ChangeTracker(parent);
        parent.insert(marker, index);
        c = c.append(ct.getChangeCommand());
      }
    }
    else {
      c = m.placeAt(marker, p);
    }

    if (afterBurnerKey != null && !afterBurnerKey.isNull()) {
      marker.setProperty(Properties.SNAPSHOT, ((PropertyExporter) marker).getProperties());
      try {
        RecursionLimiter.startExecution(this);
        c.append(marker.keyEvent(afterBurnerKey.getKeyStroke()));
      }
      catch (RecursionLimitException e) {
        RecursionLimiter.infiniteLoop(e);
      }
      finally {
        RecursionLimiter.endExecution();
      }
    }

    if (getProperty(Properties.SELECTED) == Boolean.TRUE)
      selectMarker(marker);

    if (markerText != null) {
      if (Stream.of(Properties.OBSCURED_TO_OTHERS, Properties.OBSCURED_TO_ME, Properties.INVISIBLE_TO_OTHERS)
                .noneMatch(s -> Boolean.TRUE.equals(outer.getProperty(s)))) {
        final String location = m.locationName(getPosition());
        if (location != null) {
          final Command display = new Chatter.DisplayText(
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
   * @return new Marker
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
   * @return New Base Marker
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
        final Configurable conf = c[c.length - 1];

        if (conf instanceof PieceSlot) {
          piece = ((PieceSlot) conf).getPiece();
          newGpId = ((PieceSlot) conf).getGpId();
        }
      }
      catch (ComponentPathBuilder.PathFormatException e) {
        reportDataError(this, Resources.getString("Error.place_error"),
          e.getMessage() + " markerSpec=" + markerSpec, e); // NON-NLS
      }
    }
    return piece;
  }


  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    super.addImageNamesRecursively(s);
    if (isMarkerStandalone()) {
      final GamePiece p = createBaseMarker();
      if (p instanceof ImageSearchTarget) {
        ((ImageSearchTarget) p).addImageNamesRecursively(s);
      }
    }
  }

  /**
   * @return true if the marker is defined from scratch, false if the marker
   * is defined as a component in the Game Piece Palette
   */
  public boolean isMarkerStandalone() {
    return markerSpec != null && markerSpec.startsWith(BasicCommandEncoder.ADD);
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.PlaceMarker.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Marker.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    final String name = st.nextToken();
    key = st.nextNamedKeyStroke(null);
    command = new KeyCommand(name, key, this, this);
    if (name.length() > 0 && key != null) {
      commands = new KeyCommand[]{command};
    }
    else {
      commands = KeyCommand.NONE;
    }
    markerSpec = st.nextToken();
    if ("null".equals(markerSpec)) { // NON-NLS
      markerSpec = null;
    }
    markerText = st.nextToken("null"); // NON-NLS
    if ("null".equals(markerText)) { // NON-NLS
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

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command.getName(), getCommandDescription(description, Resources.getString("Editor.PlaceMarker.place_marker_command")));
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

  // Implement Loopable
  @Override
  public String getComponentName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  @Override
  public String getComponentTypeName() {
    return getDescription();
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof PlaceMarker)) return false;
    final PlaceMarker c = (PlaceMarker) o;

    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(markerSpec, c.markerSpec)) return false;
    if (! Objects.equals(markerText, c.markerText)) return false;
    if (! Objects.equals(xOffset, c.xOffset)) return false;
    if (! Objects.equals(yOffset, c.yOffset)) return false;
    if (! Objects.equals(matchRotation, c.matchRotation)) return false;
    if (! Objects.equals(afterBurnerKey, c.afterBurnerKey)) return false;
    if (! Objects.equals(description, c.description)) return false;
    if (! Objects.equals(gpId, c.gpId)) return false;
    if (! Objects.equals(placement, c.placement)) return false;

    return Objects.equals(above, c.above);
  }

  protected static class Ed implements PieceEditor {
    private final NamedHotKeyConfigurer keyInput;
    private final StringConfigurer commandInput;
    private final PieceSlot pieceInput;
    private final TraitConfigPanel p;
    private String markerSlotPath;
    protected JButton defineButton = new JButton(Resources.getString("Editor.PlaceMarker.define_marker"));
    protected JButton selectButton = new JButton(Resources.getString("Editor.select"));
    protected IntConfigurer xOffsetConfig;
    protected IntConfigurer yOffsetConfig;
    protected BooleanConfigurer matchRotationConfig;
    protected BooleanConfigurer aboveConfig;
    private JLabel aboveLabel;
    protected TranslatingStringEnumConfigurer placementConfig;
    protected NamedHotKeyConfigurer afterBurner;
    protected StringConfigurer descConfig;
    private final String slotId;
    private final JPanel visPanel;

    protected Ed(PlaceMarker piece) {

      final GamePiece marker = piece.createBaseMarker();
      markerSlotPath = piece.markerSpec;
      p = new TraitConfigPanel();

      descConfig = new StringConfigurer(piece.description);
      descConfig.setHintKey("Editor.description_hint");
      p.add("Editor.description_label", descConfig);

      commandInput = new StringConfigurer(piece.command.getName());
      commandInput.setHintKey("Editor.menu_command_hint");
      p.add("Editor.menu_command", commandInput);

      keyInput = new NamedHotKeyConfigurer(piece.key);
      p.add("Editor.keyboard_command", keyInput);

      final JPanel b = new JPanel(new MigLayout("ins 0", "[][][]")); // NON-NLS
      visPanel = new JPanel(new MigLayout("ins 0", "[grow]", "[grow]")); // NON-NLS
      pieceInput = new PieceSlot(marker);
      pieceInput.setGpidSupport(piece.gpidSupport);
      pieceInput.setGpId(piece.getGpId());
      visPanel.add(pieceInput.getComponent(), "grow"); // NON-NLS
      b.add(visPanel);

      defineButton.addActionListener(e -> {
        markerSlotPath = null;
        new PropertiesWindow((Frame) SwingUtilities.getAncestorOfClass(Frame.class,  p), true, pieceInput, null).setVisible(true);
        adjustVisualiserSize();
      });
      b.add(defineButton);
      adjustVisualiserSize();

      selectButton.addActionListener(e -> {
        final ChoosePieceDialog d = new ChoosePieceDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, p), PieceSlot.class);
        d.setVisible(true);
        if (d.getTarget() instanceof PieceSlot) {
          pieceInput.setPiece(((PieceSlot) d.getTarget()).getPiece());
          adjustVisualiserSize();
        }
        if (d.getPath() != null) {
          markerSlotPath = ComponentPathBuilder.getInstance().getId(d.getPath());
        }
        else {
          markerSlotPath = null;
        }
      });
      b.add(selectButton);
      p.add("Editor.Placemarker.marker_definition", b);

      xOffsetConfig = new IntConfigurer(piece.xOffset);
      p.add("Editor.PlaceMarker.horizontal_offset", xOffsetConfig);

      yOffsetConfig = new IntConfigurer(piece.yOffset);
      p.add("Editor.PlaceMarker.vertical_offset", yOffsetConfig);

      matchRotationConfig = createMatchRotationConfig();
      matchRotationConfig.setValue(piece.matchRotation);
      final JLabel matchLabel = new JLabel(matchRotationConfig.getName());
      matchRotationConfig.setName("");
      p.add(matchLabel, matchRotationConfig);

      aboveConfig = createAboveConfig();
      if (aboveConfig != null) {
        aboveConfig.setValue(piece.above);
        aboveLabel = new JLabel(aboveConfig.getName());
        aboveConfig.setName("");
        p.add(aboveLabel, aboveConfig);
        aboveConfig.getControls().setVisible(piece.matchRotation);
        aboveLabel.setVisible(piece.matchRotation);
        matchRotationConfig.addPropertyChangeListener(e -> {
          aboveConfig.getControls().setVisible(matchRotationConfig.getValueBoolean());
          aboveLabel.setVisible(matchRotationConfig.getValueBoolean());
        });
      }

      placementConfig = new TranslatingStringEnumConfigurer(
        new String[] {"0", "1", "2", "3"},
        new String[] {"Editor.Placemarker.on_top_of_stack", "Editor.Placemarker.on_bottom_of_stack", "Editor.Placemarker.above_this_piece", "Editor.Placemarker.below_this_piece"},
        String.valueOf(piece.placement)
      );
      p.add("Editor.PlaceMarker.place_marker", placementConfig);

      afterBurner = new NamedHotKeyConfigurer(piece.afterBurnerKey);
      p.add("Editor.PlaceMarker.keystroke.after.placement", afterBurner);

      slotId = piece.getGpId();
    }

    private void adjustVisualiserSize() {
      if (pieceInput.getPiece() == null) {
        final int s = (int) defineButton.getPreferredSize().getHeight();
        visPanel.setMinimumSize(new Dimension(s * 2, s));
      }
      else {
        visPanel.setMinimumSize(pieceInput.getPreferredSize());
      }
      repack(visPanel);
    }

    protected BooleanConfigurer createMatchRotationConfig() {
      return new BooleanConfigurer(null, Resources.getString("Editor.PlaceMarker.match_rotation"));
    }

    protected BooleanConfigurer createAboveConfig() {
      return null;
    }

    @Override
    public Component getControls() {
      return p;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(commandInput.getValueString());
      se.append(keyInput.getValueString());
      if (pieceInput.getPiece() == null) {
        se.append("null"); // NON-NLS
      }
      else if (markerSlotPath != null) {
        se.append(markerSlotPath);
      }
      else {
        final String spec = GameModule.getGameModule().encode(new AddPiece(pieceInput.getPiece()));
        se.append(spec);
      }
      se.append("null"); // Older versions specified a text message to echo. Now performed by the ReportState trait, // NON-NLS
                          // but we remain backward-compatible.
      se.append(xOffsetConfig.getValueString());
      se.append(yOffsetConfig.getValueString());
      se.append(matchRotationConfig.getValueString());
      se.append(afterBurner.getValueString());
      se.append(descConfig.getValueString());
      se.append(slotId);
      se.append(placementConfig.getSelectedIndex());
      se.append(aboveConfig == null ? "false" : aboveConfig.getValueString()); // NON-NLS
      return ID + se.getValue();
    }
    public static class ChoosePieceDialog extends ChooseComponentPathDialog {
      private static final long serialVersionUID = 1L;

      public ChoosePieceDialog(Frame owner, Class<PieceSlot> targetClass) {
        super(owner, targetClass);
      }

      @Override
      protected boolean isValidTarget(Object selected) {
        return super.isValidTarget(selected) || selected instanceof CardSlot;
      }
    }
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(key, afterBurnerKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(command.getName());
  }
}
