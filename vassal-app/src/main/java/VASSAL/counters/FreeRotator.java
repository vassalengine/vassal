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

import VASSAL.tools.ProblemDialog;
import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Random;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.Drawable;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imageop.GamePieceOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.RotateScaleOp;
import VASSAL.tools.swing.SwingUtils;

/**
 * A Decorator that rotates a GamePiece to an arbitrary angle
 */
public class FreeRotator extends Decorator
                         implements EditablePiece,
                                    MouseListener,
                                    MouseMotionListener,
                                    Drawable,
                                    TranslatablePiece {
  public static final String ID = "rotate;"; //$NON-NLS-1$//

  public static final String FACING = "_Facing"; //$NON-NLS-1$//

  public static final String DEGREES = "_Degrees"; //$NON-NLS-1$//

  public static final double PI_180 = Math.PI / 180.0;

  protected KeyCommand setAngleCommand;
  protected KeyCommand rotateCWCommand;
  protected KeyCommand rotateCCWCommand;
  protected KeyCommand[] commands;
  protected NamedKeyStroke setAngleKey;
  protected String setAngleText = Resources.getString("Editor.FreeRotator.default_rotate_command");
  protected NamedKeyStroke rotateCWKey;
  protected String rotateCWText = Resources.getString("Editor.FreeRotator.default_rotate_cw_command");
  protected NamedKeyStroke rotateCCWKey;
  protected String rotateCCWText = Resources.getString("Editor.FreeRotator.default_rotate_ccw_command");
  protected String name = Resources.getString("Editor.FreeRotator.default_trait_name");
  protected String description = "";

  // for Random Rotate
  protected KeyCommand rotateRNDCommand;
  protected String rotateRNDText = "";
  protected NamedKeyStroke rotateRNDKey;
  // END for Random Rotate

  protected boolean useUnrotatedShape;

  protected double[] validAngles = {0.0};
  protected int angleIndex = 0;

  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected java.util.Map<Double, Image> images = new HashMap<>();
  protected java.util.Map<Double, Rectangle> bounds = new HashMap<>();

  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected PieceImage unrotated;
  protected GamePieceOp gpOp;
  protected java.util.Map<Double, RotateScaleOp> rotOp = new HashMap<>();

  protected double tempAngle, startAngle;
  protected Point pivot;
  protected boolean drawGhost;

  protected Map startMap;
  protected Point startPosition;

  public FreeRotator() {
    // modified for random rotation (added two ; )
    this(ID + "6;];[;" + Resources.getString("Editor.FreeRotator.default_rotate_cw_command") + ";" + Resources.getString("Editor.FreeRotator.default_rotate_ccw_command") + ";;;;", null); //$NON-NLS-1$//
  }

  public FreeRotator(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public void setInner(GamePiece p) {
    // The GamePiece stack can be in an invalid state during a setInner()
    // call, so cannot regenerate gpOp now.
    gpOp = null;
    super.setInner(p);
  }

  private double centerX() {
    // The center is not on a vertex for pieces with odd widths.
    return (piece.boundingBox().width % 2) / 2.0;
  }

  private double centerY() {
    // The center is not on a vertex for pieces with odd heights.
    return (piece.boundingBox().height % 2) / 2.0;
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle b = piece.boundingBox();
    final double angle = getAngle();

    if (angle == 0.0) {
      return b;
    }

    Rectangle r;
    if ((getGpOp() != null && getGpOp().isChanged()) ||
        (r = bounds.get(angle)) == null) {

      r = AffineTransform.getRotateInstance(getAngleInRadians(),
                                            centerX(),
                                            centerY())
                         .createTransformedShape(b).getBounds();
      bounds.put(angle, r);
    }

    return new Rectangle(r);
  }

  protected GamePieceOp getGpOp() {
    if (gpOp == null) {
      if (getInner() != null) {
        gpOp = Op.piece(getInner());
      }
    }
    return gpOp;
  }

  public double getAngle() {
    return useUnrotatedShape ? 0.0 : validAngles[angleIndex];
  }

  public double getCumulativeAngle() {
    double angle = getAngle();
    // Add cumulative angle of any other FreeRotator trait in this piece
    final FreeRotator nextRotation = (FreeRotator) Decorator.getDecorator(getInner(), FreeRotator.class);
    if (nextRotation != null) {
      angle += nextRotation.getCumulativeAngle();
    }
    return angle;
  }

  public double getCumulativeAngleInRadians() {
    return -PI_180 * getCumulativeAngle();
  }

  public void setAngle(double angle) {
    if (validAngles.length == 1) {
      validAngles[angleIndex] = angle;
    }
    else {
      // We (stupidly) store allowed angles in descending order from 0.
      // Normalize the angle to be in (-360, 0] to match that.
      angle = ((angle % 360) - 360) % 360;
      // ex is the expected index of the angle in angles array
      final double ex = (-angle / 360) * validAngles.length;
      angleIndex = ((int) Math.round(ex)) % validAngles.length;
    }
  }

  /** @deprecated Use {@link #boundingBox()} instead. */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Rectangle getRotatedBounds() {
    ProblemDialog.showDeprecated("2020-08-06");
    return boundingBox();
  }

  @Override
  public Shape getShape() {
    final Shape s = piece.getShape();

    if (getAngle() == 0.0) {
      return s;
    }

    return AffineTransform.getRotateInstance(getAngleInRadians(),
                                             centerX(),
                                             centerY())
                          .createTransformedShape(s);
  }

  public double getAngleInRadians() {
    return -PI_180 * getAngle();
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    validAngles = new double[Integer.parseInt(st.nextToken())];
    for (int i = 0; i < validAngles.length; ++i) {
      validAngles[i] = -i * (360.0 / validAngles.length);
    }
    if (validAngles.length == 1) {
      setAngleKey = st.nextNamedKeyStroke(null);
      if (st.hasMoreTokens()) {
        setAngleText = st.nextToken();
      }
    }
    else {
      rotateCWKey = st.nextNamedKeyStroke(null);
      rotateCCWKey = st.nextNamedKeyStroke(null);
      rotateCWText = st.nextToken("");
      rotateCCWText = st.nextToken("");
    }
    // for random rotation
    rotateRNDKey = st.nextNamedKeyStroke(null);
    rotateRNDText = st.nextToken("");
    // end for random rotation
    name = st.nextToken("");
    description = st.nextToken("");

    commands = null;
  }

  @Override
  public void draw(final Graphics g,
                   final int x,
                   final int y,
                   final Component obs,
                   final double zoom) {
    if (getAngle() == 0.0) {
      piece.draw(g, x, y, obs, zoom);
    }
    else {
      final double angle = getAngle();
      RotateScaleOp op;

      if (getGpOp() != null && getGpOp().isChanged()) {
        gpOp = Op.piece(piece);
        bounds.clear();
        rotOp.clear();
        op = Op.rotateScale(gpOp, angle, zoom);
        rotOp.put(angle, op);
      }
      else {
        op = rotOp.get(angle);
        if (op == null || op.getScale() != zoom) {
          op = Op.rotateScale(gpOp, angle, zoom);
          rotOp.put(angle, op);
        }
      }

      final Rectangle r = boundingBox();

      final Image img = op.getImage();
      if (img != null) {
        g.drawImage(img, x + (int) (zoom * r.x), y + (int) (zoom * r.y), obs);
      }
    }
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (!drawGhost) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g.create();
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    final Point p = map.mapToDrawing(getGhostPosition(), os_scale);

    g2d.transform(
       AffineTransform.getRotateInstance(-PI_180 * tempAngle,
                                         p.x + centerX(),
                                         p.y + centerY()));
    g2d.setComposite(
       AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                         RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);
    piece.draw(g2d, p.x, p.y, map.getView(), map.getZoom() * os_scale);
    g2d.dispose();
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  private Point getGhostPosition() {
    final AffineTransform t =
      AffineTransform.getRotateInstance(-PI_180 * (tempAngle - getAngle()),
                                        pivot.x + centerX(),
                                        pivot.y + centerY());
    final Point2D newPos2D =
      new Point2D.Float(getPosition().x, getPosition().y);
    t.transform(newPos2D, newPos2D);
    return new Point((int) Math.round(newPos2D.getX()),
                     (int) Math.round(newPos2D.getY()));
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(validAngles.length);
    if (validAngles.length == 1) {
      se.append(setAngleKey);
      se.append(setAngleText);
    }
    else {
      se.append(rotateCWKey)
        .append(rotateCCWKey)
        .append(rotateCWText)
        .append(rotateCCWText);
    }
    // for random rotation
    se.append(rotateRNDKey)
      .append(rotateRNDText);
    // end for random rotation
    se.append(name);
    se.append(description);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    if (validAngles.length == 1) {
      return String.valueOf(validAngles[0]);
    }
    else {
      return String.valueOf(angleIndex);
    }
  }

  @Override
  public void mySetState(String state) {
    if (validAngles.length == 1) {
      try {
        validAngles[0] = Double.parseDouble(state);
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"), "Angle=" + state, e); // NON-NLS
      }
    }
    else {
      try {
        angleIndex = Integer.parseInt(state);
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"), "Fixed Angle Index=" + state, e); // NON-NLS
      }
    }
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<>();
      final GamePiece outer = Decorator.getOutermost(this);
      setAngleCommand = new KeyCommand(setAngleText, setAngleKey, outer, this);
      rotateCWCommand = new KeyCommand(rotateCWText, rotateCWKey, outer, this);
      rotateCCWCommand = new KeyCommand(rotateCCWText, rotateCCWKey, outer, this);

      // for random rotation
      rotateRNDCommand = new KeyCommand(rotateRNDText, rotateRNDKey, outer, this);
      // end random rotation

      if (validAngles.length == 1) {
        if (setAngleText.length() > 0) {
          l.add(setAngleCommand);
        }
        else {
          setAngleCommand.setEnabled(false);
        }
        rotateCWCommand.setEnabled(false);
        rotateCCWCommand.setEnabled(false);
      }
      else {
        if (rotateCWText.length() > 0 && rotateCCWText.length() > 0) {
          l.add(rotateCWCommand);
          l.add(rotateCCWCommand);
        }
        else if (rotateCWText.length() > 0) {
          l.add(rotateCWCommand);
          rotateCCWCommand.setEnabled(rotateCCWKey != null);
        }
        else if (rotateCCWText.length() > 0) {
          l.add(rotateCCWCommand);
          rotateCWCommand.setEnabled(rotateCWKey != null);
        }
        setAngleCommand.setEnabled(false);
      }
      // for random rotate
      if (rotateRNDText.length() > 0) {
        l.add(rotateRNDCommand);
      }
      // end for random rotate
      commands = l.toArray(new KeyCommand[0]);
    }
    setAngleCommand.setEnabled(getMap() != null &&
                               validAngles.length == 1 &&
                               setAngleText.length() > 0);
    return commands;
  }


  /*
   * Move a single piece to a destination
   */
  protected Command movePiece(GamePiece gp, Point dest) {

    // Is the piece on a map?
    final Map map = gp.getMap();
    if (map == null) {
      return null;
    }

    // Set the Old... properties
    Command c = putOldProperties(this);

    // Mark the piece moved
    final GamePiece outer = Decorator.getOutermost(gp);
    final ChangeTracker comm = new ChangeTracker(outer);
    outer.setProperty(Properties.MOVED, Boolean.TRUE);
    c = c.append(comm.getChangeCommand());

    // Move the piece
    c = c.append(map.placeOrMerge(outer, dest));

    // Apply after Move Key
    if (map.getMoveKey() != null) {
      c = c.append(outer.keyEvent(map.getMoveKey()));
    }

    // Unlink from Parent Stack (in case it is a Deck).
    final Stack parent = outer.getParent();
    if (parent != null) {
      c = c.append(parent.pieceRemoved(outer));
    }

    if (GameModule.getGameModule().isMatSupport()) {
      // If a cargo piece has been "sent", find it a new Mat if needed.
      if ("true".equals(outer.getProperty(MatCargo.IS_CARGO))) { //NON-NLS
        final MatCargo cargo = (MatCargo) Decorator.getDecorator(outer, MatCargo.class);
        if (cargo != null) {
          c = c.append(cargo.findNewMat());
        }
      }
    }

    return c;
  }

  protected Command rotateCargo(Command command, Point center, double dtheta) {
    if (!GameModule.getGameModule().isMatSupport()) {
      return command;
    }

    final GamePiece outer = getOutermost(this);
    if (outer.getMap() == null) {
      return command;
    }

    final Mat mat = (Mat) Decorator.getDecorator(outer, Mat.class);
    if (mat == null || "".equals(mat.getProperty(Mat.MAT_NAME))) {
      return command;
    }

    final AffineTransform t = AffineTransform.getRotateInstance(dtheta * -PI_180, center.x, center.y);

    // If a Mat has been rotated, make the contents orbit the Mat's center
    for (final GamePiece piece : mat.getContents()) {
      final MatCargo cargo = (MatCargo) Decorator.getDecorator(piece, MatCargo.class);
      if (cargo != null) {
        // Rotate Cargo's current position to its destination
        final Point dst = new Point();
        t.transform(piece.getPosition(), dst);
        command = command.append(movePiece(piece, dst));
      }
    }

    return command;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (setAngleCommand.matches(stroke)) {
      beginInteractiveRotate();
      return null;
    }

    Command c = null;

    final double origAngle = getAngle();

    if (rotateCWCommand.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      angleIndex = (angleIndex + 1) % validAngles.length;
      c = tracker.getChangeCommand();
    }
    else if (rotateCCWCommand.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      angleIndex = (angleIndex - 1 + validAngles.length) % validAngles.length;
      c = tracker.getChangeCommand();
    }
    // for random rotation
    else if (rotateRNDCommand.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      // get random #
      final Random rand = GameModule.getGameModule().getRNG();
      if (validAngles.length == 1) {
        // we are a free rotate, set angle to 0-360 use setAngle(double)
        setAngle(rand.nextDouble() * 360);
      }
      else {
        // we are set rotate, set angleIndex to a number between 0 and
        // validAngles.length
        angleIndex = rand.nextInt(validAngles.length);
      }
      c = tracker.getChangeCommand();
    }
    // end random rotation

    // Mat Support
    if (c != null) {
      c = rotateCargo(c, getPosition(), getAngle() - origAngle);
    }

    return c;
  }

  public void beginInteractiveRotate() {
    startPosition = getPosition();
    startMap = getMap();
    startMap.pushMouseListener(this);
    startMap.addDrawComponent(this);

    final JComponent view = startMap.getView();
    view.addMouseMotionListener(this);
    view.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));

    startMap.disableKeyListeners();

    pivot = getPosition();
  }

  public void endInteractiveRotate() {
    if (startMap != null) {
      startMap.getView().setCursor(null);
      startMap.removeDrawComponent(this);
      startMap.popMouseListener();
      startMap.getView().removeMouseMotionListener(this);
      startMap.enableKeyListeners();
      drawGhost = false;
      startMap = null;
    }
  }

  /**
   * Has the piece been moved by a Global key command since interactive
   * rotate mode was turned on?
   */
  public boolean hasPieceMoved() {
    final Map m = getMap();
    final Point p = getPosition();
    return m == null || m != startMap || p == null || !p.equals(startPosition);
  }

  /** The point around which the piece will pivot while rotating interactively */
  public void setPivot(int x, int y) {
    pivot = new Point(x, y);
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (SwingUtils.isMainMouseButtonDown(e)) {
      if (hasPieceMoved()) {
        endInteractiveRotate();
        return;
      }
      drawGhost = true;
      startAngle = getRelativeAngle(e.getPoint(), getPosition());
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    if (SwingUtils.isMainMouseButtonDown(e)) {
      if (hasPieceMoved()) {
        endInteractiveRotate();
        return;
      }

      final Map m = getMap();
      final GamePiece outer = getOutermost(this);

      try {
        final Point ghostPosition = getGhostPosition();

        Command c = null;
        final ChangeTracker tracker = new ChangeTracker(this);
        if (!getPosition().equals(ghostPosition)) {
          outer.setProperty(Properties.MOVED, Boolean.TRUE);
          c = m.placeOrMerge(outer, m.snapTo(ghostPosition));
        }
        final double origAngle = getAngle();
        setAngle(tempAngle);
        c = tracker.getChangeCommand().append(c);

        // Mat Support
        c = rotateCargo(c, getPosition(), tempAngle - origAngle);

        GameModule.getGameModule().sendAndLog(c);
      }
      finally {
        endInteractiveRotate();
      }
    }
  }

  @Override
  public void setProperty(Object key, Object val) {
    if (Properties.USE_UNROTATED_SHAPE.equals(key)) {
      useUnrotatedShape = Boolean.TRUE.equals(val);
    }
    super.setProperty(key, val);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if ((name + FACING).equals(key)) {
      return String.valueOf(angleIndex + 1);
    }
    else if ((name + DEGREES).equals(key)) {
      return String.valueOf((int) (Math.abs(validAngles[angleIndex])));
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if ((name + FACING).equals(key)) {
      return String.valueOf(angleIndex + 1);
    }
    else if ((name + DEGREES).equals(key)) {
      return String.valueOf((int) (Math.abs(validAngles[angleIndex])));
    }
    else {
      return super.getProperty(key);
    }
  }

  @Override
  public void mouseDragged(MouseEvent e) {
    if (SwingUtils.isMainMouseButtonDown(e) && !hasPieceMoved()) { // hasPieceMoved() protects from NPE if gone from map
      if (drawGhost) {
        final Point mousePos = getMap().componentToMap(e.getPoint());
        final double myAngle = getRelativeAngle(mousePos, pivot);
        tempAngle = getAngle() - (myAngle - startAngle) / PI_180;
      }
      getMap().repaint();
    }
  }

  private double getRelativeAngle(Point p, Point origin) {
    double myAngle;
    if (p.y == origin.y) {
      myAngle = p.x < origin.x ? -Math.PI / 2.0 : Math.PI / 2.0;
    }
    else {
      myAngle = Math.atan((double)(p.x - origin.x) / (origin.y - p.y));
      if (origin.y < p.y) {
        myAngle += Math.PI;
      }
    }
    return myAngle;
  }

  @Override
  public void mouseMoved(MouseEvent e) {
    if (hasPieceMoved()) {
      endInteractiveRotate();
    }
  }

  /**
   * Return a full-scale cached image of this piece, rotated to the appropriate
   * angle.
   *
   * @param angle Rotation angle
   * @param obs Component observer
   * @return Rotated Image
   * @deprecated Use a {@link GamePieceOp} if you need this Image.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image getRotatedImage(double angle, Component obs) {
    ProblemDialog.showDeprecated("2020-08-06");
    if (gpOp == null) return null;

    if (gpOp.isChanged()) gpOp = Op.piece(piece);

    return Op.rotateScale(gpOp, angle, 1.0).getImage();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.FreeRotator.trait_description", name, description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Rotate.html"); //$NON-NLS-1$//
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {setAngleText, rotateCWText, rotateCCWText, rotateRNDText},
                       new String[] {
                         getCommandDescription(name, Resources.getString("Editor.FreeRotator.set_angle_command_description")),
                         getCommandDescription(name, Resources.getString("Editor.FreeRotator.rotate_cw_command_description")),
                         getCommandDescription(name, Resources.getString("Editor.FreeRotator.rotate_ccw_command_description")),
                         getCommandDescription(name, Resources.getString("Editor.FreeRotator.rotate_random_command_description"))});
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(name + FACING);
    l.add(name + DEGREES);
    return l;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof FreeRotator)) return false;
    final FreeRotator c = (FreeRotator) o;
    if (! Objects.equals(validAngles.length, c.validAngles.length)) return false;

    if (validAngles.length == 1) {
      if (! Objects.equals(setAngleKey, c.setAngleKey)) return false;
      if (! Objects.equals(setAngleText, c.setAngleText)) return false;
    }
    else {
      if (! Objects.equals(rotateCWKey, c.rotateCWKey)) return false;
      if (! Objects.equals(rotateCCWKey, c.rotateCCWKey)) return false;
      if (! Objects.equals(rotateCWText, c.rotateCWText)) return false;
      if (! Objects.equals(rotateCCWText, c.rotateCCWText)) return false;
    }

    if (! Objects.equals(rotateRNDKey, c.rotateRNDKey)) return false;

    if (! Objects.equals(name, c.name)) return false;

    if (validAngles.length == 1) {
      return Objects.equals(validAngles[0], c.validAngles[0]);
    }
    else {
      return Objects.equals(angleIndex, c.angleIndex);
    }
  }

  private static class Ed implements PieceEditor, PropertyChangeListener {
    private final BooleanConfigurer anyConfig;
    private final NamedHotKeyConfigurer anyKeyConfig;
    private final JLabel facingsLabel;
    private final IntConfigurer facingsConfig;
    private final NamedHotKeyConfigurer cwKeyConfig;
    private final JLabel cwLabel;
    private final NamedHotKeyConfigurer ccwKeyConfig;
    private final JLabel ccwLabel;
    private final NamedHotKeyConfigurer rndKeyConfig;
    private final StringConfigurer nameConfig;
    private final StringConfigurer descConfig;
    private final JLabel anyLabel;

    private final StringConfigurer anyCommand;
    private final StringConfigurer cwCommand;
    private final StringConfigurer ccwCommand;
    private final StringConfigurer rndCommand;

    private final  TraitConfigPanel panel;

    public Ed(FreeRotator p) {

      panel = new TraitConfigPanel(
        new TraitLayout(
          false,
          TraitLayout.STANDARD_INSETS + "," + TraitLayout.STANDARD_GAPY + ",hidemode 3,wrap 3", // NON-NLS
          "[right]rel[fill,grow 1]rel[fill,grow 2]")); // NON-NLS

      descConfig = new StringConfigurer(p.description);
      descConfig.setHintKey("Editor.description_hint");
      panel.add("Editor.description_label", descConfig, "span 2,wrap"); // NON-NLS

      nameConfig = new StringConfigurer(p.name);
      nameConfig.setHintKey("Editor.trait_name_hint");
      panel.add("Editor.FreeRotator.name", nameConfig, "span 2,wrap"); // NON-NLS

      anyConfig = new BooleanConfigurer(p.validAngles.length == 1);
      panel.add("Editor.FreeRotator.allow_arbitrary_rotations", anyConfig, "wrap"); // NON-NLS

      facingsLabel = new JLabel(Resources.getString("Editor.FreeRotator.number_of_allowed_facings"));
      facingsConfig = new IntConfigurer(p.validAngles.length == 1 ? 6 : p.validAngles.length);
      panel.add(facingsLabel, facingsConfig, "wrap"); // NON-NLS

      final JLabel menuLabel = new JLabel(Resources.getString("Editor.menu_command"));
      final Font boldFont = new Font(menuLabel.getFont().getFamily(), Font.BOLD, menuLabel.getFont().getSize());
      menuLabel.setFont(boldFont);
      panel.add(new JLabel(""));
      panel.add(menuLabel, "grow 0,align center"); // NON-NLS
      final JLabel keyLabel = new JLabel(Resources.getString("Editor.keyboard_command"));
      keyLabel.setFont(boldFont);
      panel.add(keyLabel, "grow 0,align center,wrap"); // NON-NLS

      cwLabel = new JLabel(Resources.getString("Editor.FreeRotator.rotate_clockwise"));
      panel.add(cwLabel);
      cwCommand = new StringConfigurer(p.rotateCWText);
      cwCommand.setHintKey("Editor.menu_command_hint");
      panel.add(cwCommand.getControls());
      cwKeyConfig = new NamedHotKeyConfigurer(p.rotateCWKey);
      panel.add(cwKeyConfig.getControls(), "wrap"); // NON-NLS

      ccwLabel = new JLabel(Resources.getString("Editor.FreeRotator.rotate_counter_clockwise"));
      panel.add(ccwLabel);
      ccwCommand = new StringConfigurer(p.rotateCCWText);
      ccwCommand.setHintKey("Editor.menu_command_hint");
      panel.add(ccwCommand.getControls());
      ccwKeyConfig = new NamedHotKeyConfigurer(p.rotateCCWKey);
      panel.add(ccwKeyConfig.getControls(), "wrap"); // NON-NLS

      anyLabel = new JLabel(Resources.getString("Editor.FreeRotator.rotate"));
      panel.add(anyLabel);
      anyCommand = new StringConfigurer(p.setAngleText);
      anyCommand.setHintKey("Editor.menu_command_hint");
      panel.add(anyCommand.getControls());
      anyKeyConfig = new NamedHotKeyConfigurer(p.setAngleKey);
      panel.add(anyKeyConfig.getControls(), "wrap"); // NON-NLS

      final JLabel rndLabel = new JLabel(Resources.getString("Editor.FreeRotator.rotate_randomly"));
      panel.add(rndLabel);
      rndCommand = new StringConfigurer(p.rotateRNDText);
      rndCommand.setHintKey("Editor.menu_command_hint");
      panel.add(rndCommand.getControls());
      rndKeyConfig = new NamedHotKeyConfigurer(p.rotateRNDKey);
      panel.add(rndKeyConfig.getControls(), "wrap"); // NON-NLS

      anyConfig.addPropertyChangeListener(this);
      propertyChange(null);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      final boolean any = Boolean.TRUE.equals(anyConfig.getValue());

      anyCommand.getControls().setVisible(any);
      anyKeyConfig.getControls().setVisible(any);
      anyLabel.setVisible(any);

      facingsLabel.setVisible(!any);
      facingsConfig.getControls().setVisible(!any);

      cwCommand.getControls().setVisible(!any);
      cwKeyConfig.getControls().setVisible(!any);
      cwLabel.setVisible(!any);

      ccwCommand.getControls().setVisible(!any);
      ccwKeyConfig.getControls().setVisible(!any);
      ccwLabel.setVisible(!any);

      panel.revalidate();
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      if (Boolean.TRUE.equals(anyConfig.getValue())) {
        se.append("1")
          .append(anyKeyConfig.getValueString())
          .append(anyCommand.getValueString() == null ? "" : anyCommand.getValueString().trim());
      }
      else {
        se.append(facingsConfig.getValueString())
          .append(cwKeyConfig.getValueString())
          .append(ccwKeyConfig.getValueString())
          .append(cwCommand.getValueString() == null ? "" : cwCommand.getValueString().trim())
          .append(ccwCommand.getValueString() == null ? "" : ccwCommand.getValueString().trim());
      }
      se.append(rndKeyConfig.getValueString())
        .append(rndCommand.getValueString() == null ? "" : rndCommand.getValueString().trim());
      se.append(nameConfig.getValueString());
      se.append(descConfig.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "0";
    }
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(setAngleKey, rotateCWKey, rotateCCWKey, rotateRNDKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(setAngleText, rotateCWText, rotateCCWText, rotateRNDText);
  }
}


