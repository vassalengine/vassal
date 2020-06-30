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

import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Cursor;
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
import java.util.HashMap;
import java.util.List;
import java.util.Random;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
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
  public static final String ID = "rotate;";

  public static final String FACING = "_Facing";

  public static final String DEGREES = "_Degrees";

  public static final double PI_180 = Math.PI / 180.0;

  protected KeyCommand setAngleCommand;
  protected KeyCommand rotateCWCommand;
  protected KeyCommand rotateCCWCommand;
  protected KeyCommand[] commands;
  protected NamedKeyStroke setAngleKey;
  protected String setAngleText = "Rotate";
  protected NamedKeyStroke rotateCWKey;
  protected String rotateCWText = "Rotate CW";
  protected NamedKeyStroke rotateCCWKey;
  protected String rotateCCWText = "Rotate CCW";
  protected String name = "Rotate";

  // for Random Rotate
  protected KeyCommand rotateRNDCommand;
  protected String rotateRNDText = "";
  protected NamedKeyStroke rotateRNDKey;
  // END for Random Rotate

  protected boolean useUnrotatedShape;

  protected double[] validAngles = new double[] {0.0};
  protected int angleIndex = 0;

  @Deprecated
  protected java.util.Map<Double,Image> images = new HashMap<>();
  protected java.util.Map<Double,Rectangle> bounds = new HashMap<>();
  @Deprecated
  protected PieceImage unrotated;
  protected GamePieceOp gpOp;
  protected java.util.Map<Double,RotateScaleOp> rotOp = new HashMap<>();

  protected double tempAngle, startAngle;
  protected Point pivot;
  protected boolean drawGhost;

  protected Map startMap;
  protected Point startPosition;

  public FreeRotator() {
    // modified for random rotation (added two ; )
    this(ID + "6;];[;Rotate CW;Rotate CCW;;;;", null);
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
    FreeRotator nextRotation = (FreeRotator) Decorator.getDecorator(getInner(), FreeRotator.class);
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
      // Find nearest valid angle
      int newIndex = angleIndex;
      double minDist = Math.abs((validAngles[angleIndex] - angle + 360) % 360);
      for (int i = 0; i < validAngles.length; ++i) {
        if (minDist > Math.abs((validAngles[i] - angle + 360) % 360)) {
          newIndex = i;
          minDist = Math.abs((validAngles[i] - angle + 360) % 360);
        }
      }
      angleIndex = newIndex;
    }
  }

  /** @deprecated Use {@link boundingBox()} instead. */
  @Deprecated
  public Rectangle getRotatedBounds() {
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
        reportDataError(this, Resources.getString("Error.non_number_error"), "Angle="+state, e);
      }
    }
    else {
      try {
        angleIndex = Integer.parseInt(state);
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"), "Fixed Angle Index="+state, e);
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

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (setAngleCommand.matches(stroke)) {
      beginInteractiveRotate();
    }
    else if (rotateCWCommand.matches(stroke)) {
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
        // validAngles.lenth
        angleIndex = (rand.nextInt(validAngles.length));
      }
      c = tracker.getChangeCommand();
    }
    // end random rotation
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
    if (SwingUtils.isLeftMouseButton(e)) {
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
    if (SwingUtils.isLeftMouseButton(e)) {
      if (hasPieceMoved()) {
        endInteractiveRotate();
        return;
      }

      final Map m = getMap();

      try {
        final Point ghostPosition = getGhostPosition();
        Command c = null;
        final ChangeTracker tracker = new ChangeTracker(this);
        if (!getPosition().equals(ghostPosition)) {
          final GamePiece outer = Decorator.getOutermost(this);
          outer.setProperty(Properties.MOVED, Boolean.TRUE);
          c = m.placeOrMerge(outer, m.snapTo(ghostPosition));
        }
        setAngle(tempAngle);
        c = tracker.getChangeCommand().append(c);

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
    if (SwingUtils.isLeftMouseButton(e)) {
      if (drawGhost) {
        final Point mousePos = getMap().componentToMap(e.getPoint());
        final double myAngle = getRelativeAngle(mousePos, pivot);
        tempAngle = getAngle() - (myAngle - startAngle)/PI_180;
      }
      getMap().repaint();
    }
  }

  private double getRelativeAngle(Point p, Point origin) {
    double myAngle;
    if (p.y == origin.y) {
      myAngle = p.x < origin.x ? -Math.PI/2.0 : Math.PI/2.0;
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
   * @param angle
   * @param obs
   * @return
   * @deprecated Use a {@link GamePieceOp} if you need this Image.
   */
  @Deprecated
  public Image getRotatedImage(double angle, Component obs) {
    if (gpOp == null) return null;

    if (gpOp.isChanged()) gpOp = Op.piece(piece);

    return Op.rotateScale(gpOp, angle, 1.0).getImage();
  }

  @Override
  public String getDescription() {
    String d = "Can Rotate";
    if (name.length() > 0) {
      d += " - " + name;
    }
    return d;
  }

  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Rotate.htm");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {setAngleText, rotateCWText, rotateCCWText, rotateRNDText},
                       new String[] {getCommandDescription(name, "Set Angle command"), getCommandDescription(name, "Rotate CW command"), getCommandDescription(name, "Rotate CCW command"), getCommandDescription(name, "Rotate Random command")});
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    l.add(name + FACING);
    l.add(name + DEGREES);
    return l;
  }

  private static class Ed implements PieceEditor, PropertyChangeListener {
    private BooleanConfigurer anyConfig;
    private NamedHotKeyConfigurer anyKeyConfig;
    private IntConfigurer facingsConfig;
    private NamedHotKeyConfigurer cwKeyConfig;
    private NamedHotKeyConfigurer ccwKeyConfig;
    // random rotate
    private NamedHotKeyConfigurer rndKeyConfig;
    // end random rotate
    private StringConfigurer nameConfig;

    private JTextField anyCommand;
    private JTextField cwCommand;
    private JTextField ccwCommand;
    private JTextField rndCommand;
    private Box anyControls;
    private Box cwControls;
    private Box ccwControls;
    private Box rndControls;

    private JPanel panel;

    public Ed(FreeRotator p) {
      nameConfig = new StringConfigurer(null, "Description:  ", p.name);
      cwKeyConfig = new NamedHotKeyConfigurer(null, "Command to rotate clockwise:  ", p.rotateCWKey);
      ccwKeyConfig = new NamedHotKeyConfigurer(null, "Command to rotate counterclockwise:  ", p.rotateCCWKey);
      // random rotate
      rndKeyConfig = new NamedHotKeyConfigurer(null, "Command to rotate randomly:  ", p.rotateRNDKey);
      // end random rotate
      anyConfig = new BooleanConfigurer(null, "Allow arbitrary rotations",
        Boolean.valueOf(p.validAngles.length == 1));
      anyKeyConfig = new NamedHotKeyConfigurer(null, "Command to rotate:  ", p.setAngleKey);
      facingsConfig = new IntConfigurer(null, "Number of allowed facings:  ", p.validAngles.length == 1 ? 6 : p.validAngles.length);

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      panel.add(nameConfig.getControls());
      panel.add(facingsConfig.getControls());
      cwControls = Box.createHorizontalBox();
      cwControls.add(cwKeyConfig.getControls());
      cwControls.add(new JLabel(" Menu text:  "));
      cwCommand = new JTextField(12);
      cwCommand.setMaximumSize(cwCommand.getPreferredSize());
      cwCommand.setText(p.rotateCWText);
      cwControls.add(cwCommand);
      panel.add(cwControls);

      ccwControls = Box.createHorizontalBox();
      ccwControls.add(ccwKeyConfig.getControls());
      ccwControls.add(new JLabel(" Menu text:  "));
      ccwCommand = new JTextField(12);
      ccwCommand.setMaximumSize(ccwCommand.getPreferredSize());
      ccwCommand.setText(p.rotateCCWText);
      ccwControls.add(ccwCommand);
      panel.add(ccwControls);

      panel.add(anyConfig.getControls());
      anyControls = Box.createHorizontalBox();
      anyControls.add(anyKeyConfig.getControls());
      anyControls.add(new JLabel(" Menu text:  "));
      anyCommand = new JTextField(12);
      anyCommand.setMaximumSize(anyCommand.getPreferredSize());
      anyCommand.setText(p.setAngleText);
      anyControls.add(anyCommand);
      panel.add(anyControls);

      // random rotate
      rndControls = Box.createHorizontalBox();
      rndControls.add(rndKeyConfig.getControls());
      rndControls.add(new JLabel(" Menu text:  "));
      rndCommand = new JTextField(12);
      rndCommand.setMaximumSize(rndCommand.getPreferredSize());
      rndCommand.setText(p.rotateRNDText);
      rndControls.add(rndCommand);
      panel.add(rndControls);
      // end random rotate

      anyConfig.addPropertyChangeListener(this);
      propertyChange(null);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      final boolean any = Boolean.TRUE.equals(anyConfig.getValue());
      anyControls.setVisible(any);
      facingsConfig.getControls().setVisible(!any);
      cwControls.setVisible(!any);
      ccwControls.setVisible(!any);
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
          .append(anyCommand.getText() == null
                  ? "" : anyCommand.getText().trim());
      }
      else {
        se.append(facingsConfig.getValueString())
          .append(cwKeyConfig.getValueString())
          .append(ccwKeyConfig.getValueString())
          .append(cwCommand.getText() == null
                  ? "" : cwCommand.getText().trim())
          .append(ccwCommand.getText() == null
                  ? "" : ccwCommand.getText().trim());
      }
      // random rotate
      se.append(rndKeyConfig.getValueString())
        .append(rndCommand.getText() == null
                ? "" : rndCommand.getText().trim());
      // end random rotate
      se.append(nameConfig.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "0";
    }
  }
}
