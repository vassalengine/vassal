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
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;

import javax.swing.Box;
import javax.swing.BoxLayout;
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
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.tools.SequenceEncoder;

/**
 * A Decorator that rotates a GamePiece to an arbitrary angle
 */
public class FreeRotator extends Decorator implements EditablePiece, MouseListener, MouseMotionListener, Drawable {
  public static final String ID = "rotate;";
  
  public static final String FACING = "_Facing";

  public static final String DEGREES = "_Degrees";

  public static final double PI_180 = Math.PI / 180.0;

  protected KeyCommand setAngleCommand;
  protected KeyCommand rotateCWCommand;
  protected KeyCommand rotateCCWCommand;
  protected KeyCommand[] commands;
  protected KeyStroke setAngleKey;
  protected String setAngleText = "Rotate";
  protected KeyStroke rotateCWKey;
  protected String rotateCWText = "Rotate CW";
  protected KeyStroke rotateCCWKey;
  protected String rotateCCWText = "Rotate CCW";
  protected String name = "Rotate";

  // for Random Rotate
  protected KeyCommand rotateRNDCommand;
  protected String rotateRNDText = "";
  protected KeyStroke rotateRNDKey;
  // END for Random Rotate

  protected boolean useUnrotatedShape;

  protected double[] validAngles = new double[] {0.0};
  protected int angleIndex = 0;

  protected java.util.Map images = new HashMap();
  protected java.util.Map bounds = new HashMap();
  protected PieceImage unrotated;

  protected double tempAngle, startAngle;
  protected Point pivot;
  protected boolean drawGhost;

  public FreeRotator() {
    // modified for random rotation (added two ; )
    this(ID + "6;];[;Rotate CW;Rotate CCW;;;;", null);
  }

  public FreeRotator(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getName() {
    return piece.getName();
  }

  public void setInner(GamePiece p) {
    unrotated = new PieceImage(p);
    super.setInner(p);
  }

  private double centerX() {
    // The center is not on a vertex for pieces with odd widths.
    return (piece.boundingBox().getWidth() % 2) / 2.0;
  }

  private double centerY() {
    // The center is not on vertex for pieces with odd heights.
    return (piece.boundingBox().getHeight() % 2) / 2.0;
  }

  public Rectangle boundingBox() {
    if (getAngle() == 0.0) {
      return piece.boundingBox();
    }
    else {
      return AffineTransform
         .getRotateInstance(-PI_180 * getAngle(), centerX(), centerY())
         .createTransformedShape(piece.boundingBox()).getBounds();
    }
  }

  public double getAngle() {
    if (useUnrotatedShape) {
      return 0.0;
    }
    return validAngles[angleIndex];
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

  public Rectangle getRotatedBounds() {
    Rectangle r = (Rectangle) bounds.get(new Double(getAngle()));
    if (r == null) {
      r = piece.boundingBox();
    }
    return r;
  }

  public Shape getShape() {
    if (getAngle() == 0.0) {
      return piece.getShape();
    }
    return AffineTransform
      .getRotateInstance(getAngleInRadians(), centerX(), centerY()) 
      .createTransformedShape(piece.getShape());
  }

  public double getAngleInRadians() {
    return -PI_180 * getAngle();
  }

  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    validAngles = new double[Integer.parseInt(st.nextToken())];
    for (int i = 0; i < validAngles.length; ++i) {
      validAngles[i] = -i * (360.0 / validAngles.length);
    }
    if (validAngles.length == 1) {
      setAngleKey = st.nextKeyStroke(null);
      if (st.hasMoreTokens()) {
        setAngleText = st.nextToken();
      }
    }
    else {
      rotateCWKey = st.nextKeyStroke(null);
      rotateCCWKey = st.nextKeyStroke(null);
      rotateCWText = st.nextToken("");
      rotateCCWText = st.nextToken("");
    }
    // for random rotation
    rotateRNDKey = st.nextKeyStroke(null);
    rotateRNDText = st.nextToken("");
    // end for random rotation
    name = st.nextToken("");
    commands = null;
  }

  public void draw(final Graphics g, final int x, final int y, final Component obs, final double zoom) {
    if (getAngle() == 0.0) {
      piece.draw(g, x, y, obs, zoom);
    }
    else {
      Image rotated = getRotatedImage(getAngle(), obs);
      if (rotated != null) {
        Rectangle r = getRotatedBounds();
        Image zoomed = GameModule.getGameModule().getDataArchive().getScaledImage(rotated, zoom);
        g.drawImage(zoomed, x + (int) (zoom * r.x), y + (int) (zoom * r.y), obs);
      }
    }
  }

  public void draw(Graphics g, Map map) {
    if (drawGhost) {
      Point p = map.componentCoordinates(getGhostPosition());
      Graphics2D g2d = (Graphics2D) g;
      AffineTransform t = g2d.getTransform();
      g2d.transform(
         AffineTransform.getRotateInstance(-PI_180 * tempAngle,
                                           p.x + centerX(),
                                           p.y + centerY()));
      g2d.setComposite(
         AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                           RenderingHints.VALUE_INTERPOLATION_BILINEAR);
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON);
      piece.draw(g, p.x, p.y, map.getView(), map.getZoom());
      g2d.setTransform(t);
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  private Point getGhostPosition() {
    AffineTransform t = 
      AffineTransform.getRotateInstance(-PI_180 * (tempAngle - getAngle()),
                                        pivot.x + centerX(),
                                        pivot.y + centerY());
    Point2D newPos2D = new Point2D.Float(getPosition().x, getPosition().y);
    t.transform(newPos2D, newPos2D);
    return new Point((int) Math.round(newPos2D.getX()),
                     (int) Math.round(newPos2D.getY()));
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(validAngles.length);
    if (validAngles.length == 1) {
      se.append(setAngleKey);
      se.append(setAngleText);
    }
    else {
      se.append(rotateCWKey).append(rotateCCWKey).append(rotateCWText).append(rotateCCWText);
    }
    // for random rotation
    se.append(rotateRNDKey);
    se.append(rotateRNDText);
    // end for random rotation
    se.append(name);
    return ID + se.getValue();
  }

  public String myGetState() {
    if (validAngles.length == 1) {
      return "" + validAngles[0];
    }
    else {
      return "" + angleIndex;
    }
  }

  public void mySetState(String state) {
    if (validAngles.length == 1) {
      validAngles[0] = Double.valueOf(state).doubleValue();
    }
    else {
      angleIndex = Integer.parseInt(state);
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      java.util.List l = new ArrayList();
      GamePiece outer = Decorator.getOutermost(this);
      setAngleCommand = new KeyCommand(setAngleText, setAngleKey, outer);
      rotateCWCommand = new KeyCommand(rotateCWText, rotateCWKey, outer);

      rotateCCWCommand = new KeyCommand(rotateCCWText, rotateCCWKey, outer);

      // for random rotation
      rotateRNDCommand = new KeyCommand(rotateRNDText, rotateRNDKey, outer);
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
      commands = (KeyCommand[]) l.toArray(new KeyCommand[l.size()]);
    }
    setAngleCommand.setEnabled(getMap() != null && validAngles.length == 1 && setAngleText.length() > 0);
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (setAngleCommand.matches(stroke)) {
      beginInteractiveRotate();
    }
    else if (rotateCWCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      angleIndex = (angleIndex + 1) % validAngles.length;
      c = tracker.getChangeCommand();
    }
    else if (rotateCCWCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      angleIndex = (angleIndex - 1 + validAngles.length) % validAngles.length;
      c = tracker.getChangeCommand();
    }
    // for random rotation
    else if (rotateRNDCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      // get random #
      Random rand = GameModule.getGameModule().getRNG();
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
    getMap().pushMouseListener(this);
    getMap().addDrawComponent(this);
    getMap().getView().addMouseMotionListener(this);
    getMap().getView().setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
    pivot = getPosition();
  }

  /** The point around which the piece will pivot while rotating interactively */
  public void setPivot(int x, int y) {
    pivot = new Point(x, y);
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
    drawGhost = true;
    startAngle = getRelativeAngle(e.getPoint(), getPosition());
  }

  public void mouseReleased(MouseEvent e) {
    try {
      Point ghostPosition = getGhostPosition();
      Command c = null;
      ChangeTracker tracker = new ChangeTracker(this);
      if (!getPosition().equals(ghostPosition)) {
        GamePiece outer = Decorator.getOutermost(this);
        outer.setProperty(Properties.MOVED, Boolean.TRUE);
        c = getMap().placeOrMerge(outer, getMap().snapTo(ghostPosition));
      }
      setAngle(tempAngle);
      c = tracker.getChangeCommand().append(c);

      GameModule.getGameModule().sendAndLog(c);
    }
    finally {
      getMap().getView().setCursor(null);
      getMap().removeDrawComponent(this);
      getMap().popMouseListener();
      getMap().getView().removeMouseMotionListener(this);
    }
  }

  public void setProperty(Object key, Object val) {
    if (Properties.USE_UNROTATED_SHAPE.equals(key)) {
      useUnrotatedShape = Boolean.TRUE.equals(val);
    }
    super.setProperty(key, val);
  }

  public Object getProperty(Object key) {
  	if ((name+FACING).equals(key)) {
      return String.valueOf(angleIndex + 1);
	  } 
    else if ((name+DEGREES).equals(key)) {
      return String.valueOf((int) (Math.abs(validAngles[angleIndex])));
    }
    else {
	    return super.getProperty(key);
    }
  }
  
  public void mouseDragged(MouseEvent e) {
    if (drawGhost) {
      Point mousePos = getMap().mapCoordinates(e.getPoint());
      double myAngle = getRelativeAngle(mousePos, pivot);
      tempAngle = getAngle() - (myAngle - startAngle)/PI_180;
    }
    getMap().repaint();
  }

  private double getRelativeAngle(Point p, Point origin) {
    double myAngle;
    if (p.y == origin.y) {
      myAngle = p.x < origin.x ? -Math.PI/2.0 : Math.PI/2.0;
    }
    else {
      myAngle = Math.atan((double)(p.x - origin.x) / (double)(origin.y - p.y));
      if (origin.y < p.y) {
        myAngle += Math.PI;
      }
    }
    return myAngle;
  }

  public void mouseMoved(MouseEvent e) {
  }

  /**
   * Return a full-scale cached image of this piece, rotated to the appropriate
   * angle.
   * 
   * @param angle
   * @param obs
   * @return
   */
  public Image getRotatedImage(double angle, Component obs) {
    Image rotated = getCachedUnrotatedImage(angle);
    if (unrotated.isChanged()) {
      clearCachedImages();
      rotated = null;
    }
    if (rotated == null) {
      Rectangle rotatedBounds;
      Rectangle unrotatedBounds = piece.boundingBox();
      rotatedBounds = boundingBox();
      if (rotatedBounds.width > 0 && rotatedBounds.height > 0) {
        rotated = new BufferedImage(rotatedBounds.width,
                                    rotatedBounds.height,
                                    BufferedImage.TYPE_4BYTE_ABGR);
        ((BufferedImage) rotated).setRGB(0, 0,
                                         rotatedBounds.width,
                                         rotatedBounds.height,
            new int[rotatedBounds.width * rotatedBounds.height], 0,
                                         rotatedBounds.width);
        Graphics2D g2d = ((BufferedImage) rotated).createGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                             RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        AffineTransform t = AffineTransform
            .getTranslateInstance(-rotatedBounds.x, -rotatedBounds.y);
        t.rotate(-PI_180 * angle, centerX(), centerY()); 
        t.translate(unrotatedBounds.x, unrotatedBounds.y);

        g2d.drawImage(unrotated.getImage(obs), t, obs);
        images.put(new Double(angle), rotated);
        bounds.put(new Double(angle), rotatedBounds);
      }
      else {
          rotated = null;
      }
    }
    return rotated;
  }

  private Image getCachedUnrotatedImage(double angle) {
    if (validAngles.length == 1) {
      angle = validAngles[0];
    }
    Image rotated = (Image) images.get(new Double(angle));
    return rotated;
  }

  private void clearCachedImages() {
    for (Iterator it = images.values().iterator(); it.hasNext();) {
      Image im = (Image) it.next();
      GameModule.getGameModule().getDataArchive().unCacheImage(im);
    }
    images.clear();
    bounds.clear();
  }

  public String getDescription() {
    return "Can Rotate";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Rotate.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {setAngleText, rotateCWText, rotateCCWText, rotateRNDText}, 
                       new String[] {getCommandDescription(name, "Set Angle command"), getCommandDescription(name, "Rotate CW command"), getCommandDescription(name, "Rotate CCW command"), getCommandDescription(name, "Rotate Random command")});
  }

  private static class Ed implements PieceEditor, java.beans.PropertyChangeListener {
    private BooleanConfigurer anyConfig;
    private HotKeyConfigurer anyKeyConfig;
    private IntConfigurer facingsConfig;
    private HotKeyConfigurer cwKeyConfig;
    private HotKeyConfigurer ccwKeyConfig;
    // random rotate
    private HotKeyConfigurer rndKeyConfig;
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
      cwKeyConfig = new HotKeyConfigurer(null, "Command to rotate clockwise:  ", p.rotateCWKey);
      ccwKeyConfig = new HotKeyConfigurer(null, "Command to rotate counterclockwise:  ", p.rotateCCWKey);
      // random rotate
      rndKeyConfig = new HotKeyConfigurer(null, "Command to rotate randomly:  ", p.rotateRNDKey);
      // end random rotate
      anyConfig = new BooleanConfigurer(null, "Allow arbitrary rotations", new Boolean(p.validAngles.length == 1));
      anyKeyConfig = new HotKeyConfigurer(null, "Command to rotate:  ", p.setAngleKey);
      facingsConfig = new IntConfigurer(null, "Number of allowed facings:  ", new Integer(p.validAngles.length == 1 ? 6 : p.validAngles.length));

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

    public void propertyChange(java.beans.PropertyChangeEvent evt) {
      boolean any = Boolean.TRUE.equals(anyConfig.getValue());
      anyControls.setVisible(any);
      facingsConfig.getControls().setVisible(!any);
      cwControls.setVisible(!any);
      ccwControls.setVisible(!any);
      panel.revalidate();
    }

    public Component getControls() {
      return panel;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      if (Boolean.TRUE.equals(anyConfig.getValue())) {
        se.append("1");
        se.append((KeyStroke) anyKeyConfig.getValue());
        se.append(anyCommand.getText() == null ? "" : anyCommand.getText().trim());
      }
      else {
        se.append(facingsConfig.getValueString());
        se.append((KeyStroke) cwKeyConfig.getValue());
        se.append((KeyStroke) ccwKeyConfig.getValue());
        se.append(cwCommand.getText() == null ? "" : cwCommand.getText().trim());
        se.append(ccwCommand.getText() == null ? "" : ccwCommand.getText().trim());
      }
      // random rotate
      se.append((KeyStroke) rndKeyConfig.getValue());
      se.append(rndCommand.getText() == null ? "" : rndCommand.getText().trim());
      // end random rotate
      se.append(nameConfig.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "0";
    }
  }
}
