/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MovementReporter;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.MoveTracker;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DoubleConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * Provides commands to pivot a Game Piece around a given point
 */
public class Pivot extends Decorator implements TranslatablePiece {
  public static final String ID = "pivot;";
  public static final String DEGREES = "_Degrees";
  protected int pivotX;
  protected int pivotY;
  protected double angle;
  protected String command;
  protected NamedKeyStroke key;
  protected boolean fixedAngle;
  protected KeyCommand[] commands;
  protected KeyCommand pivotCommand;
  protected FreeRotator rotator;

  public Pivot() {
    this(ID, null);
  }

  public Pivot(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getDescription() {
    return "Can Pivot";
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Pivot.htm");
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    command = st.nextToken("Pivot");
    key = st.nextNamedKeyStroke(null);
    pivotX = st.nextInt(0);
    pivotY = st.nextInt(0);
    fixedAngle = st.nextBoolean(true);
    angle = st.nextDouble(90.0);
    commands = null;
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      pivotCommand = new KeyCommand(command, key, Decorator.getOutermost(this), this);
      if (command.length() > 0 && key != null && !key.isNull()) {
        commands = new KeyCommand[]{pivotCommand};
      }
      else {
        commands = new KeyCommand[0];
      }
      rotator = (FreeRotator) Decorator.getDecorator(this, FreeRotator.class);
      pivotCommand.setEnabled(rotator != null);
    }
    return commands;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(command)
        .append(key)
        .append(pivotX)
        .append(pivotY)
        .append(fixedAngle)
        .append(angle);
    return ID + se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (pivotCommand.matches(stroke)) {
      if (fixedAngle) {
        ChangeTracker t = new ChangeTracker(this);
        double oldAngle = rotator.getAngle();
        rotator.setAngle(oldAngle - angle);
        double newAngle = rotator.getAngle();
        if (getMap() != null) {
          c = setOldProperties(this);
          Point pos = getPosition();
          pivotPoint(pos, -Math.PI * oldAngle / 180.0, -Math.PI * newAngle / 180.0);
          GamePiece outer = Decorator.getOutermost(this);
          if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
            pos = getMap().snapTo(pos);
          }
          outer.setProperty(Properties.MOVED, Boolean.TRUE);
          c = c.append(t.getChangeCommand());
          MoveTracker moveTracker = new MoveTracker(outer);
          getMap().placeOrMerge(outer, pos);
          c = c.append(moveTracker.getMoveCommand());
          MovementReporter r = new MovementReporter(c);
          Command reportCommand = r.getReportCommand();
          if (reportCommand != null) {
            reportCommand.execute();
          }
          c = c.append(reportCommand);
          c = c.append(r.markMovedPieces());
          getMap().ensureVisible(getMap().selectionBoundsOf(outer));
        }
        else {
          c = t.getChangeCommand();
        }
      }
      else if (getMap() != null) {
        setOldProperties(this);
        final double oldAngle = rotator.getAngleInRadians();
        Point2D pivot2D = new Point2D.Double(pivotX, pivotY);
        AffineTransform t = AffineTransform.getRotateInstance(oldAngle);
        t.transform(pivot2D, pivot2D);
        rotator.beginInteractiveRotate();
        rotator.setPivot(getPosition().x + (int) Math.round(pivot2D.getX()),
                         getPosition().y + (int) Math.round(pivot2D.getY()));
      }
    }
    // Apply map auto-move key
    if (c != null && getMap() != null && getMap().getMoveKey()!= null) {
      c.append(Decorator.getOutermost(this).keyEvent(getMap().getMoveKey()));
    }
    return c;
  }

  /**
   * Pivot the given point around the pivot point from oldAngle to newAngle
   * @param oldAngle
   * @param newAngle
   */
  private void pivotPoint(Point p, double oldAngle, double newAngle) {
    Point2D pivot2D = new Point2D.Double(pivotX, pivotY);
    AffineTransform t = AffineTransform.getRotateInstance(oldAngle);
    t.transform(pivot2D, pivot2D);
    t = AffineTransform.getRotateInstance(newAngle - oldAngle, pivot2D.getX(), pivot2D.getY());
    Point2D newPos2D = new Point2D.Float(0, 0);
    t.transform(newPos2D, newPos2D);
    p.x += (int) Math.round(newPos2D.getX());
    p.y += (int) Math.round(newPos2D.getY());
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Rectangle boundingBox() {
    return getInner().boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getInner().draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return getInner().getName();
  }

  @Override
  public Shape getShape() {
    return getInner().getShape();
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command, "Pivot command");
  }


  public static class Ed implements PieceEditor {
    private StringConfigurer command;
    private NamedHotKeyConfigurer key;
    private IntConfigurer xOff, yOff;
    private DoubleConfigurer angle;
    private BooleanConfigurer fixedAngle;
    private JPanel controls;
    public Ed(Pivot p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      command = new StringConfigurer(null, "Command:  ", p.command);
      controls.add(command.getControls());

      key = new NamedHotKeyConfigurer(null, "Keyboard command:  ", p.key);
      controls.add(key.getControls());

      Box b = Box.createHorizontalBox();
      xOff = new IntConfigurer(null, "Pivot point:  ", p.pivotX);
      b.add(xOff.getControls());
      yOff = new IntConfigurer(null, ", ", p.pivotY);
      b.add(yOff.getControls());
      controls.add(b);

      fixedAngle = new BooleanConfigurer(null, "Pivot through fixed angle?",
                                         Boolean.valueOf(p.fixedAngle));
      controls.add(fixedAngle.getControls());

      angle = new DoubleConfigurer(null, "Angle:  ", p.angle);
      controls.add(angle.getControls());

      angle.getControls().setVisible(p.fixedAngle);
      fixedAngle.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          angle.getControls().setVisible(Boolean.TRUE.equals(fixedAngle.getValue()));
        }
      });
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(command.getValueString())
          .append(key.getValueString())
          .append(xOff.getValueString())
          .append(yOff.getValueString())
          .append(Boolean.TRUE.equals(fixedAngle.getValue()))
          .append(angle.getValueString());
      return ID + se.getValue();
    }
  }
}
