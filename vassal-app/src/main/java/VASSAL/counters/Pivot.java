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

import VASSAL.build.module.GlobalOptions;
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
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import net.miginfocom.swing.MigLayout;

/**
 * Provides commands to pivot a Game Piece around a given point
 */
public class Pivot extends Decorator implements TranslatablePiece {
  public static final String ID = "pivot;"; // NON-NLS
  public static final String DEGREES = "_Degrees"; // NON-NLS
  protected int pivotX;
  protected int pivotY;
  protected double angle;
  protected String command;
  protected NamedKeyStroke key;
  protected boolean fixedAngle;
  protected KeyCommand[] commands;
  protected KeyCommand pivotCommand;
  protected FreeRotator rotator;
  protected String description;

  public Pivot() {
    this(ID, null);
  }

  public Pivot(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.Pivot.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Pivot.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    command = st.nextToken(Resources.getString("Editor.Pivot.default_pivot_command"));
    key = st.nextNamedKeyStroke(null);
    pivotX = st.nextInt(0);
    pivotY = st.nextInt(0);
    fixedAngle = st.nextBoolean(true);
    angle = st.nextDouble(90.0);
    description = st.nextToken("");
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
        commands = KeyCommand.NONE;
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
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(command)
        .append(key)
        .append(pivotX)
        .append(pivotY)
        .append(fixedAngle)
        .append(angle)
        .append(description);
    return ID + se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (pivotCommand.matches(stroke)) {
      if (fixedAngle) {
        final ChangeTracker t = new ChangeTracker(this);
        final double oldAngle = rotator.getAngle();
        rotator.setAngle(oldAngle - angle);
        final double newAngle = rotator.getAngle();
        if (getMap() != null) {
          c = putOldProperties(this);
          Point pos = getPosition();
          pivotPoint(pos, -Math.PI * oldAngle / 180.0, -Math.PI * newAngle / 180.0);
          final GamePiece outer = Decorator.getOutermost(this);
          if (!Boolean.TRUE.equals(outer.getProperty(Properties.IGNORE_GRID))) {
            pos = getMap().snapTo(pos);
          }
          outer.setProperty(Properties.MOVED, Boolean.TRUE);
          c = c.append(t.getChangeCommand());
          final MoveTracker moveTracker = new MoveTracker(outer);
          getMap().placeOrMerge(outer, pos);
          c = c.append(moveTracker.getMoveCommand());
          final MovementReporter r = new MovementReporter(c);
          if (GlobalOptions.getInstance().autoReportEnabled()) {
            final Command reportCommand = r.getReportCommand();
            if (reportCommand != null) {
              reportCommand.execute();
            }
            c = c.append(reportCommand);
          }
          c = c.append(r.markMovedPieces());
          getMap().ensureVisible(getMap().selectionBoundsOf(outer));
        }
        else {
          c = t.getChangeCommand();
        }
      }
      else if (getMap() != null) {
        c = putOldProperties(this);
        final double oldAngle = rotator.getAngleInRadians();
        final Point2D pivot2D = new Point2D.Double(pivotX, pivotY);
        final AffineTransform t = AffineTransform.getRotateInstance(oldAngle);
        t.transform(pivot2D, pivot2D);
        rotator.beginInteractiveRotate();
        rotator.setPivot(getPosition().x + (int) Math.round(pivot2D.getX()),
                         getPosition().y + (int) Math.round(pivot2D.getY()));
      }
    }
    // Apply map auto-move key
    if (c != null && getMap() != null && getMap().getMoveKey() != null) {
      c = c.append(Decorator.getOutermost(this).keyEvent(getMap().getMoveKey()));
    }
    return c;
  }

  /**
   * Pivot the given point around the pivot point from oldAngle to newAngle
   * @param oldAngle Old Angle
   * @param newAngle New Angle
   */
  private void pivotPoint(Point p, double oldAngle, double newAngle) {
    final Point2D pivot2D = new Point2D.Double(pivotX, pivotY);
    AffineTransform t = AffineTransform.getRotateInstance(oldAngle);
    t.transform(pivot2D, pivot2D);
    t = AffineTransform.getRotateInstance(newAngle - oldAngle, pivot2D.getX(), pivot2D.getY());
    final Point2D newPos2D = new Point2D.Float(0, 0);
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
    return getI18nData(command, Resources.getString("Editor.Pivot.pivot_command"));
  }


  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Pivot)) return false;
    final Pivot c = (Pivot) o;

    if (! Objects.equals(command, c.command)) return false;
    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(pivotX, c.pivotX)) return false;
    if (! Objects.equals(pivotY, c.pivotY)) return false;
    if (! Objects.equals(fixedAngle, c.fixedAngle)) return false;
    if (! Objects.equals(description, c.description)) return false;
    return Objects.equals(angle, c.angle);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer command;
    private final NamedHotKeyConfigurer key;
    private final IntConfigurer xOff;
    private final IntConfigurer yOff;
    private final DoubleConfigurer angle;
    private final JLabel angleLabel;
    private final BooleanConfigurer fixedAngle;
    private final TraitConfigPanel controls;
    private final StringConfigurer desc;

    public Ed(Pivot p) {
      controls = new TraitConfigPanel();

      desc = new StringConfigurer(p.description);
      desc.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", desc);

      command = new StringConfigurer(p.command);
      command.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", command);

      key = new NamedHotKeyConfigurer(p.key);
      controls.add("Editor.keyboard_command", key);

      controls.add(new JLabel(Resources.getString("Editor.Pivot.pivot_point")));
      final JPanel b = new JPanel(new MigLayout("ins 0", "[]2[]2[]")); // NON-NLS
      xOff = new IntConfigurer(p.pivotX);
      b.add(xOff.getControls());
      b.add(new JLabel(","));
      yOff = new IntConfigurer(p.pivotY);
      b.add(yOff.getControls());
      controls.add(b);

      fixedAngle = new BooleanConfigurer(p.fixedAngle);
      controls.add("Editor.Pivot.pivot_through_fixed_angle", fixedAngle);

      angleLabel = new JLabel(Resources.getString("Editor.Pivot.angle"));
      angle = new DoubleConfigurer(p.angle);

      controls.add(angleLabel);
      controls.add(angle.getControls());

      angle.getControls().setVisible(p.fixedAngle);
      fixedAngle.addPropertyChangeListener(evt -> {
        angle.getControls().setVisible(Boolean.TRUE.equals(fixedAngle.getValue()));
        angleLabel.setVisible(Boolean.TRUE.equals(fixedAngle.getValue()));
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
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(command.getValueString())
          .append(key.getValueString())
          .append(xOff.getValueString())
          .append(yOff.getValueString())
          .append(Boolean.TRUE.equals(fixedAngle.getValue()))
          .append(angle.getValueString())
          .append(desc.getValueString());
      return ID + se.getValue();
    }
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.singletonList(key);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(command);
  }
}
