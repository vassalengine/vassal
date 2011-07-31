/*
 * $Id$
 *
 * Copyright (c) 2005 by Scott Giese, Rodney Kinney, Brent Easton
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
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MapShader;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.GeometricGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * @author Scott Giese sgiese@sprintmail.com
 *
 * Displays a transparency surrounding the GamePiece which represents the Area of Effect of the GamePiece
 */
public class AreaOfEffect extends Decorator implements TranslatablePiece, MapShader.ShadedPiece {
  public static final String ID = "AreaOfEffect;";
  protected static final Color defaultTransparencyColor = Color.GRAY;
  protected static final float defaultTransparencyLevel = 0.3F;
  protected static final int defaultRadius = 1;

  protected Color transparencyColor;
  protected float transparencyLevel;
  protected int radius;
  protected boolean alwaysActive;
  protected boolean active;
  protected String activateCommand;
  protected NamedKeyStroke activateKey;
  protected KeyCommand[] commands;
  protected String mapShaderName;
  protected MapShader shader;
  protected KeyCommand keyCommand;
  protected boolean fixedRadius = true;
  protected String radiusMarker = "";
  protected String description = "";

  public AreaOfEffect() {
    this(ID + ColorConfigurer.colorToString(defaultTransparencyColor), null);
  }

  public AreaOfEffect(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getDescription() {
    String d = "Area Of Effect";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(transparencyColor);
    se.append((int) (transparencyLevel * 100));
    se.append(radius);
    se.append(alwaysActive);
    se.append(activateCommand);
    se.append(activateKey);
    se.append(mapShaderName == null ? "" : mapShaderName);
    se.append(fixedRadius);
    se.append(radiusMarker);
    se.append(description);

    return ID + se.getValue();
  }

  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();    // Discard ID
    transparencyColor = st.nextColor(defaultTransparencyColor);
    transparencyLevel = st.nextInt((int) (defaultTransparencyLevel * 100)) / 100.0F;
    radius = st.nextInt(defaultRadius);
    alwaysActive = st.nextBoolean(true);
    activateCommand = st.nextToken("Show Area");
    activateKey = st.nextNamedKeyStroke(null);
    keyCommand = new KeyCommand(activateCommand, activateKey, Decorator.getOutermost(this), this);
    mapShaderName = st.nextToken("");
    if (mapShaderName.length() == 0) {
      mapShaderName = null;
    }
    fixedRadius = st.nextBoolean(true);
    radiusMarker = st.nextToken("");
    description = st.nextToken("");
    shader = null;
    commands = null;
  }

  // State does not change during the game
  public String myGetState() {
    return alwaysActive ? "" : String.valueOf(active);
  }

  // State does not change during the game
  public void mySetState(String newState) {
    if (!alwaysActive) {
      active = "true".equals(newState);
    }
  }

  public Rectangle boundingBox() {
    // TODO: Need the context of the parent Component, because the transparency is only drawn
    // on a Map.View object.  Because this context is not known, the bounding box returned by
    // this method does not encompass the bounds of the transparency.  The result of this is
    // that portions of the transparency will not be drawn after scrolling the Map window.
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if ((alwaysActive || active) && mapShaderName == null) {
      // The transparency is only drawn on a Map.View component. Only the
      // GamePiece is drawn within other windows (Counter Palette, etc.).
      if (obs instanceof Map.View && getMap() != null) {
        final Graphics2D g2d = (Graphics2D) g;

        final Color oldColor = g2d.getColor();
        g2d.setColor(transparencyColor);

        final Composite oldComposite = g2d.getComposite();
        g2d.setComposite(AlphaComposite.getInstance(
          AlphaComposite.SRC_OVER, transparencyLevel));

        Area a = getArea();

        if (zoom != 1.0) {
          a = new Area(AffineTransform.getScaleInstance(zoom,zoom)
                                      .createTransformedShape(a));
        }

        g2d.fill(a);

        g2d.setColor(oldColor);
        g2d.setComposite(oldComposite);
      }
    }

    // Draw the GamePiece
    piece.draw(g, x, y, obs, zoom);
  }

  protected Area getArea() {
    Area a;
    final Map map = getMap();
    // Always draw the area centered on the piece's current position
    // (For instance, don't draw it at an offset if it's in an expanded stack)
    final Point mapPosition = getPosition();
    final int myRadius = getRadius();

    final Board board = map.findBoard(mapPosition);
    final MapGrid grid = board == null ? null : board.getGrid();

    if (grid instanceof GeometricGrid) {
      final GeometricGrid gGrid = (GeometricGrid) grid;

      final Rectangle boardBounds = board.bounds();
      final Point boardPosition = new Point(
        mapPosition.x-boardBounds.x, mapPosition.y-boardBounds.y);

      a = gGrid.getGridShape(boardPosition, myRadius); // In board co-ords
      final AffineTransform t = AffineTransform.getTranslateInstance(
        boardBounds.x, boardBounds.y); // Translate back to map co-ords

      final double mag = board.getMagnification();
      if (mag != 1.0) {
        t.translate(boardPosition.x, boardPosition.y);
        t.scale(mag, mag);
        t.translate(-boardPosition.x, -boardPosition.y);
      }
      a = a.createTransformedArea(t);
    }
    else {
      a = new Area(
        new Ellipse2D.Double(mapPosition.x - myRadius,
                             mapPosition.y - myRadius,
                             myRadius * 2, myRadius * 2));
    }
    return a;
  }

  protected int getRadius() {
    if (fixedRadius) {
      return radius;
    }
    else {
      final String r =
        (String) Decorator.getOutermost(this).getProperty(radiusMarker);
      try {
        return Integer.parseInt(r);
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"), "radius["+radiusMarker+"]="+r, e);
        return 0;
      }
    }
  }

  // No hot-keys
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (alwaysActive || activateCommand.length() == 0) {
        commands = new KeyCommand[0];
      }
      else {
        commands = new KeyCommand[]{keyCommand};
      }
    }
    return commands;
  }

  // No hot-keys
  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (!alwaysActive
        && keyCommand.matches(stroke)) {
      final ChangeTracker t = new ChangeTracker(this);
      active = !active;
      c = t.getChangeCommand();
    }
    return c;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("AreaOfEffect.htm");
  }

  public PieceEditor getEditor() {
    return new TraitEditor(this);
  }

  public Area getArea(MapShader shader) {
    Area a = null;
    final MapShader.ShadedPiece shaded = (MapShader.ShadedPiece) Decorator.getDecorator(piece,MapShader.ShadedPiece.class);
    if (shaded != null) {
      a = shaded.getArea(shader);
    }
    if (alwaysActive || active) {
      if (shader.getConfigureName().equals(mapShaderName)) {
        Area myArea = getArea();
        if (a == null) {
          a = myArea;
        }
        else {
          a.add(myArea);
        }
      }
    }
    return a;
  }

  protected static class TraitEditor implements PieceEditor {
    protected JPanel panel;
    protected ColorConfigurer transparencyColorValue;
    protected IntConfigurer transparencyValue;
    protected IntConfigurer radiusValue;
    protected BooleanConfigurer alwaysActive;
    protected StringConfigurer activateCommand;
    protected NamedHotKeyConfigurer activateKey;
    protected BooleanConfigurer useMapShader;
    protected BooleanConfigurer fixedRadius;
    protected StringConfigurer radiusMarker;
    protected StringConfigurer descConfig;
    protected Box selectShader;
    protected String mapShaderId;

    protected TraitEditor(AreaOfEffect trait) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      panel.add(new JLabel("Contributed by Scott Giese (sgiese@sprintmail.com)", JLabel.CENTER));
      panel.add(new JSeparator());
      panel.add(new JLabel(" "));

      descConfig = new StringConfigurer(null, "Description:  ", trait.description);
      panel.add(descConfig.getControls());

      useMapShader = new BooleanConfigurer(null, "Use Map Shading?", trait.mapShaderName != null);
      mapShaderId = trait.mapShaderName;
      panel.add(useMapShader.getControls());
      selectShader = Box.createHorizontalBox();

      panel.add(selectShader);
      final JLabel l = new JLabel("Map Shading:  ");
      selectShader.add(l);
      final JTextField tf = new JTextField(12);
      tf.setEditable(false);
      selectShader.add(tf);
      tf.setText(trait.mapShaderName);
      final JButton b = new JButton("Select");
      selectShader.add(b);
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, panel), MapShader.class);
          d.setVisible(true);
          if (d.getTarget() != null) {
            mapShaderId = d.getTarget().getConfigureName();
            tf.setText(mapShaderId);
          }
          else {
            mapShaderId = null;
            tf.setText("");
          }
        }
      });

      transparencyColorValue = new ColorConfigurer(null, "Fill Color:  ", trait.transparencyColor);
      panel.add(transparencyColorValue.getControls());
      transparencyValue = new IntConfigurer(null, "Opacity (%):  ", (int) (trait.transparencyLevel * 100));
      panel.add(transparencyValue.getControls());

      fixedRadius = new BooleanConfigurer(null, "Fixed Radius?",
                                          Boolean.valueOf(trait.fixedRadius));
      fixedRadius.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateRangeVisibility();
        }
      });
      panel.add(fixedRadius.getControls());

      radiusValue = new IntConfigurer(null, "Radius: ", trait.radius);
      panel.add(radiusValue.getControls());

      radiusMarker = new StringConfigurer(null, "Radius Marker: ", trait.radiusMarker);
      panel.add(radiusMarker.getControls());

      alwaysActive = new BooleanConfigurer(null, "Always visible?", trait.alwaysActive ? Boolean.TRUE : Boolean.FALSE);
      activateCommand = new StringConfigurer(null, "Toggle visible command:  ", trait.activateCommand);
      activateKey = new NamedHotKeyConfigurer(null, "Toggle visible keyboard shortcut:  ", trait.activateKey);

      updateRangeVisibility();

      alwaysActive.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateCommandVisibility();
        }
      });
      updateCommandVisibility();

      useMapShader.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateFillVisibility();
        }
      });
      updateFillVisibility();

      panel.add(alwaysActive.getControls());
      panel.add(activateCommand.getControls());
      panel.add(activateKey.getControls());
    }

    protected void updateFillVisibility() {
      final boolean useShader = Boolean.TRUE.equals(useMapShader.getValue());
      transparencyColorValue.getControls().setVisible(!useShader);
      transparencyValue.getControls().setVisible(!useShader);
      selectShader.setVisible(useShader);
      repack();
    }

    protected void updateRangeVisibility() {
      final boolean fixedRange = fixedRadius.booleanValue().booleanValue();
      radiusValue.getControls().setVisible(fixedRange);
      radiusMarker.getControls().setVisible(!fixedRange);
      repack();
    }

    protected void updateCommandVisibility() {
      final boolean alwaysActiveSelected = Boolean.TRUE.equals(alwaysActive.getValue());
      activateCommand.getControls().setVisible(!alwaysActiveSelected);
      activateKey.getControls().setVisible(!alwaysActiveSelected);
      repack();
    }

    protected void repack() {
      final Window w = SwingUtilities.getWindowAncestor(alwaysActive.getControls());
      if (w != null) {
        w.pack();
      }
    }

    public Component getControls() {
      return panel;
    }

    public String getState() {
      return "false";
    }

    public String getType() {
      final boolean alwaysActiveSelected = Boolean.TRUE.equals(alwaysActive.getValue());
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(transparencyColorValue.getValueString());
      se.append(transparencyValue.getValueString());
      se.append(radiusValue.getValueString());
      se.append(alwaysActiveSelected);
      se.append(activateCommand.getValueString());
      se.append(activateKey.getValueString());
      if (Boolean.TRUE.equals(useMapShader.getValue()) && mapShaderId != null) {
        se.append(mapShaderId);
      }
      else {
        se.append("");
      }
      se.append(fixedRadius.getValueString());
      se.append(radiusMarker.getValueString());
      se.append(descConfig.getValueString());

      return AreaOfEffect.ID + se.getValue();
    }
  }

  public PieceI18nData getI18nData() {
    return getI18nData(activateCommand, getCommandDescription(description, "Toggle Visible command"));
  }
}
