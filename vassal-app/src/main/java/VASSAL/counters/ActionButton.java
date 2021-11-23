/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.PolygonConfigurer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.PolygonEditor;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.swing.SwingUtils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.StringTokenizer;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;

/**
 * A Trait (aka {@link Decorator} that acts like a button on a GamePiece, such that clicking on a
 * particular area of the piece invokes a key command
 *
 * @author rkinney
 */
public class ActionButton extends Decorator implements EditablePiece, Loopable {
  public static final String ID = "button;"; // NON-NLS
  public static final String LAUNCH_POPUP_MENU = "LaunchPopupMenu"; //NON-NLS
  public static final int ACTION_BUTTON_VERSION = 1;
  protected NamedKeyStroke stroke;
  protected Rectangle bounds = new Rectangle();
  protected Polygon polygon = new Polygon();
  protected ButtonPusher pusher;
  protected String description = "";
  protected int version;
  protected static final ButtonPusher globalPusher = new ButtonPusher();
  protected boolean launchPopupMenu = false; //BR// If clicking this button should launch the piece's context menu
  protected boolean useWholeShape   = false; //BR// If we should just use the whole piece shape as the hotspot for the button

  public ActionButton() {
    this(ID, null);
    //this(ID + "65,0;-20;-20;40;40;;false,true;1;0", null); //NON-NLS // Starts new ones as "version 1"
  }

  public ActionButton(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
    pusher = globalPusher;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(stroke)
      .append(bounds.x)
      .append(bounds.y)
      .append(bounds.width)
      .append(bounds.height)
      .append(description)
      .append(launchPopupMenu)
      .append(useWholeShape)
      .append(ACTION_BUTTON_VERSION)
      .append(polygon.npoints);

    for (int point = 0; point < polygon.npoints; point++) {
      // Point-by-point instead of e.g. all-x-first makes it easier to build the polygon at the other end
      se.append(polygon.xpoints[point]).append(polygon.ypoints[point]);
    }

    return ID + se.getValue();
  }

  @Override
  public Object getProperty(Object key) {
    if (LAUNCH_POPUP_MENU.equals(key)) {
      return String.valueOf(launchPopupMenu);
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (LAUNCH_POPUP_MENU.equals(key)) {
      return String.valueOf(launchPopupMenu);
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public void setMap(Map m) {
    // Register the map for button pushes
    pusher.register(m);
    piece.setMap(m);
  }

  @Override
  public String getDescription() {
    String s = buildDescription("Editor.ActionButton.trait_description", description);
    s += getCommandDesc("", stroke);
    return s;
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.ActionButton.trait_description");
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(stroke);
  }


  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    stroke = st.nextNamedKeyStroke('A');
    bounds.x = st.nextInt(0);
    bounds.y = st.nextInt(0);
    bounds.width = st.nextInt(0);
    bounds.height = st.nextInt(0);
    description = st.nextToken("");
    launchPopupMenu = st.nextBoolean(false);
    useWholeShape = st.nextBoolean(false);

    version = st.nextInt(0);

    polygon.reset();
    if (version < 1) {
      // If it's a genuine old-format then convert the old bounds over; otherwise start with empty polygon and use "whole piece" by default
      if ((bounds.width > 0) && (bounds.height > 0)) {
        polygon.addPoint(bounds.x, bounds.y);
        polygon.addPoint(bounds.x + bounds.width, bounds.y);
        polygon.addPoint(bounds.x + bounds.width, bounds.y + bounds.height);
        polygon.addPoint(bounds.x, bounds.y + bounds.height);
      }
      else {
        useWholeShape = true;
      }
    }
    else {
      final int nPoints = st.nextInt(0);
      for (int point = 0; point < nPoints; point++) {
        final int x = st.nextInt(0);
        final int y = st.nextInt(0);
        polygon.addPoint(x, y);
      }
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ActionButton.html"); // NON-NLS
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof ActionButton)) return false;
    final ActionButton c = (ActionButton) o;
    if (! Objects.equals(bounds, c.bounds)) return false;
    if (! Objects.equals(launchPopupMenu, c.launchPopupMenu)) return false;
    if (! Objects.equals(useWholeShape, c.useWholeShape)) return false;
    if (version == c.version) {
      if (!Objects.equals(polygon, c.polygon)) return false;
    }
    return Objects.equals(description, c.description);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class VisualEditor implements PolygonConfigurer {

    private PolygonEditor editor;

    Polygon polygon;

    private Decorator outer;
    private JDialog frame;
    private Point offsetView; // We need to offset the coordinate space in the PolygonEditor so that 0,0 will be at the center of our coordinate space rather than the upper left corner.
    protected AdjustableSpeedScrollPane scroll;
    protected FlowLabel coordsLabel;
    protected JLabel coordLabel;
    protected final JLabel warning = new JLabel(Resources.getString("Editor.ActionButton.empty_polygon"));

    public void show(ActionButton piece, Polygon poly, Ed pieceEditor) {
      this.polygon = poly;
      outer = (Decorator)Decorator.getOutermost(piece);

      final Rectangle bounds = outer.boundingBox();

      // If we don't have a legitimate polygon right now, see if we can create a default starting one from the bounds of our piece
      if (polygon.npoints < 3) {
        polygon.reset();
        if ((bounds.width > 0) && (bounds.height > 0)) {
          polygon.addPoint(bounds.x, bounds.y);
          polygon.addPoint(bounds.x + bounds.width, bounds.y);
          polygon.addPoint(bounds.x + bounds.width, bounds.y + bounds.height);
          polygon.addPoint(bounds.x, bounds.y + bounds.height);
        }
      }

      int spaceWidth  = (int)Math.max(bounds.getWidth() * 2, 500);
      int spaceHeight = (int)Math.max(bounds.getHeight() * 2, 500);

      // Space out coordinate space boundaries at least 10% from any existing polygon
      for (int point = 0; point < polygon.npoints; point++) {
        spaceWidth  = Math.max(spaceWidth, Math.abs(polygon.xpoints[point] * 2 * 11 / 10));
        spaceHeight = Math.max(spaceHeight, Math.abs(polygon.ypoints[point] * 2 * 11 / 10));
      }

      // Define a reasonable sized coordinate space, but big enough for our (a) piece bounding box if any, (b) current size of hotspot polygon if any, and (c) 500x500 minimum
      // Must be final since used in inner class
      final int width  = spaceWidth;
      final int height = spaceHeight;

      final Dimension viewSize = new Dimension(width, height);

      offsetView = new Point(width/2, height/2);

      editor = new PolygonEditor(new Polygon(polygon.xpoints, polygon.ypoints, polygon.npoints), offsetView) {
        private static final long serialVersionUID = 1L;

        @Override
        protected void paintBackground(Graphics g) {
          super.paintBackground(g);

          // Draw crosshairs at origin
          g.setColor(Color.BLACK);
          g.drawLine(-5 + offsetView.x, offsetView.y, 5 + offsetView.x, offsetView.y);
          g.drawLine(offsetView.x, -5 + offsetView.y, offsetView.x, 5 + offsetView.y);

          // Draw graphics for piece (if any)
          outer.draw(g, width/2, height/2, null, 1.0);

          warning.setVisible(editor != null && (editor.getPolygon() == null || editor.getPolygon().npoints == 0));
        }
      };

      editor.setMyConfigurer(this);

      frame = new JDialog(GameModule.getGameModule().getPlayerWindow(), Resources.getString("Editor.ActionButton.define_hotspot"), true);
      frame.setFocusTraversalKeysEnabled(false);

      frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
      final JPanel labels = new JPanel();
      labels.setLayout(new GridLayout(3, 2));
      labels.add(new JLabel(Resources.getString("Editor.Zone.drag_to_create_initial_shape")));
      labels.add(new JLabel(Resources.getString("Editor.Zone.right_click_to_add_point")));
      labels.add(new JLabel(Resources.getString("Editor.Zone.left_drag_to_move_points")));
      labels.add(new JLabel(Resources.getString("Editor.Zone.del_to_remove_points")));
      labels.setAlignmentX(0.0f);
      frame.add(labels);

      warning.setForeground(Color.red);
      warning.setVisible(false);
      frame.add(warning);

      final JButton direct = new JButton(Resources.getString("Editor.ActionButton.set_coordinates_directly"));
      direct.setFocusable(false);
      direct.addActionListener(e -> {
        String newShape = JOptionPane.showInputDialog(frame, Resources.getString("Editor.ActionButton.enter_points_instructions"), PolygonEditor
          .polygonToString(editor.getPolygon()).replace(';', ' '));
        if (newShape != null) {
          final StringBuilder buffer = new StringBuilder();
          final StringTokenizer st = new StringTokenizer(newShape);
          while (st.hasMoreTokens()) {
            buffer.append(st.nextToken());
            if (st.hasMoreTokens()) {
              buffer.append(';');
            }
          }
          newShape = buffer.toString();
          final Polygon newPoly = new Polygon();
          PolygonEditor.reset(newPoly, newShape);
          editor.setPolygon(newPoly); // Need to send it back in this way to get our offsetView respected
          editor.repaint();
        }
      });
      direct.setAlignmentX(0.0f);
      frame.add(direct);

      scroll = new AdjustableSpeedScrollPane(editor, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
      editor.setScroll(scroll);
      frame.add(scroll);

      final Box coordPanel = Box.createVerticalBox();
      coordsLabel = new FlowLabel("");
      coordLabel = new JLabel("");
      coordPanel.add(coordLabel, BorderLayout.CENTER);
      coordPanel.add(coordsLabel, BorderLayout.CENTER);
      frame.add(coordPanel);

      final JPanel buttonPanel = new JPanel();

      final JButton closeButton = new JButton(Resources.getString("General.ok"));
      closeButton.setFocusable(false);
      closeButton.addActionListener(e -> {
        polygon = editor.getPolygon();
        pieceEditor.setPolygon(polygon == null ? new Polygon() : polygon);
        frame.setVisible(false);
      });

      final JButton canButton = new JButton(Resources.getString("General.cancel"));
      canButton.setFocusable(false);
      canButton.addActionListener(e -> frame.setVisible(false));

      buttonPanel.add(closeButton);
      buttonPanel.add(canButton);
      frame.add(buttonPanel);

      editor.setPreferredSize(viewSize);
      editor.reset();
      editor.setPolygon((polygon.npoints == 0) ? null : new Polygon(polygon.xpoints, polygon.ypoints, polygon.npoints));
      if (editor.getPolygon() != null) {
        final Rectangle currentBounds = editor.getPolygon().getBounds();
        final Point polyCenter = new Point(currentBounds.x + currentBounds.width / 2 - offsetView.x,
          currentBounds.y + currentBounds.height / 2 - offsetView.y);
        editor.center(polyCenter);
      }

      updateCoords();
      updateCoord("");

      frame.pack();
      final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setSize(Math.min(frame.getWidth(), d.width * 2 / 3), Math.min(frame.getHeight(), d.height * 2 / 3));
      frame.setTitle(Resources.getString("Editor.ActionButton.define_hotspot"));
      frame.setVisible(true);
    }

    @Override
    public void updateCoords(Polygon polygon) {
      final StringBuilder s = new StringBuilder("");
      if (polygon != null) {
        for (int p = 0; p < polygon.npoints; p++) {
          s.append('(');
          s.append(polygon.xpoints[p] - offsetView.x);
          s.append(',');
          s.append(polygon.ypoints[p] - offsetView.y);
          s.append(") ");
        }
        coordsLabel.setText(s.toString());
      }
      else {
        coordsLabel.setText("");
      }
      coordsLabel.repaint();
    }

    @Override
    public void updateCoords() {
      updateCoords(editor.getRawPolygon());
    }

    @Override
    public void updateCoord(String s) {
      coordLabel.setText(s);
      coordLabel.repaint();
    }

    @Override
    public void updateCoord(int x, int y) {
      updateCoord((x - offsetView.x) + "," + (y - offsetView.y));
    }
  }

  public static class Ed implements PieceEditor {
    private final TraitConfigPanel box;
    private final JButton defineButton;
    private final NamedHotKeyConfigurer strokeConfig;
    private final BooleanConfigurer launchConfig;
    private final BooleanConfigurer useShapeConfig;
    private final IntConfigurer xConfig;
    private final IntConfigurer yConfig;
    private final IntConfigurer widthConfig;
    private final IntConfigurer heightConfig;
    protected StringConfigurer descConfig;
    private Polygon polygon;

    public Ed(ActionButton p) {
      box = new TraitConfigPanel();

      polygon = p.polygon;

      descConfig = new StringConfigurer(p.description);
      descConfig.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descConfig);

      strokeConfig = new NamedHotKeyConfigurer(p.stroke);
      box.add("Editor.ActionButton.invoke_key_command", strokeConfig);

      launchConfig = new BooleanConfigurer(p.launchPopupMenu);
      box.add("Editor.ActionButton.launch_popup_menu", launchConfig);

      useShapeConfig = new BooleanConfigurer(p.useWholeShape);
      useShapeConfig.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          defineButton.setVisible(!((Boolean)evt.getNewValue()));
          repack(box);
          box.repaint();
        }
      });
      box.add("Editor.ActionButton.use_whole_shape", useShapeConfig);

      xConfig = new IntConfigurer(p.bounds.x);
      //box.add("Editor.ActionButton.button_x_offset", xConfig);

      yConfig = new IntConfigurer(p.bounds.y);
      //box.add("Editor.ActionButton.button_y_offset", yConfig);

      widthConfig = new IntConfigurer(p.bounds.width);
      //box.add("Editor.ActionButton.button_width", widthConfig);

      heightConfig = new IntConfigurer(p.bounds.height);
      //box.add("Editor.ActionButton.button_height", heightConfig);

      defineButton = new JButton(Resources.getString("Editor.ActionButton.define_shape"));
      defineButton.addActionListener(e -> {
        final VisualEditor ve = new VisualEditor();
        ve.show(p, polygon, this);
      });
      box.add(defineButton);

      defineButton.setVisible(!useShapeConfig.getValueBoolean());
    }

    public void setPolygon(Polygon polygon) {
      this.polygon = polygon;
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(strokeConfig.getValueString())
        .append(xConfig.getValueString())
        .append(yConfig.getValueString())
        .append(widthConfig.getValueString())
        .append(heightConfig.getValueString())
        .append(descConfig.getValueString())
        .append(launchConfig.getValueString())
        .append(useShapeConfig.getValueString())
        .append(ACTION_BUTTON_VERSION)
        .append(polygon.npoints);

      for (int point = 0; point < polygon.npoints; point++) {
        se.append(polygon.xpoints[point]).append(polygon.ypoints[point]);
      }

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }

  /**
   * Registers mouse listeners with Maps and other components. Clicking the
   * mouse checks for pieces with an ActionButton trait and invokes them if the
   * click falls within the button's boundaries
   */
  protected static class ButtonPusher {
    private final Set<Map> maps = new HashSet<>();
    private final java.util.Map<Component, ComponentMouseListener> componentMouseListeners = new HashMap<>();

    public void register(Map map) {
      if (map != null && !maps.contains(map)) {
        map.addLocalMouseListener(new MapMouseListener(map));
        maps.add(map);
      }
    }

    @Deprecated(since = "2020-10-26", forRemoval = true)
    public void register(Component obs, GamePiece piece, int x, int y) {
      if (obs != null) {
        ComponentMouseListener l = componentMouseListeners.get(obs);
        if (l == null) {
          l = new ComponentMouseListener(piece, x, y);
          obs.addMouseListener(l);
          componentMouseListeners.put(obs, l);
        }
        else {
          l.xOffset = x;
          l.yOffset = y;
          l.target = piece;
        }
      }
    }

    /**
     * Handle a mouse click on the given GamePiece at the given location (where
     * 0,0 is the center of the piece). Activate all Action Buttons in sequence
     * that are not Masked or Hidden
     *
     * @param p
     *          A function to determine the offset of the target piece. This
     *          callback is done for efficiency reasons, since computing the
     *          offset may be expensive (as in the case of a piece in an
     *          expanded stack on a map) and is only needed if the piece has the
     *          ActionButton trait
     */
    public void doClick(GamePiece p, Point point) {
      for (GamePiece piece = p; piece instanceof Decorator;
           piece = ((Decorator) piece).getInner()) {
        if (piece instanceof Obscurable) {
          if (((Obscurable) piece).obscuredToMe()) {
            return;
          }
        }
        else if (piece instanceof Hideable) {
          if (((Hideable) piece).invisibleToMe()) {
            return;
          }
        }
        else if (piece instanceof ActionButton) {
          final ActionButton action = (ActionButton) piece;

          if (action.stroke == null || action.stroke.getKeyStroke() == null) {
            continue;
          }

          // If we're not a use-whole-shape piece, then check this click against the special designated polygon
          if (!action.useWholeShape) {
            // Handle rotation of the piece, movement is relative to the current facing of the unit.
            // Traverse any traits outward from this trait (ones that could rotate this trait),
            // and find out what the cumulative rotation is
            Decorator rotateTrait = action.getOuter();
            double cumulative = 0.0;
            while (rotateTrait != null) {
              if (rotateTrait instanceof FreeRotator) {
                cumulative += ((FreeRotator) rotateTrait).getAngleInRadians();
              }
              else if (rotateTrait instanceof MatCargo) {
                cumulative += ((MatCargo) rotateTrait).getMatAngleInRadians();
              }
              rotateTrait = rotateTrait.getOuter();
            }

            final Shape shape;
            // If rotated, apply the rotation
            if (cumulative != 0.0) {
              shape = AffineTransform.getRotateInstance(cumulative,
                0,
                0)
                .createTransformedShape(action.polygon);
            }
            else {
              shape = action.polygon;
            }
            if (!shape.contains(point)) {
              continue;
            }
          }

          // Save state prior to command
          p.setProperty(Properties.SNAPSHOT, ((PropertyExporter) p).getProperties());
          try {
            RecursionLimiter.startExecution(action);
            final Command command = p.keyEvent(action.stroke.getKeyStroke());
            GameModule.getGameModule().sendAndLog(command);
          }
          catch (RecursionLimitException e) {
            RecursionLimiter.infiniteLoop(e);
          }
          finally {
            RecursionLimiter.endExecution();
          }
        }
      }
    }

    protected class MapMouseListener extends MouseAdapter {
      //
      // Buttons standardly work like this in GUIs:
      //
      // * Pressing a button arms it
      // * Releasing over an armed button fires it
      // * Releasing anywhere else disarms an armed button
      //
      // What Java reports via mouseClicked is USELESS for this.
      //

      private final Map map;
      private GamePiece armedForClick = null;

      public MapMouseListener(Map map) {
        this.map = map;
      }

      @Override
      public void mousePressed(MouseEvent e) {
        if (SwingUtils.isMainMouseButtonDown(e) && !e.isAltDown() && !e.isShiftDown() && !e.isPopupTrigger() && !SwingUtils.isSelectionToggle(e)) {
          final Point point = e.getPoint();
          final GamePiece p = map.findPiece(point, PieceFinder.PIECE_IN_STACK);
          if (p != null) {
            // arm the pressed button
            armedForClick = p;
          }
        }
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        // check if the release was over the armed button
        if (armedForClick != null) {
          if (SwingUtils.isMainMouseButtonDown(e) && !e.isAltDown() && !e.isShiftDown() && !e.isPopupTrigger() && !SwingUtils.isSelectionToggle(e) &&
              armedForClick.getMap() == map) {
            final Point epos = e.getPoint();
            final Point rel = map.positionOf(armedForClick);
            epos.translate(-rel.x, -rel.y);
            final Shape s = armedForClick.getShape();
            if (s.contains(epos)) {
              // fire the button
              doClick(armedForClick, epos);
            }
          }
          armedForClick = null;
        }
      }
    }

    @Deprecated(since = "2020-10-26", forRemoval = true)
    protected class ComponentMouseListener extends MouseAdapter {
      private GamePiece target;
      private int xOffset;
      private int yOffset;

      public ComponentMouseListener(GamePiece piece, int x, int y) {
        target = piece;
        xOffset = x;
        yOffset = y;
      }

      @Override
      public void mouseClicked(MouseEvent e) {
        if (SwingUtils.isMainMouseButtonDown(e)) {
          final Point point = e.getPoint();
          point.translate(-xOffset, -yOffset);
          doClick(target, point);
          e.getComponent().repaint();
        }
      }
    }
  }
}
