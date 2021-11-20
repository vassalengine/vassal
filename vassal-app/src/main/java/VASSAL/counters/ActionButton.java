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
  protected NamedKeyStroke stroke;
  protected Rectangle bounds = new Rectangle();
  protected ButtonPusher pusher;
  protected String description = "";
  protected static final ButtonPusher globalPusher = new ButtonPusher();
  protected boolean launchPopupMenu = false; //BR// If clicking this button should launch the piece's context menu

  public ActionButton() {
    this(ID, null);
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
      .append(launchPopupMenu);
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
    bounds.x = st.nextInt(-20);
    bounds.y = st.nextInt(-20);
    bounds.width = st.nextInt(40);
    bounds.height = st.nextInt(40);
    description = st.nextToken("");
    launchPopupMenu = st.nextBoolean(false);
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
    return Objects.equals(description, c.description);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class VisualEditor implements PolygonConfigurer {

    private PolygonEditor editor;

    Polygon polygon;

    private ActionButton action;
    private Decorator outer;
    private JDialog frame;
    protected AdjustableSpeedScrollPane scroll;
    protected FlowLabel coordsLabel;
    protected JLabel coordLabel;

    VisualEditor() {
      polygon = new Polygon();
    }


    public void show(ActionButton piece) {

      this.action = piece;
      outer = (Decorator)Decorator.getOutermost(action);

      final Rectangle bounds = outer.boundingBox();

      final int width  = (int)Math.max(bounds.getHeight() * 2, 500);
      final int height = (int)Math.max(bounds.getWidth() * 2, 500);
      final Dimension viewSize = new Dimension(width, height);

      editor = new PolygonEditor(new Polygon(polygon.xpoints, polygon.ypoints, polygon.npoints), new Point(width/2, height/2)) {
        private static final long serialVersionUID = 1L;

        @Override
        protected void paintBackground(Graphics g) {
          super.paintBackground(g);

          outer.draw(g, width/2, height/2, null, 1.0);
        }
      };

      editor.setMyConfigurer(this);

      frame = new JDialog(GameModule.getGameModule().getPlayerWindow(), "Splortle Blortle", true);
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

      final JButton direct = new JButton(Resources.getString("Set Coordinates Directly"));
      direct.setFocusable(false);
      direct.addActionListener(e -> {
        String newShape = JOptionPane.showInputDialog(frame, Resources.getString("Enter Points Instructions"), PolygonEditor
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
          PolygonEditor.reset(editor.getPolygon(), newShape);
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
      updateCoords();
      updateCoord("");
      frame.add(coordPanel);

      final JPanel buttonPanel = new JPanel();

      final JButton closeButton = new JButton(Resources.getString("General.ok"));
      closeButton.setFocusable(false);
      closeButton.addActionListener(e -> {
        //setValue((Object) getValueString());
        frame.setVisible(false);
      });

      final JButton canButton = new JButton(Resources.getString("General.cancel"));
      canButton.setFocusable(false);
      canButton.addActionListener(e -> {
        //editor.setPolygon(savePoly);
        //setValue((Object) getValueString());
        frame.setVisible(false);
      });

      buttonPanel.add(closeButton);
      buttonPanel.add(canButton);
      frame.add(buttonPanel);

      editor.setPreferredSize(viewSize);
      editor.reset();
      //savePoly = new Polygon(zone.myPolygon.xpoints, zone.myPolygon.ypoints, zone.myPolygon.npoints);
      editor.setPolygon((polygon.npoints == 0) ? null : new Polygon(polygon.xpoints, polygon.ypoints, polygon.npoints));
      if (editor.getPolygon() != null) {
        final Rectangle polyBounds = editor.getPolygon().getBounds();
        final Point polyCenter = new Point(polyBounds.x + polyBounds.width / 2,
          polyBounds.y + polyBounds.height / 2);
        if (!editor.getVisibleRect().contains(polyCenter)) {
          editor.center(polyCenter);
        }
      }
      frame.pack();
      final Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
      frame.setSize(Math.min(frame.getWidth(), d.width * 2 / 3), Math.min(frame.getHeight(), d.height * 2 / 3));
      frame.setTitle("Sputtle Bluttle");

      frame.setVisible(true);
    }


    @Override
    public void updateCoord(String coordString) {

    }

    public void updateCoord(int x, int y) {

    }

    public void updateCoords(Polygon polygon) {

    }

    public void updateCoords() {

    }
  }

  public static class Ed implements PieceEditor {
    private final TraitConfigPanel box;
    private final IntConfigurer xConfig;
    private final IntConfigurer yConfig;
    private final IntConfigurer widthConfig;
    private final IntConfigurer heightConfig;
    private final JButton defineButton;
    private final NamedHotKeyConfigurer strokeConfig;
    private final BooleanConfigurer launchConfig;
    protected StringConfigurer descConfig;

    public Ed(ActionButton p) {
      box = new TraitConfigPanel();

      descConfig = new StringConfigurer(p.description);
      descConfig.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descConfig);

      strokeConfig = new NamedHotKeyConfigurer(p.stroke);
      box.add("Editor.ActionButton.invoke_key_command", strokeConfig);

      launchConfig = new BooleanConfigurer(p.launchPopupMenu);
      box.add("Editor.ActionButton.launch_popup_menu", launchConfig);

      xConfig = new IntConfigurer(p.bounds.x);
      box.add("Editor.ActionButton.button_x_offset", xConfig);

      yConfig = new IntConfigurer(p.bounds.y);
      box.add("Editor.ActionButton.button_y_offset", yConfig);

      widthConfig = new IntConfigurer(p.bounds.width);
      box.add("Editor.ActionButton.button_width", widthConfig);

      heightConfig = new IntConfigurer(p.bounds.height);
      box.add("Editor.ActionButton.button_height", heightConfig);

      defineButton = new JButton("Define");
      defineButton.addActionListener(e -> {
        final VisualEditor ve = new VisualEditor();
        ve.show(p);
      });
      box.add(defineButton);
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
        .append(launchConfig.getValueString());
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

          // Handle rotation of the piece, movement is relative to the current facing of the unit.
          // Traverse any traits outward from this trait (ones that could rotate this trait),
          // and find out what the cumulative rotation is
          Decorator rotateTrait = action.getOuter();
          double cumulative = 0.0;
          while (rotateTrait != null) {
            if (rotateTrait instanceof FreeRotator) {
              cumulative += ((FreeRotator)rotateTrait).getAngleInRadians();
            }
            else if (rotateTrait instanceof MatCargo) {
              cumulative += ((MatCargo)rotateTrait).getMatAngleInRadians();
            }
            rotateTrait = rotateTrait.getOuter();
          }

          final Shape shape;
          // If rotated, apply the rotation
          if (cumulative != 0.0) {
            shape = AffineTransform.getRotateInstance(cumulative,
              0,
              0)
              .createTransformedShape(action.bounds);
          }
          else {
            shape = action.bounds;
          }

          if (action.stroke != null && action.stroke.getKeyStroke() != null && shape.contains(point)) {
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
