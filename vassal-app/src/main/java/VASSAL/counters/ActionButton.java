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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;

/**
 * A Trait (aka {@link Decorator} that acts like a button on a GamePiece, such that clicking on a
 * particular area of the piece invokes a key command
 *
 * @author rkinney
 */
public class ActionButton extends Decorator implements EditablePiece, Loopable {
  public static final String ID = "button;"; // NON-NLS
  protected NamedKeyStroke stroke;
  protected Rectangle bounds = new Rectangle();
  protected ButtonPusher pusher;
  protected String description = "";
  protected static final ButtonPusher globalPusher = new ButtonPusher();

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
      .append(description);
    return ID + se.getValue();
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
    return buildDescription("Editor.ActionButton.trait_description", description);
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
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ActionButton.html"); // NON-NLS
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
    if (! (o instanceof ActionButton)) return false;
    final ActionButton c = (ActionButton) o;
    if (! Objects.equals(bounds, c.bounds)) return false;
    return Objects.equals(description, c.description);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {
    private final TraitConfigPanel box;
    private final IntConfigurer xConfig;
    private final IntConfigurer yConfig;
    private final IntConfigurer widthConfig;
    private final IntConfigurer heightConfig;
    private final NamedHotKeyConfigurer strokeConfig;
    protected StringConfigurer descConfig;

    public Ed(ActionButton p) {
      box = new TraitConfigPanel();

      descConfig = new StringConfigurer(p.description);
      descConfig.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descConfig);

      strokeConfig = new NamedHotKeyConfigurer(p.stroke);
      box.add("Editor.ActionButton.invoke_key_command", strokeConfig);

      xConfig = new IntConfigurer(p.bounds.x);
      box.add("Editor.ActionButton.button_x_offset", xConfig);

      yConfig = new IntConfigurer(p.bounds.y);
      box.add("Editor.ActionButton.button_y_offset", yConfig);

      widthConfig = new IntConfigurer(p.bounds.width);
      box.add("Editor.ActionButton.button_width", widthConfig);

      heightConfig = new IntConfigurer(p.bounds.height);
      box.add("Editor.ActionButton.button_height", heightConfig);
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
        .append(descConfig.getValueString());
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
          if (action.stroke != null && action.stroke.getKeyStroke() != null && action.bounds.contains(point)) {
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
        if (SwingUtils.isMainMouseButtonDown(e)) {
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
          if (SwingUtils.isMainMouseButtonDown(e) &&
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
