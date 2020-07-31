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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.swing.Box;
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
 * A trait that acts like a button on a GamePiece, such that clicking on a
 * particular area of the piece invokes a keyboard command
 *
 * @author rkinney
 *
 */
public class ActionButton extends Decorator implements EditablePiece, Loopable {
  public static final String ID = "button;";
  protected NamedKeyStroke stroke;
  protected Rectangle bounds = new Rectangle();
  protected ButtonPusher pusher;
  protected String description = "";
  protected static ButtonPusher globalPusher = new ButtonPusher();

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
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(stroke).append(bounds.x).append(bounds.y).append(bounds.width).append(bounds.height).append(description);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
    if (getMap() != null) {
      pusher.register(getMap());
    }
    else {
      // Do not allow button pushes if piece is not on a map
      // pusher.register(obs, Decorator.getOutermost(this), x, y);
    }
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
  public String getDescription() {
    return description.length() == 0 ? "Action Button" : "Action Button - " + description;
  }

  @Override
  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
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
    return HelpFile.getReferenceManualPage("ActionButton.htm");
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
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public static class Ed implements PieceEditor {
    private Box box;
    private IntConfigurer xConfig;
    private IntConfigurer yConfig;
    private IntConfigurer widthConfig;
    private IntConfigurer heightConfig;
    private NamedHotKeyConfigurer strokeConfig;
    protected StringConfigurer descConfig;

    public Ed(ActionButton p) {
      box = Box.createVerticalBox();
      descConfig = new StringConfigurer(null, "Description:  ", p.description);
      box.add(descConfig.getControls());
      strokeConfig = new NamedHotKeyConfigurer(null, "Invoke Key Command:  ", p.stroke);
      box.add(strokeConfig.getControls());
      xConfig = new IntConfigurer(null, "Button X-offset:  ", p.bounds.x);
      box.add(xConfig.getControls());
      yConfig = new IntConfigurer(null, "Button Y-offset:  ", p.bounds.y);
      box.add(yConfig.getControls());
      widthConfig = new IntConfigurer(null, "Button Width:  ", p.bounds.width);
      box.add(widthConfig.getControls());
      heightConfig = new IntConfigurer(null, "Button Height:  ", p.bounds.height);
      box.add(heightConfig.getControls());
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(strokeConfig.getValueString()).append(xConfig.getValueString()).append(yConfig.getValueString()).append(widthConfig.getValueString()).append(
          heightConfig.getValueString()).append(descConfig.getValueString());
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
    private Set<Map> maps = new HashSet<>();
    private java.util.Map<Component, ComponentMouseListener>
      componentMouseListeners = new HashMap<>();

    public void register(Map map) {
      if (map != null) {
        if (!maps.contains(map)) {
          map.addLocalMouseListener(new MapMouseListener(map));
          maps.add(map);
        }
      }
    }

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
     * @param x
     * @param y
     * @param Offset
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
        if (piece instanceof ActionButton) {
          ActionButton action = (ActionButton) piece;
          if (action.stroke != null && action.stroke.getKeyStroke() != null && action.bounds.contains(point)) {
            // Save state prior to command
            p.setProperty(Properties.SNAPSHOT,
              PieceCloner.getInstance().clonePiece(p));
            try {
              RecursionLimiter.startExecution(action);
              Command command = p.keyEvent(action.stroke.getKeyStroke());
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
      private Map map;

      public MapMouseListener(Map map) {
        this.map = map;
      }

      @Override
      public void mouseClicked(MouseEvent e) {
        if (SwingUtils.isMainMouseButtonDown(e)) {
          final Point point = e.getPoint();
          final GamePiece p = map.findPiece(point, PieceFinder.PIECE_IN_STACK);
          if (p != null) {
            final Point rel = map.positionOf(p);
            point.translate(-rel.x, -rel.y);
            doClick(p, point);
          }
        }
      }
    }

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
