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
/*
 * Created by IntelliJ IDEA.
 * User: unknown
 * Date: Dec 30, 2002
 * Time: 12:42:01 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * A GamePiece with this trait will automatically be marked whenever it is moved.  A marked piece is
 * indicated by drawing a specified image at a specified location
 */
public class MovementMarkable extends Decorator implements TranslatablePiece {
  public static final String ID = "markmoved;";

  private int xOffset = 0;
  private int yOffset = 0;
  private String command;
  private NamedKeyStroke key;
  private IconConfigurer movedIcon = new IconConfigurer(null, "Marker Image:  ", "/images/moved.gif");
  private boolean hasMoved = false;

  public MovementMarkable() {
    this(ID + "moved.gif;0;0", null);
  }

  public MovementMarkable(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public boolean isMoved() {
    return hasMoved;
  }

  public void setMoved(boolean b) {
    hasMoved = b;
  }

  @Override
  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    movedIcon.setValue(st.nextToken());
    xOffset = st.nextInt(0);
    yOffset = st.nextInt(0);
    command = st.nextToken("Mark Moved");
    key = st.nextNamedKeyStroke('M');
  }

  @Override
  public void mySetState(String newState) {
    hasMoved = "true".equals(newState);
  }

  @Override
  public String myGetState() {
    return String.valueOf(hasMoved);
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(movedIcon.getValueString()).append(xOffset).append(yOffset).append(command).append(key);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return command.isEmpty() || key == null || key.isNull() ?
      new KeyCommand[0] :
      new KeyCommand[] {
        new KeyCommand(command, key, Decorator.getOutermost(this), this)
      };
  }

  @Override
  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    if (stroke != null && key.equals(stroke)) {
      ChangeTracker c = new ChangeTracker(this);
      // Set the property on the entire piece so all traits can respond
      Decorator.getOutermost(this).setProperty(Properties.MOVED, !hasMoved);
      return c.getChangeCommand();
    }
    else {
      return null;
    }
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle r = piece.boundingBox();
    r.add(piece.boundingBox());
    final Dimension d = getImageSize();
    r.add(new Rectangle(xOffset, yOffset, d.width, d.height));
    return r;
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
    if (hasMoved
        && movedIcon.getIconValue() != null) {
      Graphics2D g2d = (Graphics2D) g;
      AffineTransform transform = g2d.getTransform();
      g2d.scale(zoom, zoom);
      movedIcon.getIconValue().paintIcon(obs, g,
                                         (int) Math.round(x / zoom) + xOffset,
                                         (int) Math.round(y / zoom) + yOffset);
      g2d.setTransform(transform);
    }
  }

  private Dimension getImageSize() {
    Icon icon = movedIcon.getIconValue();
    return icon != null ? new Dimension(icon.getIconWidth(), icon.getIconHeight()) : new Dimension();
  }

  @Override
  public String getDescription() {
    return "Mark When Moved";
  }

  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MarkMoved.htm");
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.MOVED.equals(key)) {
      return isMoved() ? Boolean.TRUE : Boolean.FALSE;
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (Properties.MOVED.equals(key)) {
      return isMoved() ? Boolean.TRUE : Boolean.FALSE;
    }
    else {
      return super.getProperty(key);
    }
  }

  @Override
  public void setProperty(Object key, Object val) {
    if (Properties.MOVED.equals(key)) {
      setMoved(Boolean.TRUE.equals(val));
      piece.setProperty(key, val); // So other traits can respond to the property change
    }
    else {
      super.setProperty(key, val);
    }
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command, "Mark Moved command");
  }


  private static class Ed implements PieceEditor {
    private IconConfigurer iconConfig;
    private IntConfigurer xOff;
    private IntConfigurer yOff;
    private StringConfigurer command;
    private NamedHotKeyConfigurer key;
    private Box box;

    private Ed(MovementMarkable p) {
      iconConfig = p.movedIcon;
      box = Box.createVerticalBox();
      command = new StringConfigurer(null,"Command:  ",p.command);
      box.add(command.getControls());
      key = new NamedHotKeyConfigurer(null,"Keyboard command:  ",p.key);
      box.add(key.getControls());
      box.add(iconConfig.getControls());
      xOff = new IntConfigurer(null, "Horizontal Offset:  ", p.xOffset);
      yOff = new IntConfigurer(null, "Vertical Offset:  ", p.yOffset);
      box.add(xOff.getControls());
      box.add(yOff.getControls());
    }

    @Override
    public Component getControls() {
      boolean enabled = false;
      for (Map m : Map.getMapList()) {
        String value = m.getAttributeValueString(Map.MARK_MOVED);
        enabled = enabled
            || GlobalOptions.ALWAYS.equals(value)
            || GlobalOptions.PROMPT.equals(value);
      }
      if (!enabled) {
        Runnable runnable = new Runnable() {
          @Override
          public void run() {
            JOptionPane.showMessageDialog(box, "You must enable the \"Mark Pieces that Move\" option in one or more Map Windows", "Option not enabled", JOptionPane.WARNING_MESSAGE);
          }
        };
        SwingUtilities.invokeLater(runnable);
      }
      return box;
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(iconConfig.getValueString())
          .append(xOff.getValueString())
          .append(yOff.getValueString())
          .append(command.getValueString())
          .append(key.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "false";
    }
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    l.add(Properties.MOVED);
    return l;
  }
}
