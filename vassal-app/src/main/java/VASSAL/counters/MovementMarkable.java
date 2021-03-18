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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import java.util.Objects;
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
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * A GamePiece with this trait will automatically be marked whenever it is moved.  A marked piece is
 * indicated by drawing a specified image at a specified location
 */
public class MovementMarkable extends Decorator implements TranslatablePiece {
  public static final String ID = "markmoved;"; // NON-NLS

  private int xOffset = 0;
  private int yOffset = 0;
  private String command;
  private NamedKeyStroke key;
  private final IconConfigurer movedIcon = new IconConfigurer("/images/moved.gif"); // NON-NLS
  private boolean hasMoved = false;
  private String description;

  public MovementMarkable() {
    this(ID + "moved.gif;0;0", null); // NON-NLS
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
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    movedIcon.setValue(st.nextToken());
    xOffset = st.nextInt(0);
    yOffset = st.nextInt(0);
    command = st.nextToken(Resources.getString("Editor.MovementMarkable.default_command"));
    key = st.nextNamedKeyStroke('M');
    description = st.nextToken("");
  }

  @Override
  public void mySetState(String newState) {
    hasMoved = "true".equals(newState); // NON-NLS
  }

  @Override
  public String myGetState() {
    return String.valueOf(hasMoved);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(movedIcon.getValueString()).append(xOffset).append(yOffset).append(command).append(key).append(description);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return command.isEmpty() || key == null || key.isNull() ?
      KeyCommand.NONE :
      new KeyCommand[] {
        new KeyCommand(command, key, Decorator.getOutermost(this), this)
      };
  }

  @Override
  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    if (stroke == null) return null;

    if (key.equals(stroke)) {
      final ChangeTracker c = new ChangeTracker(this);
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
      final Graphics2D g2d = (Graphics2D) g;
      final AffineTransform transform = g2d.getTransform();
      g2d.scale(zoom, zoom);
      movedIcon.getIconValue().paintIcon(obs, g,
                                         (int) Math.round(x / zoom) + xOffset,
                                         (int) Math.round(y / zoom) + yOffset);
      g2d.setTransform(transform);
    }
  }

  private Dimension getImageSize() {
    final Icon icon = movedIcon.getIconValue();
    return icon != null ? new Dimension(icon.getIconWidth(), icon.getIconHeight()) : new Dimension();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.MovementMarkable.trait_description", description);
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MarkMoved.html"); // NON-NLS
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
    return getI18nData(command, Resources.getString("Editor.MovementMarkable.mark_moved_command"));
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof MovementMarkable)) return false;
    final MovementMarkable c = (MovementMarkable) o;
    if (! Objects.equals(movedIcon.getValueString(), c.movedIcon.getValueString())) return false;
    if (! Objects.equals(xOffset, c.xOffset)) return false;
    if (! Objects.equals(yOffset, c.yOffset)) return false;
    if (! Objects.equals(command, c.command)) return false;
    if (! Objects.equals(key, c.key)) return false;
    return Objects.equals(hasMoved, c.hasMoved);
  }

  private static class Ed implements PieceEditor {
    private final IconConfigurer iconConfig;
    private final IntConfigurer xOff;
    private final IntConfigurer yOff;
    private final StringConfigurer command;
    private final NamedHotKeyConfigurer key;
    private final TraitConfigPanel box;
    private final StringConfigurer descInput;

    private Ed(MovementMarkable p) {

      box = new TraitConfigPanel();

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descInput);

      command = new StringConfigurer(p.command);
      command.setHintKey("Editor.menu_command_hint");
      box.add("Editor.menu_command", command);

      key = new NamedHotKeyConfigurer(p.key);
      box.add("Editor.keyboard_command", key);

      iconConfig = p.movedIcon;
      box.add("Editor.MovementMarkable.marker_image", iconConfig);

      xOff = new IntConfigurer(p.xOffset);
      box.add("Editor.MovementMarkable.horizontal_offset", xOff);

      yOff = new IntConfigurer(p.yOffset);
      box.add("Editor.MovementMarkable.vertical_offset", yOff);
    }

    @Override
    public Component getControls() {
      boolean enabled = false;
      for (final Map m : Map.getMapList()) {
        final String value = m.getAttributeValueString(Map.MARK_MOVED);
        enabled = enabled
            || GlobalOptions.ALWAYS.equals(value)
            || GlobalOptions.PROMPT.equals(value);
      }
      if (!enabled) {
        final Runnable runnable = () -> JOptionPane.showMessageDialog(box, Resources.getString("Editor.MovementMarkable.enable_text"), Resources.getString("Editor.MovementMarkable.option_not_enabled"), JOptionPane.WARNING_MESSAGE);
        SwingUtilities.invokeLater(runnable);
      }
      return box;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(iconConfig.getValueString())
          .append(xOff.getValueString())
          .append(yOff.getValueString())
          .append(command.getValueString())
          .append(key.getValueString())
          .append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "false"; // NON-NLS
    }
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(Properties.MOVED);
    return l;
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

  @Override
  public void addLocalImageNames(Collection<String> s) {
    final String iconName = movedIcon.getValueString();
    if (iconName != null) s.add(iconName);
  }
}
