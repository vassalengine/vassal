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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.swing.KeyStroke;

public class Hideable extends Decorator implements TranslatablePiece {

  public static final String ID = "hide;"; // NON-NLS
  public static final String HIDDEN_BY = "hiddenBy"; // NON-NLS
  public static final String TRANSPARENCY = "transparency"; // NON-NLS

  protected String hiddenBy;
  protected NamedKeyStroke hideKey;
  protected String command = Resources.getString("Editor.Hideable.default_command");
  protected PieceAccess access = PlayerAccess.getInstance();

  protected float transparency = 0.3f;
  protected Color bgColor;

  protected KeyCommand[] commands;
  protected KeyCommand hideCommand;
  protected String description = "";

  @Override
  public void setProperty(Object key, Object val) {
    if (HIDDEN_BY.equals(key)) {
      hiddenBy = (String) val;
    }
    else {
      super.setProperty(key, val);
    }
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (invisibleToMe()) {
      return ((BasicPiece) Decorator.getInnermost(this)).getLocalizedPublicProperty(key);
    }
    else if (HIDDEN_BY.equals(key)) {
      return hiddenBy;
    }
    else if (Properties.INVISIBLE_TO_ME.equals(key)) {
      return invisibleToMe() ? Boolean.TRUE : Boolean.FALSE;
    }
    else if (Properties.INVISIBLE_TO_OTHERS.equals(key)) {
      return invisibleToOthers() ? Boolean.TRUE : Boolean.FALSE;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return Boolean.toString(invisibleToOthers()) + invisibleToMe() + piece.getProperty(key);
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (HIDDEN_BY.equals(key)) {
      return hiddenBy;
    }
    else if (Properties.INVISIBLE_TO_ME.equals(key)) {
      return invisibleToMe() ? Boolean.TRUE : Boolean.FALSE;
    }
    else if (Properties.INVISIBLE_TO_OTHERS.equals(key)) {
      return invisibleToOthers() ? Boolean.TRUE : Boolean.FALSE;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return Boolean.toString(invisibleToOthers()) + invisibleToMe() + piece.getProperty(key);
    }
    else {
      return super.getProperty(key);
    }
  }

  public Hideable() {
    this(ID + "I", null); // NON-NLS
  }

  public Hideable(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    hideKey = st.nextNamedKeyStroke('I');
    command = st.nextToken(Resources.getString("Editor.Hideable.default_command"));
    bgColor = st.nextColor(null);
    access = PieceAccessConfigurer.decode(st.nextToken(null));
    transparency = Math.max(0.0f, Math.min(1.0f, st.hasMoreTokens() ? (float) st.nextDouble(0.3) : 0.3f));
    description = st.nextToken("");
    commands = null;
  }

  @Override
  public void mySetState(String in) {
    hiddenBy = "null".equals(in) ? null : in; // NON-NLS
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(hideKey)
      .append(command)
      .append(bgColor)
      .append(PieceAccessConfigurer.encode(access))
      .append(transparency)
      .append(description);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    return hiddenBy == null ? "null" : hiddenBy; // NON-NLS
  }

  public boolean invisibleToMe() {
    return !access.currentPlayerHasAccess(hiddenBy);
  }

  public boolean invisibleToOthers() {
    return hiddenBy != null;
  }

  @Override
  public Shape getShape() {
    if (invisibleToMe()) {
      return new Rectangle();
    }
    else {
      return piece.getShape();
    }
  }

  @Override
  public Rectangle boundingBox() {
    if (invisibleToMe()) {
      return new Rectangle();
    }
    else {
      return piece.boundingBox();
    }
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (invisibleToMe()) {
      return;
    }
    else if (invisibleToOthers()) {
      final Graphics2D g2d = (Graphics2D) g;

      if (bgColor != null) {
        g.setColor(bgColor);
        final AffineTransform t = AffineTransform.getScaleInstance(zoom, zoom);
        t.translate(x / zoom, y / zoom);
        g2d.fill(t.createTransformedShape(piece.getShape()));
      }

      final Composite oldComposite = g2d.getComposite();
      g2d.setComposite(
        AlphaComposite.getInstance(AlphaComposite.SRC_OVER, transparency)
      );
      piece.draw(g, x, y, obs, zoom);
      g2d.setComposite(oldComposite);
    }
    else {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  @Override
  public String getName() {
    if (invisibleToMe()) {
      return "";
    }
    else if (invisibleToOthers()) {
      return piece.getName() + "(" + command + ")";
    }
    else {
      return piece.getName();
    }
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      hideCommand = new KeyCommand(command, hideKey, Decorator.getOutermost(this), this);
      if (command.length() > 0 && hideKey != null && ! hideKey.isNull()) {
        commands = new KeyCommand[] {hideCommand};
      }
      else {
        commands = KeyCommand.NONE;
      }
    }
    hideCommand.setEnabled(access.currentPlayerCanModify(hiddenBy));
    return commands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (hideCommand.matches(stroke)) {
      final ChangeTracker tracker = new ChangeTracker(this);
      if (invisibleToOthers()) {
        hiddenBy = null;
      }
      else if (!invisibleToMe()) {
        hiddenBy = access.getCurrentPlayerId();
      }
      return tracker.getChangeCommand();
    }
    return null;
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.Hideable.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Hideable.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * If true, then all hidden pieces are considered invisible to all players.
   * Used to temporarily draw pieces as they appear to other players
   *
   * @param allHidden true if all pieces should be considered hidden
   * @deprecated
   */
  @Deprecated
  public static void setAllHidden(boolean allHidden) {
    if (allHidden) {
      PieceAccess.GlobalAccess.hideAll();
    }
    else {
      PieceAccess.GlobalAccess.revertAll();
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command, Resources.getString("Editor.Hideable.hide_command"));
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(Properties.INVISIBLE_TO_OTHERS);
    return l;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Hideable)) return false;
    final Hideable c = (Hideable) o;

    if (! Objects.equals(hideKey, c.hideKey)) return false;
    if (! Objects.equals(command, c.command)) return false;
    if (! Objects.equals(bgColor, c.bgColor)) return false;
    if (! Objects.equals(PieceAccessConfigurer.encode(access), PieceAccessConfigurer.encode(c.access))) return false;
    if (! Objects.equals(transparency, c.transparency)) return false;

    return Objects.equals(hiddenBy, c.hiddenBy);
  }

  protected static class Ed implements PieceEditor {
    protected NamedHotKeyConfigurer hideKeyInput;
    protected StringConfigurer hideCommandInput;
    protected ColorConfigurer colorConfig;
    protected IntConfigurer transpConfig;
    protected PieceAccessConfigurer accessConfig;
    protected TraitConfigPanel controls;
    private final StringConfigurer descInput;

    public Ed(Hideable p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      hideCommandInput = new StringConfigurer(p.command);
      hideCommandInput.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", hideCommandInput);

      hideKeyInput = new NamedHotKeyConfigurer(p.hideKey);
      controls.add("Editor.keyboard_command", hideKeyInput);

      colorConfig = new ColorConfigurer(p.bgColor);
      controls.add("Editor.Hideable.background_color", colorConfig);

      transpConfig = new IntConfigurer((int) (p.transparency * 100));
      controls.add("Editor.Hideable.opacity", transpConfig);

      accessConfig = new PieceAccessConfigurer(p.access);
      controls.add("Editor.Hideable.can_be_hidden_by", accessConfig);
    }

    @Override
    public String getState() {
      return "null"; // NON-NLS
    }

    @Override
    public String getType() {
      final float transp = Math.max(0.0f, Math.min(1.0f, transpConfig.getIntValue(30) / 100.0f));

      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(hideKeyInput.getValueString())
        .append(hideCommandInput.getValueString())
        .append(colorConfig.getValue() == null ? "" : colorConfig.getValueString())
        .append(accessConfig.getValueString())
        .append(transp)
        .append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(hideKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(command);
  }
}
