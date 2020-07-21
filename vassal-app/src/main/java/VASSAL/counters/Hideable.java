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
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

public class Hideable extends Decorator implements TranslatablePiece {

  public static final String ID = "hide;";
  public static final String HIDDEN_BY = "hiddenBy";
  public static final String TRANSPARENCY = "transparency";

  protected String hiddenBy;
  protected NamedKeyStroke hideKey;
  protected String command = "Invisible";
  protected PieceAccess access = PlayerAccess.getInstance();

  protected float transparency = 0.3f;
  protected Color bgColor;

  protected KeyCommand[] commands;
  protected KeyCommand hideCommand;

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
      return String.valueOf(invisibleToOthers()) +
             invisibleToMe() + piece.getProperty(key);
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
      return String.valueOf(invisibleToOthers()) +
        invisibleToMe() + piece.getProperty(key);
    }
    else {
      return super.getProperty(key);
    }
  }

  public Hideable() {
    this(ID + "I", null);
  }

  public Hideable(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  @Override
  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    hideKey = st.nextNamedKeyStroke('I');
    command = st.nextToken("Invisible");
    bgColor = st.nextColor(null);
    access = PieceAccessConfigurer.decode(st.nextToken(null));
    transparency = st.hasMoreTokens() ? (float) st.nextDouble(0.3) : 0.3f;
    commands = null;
  }

  @Override
  public void mySetState(String in) {
    hiddenBy = "null".equals(in) ? null : in;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(hideKey)
      .append(command)
      .append(bgColor)
      .append(PieceAccessConfigurer.encode(access))
      .append(transparency);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    return hiddenBy == null ? "null" : hiddenBy;
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
        commands = new KeyCommand[0];
      }
    }
    hideCommand.setEnabled(access.currentPlayerCanModify(hiddenBy));
    return commands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (hideCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
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
    return "Invisible";
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Hideable.htm");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * If true, then all hidden pieces are considered invisible to all players.
   * Used to temporarily draw pieces as they appear to other players
   *
   * @param allHidden
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
    return getI18nData(command, "Hide command");
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    l.add(Properties.INVISIBLE_TO_OTHERS);
    return l;
  }

  protected static class Ed implements PieceEditor {
    protected NamedHotKeyConfigurer hideKeyInput;
    protected JTextField hideCommandInput;
    protected ColorConfigurer colorConfig;
    protected IntConfigurer transpConfig;
    protected PieceAccessConfigurer accessConfig;
    protected JPanel controls;

    public Ed(Hideable p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      hideKeyInput = new NamedHotKeyConfigurer(null, "Keyboard command:  ", p.hideKey);
      controls.add(hideKeyInput.getControls());

      Box b = Box.createHorizontalBox();
      hideCommandInput = new JTextField(16);
      hideCommandInput.setMaximumSize(hideCommandInput.getPreferredSize());
      hideCommandInput.setText(p.command);
      b.add(new JLabel("Menu Text:  "));
      b.add(hideCommandInput);
      controls.add(b);

      colorConfig = new ColorConfigurer(null, "Background color:  ", p.bgColor);
      controls.add(colorConfig.getControls());

      transpConfig = new IntConfigurer(null, "Transparency (%):  ", (int) (p.transparency * 100));
      controls.add(transpConfig.getControls());

      accessConfig = new PieceAccessConfigurer(null, "Can by hidden by:  ", p.access);
      controls.add(accessConfig.getControls());
    }

    @Override
    public String getState() {
      return "null";
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(hideKeyInput.getValueString())
        .append(hideCommandInput.getText())
        .append(
          colorConfig.getValue() == null ? "" : colorConfig.getValueString()
        )
        .append(accessConfig.getValueString())
        .append(transpConfig.getIntValue(30) / 100.0f);
      return ID + se.getValue();
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }

}
