/*
 * $Id$
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
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.tools.SequenceEncoder;

public class Hideable extends Decorator implements EditablePiece {

  public static final String ID = "hide;";
  public static final String HIDDEN_BY = "hiddenBy";

  protected String hiddenBy;
  protected KeyStroke hideKey;
  protected String command = "Invisible";
  protected PieceAccess access = PlayerAccess.getInstance();

  protected Color bgColor;

  protected KeyCommand[] commands;
  protected KeyCommand hideCommand;

  public void setProperty(Object key, Object val) {
    if (HIDDEN_BY.equals(key)) {
      hiddenBy = (String) val;
    }
    else {
      super.setProperty(key, val);
    }
  }

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

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    hideKey = st.nextKeyStroke('I');
    command = st.nextToken("Invisible");
    bgColor = st.nextColor(null);
    access = PieceAccessConfigurer.decode(st.nextToken(null));
    commands = null;
  }

  public void mySetState(String in) {
    hiddenBy = "null".equals(in) ? null : in;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(hideKey).append(command).append(bgColor).append(PieceAccessConfigurer.encode(access));
    return ID + se.getValue();
  }

  public String myGetState() {
    return hiddenBy == null ? "null" : hiddenBy;
  }

  public boolean invisibleToMe() {
    return !access.currentPlayerHasAccess(hiddenBy);
  }

  public boolean invisibleToOthers() {
    return hiddenBy != null;
  }

  public Shape getShape() {
    if (invisibleToMe()) {
      return new Rectangle();
    }
    else {
      return piece.getShape();
    }
  }

  public Rectangle boundingBox() {
    if (invisibleToMe()) {
      return new Rectangle();
    }
    else {
      return piece.boundingBox();
    }
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (invisibleToMe()) {
      return;
    }
    else if (invisibleToOthers()) {
      Graphics2D g2d = (Graphics2D) g;

      if (bgColor != null) {
        g.setColor(bgColor);
        AffineTransform t = AffineTransform.getScaleInstance(zoom, zoom);
        t.translate(x / zoom, y / zoom);
        g2d.fill(t.createTransformedShape(piece.getShape()));
      }

      Composite oldComposite = g2d.getComposite();
      g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3F));
      piece.draw(g, x, y, obs, zoom);
      g2d.setComposite(oldComposite);
    }
    else {
      piece.draw(g, x, y, obs, zoom);
    }
  }

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

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      hideCommand = new KeyCommand(command, hideKey, Decorator.getOutermost(this));
      if (command.length() > 0 && hideKey != null) {
        commands = new KeyCommand[] {hideCommand};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    hideCommand.setEnabled(access.currentPlayerCanModify(hiddenBy));
    return commands;
  }

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

  public String getDescription() {
    return "Invisible";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Hideable.htm");
  }

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
  public static void setAllHidden(boolean allHidden) {
    if (allHidden) {
      PieceAccess.GlobalAccess.hideAll();
    }
    else {
      PieceAccess.GlobalAccess.revertAll();
    }
  }

  protected static class Ed implements PieceEditor {
    protected HotKeyConfigurer hideKeyInput;
    protected JTextField hideCommandInput;
    protected ColorConfigurer colorConfig;
    protected PieceAccessConfigurer accessConfig;
    protected JPanel controls;

    public Ed(Hideable p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      hideKeyInput = new HotKeyConfigurer(null, "Keyboard command:  ", p.hideKey);
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

      accessConfig = new PieceAccessConfigurer(null, "Can by hidden by:  ", p.access);
      controls.add(accessConfig.getControls());
    }

    public String getState() {
      return "null";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append((KeyStroke) hideKeyInput.getValue()).append(hideCommandInput.getText()).append(
          colorConfig.getValue() == null ? "" : colorConfig.getValueString()).append(accessConfig.getValueString());
      return ID + se.getValue();
    }

    public Component getControls() {
      return controls;
    }
  }

}
