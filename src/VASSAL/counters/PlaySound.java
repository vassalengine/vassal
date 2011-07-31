/*
 * $Id$
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

import java.applet.AudioClip;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.PlayAudioClipCommand;
import VASSAL.configure.AudioClipConfigurer;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * A trait that plays a sound clip
 *
 * @author rkinney
 *
 */
public class PlaySound extends Decorator implements TranslatablePiece {
  public static final String ID = "playSound;";
  protected String menuText;
  protected NamedKeyStroke stroke;
  protected boolean sendToOthers;
  protected KeyCommand command;
  protected KeyCommand[] commands;
  protected FormattedString format = new FormattedString();

  public PlaySound() {
    this(ID, null);
  }

  public PlaySound(String type, GamePiece piece) {
    mySetType(type);
    setInner(piece);
  }

  public void mySetState(String newState) {
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(format.getFormat())
      .append(menuText)
      .append(stroke)
      .append(sendToOthers);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      command = new KeyCommand(menuText, stroke, Decorator.getOutermost(this), this);
      if (menuText.length() > 0 && stroke != null && !stroke.isNull()) {
        commands = new KeyCommand[] {command};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (command.matches(stroke)) {
      final String clipName = format.getText(Decorator.getOutermost(this));
      c = new PlayAudioClipCommand(clipName);
      try {
        final AudioClip clip = GameModule.getGameModule()
                                         .getDataArchive()
                                         .getCachedAudioClip(clipName);
        if (clip != null) {
          clip.play();
        }
      }
      catch (IOException e) {
        reportDataError(this, Resources.getString("Error.not_found", "Audio Clip"), "Clip="+clipName, e);
      }
    }
    return c;
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getName() {
    return piece.getName();
  }

  public String getDescription() {
    return format.getFormat().length() == 0 ? "Play Sound" : "Play Sound - " + format.getFormat();
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    format = new FormattedString(st.nextToken(""));
    menuText = st.nextToken("Play Sound");
    stroke = st.nextNamedKeyStroke('P');
    sendToOthers = st.nextBoolean(false);
    commands = null;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PlaySound.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public PieceI18nData getI18nData() {
    return getI18nData(menuText, "Play Sound command");
  }

  public static class Ed implements PieceEditor {
    private StringConfigurer menuConfig;
    private NamedHotKeyConfigurer keyConfig;
    private AudioClipConfigurer soundConfig;
    private BooleanConfigurer sendConfig;
    private JPanel panel;

    public Ed(PlaySound p) {
      menuConfig = new StringConfigurer(null, "Menu Text:  ", p.menuText);
      keyConfig = new NamedHotKeyConfigurer(null, "Keyboard Command:  ", p.stroke);
      soundConfig = new AudioClipConfigurer(null, "Sound Clip:  ", GameModule.getGameModule().getArchiveWriter());
      soundConfig.setValue(p.format.getFormat());
      soundConfig.setEditable(true);
      sendConfig = new BooleanConfigurer(null, "Send sound to other players?", p.sendToOthers);
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.add(menuConfig.getControls());
      panel.add(keyConfig.getControls());
      panel.add(soundConfig.getControls());
      panel.add(sendConfig.getControls());
    }

    public Component getControls() {
      return panel;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(soundConfig.getValueString()).append(menuConfig.getValueString()).append(keyConfig.getValueString()).append(sendConfig.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
