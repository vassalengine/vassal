/*
 *
 * Copyright (c) 2020 Vassalengine.org, Brian Reynolds
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

import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.List;

import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

/**
 * A trait to expose a translated string as a readable/displayable property.
 */
public class TranslatableMessage extends Decorator implements TranslatablePiece {
  public static final char DELIMITER = ';';
  public static final String ID = "locmsg" + DELIMITER; // NON-NLS

  protected String key;
  protected String description;
  protected String message;
  protected String localisedMessage;


  public TranslatableMessage() {
    this(ID, null);
  }

  public TranslatableMessage(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  @Override
  public void mySetType(String s) {

    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, DELIMITER);
    sd.nextToken(); // Skip over command prefix
    key = sd.nextToken("name");
    description = sd.nextToken("");
    message = sd.nextToken("");
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
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
  public Object getProperty(Object key) {
    if (this.key.equals(key)) {
      return message;
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (this.key.equals(key)) {
      if (localisedMessage != null) {
        return localisedMessage;
      }

      localisedMessage = getI18nData().translate(message);
      return localisedMessage;
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(DELIMITER);
    se.append(key);
    se.append(description);
    se.append(message);
    return ID + se.getValue();
  }

  @Override
  public void mySetState(String state) {
  }

  @Override
  public String myGetState() {
    return "";
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
  public String getDescription() {
    return buildDescription("Editor.TranslatableMessage.trait_description", key, description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TranslatableMessage.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new VASSAL.counters.TranslatableMessage.Ed(this);
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    return List.of(key);
  }



  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof TranslatableMessage)) return false;
    final TranslatableMessage c = (TranslatableMessage) o;
    if (!key.equals(c.key)) return false;
    if (!description.equals(c.description)) return false;
    return (message.equals(c.message));
  }

  private static class Ed implements PieceEditor {
    private final StringConfigurer propName;
    private final StringConfigurer propDesc;
    private final StringConfigurer propValue;
    private final TraitConfigPanel panel;

    private Ed(TranslatableMessage m) {
      panel = new TraitConfigPanel();

      propDesc = new StringConfigurer(m.description);
      panel.add("Editor.TranslatableMessage.description", propDesc);

      propName = new StringConfigurer(m.key);
      panel.add("Editor.TranslatableMessage.property_name", propName);

      propValue = new StringConfigurer(m.message);
      panel.add("Editor.TranslatableMessage.message", propValue);
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(propName.getValueString());
      se.append(propDesc.getValueString());
      se.append(propValue.getValueString());
      return VASSAL.counters.TranslatableMessage.ID + se.getValue();
    }
  }


  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(
      new String[] { message },
      new String[] {
        Resources.getString("Editor.TranslatableMessage.message"),
      });
  }

  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(message);
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(key);
  }
}
