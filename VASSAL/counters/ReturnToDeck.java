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

import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.DrawPile;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

/**
 * GamePiece trait that returns a piece to a {@link DrawPile}
 */
public class ReturnToDeck extends Decorator implements TranslatablePiece {
  public static final String ID = "return;";
  protected String deckId;
  protected String returnCommand;
  protected KeyStroke returnKey;
  protected DrawPile deck;

  protected KeyCommand[] commands;
  protected KeyCommand myCommand;

  public ReturnToDeck() {
    this(ID + "Return to Deck;R;null", null);
  }

  public ReturnToDeck(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      myCommand = new KeyCommand(returnCommand, returnKey, Decorator.getOutermost(this));
      if (returnCommand.length() > 0 && returnKey != null) {
        commands =
            new KeyCommand[]{myCommand};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    return commands;
  }

  public String myGetState() {
    return "";
  }

  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    returnCommand = st.nextToken();
    returnKey = st.nextKeyStroke(null);
    deckId = st.nextToken();
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    return ID + se.append(returnCommand).append(returnKey).append(deckId).getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command comm = null;
    if (myCommand.matches(stroke)) {
      if (deck == null) {
        findDeck();
      }
      comm = deck.addToContents(Decorator.getOutermost(this));
      // Apply Auto-move key
      Map m = deck.getMap();
      if (m != null && m.getMoveKey() != null) {
        comm.append(Decorator.getOutermost(this).keyEvent(m.getMoveKey()));
      }
      deck.getMap().repaint();
    }
    return comm;
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  private void findDeck() {
    DrawPile pile = DrawPile.findDrawPile(deckId);
    if (pile == null) {
      throw new IllegalArgumentException("Could not find deck "+deckId);
    }
    deck = pile;
  }

  public void mySetState(String newState) {
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public String getDescription() {
    String d = "Return to Deck";
    if (deck != null) {
      findDeck();
      if (deck != null) {
        d += " - " + deck.getConfigureName();
      }
    }
    return d;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ReturnToDeck.htm");
  }

  public PieceI18nData getI18nData() {
    return getI18nData(returnCommand, "Return to Deck command");
  }
  
  private static class Ed implements PieceEditor {
    private StringConfigurer menuName;
    private HotKeyConfigurer menuKey;
    private JPanel controls;
    private String deckId;
    private final JTextField tf = new JTextField(12);

    public Ed(ReturnToDeck p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      menuName = new StringConfigurer(null, "Menu Text:  ", p.returnCommand);
      controls.add(menuName.getControls());
      menuKey = new HotKeyConfigurer(null,"Keyboard Command:  ",p.returnKey);
      deckId = p.deckId;
      controls.add(menuKey.getControls());
      JButton select = new JButton("Select Deck");
      tf.setEditable(false);
      updateDeckName();
      select.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          VASSAL.configure.ChooseComponentDialog d = new VASSAL.configure.ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, controls), DrawPile.class);
          d.setTitle("Select Deck");
          d.setVisible(true);
          if (d.getTarget() != null) {
            deckId = UniqueIdManager.getIdentifier((UniqueIdManager.Identifyable)d.getTarget());
            updateDeckName();
          }
        }
      });
      Box box = Box.createHorizontalBox();
      box.add(select);
      box.add(tf);
      controls.add(box);
    }

    private void updateDeckName() {
      DrawPile p = DrawPile.findDrawPile(deckId);
      tf.setText(p != null ? p.getConfigureName() : "<none>");
    }

    public Component getControls() {
      return controls;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      return ID + se.append(menuName.getValueString()).append((KeyStroke)menuKey.getValue()).append(deckId).getValue();
    }
  }
}
