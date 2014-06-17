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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.DrawPile;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.UniqueIdManager;

/**
 * GamePiece trait that returns a piece to a {@link DrawPile}
 */
public class ReturnToDeck extends Decorator implements TranslatablePiece {
  public static final String ID = "return;";
  protected String deckId;
  protected String returnCommand;
  protected String selectDeckPrompt="Select destination";
  protected NamedKeyStroke returnKey;
  protected DrawPile deck;

  protected KeyCommand[] commands;
  protected KeyCommand myCommand;

  public ReturnToDeck() {
    this(ID + "Return to Deck;R;", null);
  }

  public ReturnToDeck(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      myCommand = new KeyCommand(returnCommand, returnKey, Decorator.getOutermost(this), this);
      if (returnCommand.length() > 0 && returnKey != null && !returnKey.isNull()) {
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
    returnKey = st.nextNamedKeyStroke(null);
    deckId = st.nextToken("");
    selectDeckPrompt = st.nextToken(selectDeckPrompt);
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    return ID + se.append(returnCommand).append(returnKey).append(deckId).append(selectDeckPrompt).getValue();
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command comm = null;
    if (myCommand.matches(stroke)) {
      DrawPile pile = deck;
      if (pile == null || deckId.length() == 0)
        pile = findDeck();
      if (pile == null)
        return null;
      final Map preMap = getMap();
      final Point prePos = getPosition();
      setOldProperties();
      comm = pile.addToContents(Decorator.getOutermost(this));
      // Apply Auto-move key if the piece has moved
      Map m = pile.getMap();
      if (m != null && m.getMoveKey() != null && (m != preMap || !getPosition().equals(prePos))) {
        comm.append(Decorator.getOutermost(this).keyEvent(m.getMoveKey()));
      }
      pile.getMap().repaint();
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

  private DrawPile findDeck() {
    DrawPile pile = null;
    if (deckId.length() > 0)
      pile = DrawPile.findDrawPile(deckId);
    if (pile == null)
      return promptForDrawPile();
    // cache
    deck = pile;
    return pile;
  }

  private DrawPile promptForDrawPile() {
    final JDialog d = new JDialog(GameModule.getGameModule().getFrame(), true);
    d.setTitle(Decorator.getInnermost(this).getName()); //$NON-NLS-1$
    d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));

    class AvailableDeck {
      private DrawPile pile;

      public AvailableDeck(DrawPile pile) {
        this.pile = pile;
      }

      public String toString() {
        return pile.getConfigureName();
      }
    }

    final List<DrawPile> piles =
      GameModule.getGameModule().getAllDescendantComponentsOf(DrawPile.class);

    if (piles.size() == 0) {
      reportDataError(this, "No decks in module.");
      return null;
    }

    final AvailableDeck[] decks = new AvailableDeck[piles.size()];
    int i = 0;
    for (DrawPile p : piles)
      decks[i++] = new AvailableDeck(p);

    final JList list = new JList(decks);
    list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JLabel prompt = new JLabel(selectDeckPrompt);
    prompt.setAlignmentX(0.5f);
    d.add(prompt); //$NON-NLS-1$
    d.add(new ScrollPane(list));
    Box box = Box.createHorizontalBox();
    box.setAlignmentX(0.5F);
    JButton b = new JButton(Resources.getString(Resources.OK));
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        AvailableDeck selection = (AvailableDeck) list.getSelectedValue();
        if (selection != null)
          deck = selection.pile;
        d.dispose();
      }
    });
    box.add(b);
    b = new JButton(Resources.getString(Resources.CANCEL));
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        d.dispose();
      }
    });
    box.add(b);
    d.add(box);
    d.pack();
    d.setLocationRelativeTo(d.getOwner());
    d.setVisible(true);
    // don't cache -- ask again next time
    DrawPile pile = deck;
    deck = null;
    return pile;
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
    private NamedHotKeyConfigurer menuKey;
    private JPanel controls;
    private String deckId;
    private final JTextField tf = new JTextField(12);
    private StringConfigurer promptText;
    private JCheckBox prompt;

    public Ed(ReturnToDeck p) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
      menuName = new StringConfigurer(null, "Menu Text:  ", p.returnCommand);
      controls.add(menuName.getControls());
      menuKey = new NamedHotKeyConfigurer(null,"Keyboard Command:  ",p.returnKey);
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
      final Box box = Box.createHorizontalBox();
      box.add(select);
      box.add(tf);
      controls.add(box);
      promptText = new StringConfigurer(null,"Prompt for destination deck:  ",p.selectDeckPrompt);
      prompt = new JCheckBox("Choose destination deck at game time?");
      controls.add(prompt);
      controls.add(promptText.getControls());
      promptText.getControls().setVisible(p.deckId == null || p.deckId.length() == 0);
      box.setVisible(p.deckId != null && p.deckId.length() > 0);
      prompt.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          box.setVisible(!prompt.isSelected());
          promptText.getControls().setVisible(prompt.isSelected());
        }
      });
      prompt.setSelected(p.deckId == null || p.deckId.length() == 0);
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
      return ID + se.append(menuName.getValueString()).append(menuKey.getValueString()).append(prompt.isSelected() ? "" : deckId).append(promptText.getValueString()).getValue();
    }
  }
}
