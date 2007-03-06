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
package VASSAL.build.module.map.boardPicker;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.OverlayLayout;
import VASSAL.build.module.map.BoardPicker;
import VASSAL.tools.BackgroundTask;

public class BoardSlot extends JPanel implements Icon, ActionListener {
  private static final long serialVersionUID = 1L;

  private String prompt = "Select board";

  protected BoardPicker picker;
  protected Board board = null;

  protected JComboBox boards;
  protected JCheckBox reverseCheckBox;

  public BoardSlot(BoardPicker bp) {
    this(bp,"Select board");
  }
  public BoardSlot(BoardPicker bp, String prompt) {
    this.prompt = prompt;
    picker = bp;
    boards = new JComboBox();
    boards.addItem(prompt);
    String s[] = picker.getAllowableBoardNames();
    for (int i = 0; i < s.length; ++i) {
      boards.addItem(s[i]);
    }
    boards.setSelectedIndex(s.length == 1 ? 1 : 0);
    boards.addActionListener(this);

    reverseCheckBox = new JCheckBox("flip");
    reverseCheckBox.addItemListener(new ItemListener() {
      public void itemStateChanged(ItemEvent e) {
        if (getBoard() != null) {
          getBoard().setReversed(reverseCheckBox.isSelected());
          picker.repaint();
        }
      }
    });

    reverseCheckBox.setVisible(false);

    setLayout(new OverlayLayout(this));
    JPanel p = new JPanel();
    Box b = Box.createHorizontalBox();
    b.add(boards);
    b.add(reverseCheckBox);
    p.add(b);
    p.setOpaque(false);
    p.setAlignmentX(0.5F);
    JLabel l = new JLabel(this);
    l.setAlignmentX(0.5F);

    add(p);
    add(l);

    actionPerformed(null);
  }

  public void actionPerformed(ActionEvent e) {
    if (prompt.equals(boards.getSelectedItem())) {
      setBoard(null);
    }
    else {
      Board b = picker.getBoard((String) boards.getSelectedItem());
      if (picker.getBoardsFromControls().contains(b)) {
        b = b.copy();
      }
      setBoard(b);
    }
  }

  public Board getBoard() {
    return board;
  }

  public void setBoard(final Board b) {
    board = b;
    System.gc();
    if (b != null) {
      reverseCheckBox.setVisible("true".equals(b.getAttributeValueString(Board.REVERSIBLE)));
      reverseCheckBox.setSelected(b.isReversed());

      board = b;
      picker.warn("Loading " + b.getName() + " ...");
      final javax.swing.Timer t = new javax.swing.Timer(1000, new ActionListener() {
        boolean toggle = false;

        public void actionPerformed(ActionEvent evt) {
          if (toggle) {
            picker.warn("Loading " + b.getName() + " ...");
          }
          else {
            picker.warn("Loading " + b.getName());
          }
          toggle = !toggle;
        }
      });
      new BackgroundTask() {
        public void doFirst() {
          if (board != null) {
            board.fixImage();
          }
        }

        public void doLater() {
          picker.warn("Loaded " + b.getName());
          t.stop();
          setSize(getPreferredSize());
          revalidate();
          picker.pack();
          repaint();
        }
      }.start();
      t.start();
    }
    else {
      reverseCheckBox.setVisible(false);
      revalidate();
      setSize(getPreferredSize());
      picker.pack();
      repaint();
    }
  }

  public int getIconHeight() {
    if (board != null) {
      return (int)(picker.getSlotScale() * board.bounds().height);
    }
    else if (this == picker.getSlot(0)
      || picker.getSlot(0) == null) {
      return picker.getDefaultSlotSize().height;
    }
    else {
      return picker.getSlot(0).getIconHeight();
    }
  }

  public int getIconWidth() {
    if (board != null) {
      return (int)(picker.getSlotScale() * board.bounds().width);
    }
    else if (this == picker.getSlot(0)
      || picker.getSlot(0) == null) {
      return picker.getDefaultSlotSize().width;
    }
    else {
      return picker.getSlot(0).getIconWidth();
    }
  }

  public void paintIcon(Component c, Graphics g, int x, int y) {
    if (board != null) {
      board.draw(g,x,y,picker.getSlotScale(), c);
    }
    else {
      g.clearRect(x,y,getIconWidth(),getIconHeight());
    }
  }
}
