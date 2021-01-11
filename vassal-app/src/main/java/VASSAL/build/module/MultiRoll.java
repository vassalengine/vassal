/*
 *
 * Copyright (c) 2000-2003 by Brent Easton
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
package VASSAL.build.module;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;

import VASSAL.build.GameModule;
import VASSAL.build.module.dice.RollSet;
import VASSAL.preferences.Prefs;

/**
 * @author Brent Easton
 *
 * Dialog for defining a {@link DieManager RollSet}
 * For use with internet dice rollers
 */
public class MultiRoll extends JDialog implements ActionListener {
  private static final long serialVersionUID = 1L;

  private final JButton rollButton = new JButton("Roll");
  private final JButton canButton = new JButton("Cancel");
  private final JButton emailButton = new JButton("Change Email Address");

  private JDialog me;
  private JPanel serverPanel;
  private JLabel serverLabel;
  private JPanel emailPanel;
  private JLabel emailLabel;
  private JPanel descPanel;
  private JTextField descText;
  private JPanel topPanel;
  private JPanel buttonPanel;
  private JPanel detailPanel;
  protected int lastSelectedRow, lastSelectedCol;
  private String description = "";

  protected RollRow[] rollRows;

  public static final int COL_IDX = 0;
  public static final int COL_ROLL = 1;
  public static final int COL_DESC = 2;
  public static final int COL_NDICE = 3;
  public static final int COL_NSIDES = 4;
  public static final int COL_ADD = 5;
  public static final int COL_TOTAL = 6;
  public static final int NUMCOLS = 7;

  public static final int MAX_ROLLS = 10;
  public static final int ROW_HEIGHT = 20;

  public static final int COL1_WIDTH = 31;
  public static final int COL2_WIDTH = 30;
  public static final int COL3_WIDTH = 137;
  public static final int COL4_WIDTH = 50;
  public static final int COL5_WIDTH = 50;
  public static final int COL6_WIDTH = 25;
  public static final int COL7_WIDTH = 35;

  protected DieManager dieManager;
  protected DieRoll[] rolls = new DieRoll[MAX_ROLLS];
  protected boolean[] useDie = new boolean[MAX_ROLLS];
  protected String verification = "";
  protected boolean rollCancelled = false;
  protected boolean singleRoll;

  protected MultiRoll() {
    super(GameModule.getGameModule().getPlayerWindow());
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        rollCancelled = true;
        setVisible(false);
      }
    });
  }

  public MultiRoll(DieManager d, int dfltNDice, int dfltNSides) {
    this();
    dieManager = d;
    for (int i = 0; i < MAX_ROLLS; i++) {
      rolls[i] = new DieRoll("", dfltNDice, dfltNSides);
    }
    initConfig(dfltNDice, dfltNSides);
    clearDie();
  }

  private void clearDie() {
    for (int i = 0; i < MAX_ROLLS; i++) {
      useDie[i] = false;
    }
  }

  public boolean wasCancelled() {
    return rollCancelled;
  }

  public void setDescription(String s) {
    description = s;
    descText.setText(s);
  }

  public String getDescription() {
    return description;
  }

  public RollSet getRollSet() {
    final ArrayList<DieRoll> l = new ArrayList<>();
    for (int i = 0; i < MAX_ROLLS; ++i) {
      if (useDie[i]) {
        l.add(rolls[i]);
      }
    }
    final DieRoll[] rolls = l.toArray(new DieRoll[0]);
    return new RollSet(getDescription(), rolls);
  }

  /*
   * Reset the status display before making visible in case preferences
   * have been changed.
   */
  @Override
  public void setVisible(boolean b) {
    setServerHeader();
    setEmailHeader();
    super.setVisible(b);
  }

  // Multi-roll Configuration code
  private void initConfig(int nd, int ns) {

    setModal(true);

    setTitle("Multi Roller");
    setSize(380, 206);
    setBackground(Color.gray);

    // Create a panel to hold all other components
    topPanel = new JPanel();
    topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.PAGE_AXIS));

    // Build the Server/Email header
    serverPanel = new JPanel();
    serverLabel = new JLabel();
    setServerHeader();
    serverPanel.add(serverLabel);
    topPanel.add(serverPanel);

    emailPanel = new JPanel();
    emailLabel = new JLabel();
    setEmailHeader();
    emailPanel.add(emailLabel);
    topPanel.add(emailPanel);

    // And the body
    descPanel = new JPanel();
    final JLabel descLabel = new JLabel("Roll Description");
    descText = new JTextField(20);
    descText.setText(GameModule.getGameModule().getChatter().getInputField().getText());
    descText.addKeyListener(new java.awt.event.KeyAdapter() {
      @Override
      public void keyReleased(java.awt.event.KeyEvent e) {
        description = descText.getText();
      }
    });
    descPanel.add(descLabel);
    descPanel.add(descText);
    topPanel.add(descPanel);

    detailPanel = new JPanel();
    detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.PAGE_AXIS));
    detailPanel.setBorder(BorderFactory.createLineBorder(Color.black));

    detailPanel.add(new HeaderRow());

    rollRows = new RollRow[MAX_ROLLS];
    for (int i = 0; i < MAX_ROLLS; i++) {
      rollRows[i] = new RollRow(i, nd, ns);
      detailPanel.add(rollRows[i]);
    }

    topPanel.add(detailPanel);

    // Add Some buttons
    buttonPanel = new JPanel();
    rollButton.addActionListener(e -> {
      rollCancelled = false;
      int dieCount = 0;
      for (int i = 0; i < MAX_ROLLS; i++) {
        dieCount += useDie[i] ? 1 : 0;
      }
      if (dieCount == 0) {
        JOptionPane.showMessageDialog(me, "No dice selected for Roll.", "Roll Cancelled", JOptionPane.ERROR_MESSAGE);
        return;
      }
      setVisible(false);
    });

    canButton.addActionListener(e -> {
      rollCancelled = true;
      setVisible(false);
    });

    emailButton.addActionListener(e -> updateEmailAddress());

    buttonPanel.add(rollButton);
    buttonPanel.add(canButton);
    buttonPanel.add(emailButton);

    add(topPanel, BorderLayout.PAGE_START);
    add(buttonPanel, BorderLayout.PAGE_END);

    pack();
  }

  protected void setServerHeader() {
    serverLabel.setText("Server: " + dieManager.getServer().getName());
  }

  private static final String EMAIL_OFF = "Off";

  protected void setEmailHeader() {

    final String label;
    final Prefs prefs = GameModule.getGameModule().getPrefs();

    if (Boolean.TRUE.equals(prefs.getValue(DieManager.USE_EMAIL))) {
      label = (String) prefs.getValue(DieManager.SECONDARY_EMAIL);
    }
    else {
      label = EMAIL_OFF;
    }
    emailLabel.setText("Email: " + label);
  }

  protected void updateEmailAddress() {

    final Prefs prefs = GameModule.getGameModule().getPrefs();
    final String[] aBook = (String[]) prefs.getValue(DieManager.ADDRESS_BOOK);

    final JPopupMenu popup = new JPopupMenu();

    JMenuItem menuItem = new JMenuItem(EMAIL_OFF);
    menuItem.addActionListener(this);
    popup.add(menuItem);

    for (final String s : aBook) {
      menuItem = new JMenuItem(s);
      menuItem.addActionListener(this);
      popup.add(menuItem);
    }

    popup.show(emailButton, emailButton.getX(), emailButton.getY());
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    final String address = e.getActionCommand();
    final Prefs prefs = GameModule.getGameModule().getPrefs();
    if (address.equals(EMAIL_OFF)) {
      prefs.setValue(DieManager.USE_EMAIL, Boolean.FALSE);
    }
    else {
      prefs.setValue(DieManager.SECONDARY_EMAIL, address);
      prefs.setValue(DieManager.USE_EMAIL, Boolean.TRUE);
    }
    setEmailHeader();
  }

  protected static class HeaderRow extends JPanel {
    private static final long serialVersionUID = 1L;

    public HeaderRow() {

      final Border raisedbevel = BorderFactory.createRaisedBevelBorder();
      final Border myBorder = raisedbevel;

      //setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
      //setBorder(blackline);

      final JLabel col1 = new JLabel("Roll");
      col1.setPreferredSize(new Dimension(COL1_WIDTH, ROW_HEIGHT));
      col1.setHorizontalAlignment(JTextField.CENTER);
      col1.setBorder(myBorder);

//      JLabel col2 = new JLabel("Roll");
//      col2.setPreferredSize(new Dimension(COL2_WIDTH, ROW_HEIGHT));
//      col2.setHorizontalAlignment(JTextField.CENTER);
//            col2.setBorder(myBorder);

      final JLabel col3 = new JLabel("Details");
      col3.setPreferredSize(new Dimension(COL3_WIDTH, ROW_HEIGHT));
      col3.setBorder(myBorder);

      final JLabel col4 = new JLabel("nDice");
      col4.setBorder(myBorder);
      col4.setHorizontalAlignment(JTextField.CENTER);
      col4.setPreferredSize(new Dimension(COL4_WIDTH, ROW_HEIGHT));

      final JLabel col5 = new JLabel("nSides");
      col5.setBorder(myBorder);
      col5.setHorizontalAlignment(JTextField.CENTER);
      col5.setPreferredSize(new Dimension(COL5_WIDTH, ROW_HEIGHT));

      final JLabel col6 = new JLabel("add");
      col6.setBorder(myBorder);
      col6.setPreferredSize(new Dimension(COL6_WIDTH, ROW_HEIGHT));

      final JLabel col7 = new JLabel("Total");
      col7.setBorder(myBorder);
      col7.setPreferredSize(new Dimension(COL7_WIDTH, ROW_HEIGHT));

      add(col1);
      //add(col2);
      add(col3);
      add(col4);
      add(col5);
      add(col6);
      add(col7);
    }
  }

  class RollRow extends JPanel {
    private static final long serialVersionUID = 1L;

    int myRow;
    boolean selected;
    String description;
    int nDice, nSides, plus;
    boolean reportTotal;
    Dimension rowDim = new Dimension(40, ROW_HEIGHT);

    StateButton col1;
    JCheckBox col2, col7;
    JComboBox col4, col5;
    JTextField col3, col6;

    Border blackline = BorderFactory.createLineBorder(Color.black);
    Border raisedetched = BorderFactory.createEtchedBorder(EtchedBorder.RAISED);
    Border loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
    Border raisedbevel = BorderFactory.createRaisedBevelBorder();
    Border loweredbevel = BorderFactory.createLoweredBevelBorder();
    Border myBorder = raisedbevel;

    @Override
    public void setEnabled(boolean enabled) {
      col3.setEnabled(enabled);
      col4.setEnabled(enabled);
      col5.setEnabled(enabled);
      col6.setEnabled(enabled);
      col7.setEnabled(enabled);
    }

    public RollRow(int row, int nd, int ns) {

      myRow = row;

      //setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));

      col1 = new StateButton(Integer.toString(row + 1));
      col1.setPreferredSize(new Dimension(COL1_WIDTH, ROW_HEIGHT));
      col1.setState(useDie[myRow]);
      col1.addActionListener(e -> {
        col1.switchState();
        useDie[myRow] = col1.getState();
        setEnabled(col1.getState());
      });

      // Roll Description
      col3 = new JTextField(12);
      col3.setBorder(myBorder);
      col3.setPreferredSize(new Dimension(COL3_WIDTH, ROW_HEIGHT));
      col3.setText(rolls[myRow].getDescription());
      col3.addKeyListener(new java.awt.event.KeyAdapter() {
        @Override
        public void keyReleased(java.awt.event.KeyEvent e) {
          rolls[myRow].setDescription(col3.getText());
        }
      });
      col3.addMouseListener(new MouseAdapter() {
        @Override
        public void mousePressed(MouseEvent e) {
          if (!col3.hasFocus()) {
            col3.selectAll();
          }
        }
      });
      col3.setEnabled(false);

      // Number of Dice
      final int[] allowableDice = dieManager.getServer().getnDiceList();
      final String[] diceData = new String[allowableDice.length];
      int defaultNDIdx = 0;
      for (int i = 0; i < diceData.length; i++) {
        diceData[i] = Integer.toString(allowableDice[i]);
        if (nd == allowableDice[i])
          defaultNDIdx = i;
      }
      col4 = new JComboBox(diceData);
      col4.setSelectedIndex(defaultNDIdx);
      col4.setPreferredSize(new Dimension(COL4_WIDTH, ROW_HEIGHT));
      col4.addActionListener(e -> {
        final JComboBox cb = (JComboBox) e.getSource();
        rolls[myRow].setNumDice(Integer.parseInt((String) cb.getSelectedItem()));
      });
      col4.setEnabled(false);

      // Number of Sides
      final int[] allowableSides = dieManager.getServer().getnSideList();
      final String[] sideData = new String[allowableSides.length];
      int defaultNSIdx = 0;
      for (int i = 0; i < sideData.length; i++) {
        sideData[i] = Integer.toString(allowableSides[i]);
        if (ns == allowableSides[i])
          defaultNSIdx = i;
      }
      col5 = new JComboBox(sideData);
      col5.setSelectedIndex(defaultNSIdx);
      col5.setPreferredSize(new Dimension(COL5_WIDTH, ROW_HEIGHT));
      col5.addActionListener(e -> {
        final JComboBox cb = (JComboBox) e.getSource();
        rolls[myRow].setNumSides(Integer.parseInt((String) cb.getSelectedItem()));
      });
      col5.setEnabled(false);

      // Add to Total
      col6 = new JTextField(2);
      col6.setBorder(myBorder);
      col6.setPreferredSize(new Dimension(COL6_WIDTH, ROW_HEIGHT));
      col6.setText(Integer.toString(rolls[myRow].getPlus()));
      col6.addKeyListener(new java.awt.event.KeyAdapter() {
        @Override
        public void keyReleased(java.awt.event.KeyEvent e) {
          try {
            rolls[myRow].setPlus(Integer.parseInt(col3.getText()));
          }
          catch (NumberFormatException ev) {
            // TODO use IntConfigurer for col3
            rolls[myRow].setPlus(0);
          }
        }
      });
      col6.setEnabled(false);

      // Report Total Only
      col7 = new JCheckBox();
      col7.setBorder(myBorder);
      col7.setPreferredSize(new Dimension(COL7_WIDTH, ROW_HEIGHT));
      col7.setHorizontalAlignment(JCheckBox.CENTER);
      col7.setSelected(rolls[myRow].isReportTotal());
      col7.addItemListener(e -> rolls[myRow].setReportTotal((e.getStateChange() == ItemEvent.SELECTED)));
      col7.setEnabled(false);

      add(col1);
      add(col3);
      add(col4);
      add(col5);
      add(col6);
      add(col7);

    }
  }

  /*
   * An on/off button that changes state to show it's status
   */
  protected static class StateButton extends JButton {
    private static final long serialVersionUID = 1L;

    boolean state = false;

    StateButton(String s, boolean b) {
      super(s);
      setHorizontalAlignment(JButton.CENTER);
      setState(b);

    }

    StateButton(String s) {
      this(s, false);
    }

    public void setState(boolean b) {
      state = b;
      if (state) {
        setBorder(BorderFactory.createLoweredBevelBorder());
      }
      else {
        setBorder(BorderFactory.createRaisedBevelBorder());
      }
    }

    public boolean getState() {
      return state;
    }

    public void switchState() {
      setState(!state);
    }
  }


}
