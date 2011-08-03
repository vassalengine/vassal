/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.configure;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;


/**
 * @author Brent Easton
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TableConfigurer extends Configurer implements ActionListener  {

  public static final String ADD_ACTION = "Add";
  public static final String DEL_ACTION = "Remove";
  public static final String INS_ACTION = "Insert";

  protected Box controls;
  protected JPanel contents = null;

  protected Column[] columns = new Column[0];

  protected char rowDelimiter = ',';
  protected char colDelimiter = '|';

  public TableConfigurer(String key, String name, String[] headings, Class<?>[] types) {
    this(key, name);
    columns = new Column[headings.length];
    for (int i = 0; i < headings.length; i++) {
      columns[i] = new Column(headings[i], types[i]);
    }
  }

  public TableConfigurer(String key, String name) {
    super(key, name);
  }

  public String getValueString() {
    return null;
  }

  public void setValue(String s) {

  }

  public void setRowDelimiter(char c) {
    rowDelimiter = c;
  }

  public void setColDelimiter(char c) {
    colDelimiter = c;
  }

  public int getColumnCount() {
    return columns.length;
  }

  public Component getControls() {
    if (controls == null) {
      buildControls();
    }
    return controls;
  }

  protected void buildControls() {
    controls = Box.createVerticalBox();
    controls.setBorder(BorderFactory.createEtchedBorder());
    controls.add(new JLabel(getName()));

    updateContents();

    Box buttonBox = Box.createHorizontalBox();
    JButton addButton = new JButton(ADD_ACTION);
    addButton.addActionListener(this);
    buttonBox.add(addButton);
    JButton delButton = new JButton(DEL_ACTION);
    delButton.addActionListener(this);
    buttonBox.add(delButton);
    JButton insButton = new JButton(INS_ACTION);
    insButton.addActionListener(this);
    buttonBox.add(insButton);
    controls.add(buttonBox);
    repack();
  }

  protected void repack() {
    Window w = SwingUtilities.getWindowAncestor(controls);
    if (w != null) {
      w.pack();
    }
  }

  protected void updateContents() {
    if (contents != null) {
      controls.remove(contents);
      contents = null;
    }
    contents = new JPanel();
    contents.setBorder(BorderFactory.createEtchedBorder());
    contents.setLayout(new GridLayout(0, 5));

    for (int i = 0; i < columns.length; i++) {
      contents.add(new JLabel(columns[i].getName()));
    }

    if (columns[0].getRowCount() > 0) {
      for (int row = 0; row < columns[0].getRowCount(); row++) {
        for (int col = 0; col < columns.length; col++) {
          contents.add(columns[col].getControls(row));
        }
      }
    }

    controls.add(contents);
    repack();
  }

  public void actionPerformed(ActionEvent e) {
    String action = e.getActionCommand();

    if (action.equals(ADD_ACTION)) {
      for (int i = 0; i < getColumnCount(); i++) {
        columns[i].addRow();
      }
      updateContents();
    }
    else if (action.equals(DEL_ACTION)) {

    }
    else if (action.equals(INS_ACTION)) {

    }
  }

  protected static class Column {
    protected Class<?> type;
    protected String name;
    protected List<Configurer> configurers = new ArrayList<Configurer>();
    protected Box controls;
    protected JPanel contents;

    public Column(String name, Class<?> type) {
      this.name = name;
      this.type = type;
    }

    public Component getControls(int row) {
      if (row >= 0 && row < getRowCount()) {
        return configurers.get(row).getControls();
      }
      return null;
    }

    public String getName() {
      return name;
    }

    public void addRow() {
      Configurer c = AutoConfigurer.createConfigurer(type, null, "", null);
      configurers.add(c);
    }

    public int getRowCount() {
      return configurers.size();
    }
  }
}
