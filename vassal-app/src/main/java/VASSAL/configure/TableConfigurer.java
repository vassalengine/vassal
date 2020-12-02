/*
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

import VASSAL.i18n.Resources;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;


/**
 * @author Brent Easton
 */
public class TableConfigurer extends Configurer implements ActionListener  {

  public static final String ADD_ACTION = Resources.getString("Editor.TableConfigurer.add");
  public static final String DEL_ACTION = Resources.getString("Editor.TableConfigurer.remove");
  public static final String INS_ACTION = Resources.getString("Editor.TableConfigurer.insert");

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

  @Override
  public String getValueString() {
    return null;
  }

  @Override
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

  @Override
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

    final Box buttonBox = Box.createHorizontalBox();
    final JButton addButton = new JButton(ADD_ACTION);
    addButton.addActionListener(this);
    buttonBox.add(addButton);
    final JButton delButton = new JButton(DEL_ACTION);
    delButton.addActionListener(this);
    buttonBox.add(delButton);
    final JButton insButton = new JButton(INS_ACTION);
    insButton.addActionListener(this);
    buttonBox.add(insButton);
    controls.add(buttonBox);
    repack();
  }

  @Override
  protected void repack() {
    final Window w = SwingUtilities.getWindowAncestor(controls);
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

    for (final Column item : columns) {
      contents.add(new JLabel(item.getName()));
    }

    if (columns[0].getRowCount() > 0) {
      for (int row = 0; row < columns[0].getRowCount(); row++) {
        for (final Column column : columns) {
          contents.add(column.getControls(row));
        }
      }
    }

    controls.add(contents);
    repack();
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    final String action = e.getActionCommand();

    if (action.equals(ADD_ACTION)) {
      Arrays
        .stream(columns)
        .forEach(Column::addRow);
      updateContents();
    }
    // other possible actions to check:
    // DEL_ACTION
    // INS_ACTION
  }

  protected static class Column {
    protected Class<?> type;
    protected String name;
    protected List<Configurer> configurers = new ArrayList<>();
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
      final Configurer c = AutoConfigurer.createConfigurer(type, null, "", null);
      configurers.add(c);
    }

    public int getRowCount() {
      return configurers.size();
    }
  }
}
