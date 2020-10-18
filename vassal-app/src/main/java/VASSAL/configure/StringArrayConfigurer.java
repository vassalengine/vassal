/*
 *
 * Copyright (c) 2000-2011 by Rodney Kinney, Brent Easton
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
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.TitledBorder;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.ArrayUtils;

/**
 * A Configurer that returns an array of Strings
 */
public class StringArrayConfigurer extends Configurer {
  protected JPanel panel;
  protected JList<String> list;
  protected DefaultListModel<String> model;
  private static final String[] EMPTY = new String[0];
  protected JTextField textField;
  protected int minRows = 3;
  protected int maxRows = 3;

  public StringArrayConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  public StringArrayConfigurer(Object val) {
    this(null, "", val);
  }

  public StringArrayConfigurer(Object val, int minRows, int maxRows) {
    this(null, "", val);
    this.minRows = minRows;
    this.maxRows = maxRows;
  }

  public StringArrayConfigurer(String key, String name, int minRows, int maxRows) {
    this(key, name);
    this.minRows = minRows;
    this.maxRows = maxRows;
  }

  public StringArrayConfigurer(int minRows, int maxRows) {
    this(null, "", minRows, maxRows);
  }

  public StringArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public DefaultListModel<String> getModel() {
    return model;
  }

  public void addValue(String s) {
    setValue(value == null ?
      new String[]{s} : ArrayUtils.add((String[]) value, s));
  }

  public void removeValue(String s) {
    final String[] oldValue = getStringArray();
    final String[] newValue = ArrayUtils.removeElement(oldValue, s);
    if (oldValue != newValue) setValue(newValue);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel();
      panel.setBorder(new TitledBorder(name));
      panel.setLayout(new MigLayout("fill")); //NON-NLS

      JPanel buttonBox =  new JPanel(new MigLayout("ins 0", "push[][][]push")); // NON-NLS
      JPanel leftBox = new JPanel(new MigLayout("ins 0,gapy 2", "[fill,grow]")); // NON-NLS

      model = new DefaultListModel<>();
      updateModel();

      list = new JList<>(model);
      list.setBackground(Color.white);
      list.setPrototypeCellValue("MMMMMMMM"); //NON-NLS
      list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

      JButton addButton = new JButton(Resources.getString(Resources.ADD));
      addButton.addActionListener(e -> {
        String s = getTextValue();
        addValue(s);
        setTextValue("");
        updateViewable(list.getModel().getSize());
      });
      buttonBox.add(addButton);

      JButton removeButton = new JButton(Resources.getString(Resources.REMOVE));
      removeButton.addActionListener(e -> {
        list.getSelectedValuesList().forEach(this::removeValue);
        updateViewable(list.getModel().getSize());
      });
      buttonBox.add(removeButton);

      JButton insertButton = new JButton(Resources.getString(Resources.INSERT));
      ActionListener insertAction = e -> {
        if (value == null) {
          addValue(getTextValue());
        }
        else {
          int pos = list.getSelectedIndex();
          if (pos < 0) pos = list.getModel().getSize();
          setValue(ArrayUtils.insert(pos, (String[]) value, getTextValue()));
          setTextValue("");
          list.setSelectedIndex(pos + 1);
        }
        updateViewable(list.getModel().getSize() + 1);
      };
      insertButton.addActionListener(insertAction);
      buttonBox.add(insertButton);

      final Component textComponent = getTextComponent();
      addTextActionListener(insertAction);

      leftBox.add(textComponent, "growx,wrap"); // NON-NLS
      leftBox.add(buttonBox, "growx"); // NON-NLS

      JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
      pane.setBackground(Color.white);
      pane.setLeftComponent(leftBox);
      pane.setRightComponent(new ScrollPane(list));

      panel.add(pane, "grow"); //NON-NLS
      updateViewable(list.getModel().getSize());
    }
    return panel;
  }

  public void updateViewable(int rows) {
    list.setVisibleRowCount(Math.max(minRows, Math.min(rows, maxRows)));
    panel.invalidate();
  }

  protected Component getTextComponent() {
    if (textField == null) {
      textField = new JTextField(8);
      textField.setMaximumSize(new Dimension(Integer.MAX_VALUE, textField.getPreferredSize().height));
    }
    return textField;
  }

  protected String getTextValue() {
    return textField.getText();
  }

  protected void setTextValue(String s) {
    textField.setText(s);
  }

  protected void addTextActionListener(ActionListener a) {
    textField.addActionListener(a);
  }

  public String[] getStringArray() {
    if (value instanceof String[]) {
      return (String[]) value;
    }
    else {
      return EMPTY;
    }
  }

  @Override
  public String getValueString() {
    return arrayToString(getStringArray());
  }

  public static String arrayToString(String[] s) {
    if (s == null || s.length == 0) {
      return "";
    }
    SequenceEncoder se = new SequenceEncoder(',');
    for (String item : s) {
      se.append(item != null ? item : "");
    }
    return se.getValue();
  }

  @Override
  public void setValue(Object o) {
    if (o == null) {
      o = EMPTY;
    }
    super.setValue(o);
    updateModel();
  }

  @Override
  public void setValue(String s) {
    String[] val = stringToArray(s);
    setValue(val);
  }

  public static String[] stringToArray(String s) {
    if (s == null
        || s.length() == 0) {
      return EMPTY;
    }
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ',');
    List<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    return l.toArray(new String[0]);
  }

  protected void updateModel() {
    if (model != null) {
      model.removeAllElements();
      String[] s = getStringArray();
      for (String item : s) {
        model.addElement(item);
      }
    }
  }

  // TODO move test code to a manual unit test annotated with @Ignore
  public static void main(String[] args) {
    JFrame f = new JFrame();
    final StringArrayConfigurer c = new StringArrayConfigurer(null, "Visible to these players:  "); //NON-NLS
    c.addPropertyChangeListener(evt -> System.err.println(c.getName() + " = " + c.getValueString()));
    c.setValue("Rack,Shack,Benny"); //NON-NLS
    f.add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}
