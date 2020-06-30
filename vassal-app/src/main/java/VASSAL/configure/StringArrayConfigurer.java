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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
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

import VASSAL.i18n.Resources;
import VASSAL.tools.ArrayUtils;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

/**
 * A Configurer that returns an array of Strings
 */
public class StringArrayConfigurer extends Configurer {
  protected JPanel panel;
  protected JList<String> list;
  protected DefaultListModel<String> model;
  private static final String[] EMPTY = new String[0];
  protected JTextField textField;

  public StringArrayConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  public StringArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public DefaultListModel<String> getModel() {
    return model;
  }

  public void addValue(String s) {
    setValue(value == null ?
      new String[]{s} : ArrayUtils.append((String[]) value, s));
  }

  public void removeValue(String s) {
    final String[] oldValue = getStringArray();
    final String[] newValue = ArrayUtils.remove(oldValue, s);
    if (oldValue != newValue) setValue(newValue);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel();
      panel.setBorder(new TitledBorder(name));
      panel.setLayout(new MigLayout("fill"));

      Box buttonBox = Box.createHorizontalBox();
      Box leftBox = Box.createVerticalBox();

      model = new DefaultListModel<>();
      updateModel();

      list = new JList<>(model);
      list.setPrototypeCellValue("MMMMMMMM");
      list.setVisibleRowCount(2);
      list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

      JButton addButton = new JButton(Resources.getString(Resources.ADD));
      addButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          String s = getTextValue();
          addValue(s);
          setTextValue("");
        }
      });
      buttonBox.add(addButton);

      JButton removeButton = new JButton(Resources.getString(Resources.REMOVE));
      removeButton.addActionListener(e -> list.getSelectedValuesList().forEach(this::removeValue));
      buttonBox.add(removeButton);

      JButton insertButton = new JButton(Resources.getString(Resources.INSERT));
      ActionListener insertAction = new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          if (value == null) {
            addValue(getTextValue());
          }
          else {
            int pos = list.getSelectedIndex();
            if (pos < 0) pos = list.getModel().getSize();
            setValue(ArrayUtils.insert((String[]) value, pos, getTextValue()));
            setTextValue("");
            list.setSelectedIndex(pos+1);
          }
        }
      };
      insertButton.addActionListener(insertAction);
      buttonBox.add(insertButton);

      final Component textComponent = getTextComponent();
      addTextActionListener(insertAction);

      leftBox.add(textComponent);
      leftBox.add(buttonBox);

      JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
      pane.setLeftComponent(leftBox);
      pane.setRightComponent(new ScrollPane(list));

      panel.add(pane, "grow");
    }
    return panel;
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
    final StringArrayConfigurer c = new StringArrayConfigurer(null, "Visible to these players:  ");
    c.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(c.getName() + " = " + c.getValueString());
      }
    });
    c.setValue("Rack,Shack,Benny");
    f.add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}
