/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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

package VASSAL.tools.swing;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JSeparator;
import javax.swing.ListCellRenderer;
import javax.swing.border.EmptyBorder;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * A JComboBox that allows separators and headings to be included between different sections of elements
 */
public class StructuredComboBox extends JComboBox {

  private static final long serialVersionUID = 1L;

  final String separator;

  public StructuredComboBox(String[] entries) {
    this(entries, "-");
  }

  public StructuredComboBox(String[] entries, String seperator) {
    super(entries);
    this.separator = seperator;
    for (int i = 0; i < entries.length; i++) {
      if (!entries[i].startsWith(seperator)) {
        setSelectedIndex(i);
        break;
      }
    }
    setRenderer(new ComboBoxRenderer(this, seperator));
    addActionListener(new BlockComboListener(this, seperator));
  }

  @Override
  public void setSelectedIndex(int anIndex) {
    super.setSelectedIndex(anIndex);
  }

  private static class ComboBoxRenderer extends JLabel implements ListCellRenderer {

    private static final long serialVersionUID = 1L;

    final JSeparator separator;
    final String separatorMarker;
    Font headingFont;
    final JComboBox comboBox;

    public ComboBoxRenderer(JComboBox comboBox, String separatorMarker) {
      setOpaque(true);
      setBorder(new EmptyBorder(1, 1, 1, 1));
      separator = new JSeparator(JSeparator.HORIZONTAL);
      this.separatorMarker = separatorMarker;
      this.comboBox = comboBox;
    }

    @Override
    public Component getListCellRendererComponent(JList list, Object value,
                                                  int index, boolean isSelected, boolean cellHasFocus) {
      final String str = (value == null) ? "" : value.toString();
      if (separatorMarker.equals(str)) {
        return separator;
      }
      else if (str.startsWith(separatorMarker)) {
        setFont(list.getFont().deriveFont(Font.BOLD + Font.ITALIC, list.getFont().getSize()));
        setText(str.substring(separatorMarker.length()));
        setBackground(list.getBackground());
        setForeground(list.getForeground());
        return this;
      }

      if (isSelected) {
        setBackground(list.getSelectionBackground());
        setForeground(list.getSelectionForeground());
      }
      else {
        setBackground(list.getBackground());
        setForeground(list.getForeground());
      }
      setFont(list.getFont());
      setText((comboBox.isPopupVisible() ? " " : "") + str);
      return this;
    }

  }

  private class BlockComboListener implements ActionListener {
    JComboBox combo;
    Object currentItem;
    String separator;

    BlockComboListener(JComboBox combo, String separator) {
      this.combo = combo;
      currentItem = combo.getSelectedItem();
      this.separator = separator;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      final String tempItem = (String) combo.getSelectedItem();
      if (tempItem.startsWith(separator)) {
        combo.setSelectedItem(currentItem);
      }
      else {
        currentItem = tempItem;
      }
    }
  }
}
