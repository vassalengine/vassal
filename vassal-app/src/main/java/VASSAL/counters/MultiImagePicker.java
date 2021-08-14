/*
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

import java.awt.CardLayout;
import java.awt.Component;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.stream.IntStream;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import javax.swing.event.ListSelectionListener;

import VASSAL.i18n.Resources;
import VASSAL.tools.ScrollPane;

import net.miginfocom.swing.MigLayout;

public class MultiImagePicker extends JPanel {
  private static final long serialVersionUID = 1L;

  protected JList<String> imageList;
  protected DefaultListModel<String> imageListElements = new DefaultListModel<>();
  protected CardLayout cl = new CardLayout();
  protected JPanel multiPanel = new JPanel();

  public MultiImagePicker() {
    setLayout(new MigLayout("ins 0", "[grow][]", "[grow]")); // NON-NLS

    imageList = new JList<>(imageListElements);
    imageList.addListSelectionListener(e -> showSelected());
    imageList.addKeyListener(new KeyAdapter() {
      @Override
      public void keyReleased(KeyEvent evt) {
        if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
          showSelected();
        }
      }
    });
    //imageList.setVisibleRowCount(4);
    imageList.setPrototypeCellValue("Image 999"); // NON-NLS
    imageList.setMinimumSize(imageList.getPreferredSize());

    multiPanel.setLayout(cl);

    add(multiPanel, "grow"); // NON-NLS
    final JScrollPane scroll = new ScrollPane(imageList);
    scroll.getViewport().setMinimumSize(imageList.getPreferredSize());
    add(scroll);

    addEntry();
  }

  public void addListSelectionListener(ListSelectionListener l) {
    imageList.addListSelectionListener(l);
  }

  public void showSelected() {
    if (imageList.getSelectedValue() != null) {
      cl.show(multiPanel, imageList.getSelectedValue());
    }
  }

  public void addEntry() {
    final String entry = Resources.getString("Editor.MultiImagePicker.image", (imageListElements.size() + 1));
    imageListElements.addElement(entry);
    final ImagePicker pick = new ImagePicker();
    multiPanel.add(entry, pick);
    imageList.setSelectedIndex(imageListElements.size() - 1);
    cl.show(multiPanel, imageList.getSelectedValue());
  }

  public JList<String> getList() {
    return imageList;
  }

  /**
   * Returns a list of image names in this picker.
   *
   * @return the list of image names
   */
  public List<String> getImageNameList() {
    final int size = imageListElements.size();
    final ArrayList<String> names = new ArrayList<>(size);
    for (int i = 0; i < size; ++i) {
      names.add((((ImagePicker) multiPanel.getComponent(i)).getImageName()));
    }
    return names;
  }

  /** Use {@link #getImageNameList()} instead. */
  @Deprecated(since = "2021-08-06", forRemoval = true)
  public Enumeration<String> getImageNames() {
    return Collections.enumeration(getImageNameList());
  }

  public void removeEntryAt(int index) {
    if (index < 0 || index >= imageListElements.size()) {
      return;
    }

    multiPanel.remove(index);
    imageListElements.removeElementAt(index);
    if (index < imageListElements.size()) {
      imageList.setSelectedIndex(index);
    }
    else if (index > 0) {
      imageList.setSelectedIndex(index - 1);
    }
    if (imageList.getSelectedValue() != null) {
      cl.show(multiPanel, imageList.getSelectedValue());
    }
  }

  public void clear() {
    for (int i = 0; i < imageListElements.size(); ++i) {
      ((ImagePicker) multiPanel.getComponent(i)).setImageName(null);
    }
    multiPanel.removeAll();
    imageListElements.removeAllElements();
  }

  public void swap(int index1, int index2) {
    final Component[] components = new Component[imageListElements.size()];
    for (int i = 0; i < imageListElements.size(); i++) {
      components[i] = multiPanel.getComponent(i);
    }
    multiPanel.removeAll();
    cl = new CardLayout();
    multiPanel.setLayout(cl);

    for (int i = 0; i < imageListElements.size(); i++) {
      final Component c;
      if (i == index1) {
        c = components[index2];
      }
      else if (i == index2) {
        c = components[index1];
      }
      else {
        c = components[i];
      }
      multiPanel.add(c, Resources.getString("Editor.MultiImagePicker.image", (i + 1)));
    }

    imageList.setSelectedIndex(index2);
    showSelected();
  }

  public void setImageList(String[] names) {
    while (names.length > multiPanel.getComponentCount()) {
      addEntry();
    }
    IntStream
      .range(0, names.length)
      .forEach(i -> ((ImagePicker) multiPanel.getComponent(i)).setImageName(names[i]));
  }
}
