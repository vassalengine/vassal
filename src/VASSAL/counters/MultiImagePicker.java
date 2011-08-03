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

import java.awt.CardLayout;
import java.awt.Component;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionListener;

import VASSAL.tools.ScrollPane;

public class MultiImagePicker extends JPanel {
  private static final long serialVersionUID = 1L;

  protected JList imageList;
  protected DefaultListModel imageListElements = new DefaultListModel();
  protected CardLayout cl = new CardLayout();
  protected JPanel multiPanel = new JPanel();

  public MultiImagePicker() {
    setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

    imageList = new JList(imageListElements);
    imageList.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(javax.swing.event.ListSelectionEvent e) {
        showSelected();
      }
    });
    imageList.addKeyListener(new KeyAdapter() {
      public void keyReleased(KeyEvent evt) {
        if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
          showSelected();
        }
      }
    });
    imageList.setVisibleRowCount(4);
    imageList.setPrototypeCellValue("Image 999");
    imageList.setMinimumSize(imageList.getPreferredSize());

    multiPanel.setLayout(cl);

    add(multiPanel);
    JScrollPane scroll = new ScrollPane(imageList);
    scroll.getViewport().setMinimumSize(imageList.getPreferredSize());
    add(scroll);

    addEntry();
  }

  public void addListSelectionListener(ListSelectionListener l) {
    imageList.addListSelectionListener(l);
  }

  public void showSelected() {
    if (imageList.getSelectedValue() != null) {
      cl.show(multiPanel, (String) imageList.getSelectedValue());
    }
  }

  public void addEntry() {
    String entry = "Image " + (imageListElements.size() + 1);
    imageListElements.addElement(entry);
    ImagePicker pick = new ImagePicker();
    multiPanel.add(entry, pick);
    imageList.setSelectedIndex(imageListElements.size() - 1);
    cl.show(multiPanel, (String) imageList.getSelectedValue());
  }

  public JList getList() {
    return imageList;
  }

  /**
   * Returns a list of image names in this picker.
   *
   * @return the list of image names
   */
  public List<String> getImageNameList() {
    final int size = imageListElements.size();
    final ArrayList<String> names = new ArrayList<String>(size);
    for (int i = 0; i < size; ++i) {
      names.add((((ImagePicker) multiPanel.getComponent(i)).getImageName()));
    }
    return names;
  }

  /** Use {@link #getImageNameList()} instead. */
  @Deprecated
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
      cl.show(multiPanel,
              (String) imageList.getSelectedValue());
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
    Component[] components = new Component[imageListElements.size()];
    for (int i = 0; i < imageListElements.size(); i++) {
      components[i] = multiPanel.getComponent(i);
    }
    multiPanel.removeAll();
    cl = new CardLayout();
    multiPanel.setLayout(cl);

    for (int i = 0; i < imageListElements.size(); i++) {
      Component c = null;
      if (i == index1) {
        c = components[index2];
      }
      else if (i == index2) {
        c = components[index1];
      }
      else {
        c = components[i];
      }
      multiPanel.add(c, "Image " + (i+1));
    }

    imageList.setSelectedIndex(index2);
    showSelected();
  }

  public void setImageList(String names[]) {
    while (names.length > multiPanel.getComponentCount()) {
      addEntry();
    }
    for (int i = 0; i < names.length; ++i) {
      ((ImagePicker) multiPanel.getComponent(i)).
        setImageName(names[i]);
    }
  }
}
