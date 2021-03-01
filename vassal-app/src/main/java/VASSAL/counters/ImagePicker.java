/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.i18n.Resources;
import VASSAL.build.GameModule;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ImageFileFilter;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;
import VASSAL.tools.swing.SwingUtils;

public class ImagePicker extends JPanel
                         implements MouseListener, ItemListener {
  private static final long serialVersionUID = 1L;
  private static final String NO_IMAGE = "(" + Resources.getString("Editor.ImagePicker.no_image") + ")";
  private String imageName = null;
  protected static final Font FONT = new Font(Font.DIALOG, Font.PLAIN, 11);
  private final JTextArea noImage;
  private final JComboBox<String> select;
  private final OpIcon icon;
  private final JPanel imageViewer;
  private final JScrollPane imageScroller;

  public ImagePicker() {
    noImage = new JTextArea(1, 10);
    noImage.setFont(FONT);
    noImage.setText(Resources.getString("Editor.ImagePicker.double_click_here"));
    noImage.addMouseListener(this);
    noImage.setEditable(false);
    noImage.setLineWrap(true);
    noImage.setWrapStyleWord(true);
    noImage.setMinimumSize(new Dimension(15, 32));
    icon = new OpIcon();
    final JLabel imageView = new JLabel(icon);
    imageView.addMouseListener(this);

    imageViewer = new JPanel(new BorderLayout());
    imageScroller = new ScrollPane(
      imageView,
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    imageViewer.add(imageScroller, BorderLayout.CENTER);

    select = new JComboBox<>(ArrayUtils.addFirst(GameModule.getGameModule().getDataArchive().getImageNames(), NO_IMAGE));
    select.addItemListener(this);
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    add(noImage);
    add(select);
  }

  public String getImageName() {
    return imageName;
  }

  protected void setViewSize() {
  }

  public void setImageName(String name) {
    remove(0);

    if (name == null || name.isEmpty()) {
      imageName = null;
      add(noImage, 0);
    }
    else {
      if (name.equals(imageName)) {
        // We have to do this because we have no way of calling update on
        // any ImageOps which depend on this image.
        Op.clearCache();
      }
      else {
        imageName = name;
      }

      icon.setOp(Op.load(imageName));
      final Dimension d = new Dimension(icon.getIconWidth(), icon.getIconHeight());

      if (d.width > 200) d.width = 200;

      if (d.height > 200) {
        d.height = 200;
      }
      else {
        d.height += 4;
      }

      imageScroller.setPreferredSize(d);
      imageScroller.setMinimumSize(d);

      add(imageViewer, 0);
    }

    select.removeItemListener(this);
    select.setSelectedItem(name);
    if (name != null && !name.equals(select.getSelectedItem())) {
      select.setSelectedItem(name + ".gif"); // NON-NLS
    }
    select.addItemListener(this);
    revalidate();
    SwingUtils.repack(this);
    repaint();
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mouseClicked(MouseEvent e) {
    if (e.getClickCount() > 1 && SwingUtils.isMainMouseButtonDown(e)) {
      pickImage();
    }
  }

  @Override
  public void mousePressed(MouseEvent e) {
  }

  @Override
  public void mouseReleased(MouseEvent e) {
  }

  @Override
  public void itemStateChanged(ItemEvent e) {
    setImageName((String) select.getSelectedItem());
  }

  public void pickImage() {
    final GameModule gm = GameModule.getGameModule();
    final FileChooser fc = gm.getFileChooser();
    fc.setFileFilter(new ImageFileFilter());

    if (fc.showOpenDialog(this) == FileChooser.APPROVE_OPTION
         && fc.getSelectedFile().exists()) {
      final String name = fc.getSelectedFile().getName();
      gm.getArchiveWriter().addImage(fc.getSelectedFile().getPath(), name);
      select.setModel(new DefaultComboBoxModel<>(
        ArrayUtils.addFirst(gm.getDataArchive().getImageNames(), NO_IMAGE)
      ));
      setImageName(name);
    }
    else {
      setImageName(null);
    }
    repaint();
  }
}
