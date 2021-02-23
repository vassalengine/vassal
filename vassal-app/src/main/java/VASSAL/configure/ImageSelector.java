/*
 * Copyright (c) 2020 by The VASSAL development team
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

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ImageFileFilter;
import VASSAL.tools.image.LabelUtils;
import VASSAL.tools.image.MultiResolutionRenderedImage;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.ArrayUtils;

/**
 * Allows a user to select from the images currently available in a module, or
 * to open a File Dialog to import a new image.
 *
 * Designed to be a drop-in replacement for {@link VASSAL.counters.ImagePicker},
 * except implemented as a proper Configurer.
 *
 * The value stored is the name of the image file (no pathname).
 */
public class ImageSelector extends Configurer implements ItemListener {

  private static final String NO_IMAGE = "(" + Resources.getString("Editor.ImagePicker.no_image") + ")";

  private static final int DEFAULT_SIZE = 64;

  private JPanel controls;
  private JComboBox<String> select;
  private Icon noImage;
  private JLabel imageViewer;
  private String imageName;
  private final JLabel imageScale = new JLabel();
  private final OpIcon icon = new OpIcon();
  private final int maxWidth;
  private final int maxHeight;

  public ImageSelector(String key, String name, String val, int maxWidth, int maxheight) {
    super(key, name, val);
    this.maxWidth = maxWidth;
    this.maxHeight = maxheight;
    initControls();
    setValue(val);
  }

  public ImageSelector(String key, String name, String val) {
    this(key, name, val, -1, -1);
  }

  public ImageSelector(String key, String name) {
    this(key, name, null);
  }

  public ImageSelector(String val, int maxWidth, int maxheight) {
    this(null, "", val, maxWidth, maxheight);
  }

  public ImageSelector(String val) {
    this(null, "", val);
  }

  public ImageSelector() {
    this (null, "", "", DEFAULT_SIZE, DEFAULT_SIZE);
  }

  @Override
  public String getValueString() {
    return (String) value;
  }

  @Override
  public void setValue(String s) {
    if (s == null || s.isBlank()) {
      imageName = null;
      imageViewer.setIcon(getNoImageIcon());
      imageViewer.setPreferredSize(new Dimension(DEFAULT_SIZE, DEFAULT_SIZE));
    }
    else {
      if (s.equals(imageName)) {
        // We have to do this because we have no way of calling update on
        // any ImageOps which depend on this image.
        Op.clearCache();
      }
      else {
        imageName = s;
      }

      icon.setOp(Op.load(s));

      // Is the image too large?
      if (maxWidth > 0 && (icon.getIconWidth() > maxWidth || icon.getIconHeight() > maxHeight)) {
        final double xRatio = (double) maxWidth / icon.getIconWidth();
        final double yRatio = (double) maxHeight / icon.getIconHeight();
        final double newScale = Math.min(xRatio, yRatio);
        icon.setOp(Op.scale(Op.load(s), newScale));
        imageScale.setText("(" + (int) (newScale * 100) + "%)");
        imageScale.setVisible(true);
      }
      else {
        imageScale.setVisible(false);
      }

      imageViewer.setIcon(icon);
      imageViewer.setPreferredSize(new Dimension(icon.getIconWidth(), icon.getIconHeight()));
    }

    select.removeItemListener(this);
    select.setSelectedItem(s);
    if (s == null) {
      select.setSelectedIndex(0);
    }
    else if (!s.equals(select.getSelectedItem())) {
      select.setSelectedItem(s + ".gif"); // NON-NLS
    }
    select.addItemListener(this);

    controls.revalidate();
    repack(controls);

    setValue((Object) s);
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      initControls();
    }
    return controls;
  }

  private void initControls() {
    controls = new JPanel(new MigLayout("hidemode 3", "[grow][]", "[]rel[]")); // NON-NLS
    controls.setBorder(BorderFactory.createEtchedBorder());

    final JButton addButton = new JButton(Resources.getString("Editor.imageSelector.add_image"));
    addButton.addActionListener(e -> pickImage());
    final JButton clearButton = new JButton(Resources.getString("Editor.imageSelector.clear_image"));
    clearButton.addActionListener(e -> clearImage());

    select = new JComboBox<>(ArrayUtils.addFirst(GameModule.getGameModule().getDataArchive().getImageNames(), NO_IMAGE));
    select.setSelectedIndex(0);
    select.addItemListener(this);

    imageViewer = new JLabel(getNoImageIcon());
    imageViewer.setPreferredSize(new Dimension(DEFAULT_SIZE, DEFAULT_SIZE));

    controls.add(select, "grow"); // NON-NLS
    controls.add(addButton, "split,sg 1"); // NON-NLS
    controls.add(clearButton, "sg 1,wrap"); // NON-NLS
    controls.add(imageViewer, "span 2,alignx center,wrap"); // NON-NLS
    controls.add(imageScale, "span 2,alignx center"); // NON-NLS
  }

  @Override
  public void itemStateChanged(ItemEvent e) {
    setValue((String) select.getSelectedItem());
  }

  private void pickImage() {
    final GameModule gm = GameModule.getGameModule();
    final FileChooser fc = gm.getFileChooser();
    fc.setFileFilter(new ImageFileFilter());

    if (fc.showOpenDialog(gm.getPlayerWindow()) == FileChooser.APPROVE_OPTION && fc.getSelectedFile().exists()) {
      final String name = fc.getSelectedFile().getName();
      gm.getArchiveWriter().addImage(fc.getSelectedFile().getPath(), name);
      select.setModel(new DefaultComboBoxModel<>(ArrayUtils.addFirst(gm.getDataArchive().getImageNames(), NO_IMAGE)));
      setValue(name);
    }
    else {
      setValue(null);
    }
  }

  private void clearImage() {
    setValue(null);
  }

  private Icon getNoImageIcon() {
    if (noImage == null) {
      noImage = new ImageIcon(new MultiResolutionRenderedImage(
        DEFAULT_SIZE,
        DEFAULT_SIZE,
        LabelUtils::noImageBoxImage
      ));
    }
    return noImage;
  }

  public String getImageName() {
    return imageName;
  }
}
