/*
 *
 * Copyright (c) 2003 by Rodney Kinney
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
import java.io.File;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ImageFileFilter;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OwningOpMultiResolutionImage;

import net.miginfocom.swing.MigLayout;

public class IconConfigurer extends Configurer {
  private JPanel controls;
  private String imageName;
  private final String defaultImage;
  private Icon icon;
  private JPanel holdingPanel;
  private JLabel iconLabel;

  public IconConfigurer(String defaultImage) {
    this(null, "", defaultImage);
  }

  public IconConfigurer(String key, String name, String defaultImage) {
    super(key, name);
    this.defaultImage = defaultImage;
  }

  @Override
  public String getValueString() {
    return imageName;
  }


  @Override
  public void setValue(Object o) {
    super.setValue(o);
    setSize();
  }

  @Override
  public void setValue(String s) {
    icon = null;
    imageName = s == null ? "" : s;

    if (imageName.length() > 0) {
      final ImageOp sop = Op.load(imageName);
      if (sop.getImage() != null) {
        icon = new ImageIcon(new OwningOpMultiResolutionImage(sop));
      }
    }
    if (iconLabel != null) {
      iconLabel.setIcon(icon);
    }
    setValue((Object) imageName);
  }

  public Icon getIconValue() {
    return icon;
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new ConfigurerPanel(getName(), "[]rel[]rel[]", "[]rel[]rel[]rel[]", "[fill,grow]"); // NON-NLS
      if (iconLabel == null) {
        iconLabel = new JLabel();
      }
      iconLabel.setIcon(icon);

      holdingPanel = new JPanel(new MigLayout("ins 0", "[grow,fill]", "[grow,fill]")); // NON-NLS
      holdingPanel.add(iconLabel, "grow"); // NON-NLS
      controls.add(holdingPanel); // NON-NLS

      final JButton reset = new JButton(Resources.getString("Editor.select"));
      reset.addActionListener(e -> selectImage());
      controls.add(reset, "aligny center, growy 0"); // NON-NLS
      if (defaultImage != null) {
        final JButton useDefault = new JButton(Resources.getString("Editor.default"));
        useDefault.addActionListener(e -> setValue(defaultImage));
        controls.add(useDefault, "aligny center, growy 0"); // NON-NLS
      }
    }
    return controls;
  }

  private static final int MAX_ICON_DISPLAY_SIZE = 128;

  private void setSize() {
    if (holdingPanel != null) {
      if (icon == null) {
        holdingPanel.setPreferredSize(new Dimension(32, 32));
        holdingPanel.setMinimumSize(new Dimension(32, 32));
      }
      else {
        holdingPanel.setPreferredSize(new Dimension(icon.getIconWidth(), icon.getIconHeight()));
        holdingPanel.setMinimumSize(new Dimension(Math.min(icon.getIconWidth(), MAX_ICON_DISPLAY_SIZE), Math.min(icon.getIconHeight(), MAX_ICON_DISPLAY_SIZE)));
        holdingPanel.setMaximumSize(new Dimension(MAX_ICON_DISPLAY_SIZE, MAX_ICON_DISPLAY_SIZE));
      }
      repack();
      holdingPanel.repaint();
    }
  }

  private void selectImage() {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.setFileFilter(new ImageFileFilter());

    if (fc.showOpenDialog(getControls()) != FileChooser.APPROVE_OPTION) {
      setValue(null);
    }
    else {
      final File f = fc.getSelectedFile();
      if (f != null && f.exists()) {
        GameModule.getGameModule().getArchiveWriter()
                                  .addImage(f.getPath(), f.getName());
        setValue(f.getName());
      }
    }
  }
}
