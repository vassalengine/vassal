/*
 * $Id$
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
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.build.GameModule;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ImageFileFilter;
import VASSAL.tools.imageop.Op;

public class IconConfigurer extends Configurer {
  private JPanel controls;
  private String imageName;
  private String defaultImage;
  private Icon icon;

  public IconConfigurer(String key, String name, String defaultImage) {
    super(key, name);
    this.defaultImage = defaultImage;
  }

  public String getValueString() {
    return imageName;
  }

  public void setValue(String s) {
    icon = null;
    imageName = s == null ? "" : s;

    if (imageName.length() > 0) {
      final Image img = Op.load(imageName).getImage();
      if (img != null) icon = new ImageIcon(img);
    }

    setValue((Object) imageName);
  }

  public Icon getIconValue() {
    return icon;
  }

  public Component getControls() {
    if (controls == null) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls,BoxLayout.X_AXIS));
      controls.add(new JLabel(getName()));
      final JPanel p = new JPanel() {
        private static final long serialVersionUID = 1L;

        public void paint(Graphics g) {
          g.clearRect(0,0,getSize().width,getSize().height);
          final Icon i = getIconValue();
          if (i != null) {
            i.paintIcon(this, g,
                        getSize().width/2-i.getIconWidth()/2,
                        getSize().height/2-i.getIconHeight()/2);
          }
        }
      };
      p.setPreferredSize(new Dimension(32,32));
      controls.add(p);
      final JButton reset = new JButton("Select");
      reset.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectImage();
          p.repaint();
        }
      });
      controls.add(reset);
      if (defaultImage != null) {
        final JButton useDefault = new JButton("Default");
        useDefault.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            setValue(defaultImage);
            p.repaint();
          }
        });
        controls.add(useDefault);
      }
    }
    return controls;
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
                                  .addImage(f.getPath(),f.getName());
        setValue(f.getName());
      }
    }
  }
}
