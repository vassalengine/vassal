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
package VASSAL.configure;

import java.io.File;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import VASSAL.build.module.Documentation;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.FileChooser;

/**
 * A Configurer for java.io.File values
 */
public class FileConfigurer extends Configurer {
  private FileChooser globalFileChooser;
  protected VASSAL.tools.ArchiveWriter archive;

  protected JPanel p;
  protected JTextField tf;
  protected FileChooser fc;
  protected boolean editable;

  public FileConfigurer(String key, String name) {
    super(key, name);
    setValue(null);
    fc = initFileChooser();
    editable = true;
  }

  protected FileChooser initFileChooser() {
    if (globalFileChooser == null) {
      globalFileChooser = FileChooser.createFileChooser(null);
      globalFileChooser.setCurrentDirectory(
         Documentation.getDocumentationBaseDir());
    }
    return globalFileChooser;
  }

  /**
   * If a non-null {@link ArchiveWriter} is used in the constructor, then
   * invoking {@link #setValue} on this FileConfigurer will automatically add
   * the file to the archive
   */
  public FileConfigurer(String key, String name, ArchiveWriter archive) {
    this(key, name);
    this.archive = archive;
  }

  public String getValueString() {
    if (archive == null) {
      return fileValue() == null ? "null" : fileValue().getPath();
    }
    else {
      return fileValue() == null ? "null" : fileValue().getName();
    }
  }

  public void setValue(Object o) {
    File f = (File) o;
    if (f != null && f.exists()) {
    	if (archive != null) {
    		addToArchive(f);
    	}
    }
    super.setValue(f);
    if (tf != null && !noUpdate) {
      tf.setText(getValueString());
    }
  }

  protected void addToArchive(File f) {
    archive.addFile(f.getPath(), f.getName());
  }

  public void setValue(String s) {
    if (s == null)
      setValue((Object) null);
    else {
      setValue(new File(s));
    }
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));
      JButton b = new JButton("Select");
      p.add(b);
      tf = new JTextField(getValueString());
      tf.setEditable(editable);
      tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width, tf.getPreferredSize().height));
      tf.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
        public void changedUpdate(javax.swing.event.DocumentEvent evt) {
          update();
        }

        public void insertUpdate(javax.swing.event.DocumentEvent evt) {
          update();
        }

        public void removeUpdate(javax.swing.event.DocumentEvent evt) {
          update();
        }

        public void update() {
          String text = tf.getText();
          File f = text != null && text.length() > 0  && !"null".equals(text) ? new File(text) : null;
          noUpdate = true;
          setValue(f);
          noUpdate = false;
        }
      });
      p.add(tf);
      b.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent e) {
          chooseNewValue();
        }
      });
    }
    return p;
  }

  public void chooseNewValue() {
    if (fc.showOpenDialog(getControls()) != FileChooser.APPROVE_OPTION) {
      setValue((Object) null);
    }
    else {
      setValue(fc.getSelectedFile().exists() ? fc.getSelectedFile() : (Object) null);
    }
  }

  protected File fileValue() {
    return (File) value;
  }

  public static void main(String args[]) {
    JFrame f = new JFrame();
    FileConfigurer c = new ImageConfigurer(null, "Test file", new VASSAL.tools.ArchiveWriter("testArchive"));
    c.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      public void propertyChange(java.beans.PropertyChangeEvent evt) {
        System.err.println("" + evt.getNewValue());
      }
    });
    f.getContentPane().add(c.getControls());
    f.pack();
    f.setVisible(true);
  }

  public boolean isEditable() {
    return editable;
  }

  public void setEditable(boolean editable) {
    this.editable = editable;
    if (tf != null) {
      tf.setEditable(editable);
    }
  }
}
