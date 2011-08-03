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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import VASSAL.build.GameModule;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.FileChooser;

/**
 * A Configurer for java.io.File values
 */
public class FileConfigurer extends Configurer {
  protected ArchiveWriter archive;
  protected JPanel p;
  protected JTextField tf;
  protected FileChooser fc;
  protected boolean editable;
  protected DirectoryConfigurer startingDirectory;

  public FileConfigurer(String key, String name) {
    this(key, name, (DirectoryConfigurer)null);
  }

  /**
   *
   * @param key
   * @param name
   * @param startingDirectory If non-null, points to a preferences setting that specifies the starting directory for the "Select" button
   */
  public FileConfigurer(String key, String name, DirectoryConfigurer startingDirectory) {
    super(key, name);
    setValue(null);
    editable = true;
    this.startingDirectory = startingDirectory;
    fc = initFileChooser();
  }

  protected FileChooser initFileChooser() {
    FileChooser fc = FileChooser.createFileChooser(null, startingDirectory);
    if (startingDirectory == null && GameModule.getGameModule() != null) {
      fc.setCurrentDirectory((File) Prefs.getGlobalPrefs().getValue(Prefs.MODULES_DIR_KEY));
    }
    return fc;
  }

  /**
   * If a non-null {@link ArchiveWriter} is used in the constructor, then invoking {@link #setValue} on this
   * FileConfigurer will automatically add the file to the archive
   */
  public FileConfigurer(String key, String name, ArchiveWriter archive) {
    this(key, name);
    this.archive = archive;
  }

  public String getValueString() {
    if (archive == null) {
      return getFileValue() == null ? "null" : getFileValue().getPath();
    }
    else {
      return getFileValue() == null ? "null" : getFileValue().getName();
    }
  }

  public void setValue(Object o) {
// FIXME: this creates a problem when the referenced file is in the JAR
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

  public Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));
      JButton b = new JButton("Select");
      p.add(b);

      tf = new JTextField(getValueString());
      tf.setEditable(editable);
      tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width,
                                               tf.getPreferredSize().height));
      tf.getDocument().addDocumentListener(new DocumentListener() {
        public void changedUpdate(DocumentEvent evt) {
          update();
        }

        public void insertUpdate(DocumentEvent evt) {
          update();
        }

        public void removeUpdate(DocumentEvent evt) {
          update();
        }

        public void update() {
          String text = tf.getText();
          File f = text != null && text.length() > 0 && !"null".equals(text) ? new File(text) : null;
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

  public File getFileValue() {
    return (File) value;
  }

  public static void main(String args[]) {
    final JFrame f = new JFrame();
    final FileConfigurer c =
      new ImageConfigurer(null, "Test file", new ArchiveWriter("testArchive"));
    c.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(String.valueOf(evt.getNewValue()));
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
