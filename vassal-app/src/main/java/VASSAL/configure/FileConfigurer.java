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
package VASSAL.configure;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.FileChooser;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.Component;
import java.io.File;

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
   * @param key Configurer key
   * @param name Configurer label
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
    final FileChooser fc = FileChooser.createFileChooser(null, startingDirectory);
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

  @Override
  public String getValueString() {
    if (archive == null) {
      return getFileValue() == null ? "null" : getFileValue().getPath(); // NON-NLS
    }
    else {
      return getFileValue() == null ? "null" : getFileValue().getName(); // NON-NLS
    }
  }

  @Override
  public void setValue(Object o) {
// FIXME: this creates a problem when the referenced file is in the JAR
    final File f = (File) o;
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

  @Override
  public void setValue(String s) {
    if (s == null)
      setValue((Object) null);
    else {
      setValue(new File(s));
    }
  }

  @Override
  public Component getControls() {
    if (p == null) {
      p = new ConfigurerPanel(getName(), "[]rel[grow,fill]", "[]rel[]rel[grow,fill]"); // NON-NLS

      final JButton b = new JButton(Resources.getString("Editor.select"));
      p.add(b);

      tf = new JTextField(getValueString());
      tf.setEditable(editable);
      if (editable) {
        // Edit box selects all text when first focused
        tf.addFocusListener(new java.awt.event.FocusAdapter() {
          public void focusGained(java.awt.event.FocusEvent evt) {
            SwingUtilities.invokeLater(new Runnable() {
              @Override
              public void run() {
                tf.selectAll();
              }
            });
          }
        });
      }
      tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width,
                                               tf.getPreferredSize().height));
      tf.getDocument().addDocumentListener(new DocumentListener() {
        @Override
        public void changedUpdate(DocumentEvent evt) {
          update();
        }

        @Override
        public void insertUpdate(DocumentEvent evt) {
          update();
        }

        @Override
        public void removeUpdate(DocumentEvent evt) {
          update();
        }

        public void update() {
          final String text = tf.getText();
          final File f = text != null && text.length() > 0 && !"null".equals(text) ? new File(text) : null; // NON-NLS
          noUpdate = true;
          setValue(f);
          noUpdate = false;
        }
      });
      p.add(tf);
      b.addActionListener(e -> chooseNewValue());
    }
    return p;
  }

  public void chooseNewValue() {
    if (fc.showOpenDialog(getControls()) != FileChooser.APPROVE_OPTION) {
      setValue((Object) null);
    }
    else {
      setValue(fc.getSelectedFile().exists() ? fc.getSelectedFile() : null);
    }
  }

  public File getFileValue() {
    return (File) value;
  }

  public static void main(String[] args) {
    final JFrame f = new JFrame();
    final FileConfigurer c =
      new ImageConfigurer(null, "Test file", new ArchiveWriter("testArchive")); // NON-NLS
    c.addPropertyChangeListener(evt -> System.err.println(evt.getNewValue()));
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
