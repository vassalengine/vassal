/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
import java.io.File;

import javax.swing.JButton;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import VASSAL.build.GameModule;
import VASSAL.counters.EditablePiece;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.AudioFileFilter;
import VASSAL.tools.filechooser.FileChooser;

/**
 * Class for selecting an AudioClip while editing a module and adding it to
 * module
 *
 * @author rkinney
 *
 */
public class AudioClipConfigurer extends FileConfigurer {
  protected static DirectoryConfigurer resourceDirPref;
  protected FormattedExpressionConfigurer.ExpressionButton button;
  protected EditablePiece sourcePiece;

  public AudioClipConfigurer(String key, String name, ArchiveWriter archive, EditablePiece piece) {
    super(key, name, archive);
    this.sourcePiece = piece;
  }

  public AudioClipConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
  }

  @Override
  protected FileChooser initFileChooser() {
    if (resourceDirPref == null) {
      resourceDirPref = new DirectoryConfigurer("audioDir", null); //NON-NLS
      GameModule.getGameModule().getPrefs().addOption(null, resourceDirPref);
    }
    final FileChooser fc = FileChooser.createFileChooser(GameModule.getGameModule().getPlayerWindow(), resourceDirPref);
    fc.setFileFilter(new AudioFileFilter());
    return fc;
  }

  @Override
  protected void addToArchive(File f) {
    archive.addSound(f.getPath(), f.getName());
  }

  //
  // The AudioClipConfigure can take an Expression as well as a straight file name, while the super class
  // FileConfigurer only stores a File as the value and this does not play well with expressions.
  // The following overrides have been cut from FileConfigurer and modified to support this.
  @Override
  public Component getControls() {
    if (p == null) {
      p = new ConfigurerPanel(getName(), "[]rel[grow,fill,push]1[]", "[]rel[]rel[grow,fill,push]1[]"); // NON-NLS

      final JButton b = new JButton(Resources.getString("Editor.select"));
      p.add(b);

      tf = new JTextField(getValueString());
      tf.setEditable(editable);
      if (editable) {
        // Edit box selects all text when first focused
        tf.addFocusListener(new java.awt.event.FocusAdapter() {
          @Override
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
          if (text != null && text.startsWith("{")) {
            noUpdate = true;
            setValue(text);
            noUpdate = false;
          }
          else {
            final File f = text != null && text.length() > 0 && !"null".equals(text) ? new File(text) : null; // NON-NLS
            noUpdate = true;
            setValue(f);
            noUpdate = false;
          }
        }
      });
      p.add(tf, "grow");
      button = new FormattedExpressionConfigurer.ExpressionButton(this, tf.getPreferredSize().height, sourcePiece);
      p.add(button);
      b.addActionListener(e -> chooseNewValue());
    }
    return p;
  }


  @Override
  public String getValueString() {
    if (value instanceof String) {
      return (String) value;
    }
    else {
      return super.getValueString();
    }
  }

  @Override
  public void setValue(Object o) {
    if (o instanceof String && ((String) o).startsWith("{")) {
      // Bypass FileConfigurer File conversion for Beanshell expressions
      final Object oldValue = getValue();
      value = o;
      if (!frozen) {
        changeSupport.firePropertyChange(key, oldValue, value);
      }
      if (tf != null && !noUpdate) {
        tf.setText(getValueString());
      }
    }
    else {
      super.setValue(o);
    }
  }

  @Override
  public void setValue(String s) {
    if (s != null && s.startsWith("{")) {
      // Bypass FileConfigurer File conversion for Beanshell expressions
      setValue((Object) s);
    }
    else {
      super.setValue(s);
    }
  }

}
