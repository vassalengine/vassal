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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.WindowConstants;

/**
 * A Window for displaying a {@link Configurer}.  The title of the window
 * changes with the name of the Configurer
 */
public class ConfigurerWindow extends JDialog {
  private static final long serialVersionUID = 1L;

  private JButton okButton = new JButton("Ok");

  public ConfigurerWindow(Configurer c) {
    this(c, true);
  }

  public ConfigurerWindow(final Configurer c, boolean modal) {
    super((JFrame) null, modal);
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    add(c.getControls());
    c.addPropertyChangeListener
      (new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurer.NAME_PROPERTY
            .equals(evt.getPropertyName())) {
            setTitle((String) evt.getNewValue());
          }
        }
      });
    setTitle(c.getName());
    okButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        c.getValue();
        dispose();
      }
    });
    add(okButton);
    pack();
  }
}






