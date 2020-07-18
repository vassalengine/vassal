/*
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools;

import java.awt.Color;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;

import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.LayoutStyle;

import VASSAL.build.GameModule;
import VASSAL.configure.PasswordConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;

// FXIME: Would be better if this didn't set the username and password
// directly, but instead had a static method for returning them.
// FIXME: Could be made prettier if it didn't use Configurers, or if
// we made Configurers prettier.

/**
 * A dialog for setting a username and password.
 *
 * @author Joel Uckelman
 */
public class UsernameAndPasswordDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  public UsernameAndPasswordDialog(Frame parent) {
    super(parent, "Set Your Username and Password", true);
    setLocationRelativeTo(parent);
    setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

    final StringConfigurer nameConfig = new StringConfigurer(null,
      Resources.getString("WizardSupport.RealName")); //$NON-NLS-1$
    final StringConfigurer pwd = new PasswordConfigurer(null,
      Resources.getString("WizardSupport.Password")); //$NON-NLS-1$
    final StringConfigurer pwd2 = new PasswordConfigurer(null,
      Resources.getString("WizardSupport.ConfirmPassword")); //$NON-NLS-1$

    final Component nc = nameConfig.getControls();
    final Component p1 = pwd.getControls();
    final Component p2 = pwd2.getControls();

    final JLabel note =
      new JLabel(Resources.getString("WizardSupport.NameAndPasswordDetails"));

    final JLabel error = new JLabel(Resources.getString(
      "WizardSupport.EnterNameAndPassword")); //$NON-NLS-1$

    final JButton ok = new JButton(Resources.getString(Resources.OK));
    ok.setEnabled(false);
    ok.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        final Prefs p = GameModule.getGameModule().getPrefs();

        p.getOption(GameModule.REAL_NAME).setValue(nameConfig.getValueString());
        p.getOption(GameModule.SECRET_NAME).setValue(pwd.getValueString());

        try {
          p.write();
        }
        catch (IOException ex) {
          WriteErrorDialog.error(ex, p.getFile());
        }

        UsernameAndPasswordDialog.this.dispose();
      }
    });

    final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
    cancel.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        UsernameAndPasswordDialog.this.dispose();
      }
    });

    final JPanel panel = new JPanel();

    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutoCreateGaps(true);
    layout.setAutoCreateContainerGaps(true);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING, true)
        .addComponent(nc)
        .addComponent(p1)
        .addComponent(p2)
        .addComponent(note)
        .addGroup(layout.createSequentialGroup()
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(error)
          .addGap(0, 0, Integer.MAX_VALUE))
        .addGroup(layout.createSequentialGroup()
          .addGap(0, 0, Integer.MAX_VALUE)
          .addComponent(ok)
          .addComponent(cancel)));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .addComponent(nc)
        .addComponent(p1)
        .addComponent(p2)
        .addComponent(note)
        .addComponent(error)
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED,
                         GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
        .addGroup(
          layout.createParallelGroup(GroupLayout.Alignment.BASELINE, false)
            .addComponent(ok)
            .addComponent(cancel)));

    layout.linkSize(ok, cancel);

    add(panel);

    pack();
    setMinimumSize(getSize());

    // This listener handles validating the input, updating the error
    // message, and enabling the Ok button.
    final PropertyChangeListener pl = new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        if (nameConfig.getValue() == null ||
            "".equals(nameConfig.getValue())) { //$NON-NLS-1$
          if (pwd.getValue() == null || "".equals(pwd.getValue())) {
            error.setText(Resources.getString(
              "WizardSupport.EnterNameAndPassword")); //$NON-NLS-1$
          }
          else {
            error.setText(Resources.getString(
              "WizardSupport.EnterYourName")); //$NON-NLS-1$
          }
          error.setForeground(Color.black);
          ok.setEnabled(false);
        }
        else if (pwd.getValue() == null ||
                 "".equals(pwd.getValue())) { //$NON-NLS-1$
          error.setText(Resources.getString(
            "WizardSupport.EnterYourPassword")); //$NON-NLS-1$
          error.setForeground(Color.black);
          ok.setEnabled(false);
        }
        else if (pwd2.getValue() == null ||
                 "".equals(pwd2.getValue())) { //$NON-NLS-1$
          error.setText("Please confirm your password");
          error.setForeground(Color.black);
          ok.setEnabled(false);
        }
        else if (!pwd.getValue().equals(pwd2.getValue())) {
          error.setText(Resources.getString(
            "WizardSupport.PasswordsDontMatch")); //$NON-NLS-1$
          error.setForeground(Color.red);
          ok.setEnabled(false);
        }
        else {
          // everything is ok
          error.setText("");  //$NON-NLS-1$
          error.setForeground(Color.black);
          ok.setEnabled(true);
        }
      }
    };

    nameConfig.addPropertyChangeListener(pl);
    pwd.addPropertyChangeListener(pl);
    pwd2.addPropertyChangeListener(pl);
  }
}
