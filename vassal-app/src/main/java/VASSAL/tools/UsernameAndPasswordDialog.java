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

import VASSAL.build.GameModule;
import VASSAL.configure.PasswordConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;

import java.awt.Color;
import java.awt.Frame;
import java.beans.PropertyChangeListener;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

// FIXME: Would be better if this didn't set the username and password
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
    super(parent, Resources.getString("Editor.UsernameAndPasswordDialog.choose_your_weapons"), true);
    setLocationRelativeTo(parent);
    setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

    setLayout(new MigLayout("ins panel,gapy 4,wrap 2", "[right]rel[fill,grow]")); // NON-NLS

    add(new JLabel(Resources.getString("WizardSupport.RealName")));
    final StringConfigurer nameConfig = new StringConfigurer("");
    add(nameConfig.getControls());

    add(new JLabel(Resources.getString("WizardSupport.Password")));
    final StringConfigurer pwd = new PasswordConfigurer(null, "");
    add(pwd.getControls());

    add(new JLabel(Resources.getString("WizardSupport.ConfirmPassword")));
    final StringConfigurer pwd2 = new PasswordConfigurer(null, "");
    add(pwd2.getControls());

    final JLabel note = new JLabel(Resources.getString("WizardSupport.NameAndPasswordDetails"));
    add(note, "span 2,center"); // NON-NLS

    final JLabel error = new JLabel(Resources.getString("WizardSupport.EnterNameAndPassword"));
    add(error, "span 2,center"); // NON-NLS

    final JButton ok = new JButton(Resources.getString(Resources.OK));
    ok.setEnabled(false);
    ok.addActionListener(e -> {
      final Prefs p = GameModule.getGameModule().getPrefs();

      p.getOption(GameModule.REAL_NAME).setValue(nameConfig.getValueString());
      p.getOption(GameModule.SECRET_NAME).setValue(pwd.getValueString());

      try {
        p.save();
      }
      catch (IOException ex) {
        WriteErrorDialog.error(ex, p.getFile());
      }

      dispose();
    });

    final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
    cancel.addActionListener(e -> dispose());

    final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]push")); // NON-NLS
    buttonPanel.add(ok);
    buttonPanel.add(cancel);
    add(buttonPanel, "span 2,center"); // NON-NLS

    pack();
    setMinimumSize(getSize());

    // This listener handles validating the input, updating the error
    // message, and enabling the Ok button.
    final PropertyChangeListener pl = evt -> {
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
        error.setText(Resources.getString("Editor.UsernameAndPasswordDialog.turn_your_key_sir"));
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
    };

    nameConfig.addPropertyChangeListener(pl);
    pwd.addPropertyChangeListener(pl);
    pwd2.addPropertyChangeListener(pl);
  }
}
