/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.launch.install;

import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;

import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
// I18n: Complete
public class WizardDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private Screen screen;
  private Box screenBox = Box.createHorizontalBox();
  private InstallWizard wizard;
  public WizardDialog(final InstallWizard wizard) throws HeadlessException {
    super();
    this.wizard = wizard;
    setModal(false);
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
    Box buttonBox = Box.createHorizontalBox();
    JButton b = new JButton(Resources.getString(Resources.NEXT));
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        screen.next(wizard);
      }
    });
    buttonBox.add(Box.createHorizontalGlue());
    buttonBox.add(b);
    getContentPane().add("South", buttonBox);
    getContentPane().add(screenBox);
    setSize(600,400);
    setLocationRelativeTo(null);
  }
  public void setScreen(Screen screen) {
    screenBox.removeAll();
    screenBox.add(Box.createVerticalGlue());
    screenBox.add(screen.getControls());
    screenBox.add(Box.createVerticalGlue());
    this.screen = screen;
    validate();
    repaint();
  }
  public InstallWizard getWizard() {
    return wizard;
  }
  
}
