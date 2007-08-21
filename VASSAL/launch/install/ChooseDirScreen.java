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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;


/**
 * @author rkinney
 */
public class ChooseDirScreen implements Screen, ActionListener, Runnable {
  public static final String NEXT_SCREEN = "ChooseDirScreen.next";
  private JTextField tf = new JTextField(36);
  private JButton select = new JButton(InstallWizard.getResources().getString("General.select"));
  private Box controls;

  public ChooseDirScreen() {
    Box hBox = Box.createHorizontalBox();
    hBox.add(tf);
    tf.addActionListener(this);
    tf.setText(new File(System.getProperty("user.home"), "VASSAL").getPath()); //$NON-NLS-1$ //$NON-NLS-2$
    tf.setMaximumSize(new Dimension(tf.getMaximumSize().width, tf.getPreferredSize().height));
    tf.select(0, tf.getText().length());
    hBox.add(select);
    select.addActionListener(this);
    controls = Box.createVerticalBox();
    controls.add(new JLabel(InstallWizard.getResources().getString("Install.select_install_directory"))); //$NON-NLS-1$
    controls.add(hBox);
  }

  public Component getControls() {
    SwingUtilities.invokeLater(this);
    return controls;
  }

  public void next(InstallWizard wiz) {
    wiz.put(Constants.INSTALL_DIR, tf.getText());
    Screen s = wiz.next(NEXT_SCREEN, InstallJnlpScreen.class); //$NON-NLS-1$
    if (s instanceof InstallProgressScreen) {
      ((InstallProgressScreen) s).start(wiz);
    }
  }

  public void actionPerformed(ActionEvent e) {
    if (tf.equals(e.getSource())) {
    Window w = SwingUtilities.getWindowAncestor(tf);
    if (w instanceof WizardDialog) {
      next(((WizardDialog) w).getWizard());
    }
    }
    else if (select.equals(e.getSource())) {
      JFileChooser fc = new JFileChooser();
      fc.setFileFilter(new DirectoryFilter());
      fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      if (fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
        tf.setText(fc.getSelectedFile().getPath());
      }
    }
  }
  public void run() {
    tf.requestFocus();
  }
  public final class DirectoryFilter extends FileFilter {
    public boolean accept(File pathname) {
      return pathname.isDirectory();
    }

    public String getDescription() {
      return InstallWizard.getResources().getString("Install.directories"); //$NON-NLS-1$
    }
  }
}
