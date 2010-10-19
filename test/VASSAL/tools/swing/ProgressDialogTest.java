/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import org.junit.Ignore;

@Ignore
public class ProgressDialogTest {
  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        final ProgressDialog pd = new ProgressDialog(
          null, "A Sisyphean Task", "Rolling a stone up the hill..."
        );

        pd.addWindowListener(new WindowAdapter() {
          @Override
          public void windowClosed(WindowEvent e) {
            System.exit(0); 
          }
        });

        pd.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            System.exit(0); 
          }
        });

        final Timer timer = new Timer(100, new ActionListener() {
          int progress = 0;

          public void actionPerformed(ActionEvent e) {
            progress = (progress+1) % 100;
            pd.setProgress(progress);
          }
        });
        timer.start();

        pd.setDefaultCloseOperation(ProgressDialog.DISPOSE_ON_CLOSE);
        pd.setVisible(true);
      }
    });
  }
}
