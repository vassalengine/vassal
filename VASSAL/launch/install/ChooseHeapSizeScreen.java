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
import java.util.Enumeration;
import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JRadioButton;

import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class ChooseHeapSizeScreen implements Screen, Constants {
  private Box controls = Box.createVerticalBox();
  private JRadioButton b128M = new JRadioButton(Resources.getString("Install.128_mb")); //$NON-NLS-1$
  private JRadioButton b256M = new JRadioButton(Resources.getString("Install.256_mb")); //$NON-NLS-1$
  private JRadioButton b512M = new JRadioButton(Resources.getString("Install.512_mb")); //$NON-NLS-1$
  private JRadioButton b768M = new JRadioButton(Resources.getString("Install.768_mb")); //$NON-NLS-1$
  private JRadioButton b1000M = new JRadioButton(Resources.getString("Install.1_gb")); //$NON-NLS-1$
  private JRadioButton b1500M = new JRadioButton(Resources.getString("Install.1.5_gb")); //$NON-NLS-1$
  private ButtonGroup group = new ButtonGroup();
  
  public ChooseHeapSizeScreen() {
    controls.add(new JLabel(Resources.getString("Install.select_memory_size"))); //$NON-NLS-1$
    b128M.putClientProperty(HEAP_SIZE, "128m"); //$NON-NLS-1$
    group.add(b128M);
    controls.add(b128M);
    b256M.putClientProperty(HEAP_SIZE, "256m"); //$NON-NLS-1$
    group.add(b256M);
    controls.add(b256M);
    b512M.putClientProperty(HEAP_SIZE, "512m"); //$NON-NLS-1$
    group.add(b512M);
    controls.add(b512M);
    b768M.putClientProperty(HEAP_SIZE, "768m"); //$NON-NLS-1$
    group.add(b768M);
    controls.add(b768M);
    b1000M.putClientProperty(HEAP_SIZE, "1024M"); //$NON-NLS-1$
    group.add(b1000M);
    controls.add(b1000M);
    b1500M.putClientProperty(HEAP_SIZE, "1536M"); //$NON-NLS-1$
    group.add(b1500M);
    b256M.setSelected(true);
    controls.add(b1500M);
  }

  public Component getControls() {
    return controls;
  }

  public void next(InstallWizard wiz) {
    for (Enumeration e = group.getElements();e.hasMoreElements();) {
      AbstractButton b = (AbstractButton) e.nextElement();
      if (b.isSelected()) {
        wiz.put(Constants.HEAP_SIZE,(String) b.getClientProperty(HEAP_SIZE));
        break;
      }
    }
    wiz.next("ChooseHeapSizeScreen.next", ChooseDirScreen.class); //$NON-NLS-1$
  }
}