/*
 * $Id$
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
import java.io.IOException;
import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JTextField;
import VASSAL.chat.HttpRequestWrapper;

/**
 * @author rkinney
 */
public class ChooseVersionScreen implements Screen {
  private JComboBox choice = new JComboBox();
  private JTextField alternateChoice;
  private Box controls;
  public ChooseVersionScreen() {
    controls = Box.createHorizontalBox();
    controls.add(new JLabel(InstallWizard.getResources().getString("Install.select_version"))); //$NON-NLS-1$
    HttpRequestWrapper req = new HttpRequestWrapper("http://www.vassalengine.org/util/getAllVersionNumbers"); //$NON-NLS-1$
    try {
    DefaultComboBoxModel m = new DefaultComboBoxModel();
      for (String s : req.doGet(null)) { 
        m.insertElementAt(s,0);
      }
      choice.setModel(m);
      controls.add(choice);
      choice.setMaximumSize(new Dimension(choice.getPreferredSize().width,choice.getPreferredSize().height));
      choice.setSelectedIndex(0);
    }
    catch (IOException e) {
      alternateChoice = new JTextField(6);
      controls.add(alternateChoice);
    }
    
  }
  public Component getControls() {
    return controls;
  }
  public void next(InstallWizard wizard) {
    if (alternateChoice != null) {
      wizard.put(Constants.JNLP_URL,"http://www.vassalengine.org/ws/vassal-"+alternateChoice.getText()+".jnlp"); //$NON-NLS-1$ //$NON-NLS-2$
    }
    else {
      wizard.put(Constants.JNLP_URL,"http://www.vassalengine.org/ws/vassal-"+choice.getSelectedItem()+".jnlp"); //$NON-NLS-1$ //$NON-NLS-2$
    }
    wizard.next("ChooseVersionScreen.next", ChooseHeapSizeScreen.class); //$NON-NLS-1$
  }
  
}
