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

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Properties;

/**
 * Walks the user through a wizard interface. The user may choose between an auto-updating (networked jnlp) or purely
 * local installation (jnlp on local filesystem) installations, and can also select the particular version of VASSAL to install
 * 
 * @author rkinney
 */
public class InstallWizard {
  private WizardDialog dialog;
  private HashMap properties = new HashMap();
  public static final String INSTALL_PROPERTIES="/installInfo";
  public static final String INITIAL_SCREEN="initialScreen";

  public void start() throws IOException, InstantiationException, IllegalAccessException, ClassNotFoundException {
    Properties p = new Properties();
    InputStream in = getClass().getResourceAsStream(INSTALL_PROPERTIES);
    if (in != null) {
      p.load(in);
    }
    properties.putAll(p);
    dialog = new WizardDialog(this);
    dialog.setScreen((Screen) Class.forName(p.getProperty(INITIAL_SCREEN,ChooseVersionScreen.class.getName())).newInstance());
    dialog.setVisible(true);
  }
  
  public void put(String key, Object value) {
    properties.put(key,value);
  }
  
  public Object get(String key) {
    return properties.get(key);
  }
  
  public static void main(String[] args) throws Exception {
    InstallWizard wiz = new InstallWizard();
    wiz.start();
  }
  
  public WizardDialog getDialog() {
    return dialog;
  }
}
