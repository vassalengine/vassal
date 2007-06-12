/*
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
package VASSAL.launch;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.tools.FileChooser;

/**
 * Utility base class for GameModule-related actions, with auxilliary actions and error reporting
 * 
 * @author rodneykinney
 * 
 */
public abstract class GameModuleAction extends AbstractAction {
  protected FileChooser fc;
  protected Component comp;
  protected boolean actionCancelled;
  protected List<Runnable> actions = new ArrayList<Runnable>();

  public GameModuleAction(String name, Component comp) {
    super(name);
    fc = FileChooser.createFileChooser(comp, (DirectoryConfigurer) Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));
    this.comp = comp;
  }

  protected String getMessage(Exception err) {
    String msg = err.getClass().getSimpleName();
    if (err.getMessage() != null) {
      msg += ":  " + err.getMessage();
    }
    return msg;
  }

  public void actionPerformed(ActionEvent e) {
    try {
      performAction(e);
      if (!actionCancelled) {
        runActions();
      }
    }
    catch (Exception e1) {
      reportError(e1);
    }
  }

  protected abstract void performAction(ActionEvent evt) throws Exception;

  protected void reportError(Exception ex) {
    ex.printStackTrace();
    JOptionPane.showMessageDialog(comp, getMessage(ex));
  }

  /**
   * Add an auxilliary action to be performed after the core action. For example, closing a window after a module has
   * been loaded
   * 
   * @param r
   */
  public void addAction(Runnable r) {
    actions.add(r);
  }

  protected void runActions() {
    for (Runnable r : actions) {
      r.run();
    }
  }
}