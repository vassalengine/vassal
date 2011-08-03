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
package VASSAL.launch;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;

/**
 * Utility base class for {@link GameModule}-related actions, with auxilliary
 * actions and error reporting.
 *
 * @author rodneykinney
 *
 */
public abstract class GameModuleAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private static final Logger logger =
    LoggerFactory.getLogger(GameModuleAction.class);

  protected Component comp;
  protected boolean actionCancelled;
  protected List<Runnable> actions = new ArrayList<Runnable>();

  public GameModuleAction(String name, Component comp) {
    super(name);
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
    // FIXME: review error message
    catch (Exception e1) {
      reportError(e1);
    }
  }

  protected abstract void performAction(ActionEvent evt) throws Exception;

  protected void reportError(Exception ex) {
    logger.error("", ex);
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
