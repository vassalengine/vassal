/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
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
package VASSAL.tools.version;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import java.util.concurrent.ExecutionException;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ErrorLog;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class UpdateCheckAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private final Frame frame;

  public UpdateCheckAction(Frame frame) {
    super(Resources.getString("UpdateCheckAction.update_check"));
    this.frame = frame;
  }

  public void actionPerformed(ActionEvent e) {
    new Request().execute();
  }

  private class Request extends AbstractUpdateCheckRequest {
    @Override
    protected void done() {
      try {
        final VassalVersion update = get(); 
        if (update == null) {
          // running version is current
          JOptionPane.showMessageDialog(
            frame,
            Resources.getString("UpdateCheckAction.version_current_message"),
            Resources.getString("UpdateCheckAction.version_current_title"),
            JOptionPane.INFORMATION_MESSAGE
          );
        }
        else {
          // running version is obsolete
          if (JOptionPane.showConfirmDialog(
            frame,
            Resources.getString(
              "UpdateCheckAction.update_available_message",
              Info.getVersion(), update.toString()),
              Resources.getString("UpdateCheckAction.update_available_title"),
              JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
          }
        }
        return;
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
      }
      catch (ExecutionException e) {
        ErrorLog.log(e);
      }

      JOptionPane.showMessageDialog(
        frame,
        Resources.getString("UpdateCheckAction.check_failed_message"),
        Resources.getString("UpdateCheckAction.check_failed_title"),
        JOptionPane.ERROR_MESSAGE
      ); 
    }
  }
}
