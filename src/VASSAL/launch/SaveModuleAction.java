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

import java.awt.event.ActionEvent;

import VASSAL.build.GameModule;
import VASSAL.configure.SaveAction;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidationReportDialog;

public class SaveModuleAction extends SaveAction {
  private static final long serialVersionUID = 1L;

  public SaveModuleAction() {
  }

  public void actionPerformed(ActionEvent e) {
    final ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      save();
    }
    else {
      new ValidationReportDialog(report,
        new ValidationReportDialog.CallBack() {
          public void ok() {
            save();
          }

          public void cancel() {
          }
        }
      ).setVisible(true);
    }
  }

  protected void save() {
    GameModule.getGameModule().save();
  }
}
