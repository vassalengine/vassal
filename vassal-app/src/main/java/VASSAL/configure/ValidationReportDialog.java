/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.configure;

import java.awt.BorderLayout;
import java.util.List;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.ScrollPane;

/**
 * Dialog for reporting the results of validating a GameModule
 */
public class ValidationReportDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private final CallBack callback;

  public ValidationReportDialog(ValidationReport report, CallBack cb) {
    super(GameModule.getGameModule().getPlayerWindow(), false);
    setTitle(Resources.getString("Editor.ValidationReportDialog.problems"));
    this.callback = cb;
    final Box reportBox = Box.createVerticalBox();
    add(reportBox);
    final JPanel buttonPanel = new JPanel();
    add(buttonPanel, BorderLayout.SOUTH);

    final List<String> warnings = report.getWarnings();
    switch (warnings.size()) {
    case 0:
      reportBox.add(new JLabel(Resources.getString("Editor.ValidationReportDialog.no_problems")));
      buttonPanel.add(createOkButton());
      break;
    case 1:
      reportBox.add(new JLabel(Resources.getString("Editor.ValidationReportDialog.a_problem")));
      reportBox.add(new JLabel(warnings.get(0) + "."));
      buttonPanel.add(createOkButton());
      buttonPanel.add(createCancelButton());
      break;
    default:
      reportBox.add(new JLabel(Resources.getString("Editor.ValidationReportDialog.following_problems")));
      reportBox.add(new JLabel(Resources.getString("Editor.ValidationReportDialog.danger_will_robinson")));
      final JList<String> list = new JList<>(warnings.toArray(new String[0]));
      list.setVisibleRowCount(Math.min(list.getVisibleRowCount(), warnings.size()));
      reportBox.add(new ScrollPane(list));
      buttonPanel.add(createOkButton());
      buttonPanel.add(createCancelButton());
    }

    pack();
    setLocationRelativeTo(null);
  }

  private JButton createCancelButton() {
    final JButton cancel = new JButton(Resources.getString("General.cancel"));
    cancel.addActionListener(e -> {
      callback.cancel();
      dispose();
    });
    return cancel;
  }

  private JButton createOkButton() {
    final JButton ok = new JButton(Resources.getString("Editor.ValidationReportDialog.ignore"));
    ok.addActionListener(e -> {
      callback.ok();
      dispose();
    });
    return ok;
  }

  public interface CallBack {
    void ok();
    void cancel();
  }
}
