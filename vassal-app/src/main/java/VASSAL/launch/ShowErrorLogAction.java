/*
 * Copyright (c) 2021 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.launch;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.JDialog;
import javax.swing.JScrollPane;

import net.miginfocom.swing.MigLayout;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.logging.LogPane;
import VASSAL.tools.swing.SwingUtils;

class ShowErrorLogAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private final Frame frame;

  public ShowErrorLogAction(Frame frame) {
    super(Resources.getString("Help.error_log"));
    this.frame = frame;
  }

  @Override
  public void actionPerformed(ActionEvent e) {
// FIXME: don't create a new one each time!
    final File logfile = Info.getErrorLogPath();
    final LogPane lp = new LogPane(logfile);

// FIXME: this should have its own key. Probably keys should be renamed
// to reflect what they are labeling, e.g., Help.show_error_log_menu_item,
// Help.error_log_dialog_title.
    final JDialog d = new JDialog(frame, Resources.getString("Help.error_log"));
    d.setLayout(new MigLayout("insets 0")); //NON-NLS
    d.add(new JScrollPane(lp), "grow, push, w 500, h 600"); //NON-NLS

    d.setLocationRelativeTo(frame);
    d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

    SwingUtils.repack(d);
    d.setVisible(true);
  }
}
