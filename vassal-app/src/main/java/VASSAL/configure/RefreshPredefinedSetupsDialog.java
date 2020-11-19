/*
 *
 * Copyright (c) 2005 by Rodney Kinney
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

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.PredefinedSetup;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

public class RefreshPredefinedSetupsDialog extends JDialog {
  private static final long serialVersionUID = 1L;
  private JButton refreshButton;

  public RefreshPredefinedSetupsDialog(Frame owner) throws HeadlessException {
    super(owner, false);
    setTitle(Resources.getString("Editor.RefreshPredefinedSetupsDialog.title"));
    initComponents();
  }

  private void initComponents() {
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

    Box buttonsBox = Box.createHorizontalBox();
    refreshButton = new JButton(Resources.getString("Editor.RefreshPredefinedSetupsDialog.refresh_predefined"));
    refreshButton.addActionListener(e -> refreshPredefinedSetups());
    refreshButton.setEnabled(true);
    buttonsBox.add(refreshButton);
    JButton helpButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.help"));

    HelpFile hf = null;
    try {
      hf = new HelpFile(null, new File(
        new File(Documentation.getDocumentationBaseDir(), "ReferenceManual"),
        "SavedGameUpdater.html"));
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
    }

    helpButton.addActionListener(new ShowHelpAction(hf.getContents(), null));
    buttonsBox.add(helpButton);
    JButton closeButton = new JButton(Resources.getString("Editor.RefreshPredefinedSetupsDialog.close"));
    closeButton.addActionListener(e -> dispose());
    buttonsBox.add(closeButton);
    add(buttonsBox);
    pack();
    setLocationRelativeTo(getOwner());
  }

  private void refreshPredefinedSetups() {
    refreshButton.setEnabled(false);

    final GameModule mod = GameModule.getGameModule();
    List<PredefinedSetup> pdsList = new ArrayList<>();
    for (PredefinedSetup pds : mod.getAllDescendantComponentsOf(PredefinedSetup.class)) {
      if (!pds.isMenu()) {
        try {
          pds.refresh();
        }
        catch (final IOException e) {
          ErrorDialog.bug(e);
        }
      }
    }
    refreshButton.setEnabled(true);
  }

}
