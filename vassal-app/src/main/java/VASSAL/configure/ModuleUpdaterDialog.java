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

import java.awt.Frame;
import java.awt.HeadlessException;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ZipUpdater;
import VASSAL.tools.filechooser.FileChooser;

@Deprecated(since = "2020-08-05", forRemoval = true)
public class ModuleUpdaterDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  @SuppressWarnings("removal")
  public ModuleUpdaterDialog(Frame owner) throws HeadlessException {
    super(owner, false);
    setTitle(Resources.getString("Editor.ModuleUpdaterDialog.title"));
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    final FileConfigurer fileConfig =
      new FileConfigurer(null, Resources.getString("Editor.ModuleUpdaterDialog.file_older"));
    add(fileConfig.getControls());

    final Box b = Box.createHorizontalBox();

    final JButton saveButton = new JButton(Resources.getString("Editor.ModuleUpdaterDialog.create_updater"));
    saveButton.setEnabled(false);
    fileConfig.addPropertyChangeListener(evt -> saveButton.setEnabled(fileConfig.getValue() != null));
    saveButton.addActionListener(e -> {
      final FileChooser fc = GameModule.getGameModule().getFileChooser();
      if (fc.showSaveDialog(getOwner()) != FileChooser.APPROVE_OPTION)
        return;

      final File output = fc.getSelectedFile();
      ZipUpdater updater = null;
      try {
        updater = new ZipUpdater((File) fileConfig.getValue());
        updater.createUpdater(
          new File(GameModule.getGameModule().getArchiveWriter().getName()),
          output
        );
      }
      // FIXME: review error message
      catch (IOException e1) {
        String msg = e1.getMessage();
        if (msg == null) {
          msg = Resources.getString("Editor.ModuleUpdaterDialog.unable");
        }
        JOptionPane.showMessageDialog(
          this,
          msg,
          Resources.getString("Editor.ModuleUpdaterDialog.error_writing"),
          JOptionPane.ERROR_MESSAGE
        );
      }
    });

    final JButton cancelButton = new JButton(Resources.getString("Editor.ModuleUpdaterDialog.close"));
    cancelButton.addActionListener(e -> dispose());
    final JButton helpButton = new JButton(Resources.getString("Editor.ModuleUpdaterDialog.help"));
    add(b);
    HelpFile hf = null;
    try {
      hf = new HelpFile(null, new File(new File(
        VASSAL.build.module.Documentation.getDocumentationBaseDir(),
        "ReferenceManual"), "ModuleUpdater.html")); //NON-NLS
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
    }

    helpButton.addActionListener(new ShowHelpAction(hf.getContents(), null));
    b.add(saveButton);
    b.add(helpButton);
    b.add(cancelButton);
    add(b);
    pack();
    setLocationRelativeTo(getOwner());
  }
}
