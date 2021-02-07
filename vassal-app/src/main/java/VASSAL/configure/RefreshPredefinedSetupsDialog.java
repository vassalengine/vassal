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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class RefreshPredefinedSetupsDialog extends JDialog {
  private static final Logger logger = LoggerFactory.getLogger(RefreshPredefinedSetupsDialog.class);
  private static final long serialVersionUID = 1L;
  private JButton refreshButton;
  private JCheckBox nameCheck;
  private JCheckBox labelerNameCheck;
  private JCheckBox layerNameCheck;
  private JCheckBox testModeOn;
  private JCheckBox deletePieceNoMap;
  private final Set<String> options = new HashSet<>();

  public RefreshPredefinedSetupsDialog(Frame owner) throws HeadlessException {
    super(owner, false);
    setTitle(Resources.getString("Editor.RefreshPredefinedSetupsDialog.title"));
    initComponents();
  }

  private void initComponents() {
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

    final Box buttonsBox = Box.createHorizontalBox();
    refreshButton = new JButton(Resources.getString("Editor.ModuleEditor.refresh_predefined"));
    refreshButton.addActionListener(e -> refreshPredefinedSetups());
    refreshButton.setEnabled(true);
    buttonsBox.add(refreshButton);
    final JButton helpButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.help"));

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
    final JButton closeButton = new JButton(Resources.getString("Editor.RefreshPredefinedSetupsDialog.close"));
    closeButton.addActionListener(e -> dispose());
    buttonsBox.add(closeButton);
    add(buttonsBox);

    nameCheck = new JCheckBox(Resources.getString("GameRefresher.use_basic_name"));
    add(nameCheck);
    labelerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_labeler_descr"));
    add(labelerNameCheck);
    layerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_layer_descr"));
    add(layerNameCheck);
    testModeOn = new JCheckBox(Resources.getString("GameRefresher.test_mode"));
    add(testModeOn);
    deletePieceNoMap = new JCheckBox(Resources.getString("GameRefresher.delete_piece_no_map"));
    deletePieceNoMap.setSelected(true);
    add(deletePieceNoMap);
    pack();
    setLocationRelativeTo(getOwner());
  }

  protected void  setOptions() {
    options.clear();
    if (nameCheck.isSelected()) {
      options.add("UseName"); //$NON-NLS-1$
    }
    if (labelerNameCheck.isSelected()) {
      options.add("UseLabelerName"); //$NON-NLS-1$
    }
    if (layerNameCheck.isSelected()) {
      options.add("UseLayerName"); //$NON-NLS-1$
    }
    if (testModeOn.isSelected()) {
      options.add("TestMode"); //$NON-NLS-1$
    }
    if (deletePieceNoMap.isSelected()) {
      options.add("DeleteNoMap"); //$NON-NLS-1$
    }
  }

  public void log(String message) {
    GameModule.getGameModule().warn(message);
    logger.info(message);
  }

  public boolean isTestMode() {
    return options.contains("TestMode"); //$NON-NLS-1$
  }

  private void refreshPredefinedSetups() {
    refreshButton.setEnabled(false);
    setOptions();
    if (isTestMode()) {
      log(Resources.getString("GameRefresher.refresh_counters_test_mode"));
    }
    final GameModule mod = GameModule.getGameModule();
    final List<PredefinedSetup>  modulePdsAndMenus = mod.getAllDescendantComponentsOf(PredefinedSetup.class);
    final List<PredefinedSetup>  modulePds = new ArrayList<>();
    for (final PredefinedSetup pds : modulePdsAndMenus) {
      if (!pds.isMenu() && pds.isUseFile()) {
        //Exclude scenario folders (isMenu == true)
        // and exclude any "New game" entries (no predefined setup) (isUseFile == true)
        modulePds.add(pds);
      }
    }
    log(modulePds.size() + " Predefined setups found");
    for (final PredefinedSetup pds : modulePds) {
      log(pds.getAttributeValueString(pds.NAME) + " (" + pds.getFileName() + ")");
    }
    for (final PredefinedSetup pds : modulePds) {
      try {
        pds.refresh(options);
      }
      catch (final IOException e) {
        ErrorDialog.bug(e);
      }
    }
    refreshButton.setEnabled(true);
  }

}
