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
import VASSAL.tools.swing.SwingUtils;

import java.awt.Frame;
import java.awt.HeadlessException;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RefreshPredefinedSetupsDialog extends JDialog {
  private static final Logger logger = LoggerFactory.getLogger(RefreshPredefinedSetupsDialog.class);
  private static final long serialVersionUID = 1L;
  private JButton refreshButton;
  private final BooleanConfigurer nameCheck = new BooleanConfigurer(false);
  private final BooleanConfigurer testModeOn = new BooleanConfigurer(false);
  private final BooleanConfigurer labelerNameCheck = new BooleanConfigurer(false);
  private final BooleanConfigurer layerNameCheck = new BooleanConfigurer(false);
  private final BooleanConfigurer deletePieceNoMap = new BooleanConfigurer(true);
  private final Set<String> options = new HashSet<>();

  public RefreshPredefinedSetupsDialog(Frame owner) throws HeadlessException {
    super(owner, false);
    setTitle(Resources.getString("Editor.RefreshPredefinedSetupsDialog.title"));
    initComponents();
  }

  private void initComponents() {

    final ComponentConfigPanel p = new ComponentConfigPanel();
    p.setBorder(BorderFactory.createEtchedBorder());
    setLayout(new MigLayout());

    p.add("GameRefresher.use_basic_name", nameCheck);
    p.add("GameRefresher.use_labeler_descr", labelerNameCheck);
    p.add("GameRefresher.use_layer_descr", layerNameCheck);
    p.add("GameRefresher.test_mode", testModeOn);
    p.add("GameRefresher.delete_piece_no_map", deletePieceNoMap);

    final JPanel buttonsBox = new JPanel(new MigLayout("", "push[]rel[]rel[]push")); // NON-NLS

    refreshButton = new JButton(Resources.getString("General.run"));
    refreshButton.addActionListener(e -> refreshPredefinedSetups());
    refreshButton.setEnabled(true);

    final JButton helpButton = new JButton(Resources.getString("General.help"));

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

    final JButton closeButton = new JButton(Resources.getString("General.cancel"));
    closeButton.addActionListener(e -> dispose());

    buttonsBox.add(refreshButton, "sg 1"); // NON-NLS
    buttonsBox.add(closeButton, "sg 1"); // NON-NLS
    buttonsBox.add(helpButton, "sg 1"); // NON-NLS

    p.add(buttonsBox, "span 2,grow"); // NON-NLS

    add(p);
    setLocationRelativeTo(getOwner());
    SwingUtils.repack(this);
  }

  protected void  setOptions() {
    options.clear();
    if (nameCheck.booleanValue()) {
      options.add("UseName"); //$NON-NLS-1$
    }
    if (labelerNameCheck.booleanValue()) {
      options.add("UseLabelerName"); //$NON-NLS-1$
    }
    if (layerNameCheck.booleanValue()) {
      options.add("UseLayerName"); //$NON-NLS-1$
    }
    if (testModeOn.booleanValue()) {
      options.add("TestMode"); //$NON-NLS-1$
    }
    if (deletePieceNoMap.booleanValue()) {
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
        // and exclude any "New game" entries (no predefined setup) (isUseFile == false)
        // !! Some New Game entries have UseFile = true and filename empty. Check file name too
        if (pds.getFileName() != null && ! pds.getFileName().isBlank()) {
          modulePds.add(pds);
        }
      }
    }
    log(modulePds.size() + " " + Resources.getString("GameRefresher.predefined_setups_found"));
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
