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
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.PredefinedSetup;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
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
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JPanel;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    setLayout(new MigLayout("", "[fill]")); // NON-NLS

    final JPanel panel = new JPanel(new MigLayout("hidemode 3,wrap 1" + "," + ConfigurerLayout.STANDARD_GAPY, "[fill]")); // NON-NLS
    panel.setBorder(BorderFactory.createEtchedBorder());

    final JPanel buttonsBox = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
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

    buttonsBox.add(refreshButton, "tag ok,sg 1"); // NON-NLS
    buttonsBox.add(closeButton, "tag cancel,sg 1"); // NON-NLS
    buttonsBox.add(helpButton, "tag help,sg 1");     // NON-NLS

    nameCheck = new JCheckBox(Resources.getString("GameRefresher.use_basic_name"));
    panel.add(nameCheck);
    labelerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_labeler_descr"));
    panel.add(labelerNameCheck);
    layerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_layer_descr"));
    panel.add(layerNameCheck);
    testModeOn = new JCheckBox(Resources.getString("GameRefresher.test_mode"));
    panel.add(testModeOn);
    deletePieceNoMap = new JCheckBox(Resources.getString("GameRefresher.delete_piece_no_map"));
    deletePieceNoMap.setSelected(true);
    panel.add(deletePieceNoMap);

    panel.add(buttonsBox, "grow"); // NON-NLS
    add(panel, "grow"); // NON-NLS

    setLocationRelativeTo(getOwner());
    SwingUtils.repack(this);
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

    // Are we running a refresh on a main module or on an extension
    Boolean isRefreshOfExtension = true;
    final GameModule mod = GameModule.getGameModule();
    final DataArchive dataArchive = mod.getDataArchive();
    final List<ModuleExtension>  moduleExtensionList = mod.getComponentsOf(ModuleExtension.class);
    if (moduleExtensionList.isEmpty()) {
      isRefreshOfExtension = false;
    }
    final List<PredefinedSetup>  modulePdsAndMenus = mod.getAllDescendantComponentsOf(PredefinedSetup.class);
    final List<PredefinedSetup>  modulePds = new ArrayList<>();
    for (final PredefinedSetup pds : modulePdsAndMenus) {
      if (!pds.isMenu() && pds.isUseFile()) {
        //Exclude scenario folders (isMenu == true)
        // and exclude any "New game" entries (no predefined setup) (isUseFile == false)
        // !! Some New Game entries have UseFile = true and filename empty. Check file name too
        if (pds.getFileName() != null && ! pds.getFileName().isBlank()) {
          Boolean isExtensionPDS = true;
          try {
            isExtensionPDS =  !dataArchive.contains(pds.getFileName());
          }
          catch (final IOException e) {
            ErrorDialog.bug(e);
          }
          if (isExtensionPDS == isRefreshOfExtension) {
            modulePds.add(pds);
          }
        }
      }
    }
    log(modulePds.size() + " " + Resources.getString("GameRefresher.predefined_setups_found"));
    for (final PredefinedSetup pds : modulePds) {
      log(pds.getAttributeValueString(pds.NAME) + " (" + pds.getFileName() + ")");
    }

    for (final PredefinedSetup pds : modulePds) {
      GameModule.getGameModule().getGameState().setup(false);  //BR// Ensure we clear any existing game data/listeners/objects out.
      GameModule.getGameModule().setRefreshingSemaphore(true); //BR// Raise the semaphore that suppresses GameState.setup()

      try {
        pds.refresh(options);
      }
      catch (final IOException e) {
        ErrorDialog.bug(e);
      }
      finally {
        GameModule.getGameModule().setRefreshingSemaphore(false); //BR// Make sure we definitely lower the semaphore
      }

    }
    GameModule.getGameModule().getGameState().setup(false); //BR// Clear out whatever data (pieces, listeners, etc) left over from final game loaded.

    refreshButton.setEnabled(true);
  }

}
