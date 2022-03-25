/*
 *
 * Copyright (c) 2008 by Brent Easton and Joel Uckelman
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
package VASSAL.build.module.metadata;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.ZipWriter;

/**
 * Class representing the metadata for a Save Game/Log File. Details
 * about the module this saved game was created with are saved in a
 * separate module data file in the saved game zip.
 *
 * @author Brent Easton
 * @since 3.1.0
 */
public class SaveMetaData extends AbstractMetaData {

  private static final Logger logger =
    LoggerFactory.getLogger(SaveMetaData.class);

  public static final String ZIP_ENTRY_NAME = "savedata"; //NON-NLS
  public static final String DATA_VERSION = "1";
  public static final String PROMPT_LOG_COMMENT = "promptLogComment"; //NON-NLS

  protected ModuleMetaData moduleData;

  public SaveMetaData() {
    super();

    setVersion(GameModule.getGameModule().getGameVersion());

    if ((Boolean)GameModule.getGameModule().getPrefs().getValue(PROMPT_LOG_COMMENT)) {
      final JDialog d = new JDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, GameModule.getGameModule().getPlayerWindow()), true);
      d.setTitle(Resources.getString("BasicLogger.log_file_comments"));

      final JLabel desc = new JLabel("<html><body><p style='width: 400px;'>" + Resources.getString("BasicLogger.enter_comments") + "</p></body></html>");  //NON-NLS

      final JLabel commentLabel = new JLabel(Resources.getString("BasicLogger.log_file_comments"));
      final JTextField commentField = new JTextField("", 32);
      commentLabel.setLabelFor(commentField);

      final JCheckBox stopBox = new JCheckBox(Resources.getString("Editor.SaveMetaData.dont_ask_again"), false);

      final JButton okay = new JButton(Resources.getString("General.ok"));
      okay.addActionListener(e -> {
        if (stopBox.isSelected()) {
          GameModule.getGameModule().getPrefs().setValue(PROMPT_LOG_COMMENT, false);
        }
        final String comments = commentField.getText();
        setDescription(new Attribute(DESCRIPTION_ELEMENT, comments));
        d.dispose();
      });

      final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
      cancel.addActionListener(e1 -> {
        if (stopBox.isSelected()) {
          GameModule.getGameModule().getPrefs().setValue(PROMPT_LOG_COMMENT, false);
        }
        d.dispose();
      });

      d.setLayout(new MigLayout("insets dialog, nogrid", "", "[]unrel[]unrel:push[]")); //$NON-NLS-1$//

      // top row
      d.add(desc, "align left, wrap"); //$NON-NLS-1$//

      // comment row
      d.add(commentLabel, "align right, gapx rel"); //$NON-NLS-1$//
      d.add(commentField, "pushx, growx, wrap"); //$NON-NLS-1$//

      // options row
      d.add(stopBox, "align center, gapx unrel, span"); //$NON-NLS-1$//

      // buttons row
      d.add(okay, "tag ok, split"); //$NON-NLS-1$//
      d.add(cancel, "tag cancel"); //$NON-NLS-1$//

      d.getRootPane().setDefaultButton(okay); // Enter key activates search

      // Esc Key cancels
      final KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
      final int w = JComponent.WHEN_IN_FOCUSED_WINDOW;
      d.getRootPane().registerKeyboardAction(ee -> {
        if (stopBox.isSelected()) {
          GameModule.getGameModule().getPrefs().setValue(PROMPT_LOG_COMMENT, false);
        }
        d.dispose();
      }, k, w);

      commentField.requestFocus(); // Start w/ focus in search string field

      d.pack();
      d.setLocationRelativeTo(d.getParent());
      d.setVisible(true);
    }
  }

  public SaveMetaData(ZipFile zip) {
    read(zip);
  }

  public String getModuleName() {
    return moduleData == null ? "" : moduleData.getName();
  }

  public String getModuleVersion() {
    return moduleData == null ? "" : moduleData.getVersion();
  }

  public ModuleMetaData getModuleData() {
    return moduleData;
  }

  @Override
  public String getZipEntryName() {
    return ZIP_ENTRY_NAME;
  }

  @Override
  public String getMetaDataVersion() {
    return DATA_VERSION;
  }

  /**
   * Write Save Game metadata to the specified Archive
   * @param archive Save game Archive
   * @throws IOException If anything goes wrong
   */
  @Override
  public void save(FileArchive archive) throws IOException {
    super.save(archive);

    // Also save a copy of the current module metadata in the save file. Copy
    // module metadata from the module archive as it will contain full i18n
    // information.
    copyModuleMetadata(archive);
  }

  @Override
  public void save(ZipWriter zw) throws IOException {
    super.save(zw);

    // Also save a copy of the current module metadata in the save file. Copy
    // module metadata from the module archive as it will contain full i18n
    // information.
    copyModuleMetadata(zw);
  }

  /**
   * Add Elements specific to SaveMetaData
   */
  @Override
  protected void addElements(Document doc, Element root) {

  }

  /**
   * Read and validate a Saved Game/Log file.
   * Check that it has a Zip Entry named saved game.
   * If it has a metadata file, read and parse it.
   *
   * Closes the {@link ZipFile}
   *
   * @param zip Saved Game File
   */
  public void read(ZipFile zip) {
    try (zip) {
      // Try to parse the metadata. Failure is not catastrophic, we can
      // treat it like an old-style save with no metadata.
      final ZipEntry data = zip.getEntry(getZipEntryName());
      if (data == null) return;

      // set up the handler
      final XMLHandler handler = new XMLHandler();

      // parse! parse!
      try (InputStream zin = zip.getInputStream(data);
           BufferedInputStream in = new BufferedInputStream(zin)) {
        synchronized (parser) {
          parser.setContentHandler(handler);
          parser.setDTDHandler(handler);
          parser.setEntityResolver(handler);
          parser.setErrorHandler(handler);
          parser.parse(new InputSource(in));
        }
      }

      // read the matching Module data
      moduleData = new ModuleMetaData(zip);
    }
    catch (final IOException | SAXException e) {
      logger.error("", e);
    }
  }
}
