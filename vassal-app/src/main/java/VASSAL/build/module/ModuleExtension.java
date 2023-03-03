/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build.module;

import VASSAL.Info;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdChecker;
import VASSAL.build.GpIdSupport;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.ExtensionMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ComponentConfigPanel;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.swing.SwingUtils;
import VASSAL.tools.version.VersionUtils;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.NoSuchFileException;
import java.util.UUID;

/**
 * An optional extension to a GameModule
 * Like a GameModule, it is built from scratch from a 'buildFile' in a DataArchive
 * The components described in the buildFile are appended to components in the base DataArchive
 */
public class ModuleExtension extends AbstractBuildable implements GameComponent, PluginsLoader.PluginElement, GpIdSupport {

  private static final Logger logger =
    LoggerFactory.getLogger(ModuleExtension.class);

  public static final String BASE_MODULE_NAME = "module"; //$NON-NLS-1$
  public static final String BASE_MODULE_VERSION = "moduleVersion"; //$NON-NLS-1$
  public static final String VERSION = "version"; //$NON-NLS-1$
  public static final String VASSAL_VERSION_CREATED = "vassalVersion"; //$NON-NLS-1$
  // NB The following key MUST sort before the other keys for universal modules to load
  public static final String UNIVERSAL = "anyModule"; //$NON-NLS-1$
  public static final String NEXT_PIECESLOT_ID = "nextPieceSlotId"; //$NON-NLS-1$
  public static final String EXTENSION_ID = "extensionId"; //$NON-NLS-1$
  public static final String DESCRIPTION = "description"; //NON-NLS

  private final DataArchive archive;
  private String version = "0.0"; //$NON-NLS-1$
  protected boolean universal = false;

  private String lastSave;
  private String vassalVersionCreated;
  private AbstractAction editAction;

  protected int nextGpId = 0;
  protected String extensionId = ""; //NON-NLS
  protected JTextField idDisplay;
  protected String description = ""; //NON-NLS

  public ModuleExtension(DataArchive archive) {
    this.archive = archive;
  }

  public String getVersion() {
    return version;
  }

  public DataArchive getDataArchive() {
    return archive;
  }

  public String getDescription() {
    return description;
  }

  public boolean getUniversal() {
    return universal;
  }

  public void build() {
    final AbstractMetaData data = MetaDataFactory.buildMetaData(archive.getArchive().getFile());

    final GameModule g = GameModule.getGameModule();
    g.getDataArchive().addExtension(archive);

    final boolean alreadyHadDockingPieceWindow = g.getPieceWindow() != null;

    // Record that we are currently building this Extension
    g.setGpIdSupport(this);

    final String buildFile = data instanceof ExtensionMetaData &&
      VersionUtils.compareVersions(VersionUtils.truncateToMinorVersion(
        data.getVassalVersion()), "3.5" // NON-NLS
      ) < 0 ? GameModule.BUILDFILE_OLD : GameModule.BUILDFILE;

    try (BufferedInputStream in = new BufferedInputStream(archive.getInputStream(buildFile))) {
      try {
        final Document doc = Builder.createDocument(in);
        if (doc != null) {
          build(doc.getDocumentElement());
        }
      }
      catch (IOException e) {
        logger.error("Error while loading XML data from file {}", buildFile, e); //NON-NLS
        throw new ExtensionsLoader.LoadExtensionException(e);
      }
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      // This is not necessarily an error; it's normal with new extensions
      logger.info("File {} not found in archive", buildFile); //NON-NLS
    }
    catch (IOException e) {
      logger.error("Error while reading file {} from archive", buildFile, e); //NON-NLS
    }

    g.add(this);

    // If a dockable PieceWindow got registered, dock it now (since we're
    // sure the Chatter will already be safely docked by this point)
    if (!alreadyHadDockingPieceWindow) {
      final PieceWindow pw = g.getPieceWindow();
      if (pw != null) {
        pw.dockMe();
      }
    }

    g.getGameState().addGameComponent(this);

    if (archive instanceof ArchiveWriter) {
      lastSave = buildString();

      // Has an Extension Id been allocated yet?
      if (extensionId.length() == 0) {
        final String id = UUID.randomUUID().toString();
        extensionId = id.substring(id.length() - 3);
      }

      // Fix missing, duplicate and illegal GamePieceId's
      final GpIdChecker checker = new GpIdChecker(this);
      for (final Buildable b : getBuildables()) {
        checkGpIds(b, checker);
      }
      checker.fixErrors();
    }
  }

  protected void checkGpIds(Buildable b, GpIdChecker checker) {
    if (b instanceof PieceSlot) {
      checker.add((PieceSlot) b);
    }
    else if (b instanceof PrototypeDefinition) {
      checker.add((PrototypeDefinition) b);
    }
    else if (b instanceof ExtensionElement) {
      checkGpIds(((ExtensionElement) b).getExtension(), checker);
    }
    else if (b instanceof AbstractBuildable) {
      for (final Buildable buildable : ((AbstractBuildable) b).getBuildables()) {
        checkGpIds(buildable, checker);
      }
    }
  }


  /**
   * Allocate new gpid's to all PieceSlots defined in a Buildable and
   * all of it's children
   *
   */
  protected void updateGpIds() {
    for (final Buildable b : getBuildables()) {
      updateGpIds(b);
    }
  }

  protected void updateGpIds(Buildable b) {
    if (b instanceof PieceSlot) {
      ((PieceSlot) b).updateGpId();
    }
    else if (b instanceof ExtensionElement) {
      updateGpIds(((ExtensionElement) b).getExtension());
    }
    else if (b instanceof AbstractBuildable) {
      for (final Buildable buildable : ((AbstractBuildable) b).getBuildables()) {
        updateGpIds(buildable);
      }
    }
  }

  @Override
  public int getNextGpId() {
    return nextGpId;
  }

  @Override
  public void setNextGpId(int id) {
    nextGpId = id;
  }

  @Override
  public Command getRestoreCommand() {
    return new RegCmd(getName(), version);
  }

  @Override
  public void setup(boolean gameStarting) {
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      UNIVERSAL,
      VERSION,
      DESCRIPTION,
      BASE_MODULE_NAME,
      BASE_MODULE_VERSION,
      VASSAL_VERSION_CREATED,
      NEXT_PIECESLOT_ID,
      EXTENSION_ID
    };
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
    throw new IllegalBuildException(
      Resources.getString("ModuleExtension.cannot_remove")); //$NON-NLS-1$
  }

  public boolean confirmExit() {
    boolean confirm = true;
    if (archive instanceof ArchiveWriter && !buildString().equals(lastSave)) {
      switch (JOptionPane.showConfirmDialog(
        GameModule.getGameModule().getPlayerWindow(),
        Resources.getString("ModuleExtension.save_extension"), //$NON-NLS-1$
        "", JOptionPane.YES_NO_CANCEL_OPTION)) { //$NON-NLS-1$
      case JOptionPane.YES_OPTION:
        try {
          save();
        }
        // FIXME: review error message
        catch (IOException e) {
          confirm = false;
        }
        break;
      case JOptionPane.CANCEL_OPTION:
        confirm = false;
      }

    }
    return confirm;
  }

  @Override
  public String getAttributeValueString(String key) {
    String s = null;
    if (BASE_MODULE_NAME.equals(key)) {
      s = GameModule.getGameModule().getGameName();
    }
    else if (BASE_MODULE_VERSION.equals(key)) {
      s = GameModule.getGameModule().getGameVersion();
    }
    else if (VERSION.equals(key)) {
      s = version;
    }
    else if (VASSAL_VERSION_CREATED.equals(key)) {
      s = vassalVersionCreated;
    }
    else if (UNIVERSAL.equals(key)) {
      s = String.valueOf(universal);
    }
    else if (NEXT_PIECESLOT_ID.equals(key)) {
      s = String.valueOf(nextGpId);
    }
    else if (EXTENSION_ID.equals(key)) {
      s = extensionId;
    }
    else if (DESCRIPTION.equals(key)) {
      s = description;
    }
    return s;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (BASE_MODULE_NAME.equals(key)) {
      if (!universal && !GameModule.getGameModule().getGameName().equals(value)) {
        throw new ExtensionsLoader.LoadExtensionException(
          Resources.getString("ModuleExtension.extension_built", getName(), value)); //$NON-NLS-1$
      }
    }
    else if (BASE_MODULE_VERSION.equals(key)) {
      final String version = (String) value;
      if (!universal && VersionUtils.compareVersions(GameModule.getGameModule().getGameVersion(), version) < 0) {
        GameModule.getGameModule().warn(
            Resources.getString("ModuleExtension.wrong_module_version", //NON-NLS
                getName(), version, GameModule.getGameModule().getGameVersion(),
                               GameModule.getGameModule().getGameName()));
      }
    }
    else if (VASSAL_VERSION_CREATED.equals(key)) {
      vassalVersionCreated = (String) value;
      final String runningVersion = Info.getVersion();
      if (VersionUtils.compareVersions(vassalVersionCreated, runningVersion) > 0) {
        GameModule.getGameModule().warn(
          Resources.getString(
            "ModuleExtension.wrong_vassal_version", //$NON-NLS-1$
            getName(),
            value,
            runningVersion));
      }
    }
    else if (VERSION.equals(key)) {
      version = (String) value;
    }
    else if (UNIVERSAL.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      universal = (Boolean) value;
    }
    else if (NEXT_PIECESLOT_ID.equals(key)) {
      try {
        nextGpId = Integer.parseInt((String) value);
      }
      // FIXME: review error message
      catch (NumberFormatException e) {
      }
    }
    else if (EXTENSION_ID.equals(key)) {
      extensionId = (String) value;
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
  }

  public String getExtensionId() {
    return extensionId;
  }

  /**
   * Generate a new Unique GamePiece Id
   */
  @Override
  public String generateGpId() {
    return extensionId + ":" + nextGpId++;
  }

  @Override
  public void addTo(Buildable parent) {
  }

  public String getName() {
    String name = "Extension"; //NON-NLS
    if (archive != null) {
      name = archive.getName();
      int index = name.lastIndexOf(File.separatorChar);
      if (index < name.length()) {
        name = name.substring(index + 1);
      }
      index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index);
      }
    }
    return name;
  }
  
  protected void write(boolean saveAs) throws IOException {
    vassalVersionCreated = Info.getVersion();
    if (archive instanceof ArchiveWriter) {
      final ArchiveWriter w = (ArchiveWriter) archive;

      try {
        (new ExtensionMetaData(this)).save(w);
      }
      // FIXME: review error message
      catch (IOException e) {
        logger.error("", e); //NON-NLS
      }

      final String save = buildString();
      w.addFile(GameModule.BUILDFILE,
                new ByteArrayInputStream(save.getBytes(StandardCharsets.UTF_8)));
      w.removeFile(GameModule.BUILDFILE_OLD);

      if (saveAs) w.saveAs(true);
      else w.save(true);

      GameModule.getGameModule().warn(Resources.getString("Editor.ExtensionEditor.saved", w.getArchive().getFile().getName()));

      lastSave = save;
    }
    else {
      throw new IOException("Read-only extension"); //NON-NLS
    }
  }

  public void save() throws IOException {
    write(false);
  }

  public void saveAs() throws IOException {
    write(true);
  }

  public void remove(ExtensionElement el) {
    buildComponents.remove(el);
  }

  public Action getEditAction(final JDialog d) {
    if (editAction == null) {
      d.setName(getName());
      d.setTitle(getName());
      d.setLayout(new MigLayout("ins panel"));

      final ComponentConfigPanel panel = new ComponentConfigPanel();
      panel.setBorder(BorderFactory.createEtchedBorder());


      final StringConfigurer config = new StringConfigurer(VERSION, "", version);
      panel.add("Editor.ExtensionEditor.version", config);

      final StringConfigurer dconfig = new StringConfigurer(DESCRIPTION, "", description);
      panel.add("Editor.description_label", dconfig);

      /*
       * The Extension id should not normally be changed once saved games
       * have been created. Display a dialog with warnings.
       */
      final JPanel idBox = new JPanel(new MigLayout("ins 0"));

      idDisplay = new JTextField(12);
      idDisplay.setText(extensionId);
      idDisplay.setEditable(false);
      idBox.add(idDisplay);
      final JButton change = new JButton(Resources.getString("Editor.ExtensionEditor.change_button"));
      change.addActionListener(e -> {
        final String s = (String)JOptionPane.showInputDialog(
            GameModule.getGameModule().getPlayerWindow(),
            Resources.getString("Editor.ExtensionEditor.change_warning"),
            "", //NON-NLS
            JOptionPane.WARNING_MESSAGE,
            null,
            null,
            getExtensionId());
        if (s != null && ! s.equals(getExtensionId())) {
          extensionId = s;
          updateGpIds();
          idDisplay.setText(getExtensionId());
        }
      });
      idBox.add(change);
      panel.add("Editor.ExtensionEditor.extension_id", idBox);

      final BooleanConfigurer uconfig = new BooleanConfigurer(UNIVERSAL, "", universal);
      panel.add("Editor.ExtensionEditor.universal_checkbox", uconfig);

      final JPanel b = new JPanel(new MigLayout("ins 0", "push[][]push"));
      final JButton ok = new JButton(Resources.getString("General.save"));
      ok.addActionListener(e -> {
        setAttribute(VERSION, config.getValue());
        setAttribute(DESCRIPTION, dconfig.getValue());
        setAttribute(UNIVERSAL, uconfig.getValue());
        d.dispose();
      });
      b.add(ok);
      final JButton cancel = new JButton(Resources.getString("General.cancel"));
      cancel.addActionListener(e -> d.dispose());
      b.add(cancel);

      // Default actions on Enter/ESC
      SwingUtils.setDefaultButtons(d.getRootPane(), ok, cancel);

      d.add(panel, "wrap");
      d.add(b, "span 2,align center");

      d.pack();
      d.setLocationRelativeTo(d.getParent());
      editAction = new AbstractAction() {
        private static final long serialVersionUID = 1L;

        @Override
        public void actionPerformed(ActionEvent e) {
          config.setValue(getAttributeValueString(VERSION));
          dconfig.setValue(getAttributeValueString(DESCRIPTION));
          uconfig.setValue(getAttributeValueString(UNIVERSAL));
          idDisplay.setText(extensionId);
          d.setVisible(true);
        }
      };
      final URL iconURL = getClass().getResource("/images/Edit16.gif"); //$NON-NLS-1$
      if (iconURL != null) {
        editAction.putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
      }
      else {
        editAction.putValue(Action.NAME, Resources.getString("General.edit"));
      }
      editAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("Editor.ExtensionEditor.extension_properties"));
    }
    return editAction;
  }

  /**
   * A command that verifies that a certain extension has been loaded
   */
  public static class RegCmd extends Command {
    private final String name;
    private final String version;

    public RegCmd(String name, String version) {
      this.name = name;
      this.version = version;
    }

    public String getName() {
      return name;
    }

    public String getVersion() {
      return version;
    }

    @Override
    protected void executeCommand() {
      boolean containsExtension = false;
      for (final ModuleExtension ext :
           GameModule.getGameModule().getComponentsOf(ModuleExtension.class)) {
        if (ext.getName().equals(name)) {
          containsExtension = true;
          if (VersionUtils.compareVersions(ext.getVersion(), version) > 0) {
            GameModule.getGameModule().warn(getVersionErrorMsg(ext.getVersion()));
          }
          break;
        }
      }
      if (!containsExtension) {
        GameModule.getGameModule().warn(getNotLoadedMsg());
      }
    }

    protected String getVersionErrorMsg(String v) {
      return Resources.getString("ModuleExtension.wrong_extension_version", //$NON-NLS-1$
          version, name, v);
    }

    protected String getNotLoadedMsg() {
      return Resources.getString("ModuleExtension.load_extension", name, new ExtensionsManager(GameModule.getGameModule()).getExtensionsDirectory(false)); //$NON-NLS-1$
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }
}
