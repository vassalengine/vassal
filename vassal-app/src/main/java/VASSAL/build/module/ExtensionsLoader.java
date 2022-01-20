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

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

public class ExtensionsLoader implements CommandEncoder {
  private static final Logger logger =
    LoggerFactory.getLogger(ExtensionsLoader.class);

  public static final String COMMAND_PREFIX = "EXT\t"; //$NON-NLS-1$
  // Preferences key for the list of extensions to load
  public static final String SPECIFY_DIR_IN_PREFS = "specifyExtensionDirInPrefs"; //$NON-NLS-1$
  public static final String EXTENSION_DIR = "extensionDIR"; //$NON-NLS-1$

  protected Set<String> loadedExtensions = new HashSet<>();
  protected Map<String, String> loadedIds = new HashMap<>();

  protected ExtensionsManager extMgr;
  protected ExtensionsManager globalExtMgr;

  public void addTo(GameModule mod) {
    extMgr = new ExtensionsManager(mod);
    globalExtMgr = new ExtensionsManager("ext"); //NON-NLS
    mod.addCommandEncoder(this);
    if ("true".equals(GlobalOptions.getInstance().getAttributeValueString(SPECIFY_DIR_IN_PREFS))) { //$NON-NLS-1$
      final DirectoryConfigurer config = new DirectoryConfigurer(EXTENSION_DIR, Resources.getString("ExtensionsLoader.extensions_directory")); //$NON-NLS-1$
      config.setValue((Object) null);
      GameModule.getGameModule().getPrefs().addOption(Resources.getString("ExtensionsLoader.extensions_tab"), config); //$NON-NLS-1$
      extMgr.setExtensionsDirectory(config.getFileValue());
      if (config.getFileValue() == null) {
        config.setValue(extMgr.getExtensionsDirectory(false).getAbsoluteFile());
      }
      config.addPropertyChangeListener(evt -> {
        extMgr.setExtensionsDirectory((File) evt.getNewValue());
        addExtensions();
      });
    }
    addExtensions();
    // Force the module checksum to be regenerated
    mod.getCrc(true);
  }

  protected void addExtensions() {
    for (final File ext : globalExtMgr.getActiveExtensions()) {
      if (!addExtension(ext)) {
        globalExtMgr.setActive(ext, false);
      }
    }
    for (final File ext : extMgr.getActiveExtensions()) {
      if (!addExtension(ext)) {
        GameModule.getGameModule().warn(Resources.getString("ExtensionsLoader.deactivating_extension", ext.getName()));
        extMgr.setActive(ext, false);
      }
    }
  }

  protected boolean addExtension(File extension) {
    logger.info("Loading extension " + extension); //NON-NLS
    final String extname = extension.getPath();
    boolean success = loadedExtensions.contains(extname);
    if (!success) {
      try {
        final ModuleExtension ext = createExtension(extname);
        ext.build();

        final String id = ext.getExtensionId();
        String idMsg = "";
        if (id.length() > 0) {
          for (final String loadedId : loadedIds.keySet()) {
            if (loadedId.equals(id)) {
              idMsg = Resources.getString("ExtensionsLoader.id_conflict", extension.getName(), id, loadedIds.get(id));
            }
          }
          loadedIds.put(id, extname);
        }

        final String msg = getLoadedMessage(ext.getName(), ext.getVersion());
        loadedExtensions.add(extname);
        GameModule.getGameModule().warn(msg);
        logger.info(msg);

        if (idMsg.length() > 0) {
          GameModule.getGameModule().warn(idMsg);
          logger.info(idMsg);
        }
        success = true;
      }
      catch (ZipException e) {
        // Not a zip file. Ignore.
      }
      catch (IOException | LoadExtensionException e) {
        reportBuildError(e, extension.getName());
      }
    }
    return success;
  }

  protected ModuleExtension createExtension(String extname)
                                            throws ZipException, IOException {
    return new ModuleExtension(new DataArchive(extname));
  }

  protected String getLoadedMessage(String name, String version) {
    return Resources.getString("ExtensionsLoader.extension_loaded", name, version); //$NON-NLS-1$
  }

  private void reportBuildError(Exception e, String name) {
    String msg = e.getMessage();
    if (msg == null || msg.length() == 0) {
      msg = e.getClass().getName();
      msg = msg.substring(msg.lastIndexOf('.'));
    }
    GameModule.getGameModule().warn(getErrorMessage(name, msg)); //$NON-NLS-1$
  }

  protected String getErrorMessage(String name, String msg) {
    return Resources.getString("ExtensionsLoader.unable_to_load", name, msg);
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(COMMAND_PREFIX)) {
      return null;
    }
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command.substring(COMMAND_PREFIX.length()), '\t');
    return new ModuleExtension.RegCmd(st.nextToken(), st.nextToken());
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof ModuleExtension.RegCmd)) {
      return null;
    }
    final ModuleExtension.RegCmd cmd = (ModuleExtension.RegCmd) c;
    final SequenceEncoder se = new SequenceEncoder('\t');
    se
      .append(cmd.getName())
      .append(cmd.getVersion());
    return COMMAND_PREFIX + se.getValue();
  }

  public static class LoadExtensionException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public LoadExtensionException() {
      super();
    }

    public LoadExtensionException(String message, Throwable cause) {
      super(message, cause);
    }

    public LoadExtensionException(String message) {
      super(message);
    }

    public LoadExtensionException(Throwable cause) {
      super(cause);
    }
  }
}
