/*
 *
 * Copyright (c) 2020 by Vassalengine.org, Brian Reynolds
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

package VASSAL.preferences;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;

import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;

/**
 * A set of global preferences shared by all modules. This superclass exists primarily to make sure that the
 * Global Preferences file gets written whenever a preference setting is changed.
 */
public class GlobalPrefs extends Prefs {
  private boolean disableAutoWrite = false; // If our auto-write function has been temporarily disabled

  private static final org.slf4j.Logger log = LoggerFactory.getLogger(GameModule.class);

  public GlobalPrefs(PrefsEditor editor, String name) {
    super(editor, new File(Info.getPrefsDir(), sanitize(name)));
  }

  public GlobalPrefs(PrefsEditor editor, File file) {
    super(editor, file);
  }

  /**
   * @return true if our auto-write function has been temporarily disabled.
   */
  @Override
  public boolean isDisableAutoWrite() {
    return disableAutoWrite;
  }

  /**
   * @param b true if auto-write function should be temporarily disabled; false to re-enable it.
   */
  @Override
  public void setDisableAutoWrite(boolean b) {
    disableAutoWrite = b;
  }

  /**
   * Save the global preferences
   */
  public void saveGlobal() {
    try {
      save();
    }
    catch (IOException e) {
      log.error(Resources.getString("GlobalPrefs.failed_to_write"), e);
      final GameModule gm = GameModule.getGameModule();
      if (gm != null) {
        gm.warn(Resources.getString("GlobalPrefs.failed_to_write"));
      }
    }
  }

  /**
   * Adds the preference, and adds a listener for property changes, so that global preference file will be
   * automatically written after any change.
   *
   * @param category the tab under which to add the Configurer's controls in the editor window. If null, do not add controls.
   * @param o Configurer for the preference
   * @param prompt If non-null and the value was not read from the preferences file on initialization
   */
  @Override
  public void addOption(String category, Configurer o, String prompt) {
    super.addOption(category, o, prompt);
    o.addPropertyChangeListener(evt -> {
      if (!isDisableAutoWrite()) {
        saveGlobal();
      }
    });
  }
}