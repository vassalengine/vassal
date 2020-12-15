/*
 * Copyright (c) 2020 by Joel Uckelman
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

package VASSAL.launch;

import java.io.File;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.tools.io.ProcessLauncher;

public class ModuleManagerUpdateHelper {

  private static final Logger logger =
    LoggerFactory.getLogger(ModuleManagerUpdateHelper.class);

  private ModuleManagerUpdateHelper() {}

  public static void sendUpdate(File f) {
    try {
      new ProcessLauncher().launch(
        Info.getJavaBinPath().getAbsolutePath(),
        "-classpath", //NON-NLS
        System.getProperty("java.class.path"),
        "-Duser.dir=" + System.getProperty("user.dir"), //NON-NLS
        "VASSAL.launch.ModuleManager",
        "--update",
        f.toString()
      );
    }
    catch (IOException e) {
      logger.error("Failed to notify Module Manager of update", e);
    }
  }
}
