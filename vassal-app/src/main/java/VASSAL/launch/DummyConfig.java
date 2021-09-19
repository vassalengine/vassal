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

import java.nio.file.Path;

public class DummyConfig implements Config {
  @Override
  public String getVersion() {
    return "1.2.3";
  }

  @Override
  public String getReportableVersion() {
    return "1.2";
  }

  @Override
  public Path getBaseDir() {
    return getTempDir();
  }

  @Override
  public Path getDocDir() {
    return getTempDir();
  }

  @Override
  public Path getCacheDir() {
    return getTempDir();
  }

  @Override
  public Path getConfDir() {
    return getTempDir();
  }

  @Override
  public Path getTempDir() {
    return Path.of(System.getProperty("java.io.tmpdir"));
  }

  @Override
  public Path getPrefsDir() {
    return getTempDir();
  }

  @Override
  public Path getErrorLogPath() {
    return getConfDir().resolve("errorLog-" + getVersion());
  }

  @Override
  public Path getJavaBinPath() {
    return Path.of(System.getProperty("java.home"), "bin", "java"); //NON-NLS
  }
}
