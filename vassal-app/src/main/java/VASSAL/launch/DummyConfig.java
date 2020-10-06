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
  public String getVersion() {
    return "3.4.3";
  }

  public String getReportableVersion() {
    return "3.4";
  }

  public int getInstanceID() {
    return -1;
  }

  public Path getBaseDir() {
    return getTempDir(); 
  }

  public Path getDocDir() {
    return getTempDir(); 
  }

  public Path getConfDir() {
    return getTempDir(); 
  }

  public Path getTempDir() {
    return Path.of(System.getProperty("java.io.tmpdir"));
  }

  public Path getPrefsDir() {
    return getTempDir(); 
  }

  public Path getErrorLogPath() {
    return getConfDir().resolve("errorLog-" + getVersion());
  }

  public Path getJavaBinPath() {
    return Path.of(System.getProperty("java.home"), "bin", "java");
  }
}
