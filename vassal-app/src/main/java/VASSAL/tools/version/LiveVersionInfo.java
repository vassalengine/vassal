/*
 * Copyright (c) 2021 by Joel Uckelman
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
package VASSAL.tools.version;

import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.IOUtils;

public class LiveVersionInfo implements VersionInfo {
  private static final String baseURL = "http://www.vassalengine.org/util/"; //NON-NLS

  private static final String currentRelease = "current-release"; //NON-NLS
  private static final String currentBeta = "current-beta"; //NON-NLS

  private String release = null;
  private String beta = null;

  @Override
  public String getRelease() throws IOException {
    if (release == null) {
      release = getVersion(baseURL + currentRelease);
    }
    return release;
  }

  @Override
  public String getBeta() throws IOException {
    if (beta == null) {
      beta = getVersion(baseURL + currentBeta);
    }
    return beta;
  }

  private String getVersion(String url) throws IOException {
    try (InputStream in = new URL(url).openStream()) {
      return IOUtils.toString(in, StandardCharsets.UTF_8).trim();
    }
  }
}
