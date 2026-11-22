/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A temporary symbolic link to the java executable, named after a module.
 *
 * <p>The macOS Dock labels a process that isn't an app bundle by the file
 * name of its executable, ignoring {@code -Xdock:name}, so every Player
 * showed as "java" when hovered over. Starting the Player through a link
 * named after the module makes the Dock show the module's name. The link is
 * a few bytes in VASSAL's temporary folder, and is removed when the Player
 * exits.</p>
 */
final class DockNameLink {
  private static final Logger logger = LoggerFactory.getLogger(DockNameLink.class);

  // most file systems allow 255 bytes in a name; leave room for UTF-8
  private static final int MAX_NAME_CHARS = 80;

  private DockNameLink() {
  }

  /**
   * @param name a module name
   * @return the name made safe for use as a file name: no path separators,
   *  colons or control characters, not hidden, and not too long
   */
  static String fileName(String name) {
    final StringBuilder sb = new StringBuilder();
    if (name != null) {
      name.codePoints().forEach(c -> {
        if (c == '/' || c == ':' || c == '\\') {
          sb.append('-');
        }
        else if (!Character.isISOControl(c)) {
          sb.appendCodePoint(c);
        }
      });
    }

    String s = sb.toString().trim();
    while (s.startsWith(".")) {
      s = s.substring(1).trim();
    }
    if (s.length() > MAX_NAME_CHARS) {
      s = s.substring(0, s.offsetByCodePoints(0, s.codePointCount(0, MAX_NAME_CHARS))).trim();
    }
    return s.isEmpty() ? "VASSAL" : s; //NON-NLS
  }

  /**
   * Creates a link named after the module to the java executable, in its own
   * new folder under {@code parent}.
   *
   * @param parent the folder to create it in
   * @param name the module name
   * @param java the java executable
   * @return the link, or null if it couldn't be made, in which case java
   *  should be started directly
   */
  static Path create(Path parent, String name, Path java) {
    try {
      final Path dir = Files.createTempDirectory(parent, "dock_"); //NON-NLS
      return Files.createSymbolicLink(dir.resolve(fileName(name)), java.toAbsolutePath());
    }
    catch (IOException | UnsupportedOperationException | SecurityException e) {
      logger.warn("Unable to make the Dock name link; the Dock will show \"java\"", e); //NON-NLS
      return null;
    }
  }

  /**
   * Removes a link made by {@link #create} and its folder. Does nothing if
   * {@code link} is null.
   */
  static void delete(Path link) {
    if (link == null) {
      return;
    }
    try {
      Files.deleteIfExists(link);
      Files.deleteIfExists(link.getParent());
    }
    catch (IOException e) {
      logger.warn("Unable to remove the Dock name link {}", link, e); //NON-NLS
    }
  }
}
