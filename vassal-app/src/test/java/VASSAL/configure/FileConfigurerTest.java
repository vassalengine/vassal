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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.configure;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class FileConfigurerTest {

  /**
   * A FileConfigurer must not build its FileChooser until one is asked for.
   *
   * On Windows a JFileChooser enumerates the shell's folders as it is
   * constructed, and that can fail: when the "Network" shell folder is
   * unreachable the JDK's Win32ShellFolderManager2.getNetwork() returns null
   * and a NullPointerException comes back out. The global preferences build a
   * DirectoryConfigurer for the modules directory while the Module Manager
   * window is being constructed, so an eager chooser turned that into an
   * ExceptionInInitializerError and VASSAL could not start at all.
   *
   * @see <a href="https://github.com/vassalengine/vassal/issues/15297">#15297</a>
   */
  @Test
  public void directoryConfigurerDoesNotBuildChooserUntilAsked() {
    final DirectoryConfigurer c = new DirectoryConfigurer("modulesDir", null); //NON-NLS
    assertNull(c.fc, "FileChooser must not be created by the constructor"); //NON-NLS
  }

  /** The same for the plain file flavour. */
  @Test
  public void fileConfigurerDoesNotBuildChooserUntilAsked() {
    final FileConfigurer c = new FileConfigurer("someFile", null, (DirectoryConfigurer) null); //NON-NLS
    assertNull(c.fc, "FileChooser must not be created by the constructor"); //NON-NLS
  }
}
