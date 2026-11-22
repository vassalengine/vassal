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

package VASSAL.tools.filechooser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileSystemView;

import org.junit.jupiter.api.Test;

public class FileChooserTest {

  private static final String USE_SHELL_FOLDER = "FileChooser.useShellFolder"; //NON-NLS

  /**
   * A chooser which cannot be built must come back built another way.
   *
   * <p>On a Windows machine whose shell will not answer, {@code new
   * JFileChooser()} throws a NullPointerException out of the JDK while it is
   * enumerating the shell's folders, and that cost the user the whole
   * application: the global preferences build one during start-up. See
   * vassalengine/vassal#15297.</p>
   */
  @Test
  public void aChooserWhichWillNotBuildFallsBackToOneWithoutTheShell() {
    final JFileChooser fc = FileChooser.createJFileChooser(() -> {
      throw new NullPointerException(
        "Cannot invoke \"sun.awt.shell.Win32ShellFolder2.getIShellFolder()\" " +
        "because the return value of " +
        "\"sun.awt.shell.Win32ShellFolderManager2.getNetwork()\" is null"
      );
    });

    assertInstanceOf(PlainFileSystemView.class, fc.getFileSystemView());

    // This is the question FilePane.usesShellFolder() asks: a view which is
    // not the platform's own singleton turns the shell off throughout the
    // dialog, the Windows places bar -- which is what enumerates Network --
    // included.
    assertNotEquals(FileSystemView.getFileSystemView(), fc.getFileSystemView());

    // and the client property it asks first
    assertEquals(Boolean.FALSE, fc.getClientProperty(USE_SHELL_FOLDER));
  }

  /**
   * The JDK's own file chooser code catches InternalError from the shell in
   * two places, so that is a failure it expects of itself.
   */
  @Test
  public void anErrorFromTheShellIsCaughtAsWell() {
    final JFileChooser fc = FileChooser.createJFileChooser(
      () -> {
        throw new InternalError("shell is unwell"); //NON-NLS
      }
    );
    assertInstanceOf(PlainFileSystemView.class, fc.getFileSystemView());
  }

  /** A machine whose shell answers normally keeps the full dialog. */
  @Test
  public void aWorkingChooserIsHandedBackUntouched() {
    final JFileChooser built = new JFileChooser();
    final JFileChooser fc = FileChooser.createJFileChooser(() -> built);

    assertSame(built, fc);
    assertEquals(FileSystemView.getFileSystemView(), fc.getFileSystemView());
  }
}
