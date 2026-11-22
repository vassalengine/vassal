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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class PlainFileSystemViewTest {

  /** The package everything the shell hands back lives in. */
  private static final String SHELL_PACKAGE = "sun.awt.shell."; //NON-NLS

  private static void assertNoShellFolders(File[] files) {
    for (final File f : files) {
      assertFalse(
        f.getClass().getName().startsWith(SHELL_PACKAGE),
        () -> "came from the shell: " + f.getClass().getName() //NON-NLS
      );
    }
  }

  /**
   * The point of the class: nothing it returns comes from the shell.
   *
   * <p>This is not a Windows-only property. The default view goes through
   * {@code ShellFolder} on every platform -- the objects are
   * {@code sun.awt.shell.DefaultShellFolder} rather than
   * {@code Win32ShellFolder2} elsewhere -- so a view which quietly stopped
   * overriding {@code getFiles()} fails this on the build machines too, not
   * only on the ones it is meant for.</p>
   */
  @Test
  public void listsFilesWithoutTheShell(@TempDir Path dir) throws IOException {
    Files.createFile(dir.resolve("b.vmod"));
    Files.createFile(dir.resolve("a.vsav"));
    Files.createDirectory(dir.resolve("sub"));

    final File[] files = new PlainFileSystemView().getFiles(dir.toFile(), true);

    assertNoShellFolders(files);

    final List<String> names = Arrays.stream(files)
      .map(File::getName)
      .sorted()
      .collect(Collectors.toList());
    assertEquals(List.of("a.vsav", "b.vmod", "sub"), names);
  }

  @Test
  public void hiddenFilesAreShownOnlyWhenAskedFor(@TempDir Path dir)
                                                  throws IOException {
    Files.createFile(dir.resolve("visible.vmod"));
    final File hidden = Files.createFile(dir.resolve(".hidden")).toFile();

    final PlainFileSystemView fsv = new PlainFileSystemView();

    // nothing to check if this platform does not call that file hidden
    if (hidden.isHidden()) {
      assertEquals(1, fsv.getFiles(dir.toFile(), true).length);
    }
    assertEquals(2, fsv.getFiles(dir.toFile(), false).length);
  }

  /** A directory which cannot be listed is empty, not fatal. */
  @Test
  public void anUnlistableDirectoryIsEmpty(@TempDir Path dir) {
    final PlainFileSystemView fsv = new PlainFileSystemView();
    assertEquals(0, fsv.getFiles(null, true).length);
    assertEquals(0, fsv.getFiles(dir.resolve("nope").toFile(), true).length);
  }

  @Test
  public void rootsAreRealRootsAndNotFromTheShell() {
    final PlainFileSystemView fsv = new PlainFileSystemView();
    final File[] roots = fsv.getRoots();

    assertNoShellFolders(roots);
    assertTrue(roots.length > 0, "every platform has at least one root"); //NON-NLS

    for (final File root : roots) {
      assertTrue(fsv.isFileSystemRoot(root), () -> root + " is a root"); //NON-NLS
      assertNull(fsv.getParentDirectory(root), () -> root + " has no parent"); //NON-NLS
    }

    // the directory combo box is given the same list, not the shell's
    assertNoShellFolders(fsv.getChooserComboBoxFiles());
  }

  @Test
  public void parentOfAFileIsItsDirectory(@TempDir Path dir)
                                          throws IOException {
    final File f = Files.createFile(dir.resolve("x.vmod")).toFile();
    final PlainFileSystemView fsv = new PlainFileSystemView();

    assertEquals(dir.toFile(), fsv.getParentDirectory(f));
    assertNull(fsv.getParentDirectory(null));
    assertNull(fsv.getParentDirectory(dir.resolve("nope").toFile()));
  }

  @Test
  public void aNewFolderIsCreatedAndNotCollidedWith(@TempDir Path dir)
                                                    throws IOException {
    final PlainFileSystemView fsv = new PlainFileSystemView();

    final File first = fsv.createNewFolder(dir.toFile());
    assertTrue(first.isDirectory());

    final File second = fsv.createNewFolder(dir.toFile());
    assertTrue(second.isDirectory());
    assertNotEquals(first, second, "the second folder got its own name"); //NON-NLS

    assertThrows(IOException.class, () -> fsv.createNewFolder(null));
  }

  /** No link resolution: that is a thing only the shell can do. */
  @Test
  public void nothingIsALink(@TempDir Path dir) throws IOException {
    final File f = Files.createFile(dir.resolve("x.vmod")).toFile();
    final PlainFileSystemView fsv = new PlainFileSystemView();

    assertFalse(fsv.isLink(f));
    assertNull(fsv.getLinkLocation(f));
    assertThrows(NullPointerException.class, () -> fsv.isLink(null));
  }

  @Test
  public void displayNameIsTheFileNameOrThePathOfARoot(@TempDir Path dir)
                                                       throws IOException {
    final File f = Files.createFile(dir.resolve("x.vmod")).toFile();
    final PlainFileSystemView fsv = new PlainFileSystemView();

    assertEquals("x.vmod", fsv.getSystemDisplayName(f));
    assertNull(fsv.getSystemDisplayName(null));

    for (final File root : fsv.getRoots()) {
      assertNotNull(fsv.getSystemDisplayName(root));
      assertFalse(fsv.getSystemDisplayName(root).isEmpty());
    }
  }
}
