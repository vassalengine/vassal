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
package VASSAL.tools.filechooser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.swing.Icon;
import javax.swing.UIManager;
import javax.swing.filechooser.FileSystemView;

/**
 * A {@link FileSystemView} built only on {@link java.io.File}, which never
 * asks the platform's shell anything.
 *
 * <p>The default view does ask it, and on Windows that can fail: when the
 * "Network" shell folder cannot be reached,
 * {@code Win32ShellFolderManager2.getNetwork()} returns null and
 * {@code Win32ShellFolder2.getFileSystemPath()} dereferences it without
 * checking. The exception comes back out of {@code new JFileChooser()}, which
 * enumerates the shell's folders as it is constructed. See
 * vassalengine/vassal#15297 and #15125.</p>
 *
 * <p>Installing a view on a {@link javax.swing.JFileChooser} is also what
 * turns the shell off everywhere else in the dialog: {@code
 * FilePane.usesShellFolder()} answers "is this chooser's view the platform's
 * own singleton?", and everything in {@code FilePane} and {@code
 * WindowsFileChooserUI} which reaches for a {@code ShellFolder} is behind
 * that test -- including the Windows places bar, which is the part that
 * enumerates Network in the first place.</p>
 *
 * <p>The cost is the shell's decoration: localised drive and folder names,
 * per-file icons, shortcut resolution and the places bar. A chooser is still
 * a working chooser without them, which is the trade this is here to make.
 * It is used only as a fallback, by
 * {@link FileChooser#createJFileChooser()}, so a machine whose shell answers
 * normally keeps the full dialog.</p>
 *
 * @author The VASSAL Development Team
 */
public class PlainFileSystemView extends FileSystemView {

  @Override
  public File createNewFolder(File containingDir) throws IOException {
    if (containingDir == null) {
      throw new IOException("Containing directory is null"); //NON-NLS
    }

    final String base = UIManager.getString("FileChooser.other.newFolder"); //NON-NLS
    final String name = base == null ? "NewFolder" : base; //NON-NLS

    File f = new File(containingDir, name);
    for (int i = 2; f.exists() && i < 100; ++i) {
      f = new File(containingDir, name + " (" + i + ")"); //NON-NLS
    }

    if (f.exists()) {
      throw new IOException("Directory already exists: " + f.getAbsolutePath()); //NON-NLS
    }

    if (!f.mkdirs()) {
      throw new IOException("Could not create directory: " + f.getAbsolutePath()); //NON-NLS
    }

    return f;
  }

  @Override
  public File[] getRoots() {
    final File[] roots = File.listRoots();
    if (roots == null) {
      return new File[0];
    }

    for (int i = 0; i < roots.length; ++i) {
      roots[i] = createFileSystemRoot(roots[i]);
    }
    return roots;
  }

  @Override
  public File getHomeDirectory() {
    return new File(System.getProperty("user.home"));
  }

  @Override
  public File getDefaultDirectory() {
    return getHomeDirectory();
  }

  @Override
  public boolean isFileSystemRoot(File dir) {
    return dir != null && dir.getAbsoluteFile().getParentFile() == null;
  }

  @Override
  public boolean isComputerNode(File dir) {
    return false;
  }

  @Override
  public File[] getFiles(File dir, boolean useFileHiding) {
    if (dir == null) {
      return new File[0];
    }

    final File[] names = dir.listFiles();
    if (names == null) {
      return new File[0];
    }

    final List<File> files = new ArrayList<>(names.length);
    for (final File f : names) {
      if (Thread.currentThread().isInterrupted()) {
        break;
      }

      if (!useFileHiding || !isHiddenFile(f)) {
        files.add(f);
      }
    }

    return files.toArray(new File[0]);
  }

  @Override
  public File getParentDirectory(File dir) {
    if (dir == null || !dir.exists()) {
      return null;
    }

    final File parent = dir.getAbsoluteFile().getParentFile();
    if (parent == null) {
      return null;
    }

    return isFileSystemRoot(parent) ? createFileSystemRoot(parent) : parent;
  }

  @Override
  public File[] getChooserComboBoxFiles() {
    return getRoots();
  }

  @Override
  public boolean isLink(File file) {
    Objects.requireNonNull(file, "file is null"); //NON-NLS
    return false;
  }

  @Override
  public File getLinkLocation(File file) throws FileNotFoundException {
    Objects.requireNonNull(file, "file is null"); //NON-NLS
    return null;
  }

  @Override
  public String getSystemDisplayName(File f) {
    if (f == null) {
      return null;
    }

    // a file system root has no name of its own, so show its path, e.g. "C:\"
    final String name = f.getName();
    return name.isEmpty() ? f.getPath() : name;
  }

  @Override
  public Icon getSystemIcon(File f) {
    return f == null ? null : UIManager.getIcon(
      f.isDirectory() ? "FileView.directoryIcon" : "FileView.fileIcon" //NON-NLS
    );
  }

  @Override
  public Icon getSystemIcon(File f, int width, int height) {
    if (width < 1 || height < 1) {
      throw new IllegalArgumentException("Icon size can not be below 1"); //NON-NLS
    }

    if (f == null) {
      throw new IllegalArgumentException("The file reference should not be null"); //NON-NLS
    }

    return f.exists() ? getSystemIcon(f) : null;
  }
}
