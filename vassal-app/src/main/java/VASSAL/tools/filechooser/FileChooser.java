/*
 *
 * Copyright (c) 2006-2009 by Joel Uckelman
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

import java.awt.Component;
import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.i18n.Resources;
import org.apache.commons.lang3.SystemUtils;

import VASSAL.configure.DirectoryConfigurer;

/**
 * FileChooser provides a wrapper for {@link javax.swing.JFileChooser} and
 * {@link java.awt.FileDialog}, selecting whichever is preferred on the
 * user's OS. <code>FileChooser</code>'s methods mirror those of
 * <code>JFileChooser</code>.
 *
 * @author Joel Uckelman
 */
public abstract class FileChooser {
  protected Component parent;
  protected DirectoryConfigurer prefs;
  public static final int APPROVE_OPTION = JFileChooser.APPROVE_OPTION;
  public static final int CANCEL_OPTION = JFileChooser.CANCEL_OPTION;
  public static final int ERROR_OPTION = JFileChooser.ERROR_OPTION;
  public static final int FILES_ONLY = JFileChooser.FILES_ONLY;
  public static final int DIRECTORIES_ONLY = JFileChooser.DIRECTORIES_ONLY;

  protected FileChooser(Component parent, DirectoryConfigurer pref) {
    this.parent = parent;
    this.prefs = pref;
  }

  public static FileChooser createFileChooser(Component parent) {
    return createFileChooser(parent, null);
  }

  public static FileChooser createFileChooser(Component parent, DirectoryConfigurer prefs) {
    return createFileChooser(parent, prefs, FILES_ONLY);
  }

  /**
   * Creates a FileChooser appropriate for the user's OS.
   *
   * @param parent
   *          The Component over which the FileChooser should appear.
   * @param prefs
   *          A FileConfigure that stores the preferred starting directory of the FileChooser in preferences
   */
  public static FileChooser createFileChooser(Component parent, DirectoryConfigurer prefs, int mode) {
    final FileChooser fc;
    if (SystemUtils.IS_OS_MAC) {
      // Mac has a good native file dialog
      System.setProperty("apple.awt.fileDialogForDirectories", String.valueOf(mode == DIRECTORIES_ONLY));
      fc = new NativeFileChooser(parent, prefs, mode);
    }
    else if (mode == FILES_ONLY) {
      // Windows/Linux have a good native file dialog, but it doesn't support selecting directories
      fc = new NativeFileChooser(parent, prefs, mode);
    }
    else {
      // Use Swing's dialog for selecting directories on non-Macs
      fc = new SwingFileChooser(parent, prefs, mode);
    }
    return fc;
  }

  public abstract File getCurrentDirectory();

  public abstract void setCurrentDirectory(File dir);

  public abstract void rescanCurrentDirectory();

  public abstract File getSelectedFile();

  public abstract void setSelectedFile(File file);

  public abstract String getDialogTitle();

  public abstract void setDialogTitle(String title);

  public abstract int showOpenDialog(Component parent);

  public abstract int showSaveDialog(Component parent);

  public abstract FileFilter getFileFilter();

  public abstract void setFileFilter(FileFilter filter);

  public abstract void addChoosableFileFilter(FileFilter filter);

  public abstract boolean removeChoosableFileFilter(FileFilter filter);

  public abstract void resetChoosableFileFilters();

  /**
   * Selects <tt>filename.vsav</tt> if <tt>filename.foo</tt> is selected.
   */
  public void selectDotSavFile() {
    final File file = getSelectedFile();
    if (file != null) {
      String name = file.getPath();
      final int index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index) + ".vsav";
        setSelectedFile(new File(name));
      }
    }
  }

  /**
   * Same as {@link #showOpenDialog(Component)}, but uses the <tt>parent</tt> set on creation of this FileDialog.
   */
  public int showOpenDialog() {
    return showOpenDialog(parent);
  }

  /**
   * Same as {@link #showSaveDialog(Component)}, but uses the <tt>parent</tt> set on creation of this FileDialog.
   */
  public int showSaveDialog() {
    return showSaveDialog(parent);
  }

  protected void updateDirectoryPreference() {
    if (prefs != null &&
        getCurrentDirectory() != null &&
        !getCurrentDirectory().equals(prefs.getFileValue())) {
      prefs.setValue(getCurrentDirectory());
    }
  }

  private static class SwingFileChooser extends FileChooser {
    private final JFileChooser fc = new JFileChooser();

    public SwingFileChooser(Component parent, DirectoryConfigurer prefs, int mode) {
      super(parent, prefs);
      if (prefs != null && prefs.getFileValue() != null) {
        setCurrentDirectory(prefs.getFileValue());
      }
      if (mode == DIRECTORIES_ONLY) {
        setFileFilter(new DirectoryFileFilter());
      }
      fc.setFileSelectionMode(mode);
    }

    @Override
    public File getCurrentDirectory() {
      return fc.getCurrentDirectory();
    }

    @Override
    public void setCurrentDirectory(File dir) {
      fc.setCurrentDirectory(dir);
    }

    @Override
    public void rescanCurrentDirectory() {
      fc.rescanCurrentDirectory();
    }

    @Override
    public File getSelectedFile() {
      return fc.getSelectedFile();
    }

    @Override
    public void setSelectedFile(File file) {
      fc.setSelectedFile(file);
    }

    public int getFileSelectionMode() {
      return fc.getFileSelectionMode();
    }

    public void setFileSelectionMode(int mode) {
      fc.setFileSelectionMode(mode);
    }

    @Override
    public String getDialogTitle() {
      return fc.getDialogTitle();
    }

    @Override
    public void setDialogTitle(String title) {
      fc.setDialogTitle(title);
    }

    @Override
    public int showOpenDialog(Component parent) {
      final int value = fc.showOpenDialog(parent);
      updateDirectoryPreference();
      return value;
    }

    @Override
    public int showSaveDialog(Component parent) {
      int value = fc.showSaveDialog(parent);
      if (value == APPROVE_OPTION
          && getSelectedFile().exists()
          && JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(parent,
                                                                    Resources.getString("Editor.FileChooser.overwrite", getSelectedFile().getName()),
                                                                    Resources.getString("Editor.FileChooser.exists"),
                                                                    JOptionPane.YES_NO_OPTION)) {
        value = CANCEL_OPTION;
      }
      updateDirectoryPreference();
      return value;
    }

    @Override
    public FileFilter getFileFilter() {
      final javax.swing.filechooser.FileFilter ff = fc.getFileFilter();
      return ff instanceof FileFilter ? (FileFilter) ff : null;
    }

    @Override
    public void setFileFilter(FileFilter filter) {
      fc.setFileFilter(filter);
    }

    @Override
    public void addChoosableFileFilter(FileFilter filter) {
      fc.addChoosableFileFilter(filter);
    }

    @Override
    public boolean removeChoosableFileFilter(FileFilter filter) {
      return fc.removeChoosableFileFilter(filter);
    }

    @Override
    public void resetChoosableFileFilters() {
      fc.resetChoosableFileFilters();
    }
  }

  private static class NativeFileChooser extends FileChooser {
    private File cur;
    private String title;
    private FileFilter filter;
    private int mode;

    public NativeFileChooser(Component parent,
                             DirectoryConfigurer prefs, int mode) {
      super(parent, prefs);

      if (prefs != null && prefs.getFileValue() != null) {
        setCurrentDirectory(prefs.getFileValue());
      }

      this.mode = mode;

      if (mode == DIRECTORIES_ONLY) {
        setFileFilter(new DirectoryFileFilter());
      }
    }

    @Override
    public File getCurrentDirectory() {
      return cur == null ? null : cur.getParentFile();
    }

    @Override
    public void setCurrentDirectory(File dir) {
      cur = dir;
    }

    @Override
    public void rescanCurrentDirectory() {
    }

    @Override
    public File getSelectedFile() {
      return cur;
    }

    @Override
    public void setSelectedFile(File file) {
      cur = file;
    }

    public int getFileSelectionMode() {
      return mode;
    }

    public void setFileSelectionMode(int mode) {
      this.mode = mode;
    }

    @Override
    public String getDialogTitle() {
      return title;
    }

    @Override
    public void setDialogTitle(String title) {
      this.title = title;
    }

    protected FileDialog awt_file_dialog_init(Component parent) {
      final FileDialog fd;

      if (parent == null) {
        fd = new FileDialog((Frame) null, title);
      }
      else if (parent instanceof Dialog) {
        fd = new FileDialog((Dialog) parent, title);
      }
      else if (parent instanceof Frame) {
        fd = new FileDialog((Frame) parent, title);
      }
      else {
        final Dialog d =
          (Dialog) SwingUtilities.getAncestorOfClass(Dialog.class, parent);
        if (d != null) {
          fd = new FileDialog(d, title);
        }
        else {
          final Frame f =
            (Frame) SwingUtilities.getAncestorOfClass(Frame.class, parent);
          if (f != null) {
            fd = new FileDialog(f, title);
          }
          else {
            // should be impossible, parent is not in a dialog or frame!
            throw new IllegalArgumentException(
              "parent is contained in neither a Dialog nor a Frame");
          }
        }
      }

      fd.setModal(true);
      fd.setFilenameFilter(filter);
      if (cur != null) {
        if (cur.isDirectory())
          fd.setDirectory(cur.getPath());
        else {
          fd.setDirectory(cur.getParent());
          fd.setFile(cur.getName());
        }
      }
      return fd;
    }

    @Override
    public int showOpenDialog(Component parent) {
      final FileDialog fd = awt_file_dialog_init(parent);
      fd.setMode(FileDialog.LOAD);
      fd.setVisible(true);

      final int value;
      if (fd.getFile() != null) {
        cur = new File(fd.getDirectory(), fd.getFile());
        value = FileChooser.APPROVE_OPTION;
      }
      else {
        value = FileChooser.CANCEL_OPTION;
      }
      updateDirectoryPreference();
      return value;
    }

    @Override
    public int showSaveDialog(Component parent) {
      final FileDialog fd = awt_file_dialog_init(parent);
      fd.setMode(FileDialog.SAVE);
      fd.setVisible(true);

      final int value;
      if (fd.getFile() != null) {
        cur = new File(fd.getDirectory(), fd.getFile());
        value = FileChooser.APPROVE_OPTION;
      }
      else {
        value = FileChooser.CANCEL_OPTION;
      }
      updateDirectoryPreference();
      return value;
    }

    @Override
    public FileFilter getFileFilter() {
      return filter;
    }

    @Override
    public void setFileFilter(FileFilter filter) {
      this.filter = filter;
    }

    @Override
    public void addChoosableFileFilter(FileFilter filter) {
    }

    @Override
    public boolean removeChoosableFileFilter(FileFilter filter) {
      return false;
    }

    @Override
    public void resetChoosableFileFilters() {
    }
  }
}