/*
 * $Id$
 *
 * Copyright (c) 2006 by Joel Uckelman
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
package VASSAL.tools;

import java.awt.Component;
import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;

import VASSAL.tools.FileFilter;

/**
 *  FileChooser provides a wrapper for {@link javax.swing.JFileChooser}
 *  and {@link java.awt.FileDialog}, selecting whichever is preferred
 *  on the user's OS. FileChooser's methods mirror those of JFileChooser.
 * 
 * @author Joel Uckelman
 */
public abstract class FileChooser {
   protected Component parent;

   public final static int APPROVE_OPTION = JFileChooser.APPROVE_OPTION;
   public final static int CANCEL_OPTION = JFileChooser.CANCEL_OPTION;
   public final static int ERROR_OPTION = JFileChooser.ERROR_OPTION;

   public final static int FILES_ONLY = JFileChooser.FILES_ONLY;
   public final static int DIRECTORIES_ONLY = JFileChooser.DIRECTORIES_ONLY;
   public final static int FILES_AND_DIRECTORIES =
                                         JFileChooser.FILES_AND_DIRECTORIES;

   private FileChooser(Component parent) {
      this.parent = parent;
   }

   /**
    * Creates a FileChooser appropriate for the user's OS.
    *
    * @param parent The Component over which the FileChooser should appear.
    */
   public static FileChooser createFileChooser(Component parent) {
      // determine what OS this is
      String os = System.getProperty("os.name");
      if (os != null && (os.startsWith("Windows") ||
                         os.startsWith("Mac OS"))) {
         // use a native file chooser on Windows and Mac OS
         return new NativeFileChooser(parent);
      }
      else {
         // use a Swing file chooser anywhere without a good native one
         return new SwingFileChooser(parent);
      }
   }

   public abstract File getCurrentDirectory();
   public abstract void setCurrentDirectory(File dir);
   public abstract void rescanCurrentDirectory();
   public abstract File getSelectedFile();
   public abstract void setSelectedFile(File file);
   public abstract int getFileSelectionMode();
   public abstract void setFileSelectionMode(int mode);
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
    * Selects <tt>filename.sav</tt> if <tt>filename.foo</tt> is selected.
    */
   public void selectDotSavFile() {
      File file = getSelectedFile();
      if (file != null) {
         String name = file.getPath();
         if (name != null) {
            int index = name.lastIndexOf('.');
            if (index > 0) {
               name = name.substring(0, index) + ".sav";
               setSelectedFile(new File(name));
            }
         }
      }
   }

   /**  
    * Same as {@link #showOpenDialog(Component)}, but uses the <tt>parent</tt>
    * set on creation of this FileDialog.
    */
   public int showOpenDialog() {
      return showOpenDialog(parent);
   }

   /**  
    * Same as {@link #showSaveDialog(Component)}, but uses the <tt>parent</tt>
    * set on creation of this FileDialog.
    */
   public int showSaveDialog() {
      return showSaveDialog(parent);
   }

   private static class SwingFileChooser extends FileChooser {
      private JFileChooser fc = new JFileChooser();

      public SwingFileChooser(Component parent) {
         super(parent);
      }

      public File getCurrentDirectory() {
         return fc.getCurrentDirectory();
      }

      public void setCurrentDirectory(File dir) {
         fc.setCurrentDirectory(dir);
      }

      public void rescanCurrentDirectory() {
         fc.rescanCurrentDirectory();
      }
      
      public File getSelectedFile() {
         return fc.getSelectedFile();
      }

      public void setSelectedFile(File file) {
         fc.setSelectedFile(file);
      }
   
      public int getFileSelectionMode() {
         return fc.getFileSelectionMode();
      }

      public void setFileSelectionMode(int mode) {
         fc.setFileSelectionMode(mode);
      }

      public String getDialogTitle() {
         return fc.getDialogTitle();
      }
   
      public void setDialogTitle(String title) {
         fc.setDialogTitle(title);
      }
   
      public int showOpenDialog(Component parent) {
         return fc.showOpenDialog(parent);
      }

      public int showSaveDialog(Component parent) {
         return fc.showSaveDialog(parent);
      }
   
      public FileFilter getFileFilter() {
         javax.swing.filechooser.FileFilter ff = fc.getFileFilter();
         return ff instanceof FileFilter ? (FileFilter) ff : null;
      }

      public void setFileFilter(FileFilter filter) {
         fc.setFileFilter(filter);
      }

      public void addChoosableFileFilter(FileFilter filter) {
         fc.addChoosableFileFilter(filter);
      }

      public boolean removeChoosableFileFilter(FileFilter filter) {
         return fc.removeChoosableFileFilter(filter);
      }

      public void resetChoosableFileFilters() {
         fc.resetChoosableFileFilters();
      }
   }

   private static class NativeFileChooser extends FileChooser {
      private File cur;
      private String title;
      private FileFilter filter;

      public NativeFileChooser(Component parent) {
         super(parent);
      }

      public File getCurrentDirectory() {
         if (cur == null) return null;
         else if (cur.isDirectory()) return cur;
         else return cur.getParentFile();
      }

      public void setCurrentDirectory(File dir) {
         cur = dir;
      }

      public void rescanCurrentDirectory() {
      }

      public File getSelectedFile() {
         return cur != null && cur.isFile() ? cur : null;
      }

      public void setSelectedFile(File file) {
         cur = file;
      }

      public int getFileSelectionMode() {
         return FILES_ONLY;
      }

      public void setFileSelectionMode(int mode) {
      }

      public String getDialogTitle() {
         return title;
      }

      public void setDialogTitle(String title) {
         this.title = title;
      }

      protected FileDialog awt_file_dialog_init(Component parent) {
         Frame frame = parent instanceof Frame ? (Frame) parent
            : (Frame) SwingUtilities.getAncestorOfClass(Frame.class, parent);
         FileDialog fd = new FileDialog(frame, title);
         fd.setModal(true);
         fd.setFilenameFilter(filter);

         if (cur != null) {
            if (cur.isDirectory()) fd.setDirectory(cur.getPath());
            else {
               fd.setDirectory(cur.getParent());
               fd.setFile(cur.getName());
            }
         }
      
         return fd;
      }

      public int showOpenDialog(Component parent) {
         FileDialog fd = awt_file_dialog_init(parent);
         fd.setMode(FileDialog.LOAD);
         fd.setVisible(true);
         
         if (fd.getFile() != null) {
            cur = new File(fd.getDirectory(), fd.getFile());
            return FileChooser.APPROVE_OPTION;
         }
         else return FileChooser.CANCEL_OPTION;
      }
  
      public int showSaveDialog(Component parent) {
         FileDialog fd = awt_file_dialog_init(parent);
         fd.setMode(FileDialog.SAVE);
         fd.setVisible(true);

         if (fd.getFile() != null) {
            cur = new File(fd.getDirectory(), fd.getFile());
            return FileChooser.APPROVE_OPTION;
         }
         else return FileChooser.CANCEL_OPTION;
      }

      public FileFilter getFileFilter() {
         return filter;
      }

      public void setFileFilter(FileFilter filter) {
         this.filter = filter;
      }

      public void addChoosableFileFilter(FileFilter filter) {
      }

      public boolean removeChoosableFileFilter(FileFilter filter) {
         return false;
      }

      public void resetChoosableFileFilters() {
      }
   }
}
