/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.io;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;

/**
 * Handles temporary files. <code>TempFileManager</code> cleans up
 * stale temporary files when the singleton is constructed, and ones
 * created by the current session on exit through a shutdown hook.
 *
 * <p>A temporary director is created in <code>"user.dir" + "/tmp"</code>
 * for each <code>TempFileManager</code> instance, which, since it is
 * a singleton, amounts to one temporary directory per VASSAL instance.
 * Each session directory has a corresponding lock file, which indicates
 * that the VASSAL instance associated with that directory is live.
 * Directories which are not live will be cleaned up on creation of a
 * <code>TempFileManager</code> instance.</p>
 *
 * <p>Due to Sun Bugs
 * <a href="http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4171239">
 * #4171239</a>,
 * <a href="http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4724038">
 * #4724038</a>, and
 * <a href="http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6359560">
 * #6359560</a>, {@link File.deleteOnExit()} is unreliable on Windows,
 * and there is no way to unmap memory-mapped files so that they may
 * be deleted by {@link File.delete()}. This class overcomes these
 * shortcomings by handling temporary files directly.</p>
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @deprecated Create temporary files with {@link File.createTempFile()}
 * in {@link VASSAL.Info.getTempDir()}.
 */
@Deprecated
public class TempFileManager {
  private static final Logger logger =
    LoggerFactory.getLogger(TempFileManager.class);

  private final File tmpRoot;
  private File sessionRoot;
  private File lock;

  private static final String DIR_PREFIX = "vassal-";

  private TempFileManager() {
    //
    // set up for cleanup on shutdown
    //
    if (SystemUtils.IS_OS_WINDOWS) {
      Runtime.getRuntime().addShutdownHook(new Thread() {
        public void run() {
          // Run the garbage collector and finalize repeatedly, with
          // exponentially increasing pauses, until we succeed at deleting
          // the whole session temp directory or we hit the sleep limit.
          long sleep = 1;
          final long maxsleep = 1024;
          while (true) {
            System.gc();
            System.runFinalization();

            try {
              cleanupSessionRoot();
              break;
            }
            catch (IOException e) {
              if (sleep > maxsleep) {
                // just log, since shutdown hooks don't have long to run
                logger.error("", e);
                break;
              }

              try {
                Thread.sleep(sleep);
              }
              catch (InterruptedException ignore) {
              }

              sleep *= 2;
            }
          }
        }
      });
    }
    else {
      Runtime.getRuntime().addShutdownHook(new Thread() {
        public void run() {
          try {
            cleanupSessionRoot();
          }
          catch (IOException e) {
            // just log, since shutdown hooks don't have long to run
            logger.error("", e);
          }
        }
      });
    }

    tmpRoot = Info.getTempDir();

    //
    // clean up stale temporary files
    //
    if (tmpRoot.exists() && tmpRoot.isDirectory()) {
      final FileFilter filter = new FileFilter() {
        public boolean accept(File f) {
          return f.isDirectory() && f.getName().startsWith(DIR_PREFIX);
        }
      };

      for (File f : tmpRoot.listFiles(filter)) {
        final File lock = new File(f.getParent(), f.getName() + ".lck");
        if (!lock.exists()) {
          try {
            FileUtils.forceDelete(f);
          }
          catch (IOException e) {
            logger.error("", e);
          }
        }
      }
    }
  }

  private void cleanupSessionRoot() throws IOException {
    if (lock.exists()) FileUtils.forceDelete(lock);
    FileUtils.forceDelete(sessionRoot);
  }

  private static final TempFileManager instance = new TempFileManager();

  /**
   * Returns the singleton for this class.
   *
   * @return an instance of this class
   */
  public static TempFileManager getInstance() {
    return instance;
  }

  public File getSessionRoot() throws IOException {
    if (sessionRoot == null) sessionRoot = createSessionRoot();
    return new File(sessionRoot.toString());
  }

  private File createSessionRoot() throws IOException {
    // ensure that we have a good temporary root
    if (!tmpRoot.exists() || (!tmpRoot.isDirectory() && !tmpRoot.delete())) {
      FileUtils.forceMkdir(tmpRoot);
    }

    // get the name for our session root
    final File dir = File.createTempFile(DIR_PREFIX, "", tmpRoot);
    // delete it in case a file was created
    dir.delete();

    // create our lock file before creating the directory to prevent
    // a race with another instance of VASSAL
    lock = new File(tmpRoot, dir.getName() + ".lck");
    lock.createNewFile();
    lock.deleteOnExit();

    // now create our session root directory
    FileUtils.forceMkdir(dir);

    return dir;
  }

  /**
   * Creates a new directory with the given name in the session temporary
   * directory.
   *
   * @param dirname the name of the directory to create
   * @throws IOException if the directory cannot be created.
   */
  public File createTempDir(String dirname) throws IOException {
    if (sessionRoot == null) sessionRoot = createSessionRoot();

    final File dir = new File(sessionRoot, dirname);
    FileUtils.forceMkdir(dir);
    return dir;
  }

  /**
   * Creates a new empty file in the session temporary directory, using the
   * given prefix and suffix strings to generate its name. This method
   * is otherwise the same as {@link File.createTempFile(String,String)}.
   *
   * @param prefix
   * @param suffix
   * @return the created file
   * @throws IOException if a file could not be created
   */
  public File createTempFile(String prefix, String suffix) throws IOException{
    if (sessionRoot == null) sessionRoot = createSessionRoot();
    return File.createTempFile(prefix, suffix, sessionRoot);
  }
}
