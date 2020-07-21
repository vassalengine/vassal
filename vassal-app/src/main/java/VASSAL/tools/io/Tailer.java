/*
 *
 * Copyright (c) 2010 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.io;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.concurrent.listener.DefaultEventListenerSupport;
import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.concurrent.listener.EventListenerSupport;

/**
 * Tail a file. This class is designed to behave similarly to the UNIX
 * command <code>tail -f</code>, watching the file for changes and
 * reporting them to listeners. The tailer may be stopped when it is not
 * needed; if it is restarted, it will remember its last position and
 * resume reading where it left off.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class Tailer {
  private static final Logger logger = LoggerFactory.getLogger(Tailer.class);

  protected static final long DEFAULT_POLL_INTERVAL = 1000L;

  protected final File file;
  protected final long poll_interval;

  protected long position = 0L;
  protected volatile boolean tailing = false;

  protected final EventListenerSupport<String> lsup;

  /**
   * Creates a file tailer with the default polling interval.
   *
   * @param file the file to tail
   */
  public Tailer(File file) {
    this(file, DEFAULT_POLL_INTERVAL);
  }

  /**
   * Creates a file tailer.
   *
   * @param file the file to tail
   * @param poll_interval the polling interval, in milliseconds
   */
  public Tailer(File file, long poll_interval) {
    if (file == null) throw new IllegalArgumentException("file == null");

    this.file = file;
    this.poll_interval = poll_interval;
    this.lsup = new DefaultEventListenerSupport<>(this);
  }

  /**
   * Creates a file tailer.
   *
   * @param file the file to tail
   * @param poll_interval the polling interval, in milliseconds
   * @param lsup the listener support
   */
  public Tailer(File file, long poll_interval,
                                           EventListenerSupport<String> lsup) {
    if (file == null) throw new IllegalArgumentException("file == null");
    if (lsup == null) throw new IllegalArgumentException("lsup == null");

    this.file = file;
    this.poll_interval = poll_interval;
    this.lsup = lsup;
  }

  /**
   * Starts tailing the file.
   */
  public synchronized void start() throws IOException {
    // NB: This method is synchronized to ensure that there is never more
    // than one tailer thread at at time.
    if (!tailing) {
      if (!file.exists()) {
        throw new IOException(file.getAbsolutePath() + " does not exist");
      }

      if (file.isDirectory()) {
        throw new IOException(file.getAbsolutePath() + " is a directory");
      }

      tailing = true;
      new Thread(new Monitor(), "tailing " + file.getAbsolutePath()).start();
    }
  }

  /**
   * Stops tailing the file.
   */
  public void stop() {
    tailing = false;
  }

  /**
   * Checks whether the tailer is running.
   *
   * @return <code>true</code> if the tailer is running
   */
  public boolean isTailing() {
    return tailing;
  }

  /**
   * Gets the file being tailed.
   *
   * @return the file
   */
  public File getFile() {
    return file;
  }

  /**
   * Adds an {@link EventListener}.
   *
   * @param l the listener to add
   */
  public void addEventListener(EventListener<? super String> l) {
    lsup.addEventListener(l);
  }

  /**
   * Removes an {@link EventListener}.
   *
   * @param l the listener to remove
   */
  public void removeEventListener(EventListener<? super String> l) {
    lsup.removeEventListener(l);
  }

  /**
   * Checks whether there are any {@link EventListener}s.
   *
   * @return <code>true</code> if there are any listeners
   */
  public boolean hasEventListeners() {
    return lsup.hasEventListeners();
  }

  /**
   * Gets the list of listerners.
   *
   * @return the list of listeners
   */
  public List<EventListener<? super String>> getEventListeners() {
    return lsup.getEventListeners();
  }

  private class Monitor implements Runnable {
    @Override
    public void run() {

      RandomAccessFile raf = null;
      try {
        raf = new RandomAccessFile(file, "r");

        // read until we're told to stop
        while (tailing) {
          final long length = raf.length();

          if (length < position) {
            // file has been truncated, reopen it
            raf = new RandomAccessFile(file, "r");
            position = 0L;
          }
          else if (length > position) {
            // new lines have been written, read them
            raf.seek(position);

            String line;
            while ((line = raf.readLine()) != null) {
              // readLine strips newlines, we put them back
              lsup.notify(line + "\n");
            }

            position = raf.getFilePointer();
          }

          // we have reached EOF, sleep
          Thread.sleep(poll_interval);
        }
      }
      catch (IOException | InterruptedException e) {
// FIXME: there should be an error listener; we can't handle exceptions here
        logger.error("", e);
      }
      finally {
        if (raf != null) {
          try {
            raf.close();
          }
          catch (IOException e) {
            logger.error("Error while closing the logfile", e);
          }
        }
      }
    }
  }

  public static void main(String[] args) throws IOException {
    final Tailer t = new Tailer(new File(args[0]));

    t.addEventListener(new EventListener<>() {
      @Override
      public void receive(Object src, String s) {
        System.out.print(s);
      }
    });

    t.start();
  }
}
