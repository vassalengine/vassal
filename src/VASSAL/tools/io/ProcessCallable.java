/*
 * $Id$
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

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A {@link Callable} which wraps a {@link java.lang.Process}.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
class ProcessCallable implements Callable<Integer> {

  private static final Logger logger =
    LoggerFactory.getLogger(ProcessCallable.class);

  protected final Process proc;
  protected final InputStreamPump stdoutPump;
  protected final InputStreamPump stderrPump;
  protected final ExecutorService exec;

  /**
   * Creates a <code>ProcessCallable</code>.
   *
   * @param proc the process
   * @param stdout the stream where the process' STDOUT is redirected
   * @param stderr the stream where the process' STDERR is redirected
   * @param exec the executor which runs the stream pumps
   */
  public ProcessCallable(
    Process proc,
    InputStreamPump stdoutPump,
    InputStreamPump stderrPump,
    ExecutorService exec)
  {
    if (proc == null) throw new IllegalArgumentException("proc == null");

    if (stdoutPump == null) {
      throw new IllegalArgumentException("stdoutPump == null");
    }

    if (stderrPump == null) {
      throw new IllegalArgumentException("stderrPump == null");
    }

    if (exec == null) throw new IllegalArgumentException("exec == null");

    this.proc = proc;
    this.stdoutPump = stdoutPump;
    this.stderrPump = stderrPump;
    this.exec = exec;
  }

  /**
   * {@inheritDoc}
   *
   *  @return the return value of the process
   */
  public Integer call() {

    stdoutPump.setInputStream(proc.getInputStream());
    stderrPump.setInputStream(proc.getErrorStream());

    final Future<?> out_f = exec.submit(stdoutPump);
    final Future<?> err_f = exec.submit(stderrPump);

    try {
      final int result = proc.waitFor();

      // stop the stream pumps
      stopPump(out_f);
      stopPump(err_f);

      // close stdout, stderr, stdin
      closeStreams();

      return result;
    }
    catch (InterruptedException e) {
      // We don't log this because it's not an error, it just
      // means that the process is being cancelled.

      // cancel the futures
      out_f.cancel(true);
      err_f.cancel(true);

      // close stdout, stderr, stdin
      closeStreams();

      // kill the process
      proc.destroy();
    }

    // Pardon the interruption.
    return -1;
  }

  protected void stopPump(Future<?> f) {
    try {
      f.get(1000L, TimeUnit.MILLISECONDS);
    }
    catch (ExecutionException e) {
      logger.error("", e);
    }
    catch (InterruptedException e) {
      logger.error("", e);
    }
    catch (TimeoutException e) {
      logger.error("", e);
      f.cancel(true);
    }
  }

  protected void closeStreams() {
    IOUtils.closeQuietly(proc.getOutputStream());
    IOUtils.closeQuietly(proc.getErrorStream());
    IOUtils.closeQuietly(proc.getInputStream());
  }
}
