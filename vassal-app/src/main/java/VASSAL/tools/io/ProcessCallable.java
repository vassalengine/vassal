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

import java.io.Closeable;
import java.io.IOException;
import java.util.List;
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
   * @param stdoutPump the stream where the process' STDOUT is redirected
   * @param stderrPump the stream where the process' STDERR is redirected
   * @param exec the executor which runs the stream pumps
   */
  public ProcessCallable(
    Process proc,
    InputStreamPump stdoutPump,
    InputStreamPump stderrPump,
    ExecutorService exec) {

    if (proc == null) throw new IllegalArgumentException("proc == null");

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
  @Override
  public Integer call() {

    if (stdoutPump != null) {
      stdoutPump.setInputStream(proc.getInputStream());
    }

    if (stderrPump != null) {
      stderrPump.setInputStream(proc.getErrorStream());
    }

    final Future<?> out_f = stdoutPump != null ? exec.submit(stdoutPump) : null;
    final Future<?> err_f = stderrPump != null ? exec.submit(stderrPump) : null;

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
    if (f == null) {
      return;
    }

    try {
      f.get(1000L, TimeUnit.MILLISECONDS);
    }
    catch (ExecutionException | InterruptedException e) {
      logger.error("", e);
    }
    catch (TimeoutException e) {
      logger.error("", e);
      f.cancel(true);
    }
  }

  protected void closeStreams() {
    List.of(
      proc.getOutputStream(),
      proc.getErrorStream(),
      proc.getInputStream()
    ).forEach(this::closeCloseable);
  }

  private void closeCloseable(Closeable closeable) {
    if (closeable == null) {
      return;
    }

    try {
      closeable.close();
    }
    catch (IOException e) {
      logger.error("Error while closing stream", e); //NON-NLS
    }
  }
}
