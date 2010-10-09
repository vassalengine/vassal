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

import java.io.OutputStream;
import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.TimeUnit;

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
  protected final OutputStream stdout;
  protected final OutputStream stderr;
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
    OutputStream stdout,
    OutputStream stderr,
    ExecutorService exec)
  {
    if (proc == null) throw new IllegalArgumentException("proc == null");
    if (stdout == null) throw new IllegalArgumentException("stdout == null");
    if (stderr == null) throw new IllegalArgumentException("stderr == null");
    if (exec == null) throw new IllegalArgumentException("exec == null");

    this.proc = proc;
    this.stdout = stdout;
    this.stderr = stderr;
    this.exec = exec;
  }

  /**
   *  @return the return value of the process
   */  
  public Integer call() {
  
    final StreamPump out_pump = new StreamPump(proc.getInputStream(), stdout);
    final StreamPump err_pump = new StreamPump(proc.getErrorStream(), stderr);

    final Future<?> out_f = exec.submit(out_pump); 
    final Future<?> err_f = exec.submit(err_pump);

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
      logger.error("", e);

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
