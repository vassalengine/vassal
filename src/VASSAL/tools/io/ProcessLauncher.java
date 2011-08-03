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

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.concurrent.DaemonThreadFactory;

/**
 *
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ProcessLauncher {

  private static final Logger logger =
    LoggerFactory.getLogger(ProcessLauncher.class);

  protected final ExecutorService exec;

  public ProcessLauncher() {
    this(Executors.newCachedThreadPool(
      new DaemonThreadFactory(ProcessLauncher.class.getSimpleName())));
  }

  ProcessLauncher(ExecutorService exec) {
    this.exec = exec;
  }

  /**
   * Launches a process.
   *
   * @param args the command-line arguments
   *
   * @throws IOException if the process fails to launch
   */
  public ProcessWrapper launch(String... args) throws IOException {
    return launch(null, System.out, System.err, args);
  }

  /**
   * Launches a process.
   *
   * @param workDir the process' working directory
   * @param stdout the stream where the process' STDOUT is redirected
   * @param stderr the stream where the process' STDERR is redirected
   * @param args the command-line arguments
   *
   * @throws IOException if the process fails to launch
   */
  public ProcessWrapper launch(
    File workDir,
    OutputStream stdout,
    OutputStream stderr,
    String... args) throws IOException
  {
    final InputOutputStreamPump outP = new InputOutputStreamPump(null, stdout);
    final InputOutputStreamPump errP = new InputOutputStreamPump(null, stderr);

    return launch(null, outP, errP, args);
  }

  /**
   * Launches a process.
   *
   * @param workDir the process' working directory
   * @param stdout the stream where the process' STDOUT is redirected
   * @param stderr the stream where the process' STDERR is redirected
   * @param args the command-line arguments
   *
   * @throws IOException if the process fails to launch
   */
  public ProcessWrapper launch(
    File workDir,
    InputStreamPump stdoutPump,
    InputStreamPump stderrPump,
    String... args) throws IOException
  {
    logger.info("launching " + StringUtils.join(args, ' '));

    final ProcessBuilder pb = new ProcessBuilder(args);
    pb.directory(workDir);

    final Process proc = pb.start();
    final ProcessCallable pcall =
      new ProcessCallable(proc, stdoutPump, stderrPump, exec);
    final Future<Integer> future = exec.submit(pcall);

    return new ProcessWrapper(
      future,
      proc.getInputStream(),
      proc.getErrorStream(),
      proc.getOutputStream()
    );
  }
}
