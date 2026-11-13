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
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
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
   * The locale handed to child processes when ours cannot represent
   * non-ASCII filenames. {@code C.UTF-8} is language-neutral and present on
   * essentially every modern glibc and musl system; where it is absent,
   * setting it leaves the child no worse off than the unusable locale it
   * would otherwise have inherited.
   */
  private static final String FALLBACK_LOCALE = "C.UTF-8"; //NON-NLS

  /**
   * A character which no filename-safe encoding may reject. Any charset
   * unable to encode a basic accented Latin letter cannot represent
   * real-world filenames.
   */
  private static final String NON_ASCII_PROBE = "\u00ed";  // i-acute

  /**
   * Checks whether this JVM can represent non-ASCII filenames.
   *
   * <p>A JVM decodes filenames with the charset named by
   * {@code sun.jnu.encoding}, which it derives from the C library locale
   * during startup and which cannot be changed afterwards with {@code -D}.
   * When the locale is unusable -- unset, {@code C}/{@code POSIX}, or naming
   * locale data which is not installed -- that charset is ASCII, and every
   * path holding a non-ASCII character throws {@code InvalidPathException}.
   *
   * <p>Charsets which merely differ from UTF-8 are left alone: a legacy
   * ISO-8859-1 system round-trips its own filenames perfectly well, and
   * overriding its locale would be the regression rather than the fix.
   *
   * @return whether non-ASCII filenames are representable
   */
  static boolean filenameEncodingIsUsable() {
    final String enc = System.getProperty("sun.jnu.encoding"); //NON-NLS
    if (enc == null) {
      // nothing to judge; don't meddle
      return true;
    }

    try {
      return Charset.forName(enc).newEncoder().canEncode(NON_ASCII_PROBE);
    }
    catch (IllegalArgumentException | UnsupportedOperationException e) {
      // an unrecognised or non-encoding charset; don't meddle
      return true;
    }
  }

  /**
   * Gives a child process a usable locale when ours cannot represent
   * non-ASCII filenames, so that it does not inherit our inability to read
   * them. Has no effect on Windows, where filenames reach the OS as UTF-16
   * and the locale does not govern them.
   *
   * <p>{@code LC_ALL} is what has to be set: a {@code LANG} naming missing
   * locale data makes the C library's {@code setlocale(LC_ALL, "")} fail as
   * a whole, which defeats a per-category {@code LC_CTYPE}.
   *
   * @param env the child's environment
   */
  static void ensureUsableLocale(Map<String, String> env) {
    if (SystemUtils.IS_OS_WINDOWS || filenameEncodingIsUsable()) {
      return;
    }

    logger.info(
      "filename encoding is " + System.getProperty("sun.jnu.encoding") + //NON-NLS
      "; launching child with LC_ALL=" + FALLBACK_LOCALE //NON-NLS
    );
    env.put("LC_ALL", FALLBACK_LOCALE); //NON-NLS
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
    String... args) throws IOException {

    final InputOutputStreamPump outP = new InputOutputStreamPump(null, stdout);
    final InputOutputStreamPump errP = new InputOutputStreamPump(null, stderr);

    return launch(null, outP, errP, args);
  }

  /**
   * Launches a process.
   *
   * @param workDir the process' working directory
   * @param stdoutPump the stream where the process' STDOUT is redirected
   * @param stderrPump the stream where the process' STDERR is redirected
   * @param args the command-line arguments
   *
   * @throws IOException if the process fails to launch
   */
  public ProcessWrapper launch(
    File workDir,
    InputStreamPump stdoutPump,
    InputStreamPump stderrPump,
    String... args) throws IOException {

    logger.info("launching " + StringUtils.join(args, ' ')); //NON-NLS

    final ProcessBuilder pb = new ProcessBuilder(args);
    pb.directory(workDir);
    ensureUsableLocale(pb.environment());

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
