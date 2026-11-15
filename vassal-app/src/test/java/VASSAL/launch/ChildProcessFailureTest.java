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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.launch;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import VASSAL.tools.io.ProcessLauncher;
import VASSAL.tools.io.ProcessWrapper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.apache.commons.io.output.TeeOutputStream;
import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.Test;

public class ChildProcessFailureTest {
  @Test
  public void aSilentChildGivesThePlainMessage() {
    assertThat(
      AbstractLaunchAction.childProcessFailure(new ByteArrayOutputStream()),
      is("failed to start child process") //NON-NLS
    );
  }

  @Test
  public void whatTheChildWroteToStderrGoesIntoTheMessage() throws IOException {
    final ByteArrayOutputStream childErr = new ByteArrayOutputStream();
    childErr.write("Unrecognized VM option 'Bogus'\nError: Could not create the Java Virtual Machine.\n".getBytes(StandardCharsets.UTF_8)); //NON-NLS

    final String msg = AbstractLaunchAction.childProcessFailure(childErr);
    // the prefix is what ErrorDialog.bug() looks for to pick the dialog text
    assertThat(msg, containsString("failed to start child process")); //NON-NLS
    assertThat(msg, containsString("Could not create the Java Virtual Machine.")); //NON-NLS
    assertThat(msg, not(containsString("\n\n")));
  }

  /**
   * The launch task reads the copy of the child's stderr once the child's
   * future is done. This checks that a JVM which refuses to start has written
   * its complaint, and that the copy has it, by then.
   */
  @Test
  public void aJvmWhichDiesAtOnceHasExplainedItselfByTheTimeItIsDone()
    throws IOException, InterruptedException, ExecutionException, TimeoutException {

    final String java = Path.of(System.getProperty("java.home"), "bin", SystemUtils.IS_OS_WINDOWS ? "java.exe" : "java").toString(); //NON-NLS

    final ByteArrayOutputStream childErr = new ByteArrayOutputStream();
    final OutputStream err = new TeeOutputStream(OutputStream.nullOutputStream(), childErr);

    final ProcessWrapper proc = new ProcessLauncher().launch(
      null, OutputStream.nullOutputStream(), err,
      java, "-XX:ThisOptionDoesNotExist", "-version" //NON-NLS
    );

    assertThat(proc.future.get(30, TimeUnit.SECONDS), is(not(0)));
    assertThat(
      AbstractLaunchAction.childProcessFailure(childErr),
      containsString("Unrecognized VM option") //NON-NLS
    );
  }
}
