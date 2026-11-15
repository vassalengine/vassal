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
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

import org.junit.jupiter.api.Test;

public class SpawnHelperFailureTest {
  @Test
  public void theJdkSpawnHelperFailureIsRecognisedBehindTheExecutionException() {
    final IOException e = new IOException(
      "Cannot run program \"/usr/lib/jvm/java-21-openjdk-amd64/bin/java\": error=0, Failed to exec spawn helper: pid: 43957, exit value: 1" //NON-NLS
    );
    assertThat(AbstractLaunchAction.isSpawnHelperFailure(new ExecutionException(e)), is(true));
  }

  @Test
  public void theNewerJdkMessageIsRecognisedToo() {
    final IOException e = new IOException(
      "Cannot run program \"/usr/lib/jvm/java-21-openjdk-amd64/bin/java\": Failed to exec spawn helper: pid: 138291, exit code: 1, error: 0 (none)" //NON-NLS
    );
    assertThat(AbstractLaunchAction.isSpawnHelperFailure(new ExecutionException(e)), is(true));
  }

  @Test
  public void otherLaunchFailuresAreNotMistakenForIt() {
    assertThat(AbstractLaunchAction.isSpawnHelperFailure(
      new ExecutionException(new IOException("failed to start child process"))), is(false)); //NON-NLS
    assertThat(AbstractLaunchAction.isSpawnHelperFailure(
      new ExecutionException(new IOException((String) null))), is(false));
    assertThat(AbstractLaunchAction.isSpawnHelperFailure(
      new ExecutionException(new NullPointerException("spawn helper"))), is(false)); //NON-NLS
  }
}
