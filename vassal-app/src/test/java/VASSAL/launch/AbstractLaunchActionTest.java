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
import static org.hamcrest.Matchers.nullValue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class AbstractLaunchActionTest {
  @TempDir
  Path dir;

  @Test
  public void aRequestWithNoFilesHasNothingMissing() {
    assertThat(AbstractLaunchAction.firstMissingFile(new LaunchRequest()), is(nullValue()));
  }

  @Test
  public void existingFilesAreNotMissing() throws IOException {
    final File module = Files.createFile(dir.resolve("game.vmod")).toFile(); //NON-NLS
    final File game = Files.createFile(dir.resolve("game.vsav")).toFile(); //NON-NLS
    final LaunchRequest lr = new LaunchRequest(LaunchRequest.Mode.LOAD, module);
    lr.game = game;

    assertThat(AbstractLaunchAction.firstMissingFile(lr), is(nullValue()));
  }

  @Test
  public void aModuleWhichWasMovedIsReportedMissing() {
    final File module = dir.resolve("moved.vmod").toFile(); //NON-NLS
    final LaunchRequest lr = new LaunchRequest(LaunchRequest.Mode.LOAD, module);

    assertThat(AbstractLaunchAction.firstMissingFile(lr), is(module));
  }

  @Test
  public void aSavedGameWhichWasDeletedIsReportedMissing() throws IOException {
    final File module = Files.createFile(dir.resolve("game.vmod")).toFile(); //NON-NLS
    final File game = dir.resolve("deleted.vsav").toFile(); //NON-NLS
    final LaunchRequest lr = new LaunchRequest(LaunchRequest.Mode.LOAD, module);
    lr.game = game;

    assertThat(AbstractLaunchAction.firstMissingFile(lr), is(game));
  }
}
