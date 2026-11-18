/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
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
package VASSAL.launch;

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import VASSAL.Info;
import VASSAL.tools.lang.Pair;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Which tiler failures are worth another try with more heap.
 *
 * <p>The tiler exits 2 when, and only when, an {@code OutOfMemoryError} stopped it
 * ({@code ZipFileImageTiler}: {@code System.exit(thrown instanceof OutOfMemoryError ? 2 : 1)}).
 * {@link TilingHandler} used to retry on any non-zero value, growing the max heap by half each
 * time until it reached the machine's physical memory, and then report that tiling had run out
 * of memory whatever had actually happened.</p>
 *
 * <p>Of the thirty open reports of that message, sixteen are a tiler which could not be loaded
 * at all -- {@code ClassNotFoundException: VASSAL.tools.image.tilecache.ZipFileImageTiler} --
 * each preceded by ten to eighteen launches winding the heap up to as much as 49GB. See
 * vassalengine/vassal#13851.</p>
 */
public class TilingHandlerRetryTest {

  private static final int HEAP = 200;
  private static final int LIMIT = 8000;

  /** Runs {@link FakeTiler} in place of the real tiler, and records the heap of each launch. */
  private static class TestHandler extends TilingHandler {
    private final String mode;
    final List<Integer> launches = new ArrayList<>();

    TestHandler(String mode, File cdir) {
      super("unused.vmod", cdir, new Dimension(256, 256), LIMIT); //NON-NLS
      this.mode = mode;
    }

    @Override
    protected String[] makeSlicerCommandLine(int heap) {
      launches.add(heap);
      return new String[] {
        Info.getJavaBinPath().getAbsolutePath(),
        "-classpath", //NON-NLS
        System.getProperty("java.class.path"),
        "-Xmx" + heap + "M", //NON-NLS
        FakeTiler.class.getName(),
        mode
      };
    }
  }

  /** The handler only tells it about progress; none of that matters here. */
  private static class NullHandler implements TilingHandler.StateMachineHandler {
    @Override
    public void handleStart() {
    }

    @Override
    public void handleStartingImageState(String ipath) {
    }

    @Override
    public void handleFinishedImageState(String ipath) {
    }

    @Override
    public void handleTileWrittenState() {
    }

    @Override
    public void handleTilingFinishedState() {
    }

    @Override
    public void handleSuccess() {
    }

    @Override
    public void handleFailure() {
    }

    @Override
    public void handleRestart(Future<Integer> f) {
    }
  }

  private static Pair<Integer, Integer> run(TestHandler h) throws IOException {
    return h.runSlicer(new ArrayList<>(), HEAP, new NullHandler());
  }

  @Test
  public void testATilerWhichNeverStartsIsNotRetried(@TempDir Path dir) throws IOException {
    final TestHandler h = new TestHandler("never-starts", dir.toFile()); //NON-NLS
    final Pair<Integer, Integer> r = run(h);

    assertEquals(1, r.first, "the JVM's own exit code for a main class it cannot load");
    assertEquals(TilingHandler.SLICER_GAVE_UP, (int) r.second,
      "a tiler which never ran cannot be helped by more heap, so it must not be retried");
    assertEquals(List.of(HEAP), h.launches, "exactly one launch");
  }

  @Test
  public void testAFailureAfterTheTilerStartsIsNotRetriedEither(@TempDir Path dir) throws IOException {
    final TestHandler h = new TestHandler("other-failure", dir.toFile()); //NON-NLS
    final Pair<Integer, Integer> r = run(h);

    assertEquals(1, r.first);
    assertEquals(TilingHandler.SLICER_GAVE_UP, (int) r.second,
      "exit 1 means the tiler did not blame memory, so neither should we");
  }

  @Test
  public void testAnOutOfMemoryIsStillRetriedWithMoreHeap(@TempDir Path dir) throws IOException {
    final TestHandler h = new TestHandler("oom", dir.toFile()); //NON-NLS
    final Pair<Integer, Integer> r = run(h);

    assertEquals(TilingHandler.OUT_OF_MEMORY, (int) r.first);
    assertEquals((int) (HEAP * 1.5), (int) r.second,
      "the one failure which more heap can fix must still be retried with more heap");
  }

  @Test
  public void testSuccessIsNotRetried(@TempDir Path dir) throws IOException {
    final TestHandler h = new TestHandler("success", dir.toFile()); //NON-NLS
    final Pair<Integer, Integer> r = run(h);

    assertEquals(0, r.first);
    assertEquals(TilingHandler.SLICER_SUCCESS, (int) r.second);
    assertEquals(List.of(HEAP), h.launches);
  }

  @Test
  public void testTheTilersOwnErrorIsKept(@TempDir Path dir) throws IOException {
    final TestHandler h = new TestHandler("never-starts", dir.toFile()); //NON-NLS
    run(h);

    // Not "ran out of memory": the only statement anywhere of what really happened is the
    // child's own first line of stderr, which used to go to the log and nowhere else.
    assertTrue(h.failureMessage().contains("could not be started"),
      () -> "unexpected message: " + h.failureMessage());
    assertTrue(h.failureMessage().contains("Could not find or load main class"),
      () -> "the tiler's own error must be reported: " + h.failureMessage());
  }
}
