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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.bug;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.security.SecureClassLoader;
import java.util.Objects;

import org.junit.jupiter.api.Test;

import VASSAL.tools.DataArchive;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static java.time.Duration.ofSeconds;

/**
 * Checks that a failure in module-supplied code is recognised as such, by loading a class through
 * a class loader named the way {@link DataArchive} names its own and letting it throw.
 */
public class CustomCodeTest {

  private static final String THROWER = Thrower.class.getName();

  /**
   * Stands in for a class a module ships inside its archive. It is loaded a second time, by
   * {@link TestLoader}, so that its frames carry that loader's name.
   */
  public static class Thrower {
    public static void boom() {
      throw new IllegalArgumentException("Invalid state list change");
    }

    public static void viaPlatform() {
      // The throw happens inside a platform class, called from here.
      Objects.requireNonNull(null, "nothing");
    }

    public static void wrapped() {
      try {
        boom();
      }
      catch (RuntimeException e) {
        throw new IllegalStateException("while doing something", e);
      }
    }

    public static void viaEngine() {
      // Engine code is what fails here, even though module code called it.
      engineFailure();
    }
  }

  /**
   * Stands in for VASSAL's own code: it is loaded by the ordinary class loader, exactly as every
   * engine class is, so its frames carry no module archive's name.
   */
  public static void engineFailure() {
    throw new IllegalStateException("the engine itself broke");
  }

  /**
   * Defines {@link Thrower} itself rather than delegating, so the resulting class is attributed to
   * this loader -- exactly as a module's custom class is attributed to its {@code DataArchive}.
   */
  private static class TestLoader extends SecureClassLoader {
    TestLoader(String name) {
      super(name, CustomCodeTest.class.getClassLoader());
    }

    @Override
    public Class<?> loadClass(String name) throws ClassNotFoundException {
      if (!name.equals(THROWER)) {
        return super.loadClass(name);
      }

      final Class<?> found = findLoadedClass(name);
      if (found != null) {
        return found;
      }

      try (InputStream in = getParent().getResourceAsStream(name.replace('.', '/') + ".class")) {
        if (in == null) {
          throw new ClassNotFoundException(name);
        }
        final byte[] b = in.readAllBytes();
        return defineClass(name, b, 0, b.length);
      }
      catch (IOException e) {
        throw new ClassNotFoundException(name, e);
      }
    }
  }

  /** @return whatever {@code method} of a freshly loaded {@link Thrower} throws */
  private static Throwable thrownBy(String loaderName, String method) throws Exception {
    final Class<?> c = new TestLoader(loaderName).loadClass(THROWER);
    try {
      c.getMethod(method).invoke(null);
    }
    catch (InvocationTargetException e) {
      return e.getCause();
    }
    throw new AssertionError(method + " did not throw");
  }

  private static String moduleLoader() {
    return DataArchive.MODULE_LOADER_PREFIX + "Some Module.vmod";
  }

  @Test
  public void testBlamesCodeLoadedFromAModule() throws Exception {
    assertEquals(THROWER, CustomCode.blame(thrownBy(moduleLoader(), "boom")));
  }

  @Test
  public void testSkipsPlatformFramesAboveTheModuleCode() throws Exception {
    // The topmost frame is in java.util.Objects; the module's code is the one at fault.
    assertEquals(THROWER, CustomCode.blame(thrownBy(moduleLoader(), "viaPlatform")));
  }

  @Test
  public void testFollowsTheCausalChainToTheOriginalFailure() throws Exception {
    assertEquals(THROWER, CustomCode.blame(thrownBy(moduleLoader(), "wrapped")));
  }

  @Test
  public void testDoesNotBlameTheModuleForAFailureInEngineCode() throws Exception {
    // Module code called VASSAL, and VASSAL is what failed. That is still VASSAL's bug.
    assertNull(CustomCode.blame(thrownBy(moduleLoader(), "viaEngine")));
  }

  @Test
  public void testDoesNotBlameCodeWhichDidNotComeFromAModule() throws Exception {
    // The identical failure, from a loader which is not a module archive's, is not blamed. This
    // is also the state of affairs before DataArchive named its loader: nothing to go on.
    assertNull(CustomCode.blame(thrownBy("somebody else", "boom")));
    assertNull(CustomCode.blame(new IllegalArgumentException("thrown right here")));
  }

  @Test
  public void testToleratesNullAndSelfReferentialCauses() {
    assertNull(CustomCode.blame(null));

    assertTimeoutPreemptively(ofSeconds(5), () -> {
      final Throwable a = new RuntimeException("a");
      final Throwable b = new RuntimeException("b", a);
      a.initCause(b);
      assertNull(CustomCode.blame(b));
    });
  }
}
