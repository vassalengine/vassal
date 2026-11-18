/*
 *
 * Copyright (c) 2026 by vassalengine.org
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
package VASSAL.tools.deprecation;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class DependencyWalkerTest {

  /**
   * A record. The compiler gives it {@code equals}, {@code hashCode} and {@code toString} built on an
   * {@code invokedynamic} to {@code ObjectMethods.bootstrap}, whose trailing bootstrap arguments are one
   * field-access method handle per component. Those handles carry a <i>field</i> descriptor, which is what
   * #15262 choked on.
   */
  public record Sample(String name, int count) { }

  private List<String> walk(Class<?> c) throws IOException {
    final List<String> classes = new ArrayList<>();
    final DependencyWalker w = new DependencyWalker();
    w.setClassCallback(classes::add);

    final String resource = c.getName().replace('.', '/') + ".class";
    try (InputStream in = c.getClassLoader().getResourceAsStream(resource)) {
      assertNotNull(in, "could not read " + resource);
      w.setInput(in);
    }
    w.walk();
    return classes;
  }

  /**
   * A field-access method handle among an {@code invokedynamic}'s bootstrap arguments has a field
   * descriptor such as {@code Ljava/lang/String;}, not a method descriptor. Treating it as a method
   * descriptor sent {@code Type.getReturnType} off the end of the string looking for a {@code ')'}, which
   * threw a {@code StringIndexOutOfBoundsException} and stopped the module loading at all (#15262).
   */
  @Test
  public void testWalksRecordWithoutThrowing() {
    assertDoesNotThrow(() -> walk(Sample.class));
  }

  /**
   * The descriptor of a field-access handle still names a class the walker must report, or a record
   * component's type would escape the removed-and-deprecated check.
   */
  @Test
  public void testReportsFieldHandleDescriptorType() throws IOException {
    final List<String> classes = walk(Sample.class);
    assertTrue(classes.contains("java.lang.String"),
      "expected the record component's type among the dependencies, got: " + classes);
  }
}
