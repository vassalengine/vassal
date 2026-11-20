/*
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
package VASSAL.build;

import java.lang.reflect.InvocationTargetException;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.w3c.dom.Element;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * {@link Builder#create} does three things --- load the class named by the XML
 * tag, construct an instance of it, and build the component --- and used to
 * report a failure in any of them as "failed to load class". Of the open bug
 * reports carrying that message, most name a VASSAL class which plainly did
 * load: the fault was somewhere under build(), often in a module's own code
 * several levels down. See vassalengine/vassal#14339.
 */
public class BuilderTest {

  /**
   * Builder.create() reads nothing from the element but its tag name, and a
   * mock can carry a tag name which is not a legal XML name --- which the name
   * of a nested class is not.
   */
  private static Element element(String tag) {
    final Element e = mock(Element.class);
    when(e.getTagName()).thenReturn(tag);
    return e;
  }

  /**
   * Builder.create() uses Class.forName() only when there is no module to load
   * the class out of, which is what a test wants. Mocked statics answer null
   * by default, so this is all it takes.
   */
  private static MockedStatic<GameModule> noModule() {
    return mockStatic(GameModule.class);
  }

  @Test
  public void aClassWhichIsNotThereIsReportedAsFailingToLoad() {
    final Element e = element("no.such.Class");

    try (MockedStatic<GameModule> ignored = noModule()) {
      final IllegalBuildException ex = assertThrows(
        IllegalBuildException.class, () -> Builder.create(e)
      );
      assertThat(ex.getMessage(), is("failed to load class no.such.Class"));
      assertThat(ex.getCause(), instanceOf(ClassNotFoundException.class));
    }
  }

  @Test
  public void aClassWithNoNoArgConstructorIsNotReportedAsAFailureToLoadItBug15226() {
    final Element e = element(NoNoArgConstructor.class.getName());

    try (MockedStatic<GameModule> ignored = noModule()) {
      final IllegalBuildException ex = assertThrows(
        IllegalBuildException.class, () -> Builder.create(e)
      );
      assertThat(ex.getMessage(),
        is("failed to construct " + NoNoArgConstructor.class.getName()));
      assertThat(ex.getCause(), instanceOf(NoSuchMethodException.class));
    }
  }

  @Test
  public void aConstructorWhichThrowsIsNotReportedAsAFailureToLoadTheClassBug14339() {
    final Element e = element(ThrowsInConstructor.class.getName());

    try (MockedStatic<GameModule> ignored = noModule()) {
      final IllegalBuildException ex = assertThrows(
        IllegalBuildException.class, () -> Builder.create(e)
      );
      // The class loaded; its constructor is what blew up. The old message
      // said the class could not be loaded, which it demonstrably was.
      assertThat(ex.getMessage(),
        is("failed to construct " + ThrowsInConstructor.class.getName()));
      assertThat(ex.getCause(), instanceOf(InvocationTargetException.class));
    }
  }

  @Test
  public void aFailureWhileBuildingIsNotReportedAsAFailureToLoadTheClassBug14339() {
    final Element e = element(ThrowsInBuild.class.getName());

    try (MockedStatic<GameModule> ignored = noModule()) {
      final IllegalBuildException ex = assertThrows(
        IllegalBuildException.class, () -> Builder.create(e)
      );
      // This is the one which matters: the class loaded and the instance was
      // made, and the fault is under build() --- which, for a real component,
      // is the whole of its subtree and its descendants' addTo().
      assertThat(ex.getMessage(),
        is("failed to build " + ThrowsInBuild.class.getName()));
      assertThat(ex.getCause(), instanceOf(IllegalArgumentException.class));
    }
  }

  /** A component whose build() fails, as #14339's Map did three levels down. */
  public static class ThrowsInBuild extends AbstractBuildable {
    @Override
    public void build(Element e) {
      throw new IllegalArgumentException("not a number");
    }

    @Override
    public String[] getAttributeNames() {
      return new String[0];
    }

    @Override
    public void setAttribute(String key, Object value) {
    }

    @Override
    public String getAttributeValueString(String key) {
      return null;
    }

    @Override
    public void addTo(Buildable parent) {
    }
  }

  /** A component whose constructor fails. */
  public static class ThrowsInConstructor extends ThrowsInBuild {
    public ThrowsInConstructor() {
      throw new IllegalStateException("no");
    }
  }

  /** A component which cannot be instantiated from the buildFile at all. */
  public static class NoNoArgConstructor extends ThrowsInBuild {
    public NoNoArgConstructor(String unused) {
    }
  }
}
