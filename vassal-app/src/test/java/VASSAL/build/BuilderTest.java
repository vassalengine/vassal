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
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import VASSAL.tools.ErrorDialog;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
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

      // A class which is not there is the one build failure which can only
      // ever be bad data in the module, so it has a type of its own and
      // carries the name Builder.build() needs to report. See #15393.
      assertThat(ex, instanceOf(ComponentClassNotFoundException.class));
      assertThat(((ComponentClassNotFoundException) ex).getClassName(),
        is("no.such.Class"));
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
      assertThat(ex, not(instanceOf(ComponentClassNotFoundException.class)));
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
      assertThat(ex, not(instanceOf(ComponentClassNotFoundException.class)));
    }
  }

  /**
   * Builder.build() walks a node's element children. A mock can stand in for
   * the linked list, which is all it reads.
   */
  private static Element childElement(String tag) {
    final Element e = element(tag);
    when(e.getNodeType()).thenReturn(Node.ELEMENT_NODE);
    return e;
  }

  private static Element parentOf(Element... children) {
    final Element p = mock(Element.class);
    Node next = null;
    for (int i = children.length - 1; i >= 0; i--) {
      when(children[i].getNextSibling()).thenReturn(next);
      next = children[i];
    }
    when(p.getFirstChild()).thenReturn(next);
    return p;
  }

  /** Run Builder.build() over the children, collecting what it reported. */
  private static List<BadDataReport> buildCollectingWarnings(
    Element parent, MockedStatic<ErrorDialog> ed) {

    final List<BadDataReport> reports = new ArrayList<>();
    ed.when(() -> ErrorDialog.dataWarning(any(BadDataReport.class)))
      .thenAnswer(inv -> reports.add(inv.getArgument(0)));
    Builder.build(parent, null);
    return reports;
  }

  /**
   * A buildFile naming a class which is in neither the module archive nor
   * VASSAL is bad data in the module: VASSAL cannot build a component whose
   * class does not exist, and no version of it ever could. It used to raise an
   * automated bug report against VASSAL instead, which is how VASL 6.7.0's
   * ASLPredefinedSetup2 --- removed from VASL in 6.7.2, still named by a
   * 6.7.0-era extension --- arrived on the VASSAL tracker four times in one
   * evening. See vassalengine/vassal#15393.
   */
  @Test
  public void aMissingClassIsReportedAsBadDataNotAsABugBug15393() {
    final Element parent = parentOf(childElement("no.such.Class"));

    try (MockedStatic<GameModule> ignored = noModule();
         MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {

      final List<BadDataReport> reports = buildCollectingWarnings(parent, ed);

      assertThat(reports.size(), is(1));
      // The report names the class, which is the only thing a module designer
      // can act on and the only thing the bug report it replaces carried.
      assertThat(reports.get(0).getData(), is("no.such.Class"));
      assertThat(reports.get(0).getMessage(), containsString("no.such.Class"));

      ed.verify(() -> ErrorDialog.bug(any(Throwable.class)), never());
    }
  }

  /**
   * The control. A component whose class loads and whose build() then fails
   * can still be VASSAL's fault, so it is still reported as a bug.
   */
  @Test
  public void aFailureUnderBuildIsStillReportedAsABug() {
    final Element parent = parentOf(childElement(ThrowsInBuild.class.getName()));

    try (MockedStatic<GameModule> ignored = noModule();
         MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {

      final List<BadDataReport> reports = buildCollectingWarnings(parent, ed);

      assertThat(reports, is(List.of()));
      ed.verify(() -> ErrorDialog.bug(any(Throwable.class)));
    }
  }

  /**
   * The shape #15393 actually arrives in: the missing class is not a child of
   * the node being built but one level further down, inside an
   * ExtensionElement which calls Builder.create() itself. The failure has to
   * survive the outer create()'s own catch to be recognised.
   */
  @Test
  public void aMissingClassBelowAnotherComponentIsStillBadDataBug15393() {
    final Element parent =
      parentOf(childElement(BuildsAMissingClass.class.getName()));

    try (MockedStatic<GameModule> ignored = noModule();
         MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {

      final List<BadDataReport> reports = buildCollectingWarnings(parent, ed);

      assertThat(reports.size(), is(1));
      assertThat(reports.get(0).getData(), is("no.such.Nested"));
      ed.verify(() -> ErrorDialog.bug(any(Throwable.class)), never());
    }
  }

  /**
   * One component failing does not cost the others. This is what lets the rest
   * of a mismatched extension load, as VASL's did.
   */
  @Test
  public void aMissingClassDoesNotStopTheRestOfTheBuild() {
    final Element parent = parentOf(
      childElement("no.such.Class"),
      childElement(Counting.class.getName()),
      childElement(Counting.class.getName())
    );

    Counting.built = 0;

    try (MockedStatic<GameModule> ignored = noModule();
         MockedStatic<ErrorDialog> ed = mockStatic(ErrorDialog.class)) {

      final List<BadDataReport> reports = buildCollectingWarnings(parent, ed);

      assertThat(reports.size(), is(1));
      assertThat(Counting.built, is(2));
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

  /**
   * A component which builds a child of its own, as ExtensionElement does ---
   * and whose child's class is not there.
   */
  public static class BuildsAMissingClass extends ThrowsInBuild {
    @Override
    public void build(Element e) {
      Builder.create(element("no.such.Nested"));
    }
  }

  /** A component which builds cleanly, to show the build carried on. */
  public static class Counting extends ThrowsInBuild {
    public static int built;

    @Override
    public void build(Element e) {
      built++;
    }
  }
}
