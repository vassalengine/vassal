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
package VASSAL.build.module;

import java.io.StringReader;

import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * An ExtensionElement built its component with the no-parent form of
 * Builder.create(), so the component --- and everything under it --- had a
 * null ancestor until addTo() grafted it onto the module. Anything which finds
 * its Map by walking ancestors, a Deck or an At-Start Stack above all, then
 * had no Map. See vassalengine/vassal#13610.
 */
public class ExtensionElementTest {

  /** A stand-in for a component an extension adds. */
  public static class FakeComponent extends AbstractBuildable {
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

  private static Element element(String xml) throws Exception {
    return DocumentBuilderFactory.newInstance()
                                 .newDocumentBuilder()
                                 .parse(new InputSource(new StringReader(xml)))
                                 .getDocumentElement();
  }

  @Test
  public void theComponentIsBuiltAsAChildOfItsTargetBug13610() throws Exception {
    // An empty target means the module itself, which is the case a test can
    // set up without building a module tree to resolve a path against.
    final Element e = element(
      "<" + ExtensionElement.class.getName() + " target=\"\">" +
      "<test.Component/>" +
      "</" + ExtensionElement.class.getName() + ">"
    );

    final GameModule mod = mock(GameModule.class);
    final DataArchive archive = mock(DataArchive.class);
    // The archive is how a module resolves a buildFile tag to a class, and is
    // what lets this test name a nested class from an XML element, whose name
    // may not contain a '$'.
    when(archive.loadClass(anyString())).thenAnswer(inv -> FakeComponent.class);
    when(mod.getDataArchive()).thenReturn(archive);

    final ExtensionElement ee = new ExtensionElement();

    try (MockedStatic<GameModule> gm = mockStatic(GameModule.class)) {
      gm.when(GameModule::getGameModule).thenReturn(mod);
      ee.build(e);
    }

    final Buildable built = ee.getExtension();
    assertThat(built, instanceOf(FakeComponent.class));
    assertThat(((AbstractBuildable) built).getAncestor(), sameInstance(mod));
  }
}
