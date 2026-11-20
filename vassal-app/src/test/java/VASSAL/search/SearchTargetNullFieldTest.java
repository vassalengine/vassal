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
package VASSAL.search;

import java.util.List;

import org.junit.jupiter.api.Test;

import VASSAL.build.module.documentation.AboutScreen;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.build.module.properties.GlobalTranslatableMessage;
import VASSAL.build.module.properties.ZoneProperty;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * The Editor's search asks every component for its strings, and a component
 * the user has not finished filling in has none to give. List.of() rejects a
 * null, so the search threw rather than skipping the field.
 *
 * See vassalengine/vassal#14198 and #14149.
 */
public class SearchTargetNullFieldTest {

  @Test
  public void anAboutScreenWithNoTitleHasNoFormattedStringsBug14198() {
    assertThat(new AboutScreen().getFormattedStringList(), is(List.of()));
  }

  @Test
  public void anAboutScreenWithATitleStillOffersIt() {
    final AboutScreen as = new AboutScreen();
    as.setAttribute(AboutScreen.TITLE, "Credits");
    assertThat(as.getFormattedStringList(), is(List.of("Credits")));
  }

  @Test
  public void anUnnamedGlobalTranslatableMessageHasNoProperties() {
    final GlobalTranslatableMessage m = new GlobalTranslatableMessage();
    // Which is what an unnamed component does: the property is named after the
    // component, and setAttribute passes that name straight through.
    m.setAttribute(GlobalTranslatableMessage.NAME, null);
    assertThat(m.getPropertyList(), is(List.of()));
  }

  /**
   * A Global Property starts with an empty initial value and no numeric range
   * at all, so two of the three expressions List.of() was handed were null.
   */
  @Test
  public void aGlobalPropertyWithNoRangeOffersOnlyItsInitialValue() {
    assertThat(new GlobalProperty().getExpressionList(), is(List.of("")));
  }

  @Test
  public void aZonePropertyWithNoRangeIsTheSameCase() {
    // ZoneProperty extends GlobalProperty, and so inherited the crash.
    assertThat(new ZoneProperty().getExpressionList(), is(List.of("")));
  }

  @Test
  public void aGlobalPropertyStillOffersTheExpressionsItDoesHave() {
    final GlobalProperty gp = new GlobalProperty();
    gp.setAttribute(GlobalProperty.MIN_VALUE, "1");
    gp.setAttribute(GlobalProperty.MAX_VALUE, "9");
    assertThat(gp.getExpressionList(), is(List.of("", "1", "9")));
  }

  @Test
  public void aNamedGlobalTranslatableMessageStillOffersItsProperty() {
    final GlobalTranslatableMessage m = new GlobalTranslatableMessage();
    m.setAttribute(GlobalTranslatableMessage.NAME, "Greeting");
    assertThat(m.getPropertyList(), is(List.of("Greeting")));
  }
}
