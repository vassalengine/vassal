/*
 * Copyright 2026 Vassal Development Team
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

package VASSAL.build.widget;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.i18n.ComponentI18nData;

import java.awt.Container;

import javax.swing.JLayeredPane;
import javax.swing.JToolBar;

import org.junit.jupiter.api.Test;

public class MapWidgetTest {

  private static WidgetMap widgetMap() {
    final WidgetMap m = mock(WidgetMap.class);
    when(m.getLayeredPane()).thenReturn(new JLayeredPane());
    when(m.getToolBar()).thenReturn(new JToolBar());
    when(m.getI18nData()).thenReturn(mock(ComponentI18nData.class));
    return m;
  }

  @Test
  public void removingTheDisplayedMapEmptiesThePanel() {
    final MapWidget w = new MapWidget();
    final WidgetMap m = widgetMap();
    w.add(m);
    assertEquals(2, ((Container) w.getComponent()).getComponentCount());

    w.remove(m);
    assertEquals(0, ((Container) w.getComponent()).getComponentCount());
    assertNull(w.getMap());
  }

  @Test
  public void removingAMapThatIsNotDisplayedLeavesThePanelAlone() {
    final MapWidget w = new MapWidget();
    final WidgetMap first = widgetMap();
    final WidgetMap second = widgetMap();
    w.add(first);
    w.add(second);

    w.remove(first);
    assertSame(second, w.getMap());
    assertEquals(2, ((Container) w.getComponent()).getComponentCount());

    w.remove(second);
    assertEquals(0, ((Container) w.getComponent()).getComponentCount());
  }

  @Test
  public void removingWhenNothingIsDisplayedDoesNotFail() {
    final MapWidget w = new MapWidget();
    w.remove(widgetMap());
    assertNull(w.getMap());
  }
}
