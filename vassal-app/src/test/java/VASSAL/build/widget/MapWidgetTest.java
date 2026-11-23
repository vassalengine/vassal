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
package VASSAL.build.widget;

import java.awt.image.BufferedImage;
import java.awt.Component;
import java.util.Arrays;

import javax.swing.JPanel;

import VASSAL.tools.imageop.ImageSourceOpBitmapImpl;
import VASSAL.tools.imageop.Op;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import org.w3c.dom.Document;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;

import static java.awt.image.BufferedImage.TYPE_INT_RGB;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * {@link MapWidget#remove} used to take the panel apart through its {@code mapHolder} and
 * {@code map} fields whatever it was handed, so reaching it with no map on display called
 * {@code Container.remove(null)}:
 *
 * <pre>
 * java.lang.NullPointerException: Cannot read field "parent" because "comp" is null
 *   at java.desktop/java.awt.Container.remove(Container.java:1290)
 *   at VASSAL.build.widget.MapWidget.remove(MapWidget.java:104)
 *   at VASSAL.configure.ConfigureTree.remove(ConfigureTree.java:1376)
 *   at VASSAL.configure.ConfigureTree.delete(ConfigureTree.java:1368)
 * </pre>
 *
 * It threw before {@code super.remove(b)}, so the component stayed in the widget -- which is
 * what the reporter of vassalengine/vassal#13759 saw: "the Map Windows that I thought were
 * deleted were still there. They had not been deleted."
 */
public class MapWidgetTest {

  private MockedStatic<GameModule> staticGm;

  // Provide a dummy image for the launch button.
  // This prevents an IllegalArgument exception in the IconConfigurer for the launch button.
  // This occurs in the build phase and is unrelated to the exception under test.
  private MockedStatic<Op> op;
  private final ImageSourceOpBitmapImpl image = new ImageSourceOpBitmapImpl(new BufferedImage(1,1,TYPE_INT_RGB));

  /** A WidgetMap registers key listeners and a preference as it builds. */
  @BeforeEach
  public void mockModule() {
    staticGm = Mockito.mockStatic(GameModule.class);
    final GameModule gm = mock(GameModule.class, RETURNS_DEEP_STUBS);
    final Prefs prefs = mock(Prefs.class);
    // The components a Map builds read a handful of preferences; none of them matters here.
    when(prefs.getValue(anyString())).thenReturn(Integer.valueOf(0));
    when(gm.getPrefs()).thenReturn(prefs);
    when(gm.getDataArchive()).thenReturn(mock(DataArchive.class));
    // Not deep-stubbed: AWT calls real methods on whatever window it is handed, and a mocked
    // Window has none of its internals, which throws on the event dispatch thread.
    when(gm.getPlayerWindow()).thenReturn(null);
    staticGm.when(GameModule::getGameModule).thenReturn(gm);
    op = Mockito.mockStatic(Op.class);
    op.when(() -> Op.load(anyString())).thenReturn(image);
  }

  @AfterEach
  public void releaseModule() {
    staticGm.close();
    op.close();
  }

  /**
   * The widget with one map in it. MapWidget.add is what puts a map's components into the
   * panel, which is all this class is about, so the map is handed straight to it.
   *
   * <p>Two things Builder would also do are left out because they want a display, and neither
   * touches the panel: a map built with build(null) rather than from an element goes on to add
   * a CounterDetailViewer, and Map.addTo asks for the default DragSource. Both throw
   * HeadlessException on a machine without one, as CI is.</p>
   */
  private static MapWidget widgetWithOneMap() {
    final MapWidget w = new MapWidget();
    w.add(newMap());
    return w;
  }

  private static WidgetMap newMap() {
    final Document doc = Builder.createNewDocument();
    final WidgetMap m = new WidgetMap();
    m.build(doc.createElement(WidgetMap.class.getName()));
    return m;
  }

  private static JPanel panelOf(MapWidget w) {
    return (JPanel) w.getComponent();
  }

  private static boolean panelHolds(MapWidget w, Component c) {
    return Arrays.asList(panelOf(w).getComponents()).contains(c);
  }

  @Test
  public void testRemovingTheSameMapTwiceDoesNotThrow() {
    final MapWidget w = widgetWithOneMap();
    final WidgetMap map = w.getMap();

    w.remove(map);
    assertDoesNotThrow(() -> w.remove(map),
      "removing a map which has already gone must be a no-op");
  }

  @Test
  public void testRemovingASupersededMapDoesNotThrow() {
    final MapWidget w = widgetWithOneMap();
    final WidgetMap first = w.getMap();

    // A second map takes the panel over from the first. add() has always allowed this -- it
    // begins by dropping whatever is on display.
    final WidgetMap second = newMap();
    w.add(second);
    assertSame(second, w.getMap());

    // Removing them in turn is what ConfigureTree.delete's postorder walk does.
    w.remove(first);
    assertDoesNotThrow(() -> w.remove(second),
      "removing the second of two maps must not throw");

    // The symptom behind the reports: the NPE arrived before super.remove(b), so the map was
    // never taken out of the widget and was still in the module when it was saved.
    assertFalse(Arrays.asList(w.getConfigureComponents()).contains(first));
    assertFalse(Arrays.asList(w.getConfigureComponents()).contains(second),
      "a map which failed to delete used to come back when the module was reopened");
  }

  @Test
  public void testRemoveStillTakesTheComponentOutOfTheWidget() {
    final MapWidget w = widgetWithOneMap();
    final WidgetMap map = w.getMap();
    assertTrue(Arrays.asList(w.getConfigureComponents()).contains(map));

    w.remove(map);

    assertFalse(Arrays.asList(w.getConfigureComponents()).contains(map));
    assertNull(w.getMap(), "the widget no longer has a map on display");
  }

  @Test
  public void testRemovingAMapTakesItsComponentsOutOfThePanel() {
    final MapWidget w = widgetWithOneMap();
    final WidgetMap map = w.getMap();
    assertTrue(panelHolds(w, map.getLayeredPane()));
    assertTrue(panelHolds(w, map.getToolBar()));

    w.remove(map);

    assertFalse(panelHolds(w, map.getLayeredPane()));
    assertFalse(panelHolds(w, map.getToolBar()));
  }

  @Test
  public void testASupersededMapLeavesNothingBehindInThePanel() {
    final MapWidget w = widgetWithOneMap();
    final WidgetMap first = w.getMap();

    final WidgetMap second = newMap();
    w.add(second);

    // add() dropped the old holder but never the old toolbar, which stayed in the panel with
    // nothing owning it.
    assertFalse(panelHolds(w, first.getLayeredPane()));
    assertFalse(panelHolds(w, first.getToolBar()),
      "the superseded map's toolbar must go too");
    assertTrue(panelHolds(w, second.getLayeredPane()));
    assertTrue(panelHolds(w, second.getToolBar()));
  }

  @Test
  public void testRemovingSomethingWhichIsNotAMapIsUnaffected() {
    final MapWidget w = widgetWithOneMap();
    final WidgetMap map = w.getMap();
    final Component[] before = panelOf(w).getComponents();

    w.remove(mock(Buildable.class));

    assertSame(map, w.getMap());
    assertArrayEquals(before, panelOf(w).getComponents());
  }
}
