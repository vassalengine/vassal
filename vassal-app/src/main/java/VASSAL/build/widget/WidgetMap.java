package VASSAL.build.widget;

import javax.swing.JScrollPane;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.configure.VisibilityCondition;

import java.util.List;

public class WidgetMap extends Map {
  /*
   * Minimal setup - remove all docking and toolbar setup
   */
  @Override
  public void setup(boolean show) {
    if (show) {
      toolBar.setVisible(true);
      theMap.revalidate();
    }
    else {
      pieces.clear();
      boards.clear();
      if (!GameModule.getGameModule().isLoadOverSemaphore()) {
        toolBar.setVisible(false);
      }
    }
  }

  /**
   * Widget maps are always undocked
   */
  @Override
  public boolean shouldDockIntoMainWindow() {
    return false;
  }

  /*
   * Hide options relating to toolbar buttons
   */
  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(USE_LAUNCH_BUTTON, BUTTON_NAME, ICON, HOTKEY).contains(name)) {
      return () -> false;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  /*
   * Make the scroll pane accessible to the widget
   */
  public JScrollPane getScroll() {
    return scroll;
  }
}
