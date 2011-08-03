package VASSAL.build.widget;

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import VASSAL.build.module.Map;
import VASSAL.configure.VisibilityCondition;

public class WidgetMap extends Map {
  public WidgetMap() {
    super();
  }

  /*
   * Minimal setup - remove all docking and toolbar setup
   */
  public void setup(boolean show) {
    if (show) {
      toolBar.setVisible(true);
      theMap.revalidate();
    }
    else {
      pieces.clear();
      boards.clear();
      toolBar.setVisible(false);
    }
  }

  /**
   * Widget maps are always undocked
   */
  public boolean shouldDockIntoMainWindow() {
    return false;
  }

  /*
   * Hide options relating to toolbar buttons
   */
  public VisibilityCondition getAttributeVisibility(String name) {
    if (USE_LAUNCH_BUTTON.equals(name) || BUTTON_NAME.equals(name) || ICON.equals(name) || HOTKEY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public JComponent getView() {
    return super.getView();
  }

  /*
   * Make the scroll pane accessible to the widget
   */
  public JScrollPane getScroll() {
    return scroll;
  }
}
