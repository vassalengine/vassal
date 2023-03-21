package VASSAL.tools;

import javax.swing.JToolBar;
import java.awt.Component;

/**
 * Indicates a component with a toolbar
 * @author rkinney
 *
 */
@FunctionalInterface
public interface ToolBarComponent {
  JToolBar getToolBar();

  /**
   * Update the text labels in the toolbar's component buttons (for mutable button label support)
   */
  default void updateToolbarButtons() {
    final JToolBar toolbar = getToolBar();
    if (toolbar == null) return;

    final int tools = toolbar.getComponentCount();
    for (int t = 0; t < tools; t++) {
      final Component tool = toolbar.getComponentAtIndex(t);
      if (tool instanceof LaunchButton) {
        ((LaunchButton)tool).updateText();
      }
    }
  }
}
