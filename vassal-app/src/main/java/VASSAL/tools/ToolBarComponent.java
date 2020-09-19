package VASSAL.tools;

import javax.swing.JToolBar;

/**
 * Indicates a component with a toolbar
 * @author rkinney
 *
 */
@FunctionalInterface
public interface ToolBarComponent {
  JToolBar getToolBar();
}
