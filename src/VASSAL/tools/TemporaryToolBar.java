package VASSAL.tools;

import java.awt.Component;

import javax.swing.JToolBar;

/**
 * Stores components in a dummy toolbar, then transfers them to another toolbar
 * component when it becomes available. Used to get around lazy creation of
 * toolbars in ToolBarComponents
 *
 * @author rkinney
 *
 */
public class TemporaryToolBar implements ToolBarComponent {
  private JToolBar tempToolBar = new JToolBar();
  private ToolBarComponent delegate;

  public JToolBar getToolBar() {
    return tempToolBar != null ? tempToolBar : delegate.getToolBar();
  }

  public void setDelegate(ToolBarComponent delegate) {
    if (tempToolBar != null) {
      while (tempToolBar.getComponentCount() > 0) {
        Component c = tempToolBar.getComponent(0);
        tempToolBar.remove(c);
        delegate.getToolBar().add(c);
      }
    }
    tempToolBar = null;
    this.delegate = delegate;
  }

}
