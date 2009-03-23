package VASSAL.build.module;

import java.awt.Component;
import javax.swing.JMenuItem;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;

/**
 * Combines multiple buttons from the toolbar into a single button. Pushing the single button is equivalent to pushing
 * the other buttons in order.
 * 
 * @author rkinney
 * 
 */
public class MultiActionButton extends ToolbarMenu {

  public MultiActionButton() {
    super();
    setAttribute(BUTTON_TEXT, Resources.getString("Editor.MultiActionButton.component_type")); //$NON-NLS-1$
    setAttribute(TOOLTIP, Resources.getString("Editor.MultiActionButton.component_type")); //$NON-NLS-1$
    launch.putClientProperty(MENU_PROPERTY, null);
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] {
        Resources.getString(Resources.DESCRIPTION),
    		Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
        Resources.getString("Editor.MultiActionButton.buttons") //$NON-NLS-1$
    };
  }

  public void launch() {
    for (int i=0,n=menu.getComponentCount();i<n;++i) {
      Component c = menu.getComponent(i);
      if (c instanceof JMenuItem) {
        ((JMenuItem)c).doClick();
      }
    }
  }
  
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MultiActionButton.component_type"); //$NON-NLS-1$
  }
  
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MultiActionButton.htm"); //$NON-NLS-1$
  }
  
}
