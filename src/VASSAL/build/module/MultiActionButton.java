package VASSAL.build.module;

import java.awt.Component;
import javax.swing.JMenuItem;

import VASSAL.build.module.documentation.HelpFile;

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
    setAttribute(BUTTON_TEXT, "Multi-Action");
    setAttribute(TOOLTIP, ""); //$NON-NLS-1$
    launch.putClientProperty(MENU_PROPERTY, null);
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] {"Button text:  ", "Tooltip text:  ", "Button Icon:  ", "Hotkey:  ", "Buttons"};
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
    return "Multi-Action Button";
  }
  
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MultiActionButton.htm"); //$NON-NLS-1$
  }
  
}
