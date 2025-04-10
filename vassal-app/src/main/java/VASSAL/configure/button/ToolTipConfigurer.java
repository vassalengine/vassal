package VASSAL.configure.button;

import VASSAL.configure.Configurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.i18n.Resources;

import javax.swing.ImageIcon;

public class ToolTipConfigurer {

    private ToolTipConfigurer() {
    }

    public static void addToolTipToButtonIcon(String promptValue, Configurer config) {
        if (Resources.getString(Resources.BUTTON_ICON).equals(promptValue)
                && config instanceof IconConfigurer
                && ((IconConfigurer) config).getIconValue() instanceof ImageIcon) {
            ((IconConfigurer) config).setToolTipText(config.getValueString());
        }
    }

}
