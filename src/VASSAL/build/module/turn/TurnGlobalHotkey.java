package VASSAL.build.module.turn;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.counters.BasicPiece;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;

public class TurnGlobalHotkey extends AbstractConfigurable {

  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String MATCH = "match"; //$NON-NLS-1$
  public static final String REPORT_FORMAT = "reportFormat"; //$NON-NLS-1$

  protected PropertyExpression match = new PropertyExpression();
  protected NamedKeyStroke hotkey;
  protected FormattedString format = new FormattedString();
  protected BasicPiece checkPiece = new BasicPiece();

  public String[] getAttributeDescriptions() {
    return new String[] {
      "Description:  ",
      "Global Hotkey:  ",
      "Match Properties:  ",
      "Report Format:  "
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      NamedKeyStroke.class,
      PropertyExpression.class,
      ReportFormatConfig.class
    };
  }

  public static class ReportFormatConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PlayerIdFormattedStringConfigurer(key, name, new String[]{});
    }
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, HOTKEY, MATCH, REPORT_FORMAT};
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (HOTKEY.equals(key)) {
      return NamedHotKeyConfigurer.encode(hotkey);
    }
    else if (MATCH.equals(key)) {
      return match.getExpression();
    }
    else if (REPORT_FORMAT.equals(key)) {
      return format.getFormat();
    }
    else
      return null;
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (HOTKEY.equals(key)) {
      if (value instanceof String) {
        value = NamedHotKeyConfigurer.decode((String) value);
      }
      hotkey = (NamedKeyStroke) value;
    }
    else if (MATCH.equals(key)) {
      match.setExpression((String) value);
    }
    else if (REPORT_FORMAT.equals(key)) {
      format.setFormat((String) value);
    }

  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable parent) {

  }

  public void addTo(Buildable parent) {

  }

  public static String getConfigureTypeName() {
    return "Global Hotkey";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TurnTracker.htm","Hotkey"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void apply() {
    if (match.isNull() || match.accept(checkPiece)) {
      GameModule.getGameModule().fireKeyStroke(hotkey);
      String reportText = format.getLocalizedText();
      if (reportText.length() > 0) {
        Command c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + reportText);
        c.execute();
        GameModule.getGameModule().sendAndLog(c);
      }
    }
  }
}
