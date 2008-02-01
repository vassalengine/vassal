package VASSAL.build.module.map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.counters.ColoredBorder;

/*
 * A Container for Selection Highlighters
 */
public class SelectionHighlighters extends AbstractConfigurable {
  protected Map map;

  public SelectionHighlighters() {
    super();
  }

  public static String getConfigureTypeName() {
    return "Additional Selection Highlighters";
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String key, Object value) {
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{SelectionHighlighter.class};
  }

  public void addTo(Buildable parent) {
    map = (Map) parent;
  }

  public void removeHighlighter(SelectionHighlighter highlighter) {
    if (map.getHighlighter() instanceof ColoredBorder) {
      ((ColoredBorder) map.getHighlighter()).removeHighlighter(highlighter);
    }
  }

  public void addHighlighter(SelectionHighlighter highlighter) {
    if (map.getHighlighter() instanceof ColoredBorder) {
      ((ColoredBorder) map.getHighlighter()).addHighlighter(highlighter);
    }
  }

  public Configurer getConfigurer() {
    return null;
  }
}
