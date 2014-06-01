package VASSAL.build.module.map;

import java.util.ArrayList;
import java.util.List;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.counters.ColoredBorder;
import VASSAL.i18n.Resources;

/*
 * A Container for Selection Highlighters
 */
public class SelectionHighlighters extends AbstractConfigurable {
  protected Map map;
  protected List<SelectionHighlighter> highlighters = new ArrayList<SelectionHighlighter>();

  public SelectionHighlighters() {
    super();
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.AddedSelectionHighlights.component_type"); //$NON-NLS-1$
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

  public HelpFile getHelpFile() {
    return null;
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{SelectionHighlighter.class};
  }

  public void addTo(Buildable parent) {
    map = (Map) parent;
    for (SelectionHighlighter highlighter : highlighters) {
      addToMap(highlighter);
    }
  }

  public void removeFrom(Buildable parent) {
    for (SelectionHighlighter highlighter : highlighters) {
      removeFromMap(highlighter);
    }
  }

  public void removeHighlighter(SelectionHighlighter highlighter) {
    highlighters.remove(highlighter);
    removeFromMap(highlighter);
  }

  public void addHighlighter(SelectionHighlighter highlighter) {
    highlighters.add(highlighter);
    addToMap(highlighter);
  }

  protected void addToMap(SelectionHighlighter highlighter) {
    if (map != null) {
      if (map.getHighlighter() instanceof ColoredBorder) {
        ((ColoredBorder) map.getHighlighter()).addHighlighter(highlighter);
      }
    }
  }

  protected void removeFromMap(SelectionHighlighter highlighter) {
    if (map != null) {
      if (map.getHighlighter() instanceof ColoredBorder) {
        ((ColoredBorder) map.getHighlighter()).removeHighlighter(highlighter);
      }
    }
  }

  public Configurer getConfigurer() {
    return null;
  }
}
