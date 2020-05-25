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
  protected List<SelectionHighlighter> highlighters = new ArrayList<>();

  public SelectionHighlighters() {
    super();
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.AddedSelectionHighlights.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public void setAttribute(String key, Object value) {
  }

  @Override
  public String getAttributeValueString(String key) {
    return null;
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{SelectionHighlighter.class};
  }

  @Override
  public void addTo(Buildable parent) {
    map = (Map) parent;
    for (SelectionHighlighter highlighter : highlighters) {
      addToMap(highlighter);
    }
  }

  @Override
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

  @Override
  public Configurer getConfigurer() {
    return null;
  }
}
