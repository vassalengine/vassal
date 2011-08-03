
package VASSAL.tools;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.swing.Action;


public class ActionManager {
  private final Map<Object,Action> map = new HashMap<Object,Action>();

  private static final ActionManager instance = new ActionManager();

  private ActionManager() { }

  public static ActionManager getInstance() {
    return instance;
  }

  public Set<?> getActionIds() {
    return map.keySet();
  }

  public Action getAction(Object id) {
    return map.get(id);
  }

  public Action addAction(Action a) {
    return addAction(a.getValue(Action.ACTION_COMMAND_KEY), a);
  }

  public Action addAction(Object id, Action a) {
    return map.put(id, a);
  }

  public Action removeAction(Object id) {
    return map.remove(id);
  }

  public boolean isEnabled(Object id) {
    final Action a = map.get(id);
    return a != null && a.isEnabled();
  }

  public void setEnabled(Object id, boolean enabled) {
    final Action a = map.get(id);
    if (a != null) a.setEnabled(enabled);
  }
}
