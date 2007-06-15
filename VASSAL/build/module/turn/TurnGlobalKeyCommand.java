package VASSAL.build.module.turn;

import java.util.Collection;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.PieceFilter;
import VASSAL.counters.PropertiesPieceFilter;

public class TurnGlobalKeyCommand extends MassKeyCommand {

  public static final String CHECK = "check";
  
  protected String checkProperties = "";
  protected PieceFilter checkFilter;
  protected BasicPiece checkPiece = new BasicPiece();
  
  public TurnGlobalKeyCommand() {
    super();
  }

  public void addTo(Buildable parent) {
  }

  public void removeFrom(Buildable parent) {
  }

  
  public void apply() {
    if (checkFilter.accept(checkPiece)) {
      Collection<Map> l = GameModule.getGameModule().getComponentsOf(Map.class);
      GameModule.getGameModule().sendAndLog(globalCommand.apply((Map[]) l.toArray(new Map[l.size()]),filter));
    }
  }
  
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ICON.equals(name) || TOOLTIP.equals(name) || BUTTON_TEXT.equals(name) || HOTKEY.equals(name) || AFFECTED_PIECE_NAMES.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return false;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }
  
  public String[] getAttributeDescriptions() {
    String[] a = super.getAttributeDescriptions();
    String[] b = new String[a.length+1];
    b[0] = "Run when Global Properties match";
    System.arraycopy(a, 0, b, 1, a.length);
    return b;
  }
  
  public String[] getAttributeNames() {
    String[] a = super.getAttributeNames();
    String[] b = new String[a.length+1];
    b[0] = CHECK;
    System.arraycopy(a, 0, b, 1, a.length);
    return b;
  }
  
  public Class[] getAttributeTypes() {
    Class[] a = super.getAttributeTypes();
    Class[] b = new Class[a.length+1];
    b[0] = String.class;
    System.arraycopy(a, 0, b, 1, a.length);
    return b;
  }
  
  public String getAttributeValueString(String key) {
    if (CHECK.equals(key)) {
      return checkProperties;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }
  
  public void setAttribute(String key, Object value) {
    if (CHECK.equals(key)) {
      checkProperties = (String) value;
      checkFilter = PropertiesPieceFilter.parse(checkProperties);
    }
    else {
      super.setAttribute(key, value);
    }
  }
  
}
