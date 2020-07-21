package VASSAL.command;

import VASSAL.build.GameModule;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.property.PersistentPropertyContainer;

/**
 * This Command sets a Persistent Property in a PersistentPropertyContainer.
 * Currently only BasicPiece and Decorator implement PersistentPropertyContainer.
 * The Undo Command is a SetPropertyCommand to set the value back to the original value.
 */
public class SetPersistentPropertyCommand extends Command {
  public static final String COMMAND_PREFIX = "SPP\t";
  protected Object key;
  protected Object oldValue;  
  protected Object newValue;
  protected String id;
  

  public SetPersistentPropertyCommand(String id, Object key, Object oldValue, Object newValue) {
    setKey(key);
    setOldValue (oldValue);
    setNewValue (newValue);
    setId (id);
  }

  protected void executeCommand() {
    GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(id);
    if (target != null) {
      // Not all GamePieces will have persistent Properties
      if (target instanceof PersistentPropertyContainer) {
        ((Decorator) target).setPersistentProperty(getKey(), getNewValue());
      }
    }
  }

  protected Command myUndoCommand() {
    return new SetPersistentPropertyCommand (id, key, newValue, oldValue);
  }

  @Override
  public String getDetails() {
    return "id="+id+",key="+key+",old="+oldValue+",new="+newValue;
  }
  
  public Object getKey() {
    return key;
  }

  public void setKey(Object key) {
    this.key = key;
  }

  public Object getOldValue() {
    return oldValue;
  }

  public void setOldValue(Object oldValue) {
    this.oldValue = oldValue;
  }

  public Object getNewValue() {
    return newValue;
  }

  public void setNewValue(Object newValue) {
    this.newValue = newValue;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

}
