package VASSAL.build.module.properties;

import VASSAL.counters.DynamicProperty;
import VASSAL.script.expression.Auditable;

public class RemoteEnumeratedPropertyPrompt extends EnumeratedPropertyPrompt implements RemotePropertyChanger {

  public RemoteEnumeratedPropertyPrompt(EnumeratedPropertyPrompt prompt) {
    super(prompt.dialogParent, prompt.promptText, prompt.validValues, prompt.constraints);
  }

  @Override
  public String getNewValue(DynamicProperty target, Auditable owner, PropertySource ps) {
    return super.getNewValue("");
  }
}
