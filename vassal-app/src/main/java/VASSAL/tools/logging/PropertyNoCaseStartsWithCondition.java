package VASSAL.tools.logging;

import ch.qos.logback.core.boolex.PropertyConditionBase;

public class PropertyNoCaseStartsWithCondition extends PropertyConditionBase {
  private String key;
  private String value;

  @Override
  public void start() {
    if (key == null) {
      addError("In PropertyNoCaseStartsWithCondition, 'key' parameter must not be null");
    }
    else if (value == null) {
      addError("In PropertyNoCaseStartsWithCondition, 'value' parameter must not be null");
    }
    else {
      super.start();
    }
  }
  
  public void setKey(String key) {
    this.key = key;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public boolean evaluate() {
    if (key == null) {
      addError("key must not be null");
      return false;
    }

    final String val = p(key);
    return val != null && val.toLowerCase().startsWith(value);
  }
}
