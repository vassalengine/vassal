
package VASSAL.build.module.dice;

import VASSAL.build.module.DieRoll;

/** Describes a set of {@link DieRoll}s */
public class RollSet {
  public String description;
  public DieRoll[] dieRolls;

  public RollSet(String description, DieRoll[] rolls) {
    this.description = description;
    this.dieRolls = rolls;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public DieRoll[] getDieRolls() {
    return dieRolls;
  }

  public int getMaxDescLength() {
    int len = 0;
    for (int i = 0; i < dieRolls.length; i++) {
      len = Math.max(len, dieRolls[i].getDescription().length());
    }
    return len;
  }
}
