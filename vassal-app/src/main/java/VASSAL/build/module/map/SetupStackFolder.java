package VASSAL.build.module.map;

import VASSAL.build.AbstractFolder;

public class SetupStackFolder extends AbstractFolder {
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { SetupStack.class, this.getClass()};
  }
}
