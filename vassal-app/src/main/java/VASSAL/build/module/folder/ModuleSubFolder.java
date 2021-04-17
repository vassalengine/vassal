package VASSAL.build.module.folder;

import VASSAL.build.AbstractFolder;
import VASSAL.build.module.ChartWindow;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.DoActionButton;
import VASSAL.build.module.GlobalKeyCommand;
import VASSAL.build.module.Inventory;
import VASSAL.build.module.Map;
import VASSAL.build.module.MultiActionButton;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerHand;
import VASSAL.build.module.PrivateMap;
import VASSAL.build.module.RandomTextButton;
import VASSAL.build.module.SpecialDiceButton;
import VASSAL.build.module.StartupGlobalKeyCommand;
import VASSAL.build.module.ToolbarMenu;
import VASSAL.i18n.Resources;

public class ModuleSubFolder extends AbstractFolder {
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { this.getClass(), Map.class, PieceWindow.class,
                            ToolbarMenu.class, MultiActionButton.class, DoActionButton.class, DiceButton.class, GlobalKeyCommand.class,
                            StartupGlobalKeyCommand.class, Inventory.class, RandomTextButton.class, SpecialDiceButton.class,
                            // ChartWindow.class,  //BR// These little hierarchies weren't wanting to behave (visually) in a folder, for no reason I could discern, so presently I have deemed them "not worth it".
                            PrivateMap.class, PlayerHand.class};
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Folder.component_type");
  }
}
