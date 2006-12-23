package VASSAL.launch;

import java.io.IOException;
import VASSAL.build.GameModule;
import VASSAL.build.module.ServerConnection;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;

public class BasicModule extends GameModule {

  protected BasicModule(DataArchive archive, Prefs globalPrefs) {
    super(archive);
  }

  protected void build() throws IOException {
    // TODO Auto-generated method stub
    
  }

  public ServerConnection getServer() {
    // TODO Auto-generated method stub
    return null;
  }

}
