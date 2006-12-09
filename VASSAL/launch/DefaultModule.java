package VASSAL.launch;

import java.io.IOException;
import VASSAL.build.GameModule;
import VASSAL.build.module.ServerConnection;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;

public class DefaultModule extends GameModule {

  protected DefaultModule(DataArchive archive, Prefs globalPrefs) {
    super(archive);
    // TODO Auto-generated constructor stub
  }

  protected void build() throws IOException {
    // TODO Auto-generated method stub
    
  }

  public ServerConnection getServer() {
    // TODO Auto-generated method stub
    return null;
  }
}
