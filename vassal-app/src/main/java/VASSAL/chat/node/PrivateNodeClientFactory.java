package VASSAL.chat.node;

import java.util.Properties;

import VASSAL.chat.ChatServerConnection;

public class PrivateNodeClientFactory extends NodeClientFactory {
  public static final String PRIVATE_TYPE = "private"; //$NON-NLS-1$
  public static final String HOST = "privateHost"; //$NON-NLS-1$
  public static final String PORT = "privatePort"; //$NON-NLS-1$

  @Override
  public ChatServerConnection buildServer(Properties param) {
    param.setProperty(
      NodeClientFactory.NODE_HOST,
      param.getProperty(HOST, "localhost")
    );
    param.setProperty(
      NodeClientFactory.NODE_PORT, 
      param.getProperty(PORT, "5050")
    );
    return super.buildServer(param);
  }
}
