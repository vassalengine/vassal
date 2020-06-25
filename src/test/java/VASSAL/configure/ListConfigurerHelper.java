package VASSAL.configure;

import java.util.List;

public final class ListConfigurerHelper {
  private ListConfigurerHelper() {
  }

  public static List<Configurer> getList(ListConfigurer keyCommandListConfig) {
    return keyCommandListConfig.configurers;
  }
}
