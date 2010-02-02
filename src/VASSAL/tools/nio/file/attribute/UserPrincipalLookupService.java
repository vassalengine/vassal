package VASSAL.tools.nio.file.attribute;

import java.io.IOException;

public abstract class UserPrincipalLookupService {
  protected UserPrincipalLookupService() {}

  public abstract UserPrincipal lookupPrincipalByName(String name)
                                                            throws IOException;

  public abstract GroupPrincipal lookupPrincipalByGroupName(String name)
                                                            throws IOException;
}
