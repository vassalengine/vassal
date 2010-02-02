package VASSAL.tools.nio.file.attribute;

import java.util.Set;

public interface PosixFileAttributes extends BasicFileAttributes {
  public GroupPrincipal group();

  public UserPrincipal owner();

  public Set<PosixFilePermission> permissions();
}
