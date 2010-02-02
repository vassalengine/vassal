package VASSAL.tools.nio.file.attribute;

import java.util.Set;

public final class AclEntry {
  @Override
  public boolean equals(Object o) {
    throw new UnsupportedOperationException();
  }

  public Set<AclEntryFlag> flags() {
    throw new UnsupportedOperationException();
  }

  @Override
  public int hashCode() {
    throw new UnsupportedOperationException();
  }

  public static AclEntry.Builder newBuilder() {
    throw new UnsupportedOperationException();
  }

  public static AclEntry.Builder newBuilder(AclEntry entry) {
    throw new UnsupportedOperationException();
  }

  public Set<AclEntryPermission> permissions() {
    throw new UnsupportedOperationException();
  }

  public UserPrincipal principal() {
    throw new UnsupportedOperationException();
  }

  @Override
  public String toString() {
    throw new UnsupportedOperationException();
  }

  public AclEntryType type() {
    throw new UnsupportedOperationException();
  }

  public static final class Builder {
    public AclEntry build() {
      throw new UnsupportedOperationException();
    }

    public Builder setFlags(AclEntryFlag... flags) {
      throw new UnsupportedOperationException();
    }

    public Builder setFlags(Set<AclEntryFlag> flags) {
      throw new UnsupportedOperationException();
    }

    public Builder setPermissions(AclEntryPermission... perms) {
      throw new UnsupportedOperationException();
    }

    public Builder setPermissions(Set<AclEntryPermission> perms) {
      throw new UnsupportedOperationException();
    }

    public Builder setPrincipal(UserPrincipal who) {
      throw new UnsupportedOperationException();
    }

    public Builder setType(AclEntryType type) {
      throw new UnsupportedOperationException();
    }
  }
}
