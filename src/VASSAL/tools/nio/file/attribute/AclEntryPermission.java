package VASSAL.tools.nio.file.attribute;

public enum AclEntryPermission {
  APPEND_DATA,
  DELETE,
  DELETE_CHILD,
  EXECUTE,
  READ_ACL,
  READ_ATTRIBUTES,
  READ_DATA,
  READ_NAMED_ATTRS,
  SYNCHRONIZE,
  WRITE_ACL,
  WRITE_ATTRIBUTES,
  WRITE_DATA,
  WRITE_NAMED_ATTRS,
  WRITE_OWNER;

  public static final AclEntryPermission ADD_FILE = WRITE_DATA;
  public static final AclEntryPermission ADD_SUBDIRECTORY = APPEND_DATA;
  public static final AclEntryPermission LIST_DIRECTORY = READ_DATA;
}
