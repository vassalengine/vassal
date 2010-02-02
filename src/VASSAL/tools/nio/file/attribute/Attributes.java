package VASSAL.tools.nio.file.attribute;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import VASSAL.tools.nio.file.FileRef;
import VASSAL.tools.nio.file.FileStore;
import VASSAL.tools.nio.file.LinkOption;

public final class Attributes {
  public static List<AclEntry> getAcl(FileRef file) throws IOException {
    throw new UnsupportedOperationException();
  }

  public static UserPrincipal getOwner(FileRef file) throws IOException {
    throw new UnsupportedOperationException();
  }

  public static BasicFileAttributes readBasicFileAttributes(
                                         FileRef file, LinkOption... options)
                                                           throws IOException {
    return file.getFileAttributeView(
      BasicFileAttributeView.class, options).readAttributes();
  }

  public static DosFileAttributes readDosFileAttributes(
                                         FileRef file, LinkOption... options)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public static FileStoreSpaceAttributes readFileStoreSpaceAttributes(
                                                             FileStore store)
                                                           throws IOException {
    final FileStoreSpaceAttributeView v =
      store.getFileStoreAttributeView(FileStoreSpaceAttributeView.class);
    if (v == null) throw new UnsupportedOperationException();
    return v.readAttributes();
  }

  public static PosixFileAttributes readPosixFileAttributes(
                                         FileRef file, LinkOption... options)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public static void setAcl(FileRef file, List<AclEntry> acl) {
    throw new UnsupportedOperationException();
  }

  public static void setLastAccessTime(FileRef file, FileTime atime)
                                                           throws IOException {
    if (atime == null) throw new NullPointerException();
    file.getFileAttributeView(
      BasicFileAttributeView.class).setTimes(null, atime, null);
  }

  public static void setLastModifiedTime(FileRef file, FileTime mtime)
                                                           throws IOException {
    if (mtime == null) throw new NullPointerException();
    file.getFileAttributeView(
      BasicFileAttributeView.class).setTimes(mtime, null, null);
  }

  public static void setOwner(FileRef file, UserPrincipal owner)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }
  
  public static void setPosixFilePermissions(FileRef file,
                                             Set<PosixFilePermission> perms)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }
}
