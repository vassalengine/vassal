package VASSAL.tools.nio.file;

import java.io.IOException;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.Set;

import VASSAL.tools.io.IOUtils;

import VASSAL.tools.nio.file.attribute.Attributes;
import VASSAL.tools.nio.file.attribute.BasicFileAttributes;
import VASSAL.tools.nio.file.attribute.FileAttribute;

public final class Files {
  public static void createDirectories(Path dir, FileAttribute<?>... attrs)
                                                           throws IOException {
    if (attrs.length > 0) throw new UnsupportedOperationException();

    // iterate over all name elements, creating the ones which don't exist
    for (Iterator<Path> i = dir.iterator(); i.hasNext(); ) {
      final Path p = i.next();
      if (p.exists()) {
        if (Boolean.FALSE.equals(p.getAttribute("basic:isDirectory"))) {
          throw new FileAlreadyExistsException(dir.toString());
        }
      }
      else {
        p.createDirectory();
      }
    }
  }

  public static String probeContentType(FileRef file) {
    throw new UnsupportedOperationException();
  } 
  
  public static void walkFileTree(Path start,
                                  FileVisitor<? super Path> visitor) {
    walkFileTree(
      start,
      EnumSet.noneOf(FileVisitOption.class),
      Integer.MAX_VALUE,
      visitor
    );
  }

  public static void walkFileTree(
    Path start,
    Set<FileVisitOption> options,
    int maxDepth,
    FileVisitor<? super Path> visitor)
  {
    // We don't support any options.
    if (!options.isEmpty()) throw new UnsupportedOperationException();

    if (maxDepth < 0) throw new IllegalArgumentException();

    walk(start, 0, maxDepth, visitor);
  }

  private static FileVisitResult walk(Path path, int depth, int maxDepth,
                                      FileVisitor<? super Path> visitor) {
    if (depth > maxDepth) return FileVisitResult.CONTINUE;

    IOException ex = null;
    BasicFileAttributes attrs = null;

    try {
      attrs = Attributes.readBasicFileAttributes(path);
    }
    catch (IOException e) {
      ex = e;
    }

    if (attrs == null) return visitor.visitFileFailed(path, ex);
  
    if (attrs.isDirectory()) {
      try {
        DirectoryStream<Path> ds = null;
        FileVisitResult res;

        try {
          ds = path.newDirectoryStream();
        }
        catch (IOException e) {
          visitor.preVisitDirectoryFailed(path, e);
        }

        try {
          res = visitor.preVisitDirectory(path);
          if (res != FileVisitResult.CONTINUE) return res;
      
          for (Path child : ds) {
            switch (walk(child, depth+1, maxDepth, visitor)) {
            case TERMINATE:     return res;
            case SKIP_SIBLINGS: break;
            default:
            }
          }

          ds.close();
        }
        finally {
          IOUtils.closeQuietly(ds);
        }

      }
      catch (IOException e) {
        ex = e;
      }  

      return visitor.postVisitDirectory(path, ex); 
    }
    else {
      return visitor.visitFile(path, attrs);
    }
  }
}
