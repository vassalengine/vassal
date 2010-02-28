package VASSAL.tools.nio.file;

import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.lang.StringUtils;

import VASSAL.tools.ArrayUtils;

public abstract class AbstractPath extends Path {

  protected byte[] path;
  
  protected volatile int[] seps;

  protected abstract int[] splitPath(byte[] rawpath) ;

  protected void initSeps() {
    if (seps == null) {
      seps = splitPath(path);
    } 
  }

  public AbstractPath(byte[] path) {
    this.path = path;
  }

  public boolean endsWith(Path other) {
    if (other.isAbsolute()) {
      return this.isAbsolute() ? this.equals(other) : false;
    }
    else {
      final int oc = other.getNameCount();
      final int tc = this.getNameCount();
      return oc <= tc ? other.equals(this.subpath(tc-oc, tc)) : false;
    }
  }

  public Path getName() {
    initSeps();
    return seps.length == 0 ? null : subpath(seps.length-2, seps.length-1);
  }
  
  public Path getName(int index) {
    return subpath(index, index+1);
  }

  public int getNameCount() {
    initSeps();
    return seps.length > 0 ? seps.length-1 : 0;
  }

  public Path getParent() {
    initSeps();

    // a root has no parent
    if (seps.length == 0) return null;

    if (seps.length <= 2) {
      if (seps[0] == -1) {
        // a relative single-name path has no parent
        return null;
      }
      else {
        // an absolute single-name path has the root as its parent
        return getFileSystem().getPath(new String(path, 0, seps[0]+1));
      }
    }

    // all other paths have a parent
    return getFileSystem().getPath(new String(path, 0, seps[seps.length-2]));
  }

  public Path getRoot() {
    initSeps();

    if (seps.length == 0) return this;  // we are a root
    if (seps[0] == -1) return null;     // we are relative
    return getFileSystem().getPath(new String(path, 0, seps[0]+1));
  }

  public boolean isAbsolute() {
    initSeps();
    return seps.length == 0 || seps[0] != -1;
  }

  public Iterator<Path> iterator() {
    initSeps();  

    return new Iterator<Path>() {
      private int i = 0;

      public boolean hasNext() {
        return i < seps.length-1;
      }

      public Path next() {
        return getFileSystem().getPath(
          new String(ArrayUtils.copyOfRange(path, seps[i]+1, seps[++i])));
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  public Path normalize() {
    initSeps();  

    if (seps.length == 0) return this;   // root is already normalized
    
    int previousDirsAtBeginning = 0;

    final ArrayList<String> outputParts = new ArrayList<String>(seps.length);

    // Remove redundant parts.
    for (int i = 0; i < seps.length-1; ++i) {
      final String currentInputPart =
        new String(ArrayUtils.copyOfRange(path, seps[i]+1, seps[i+1]));

      // ".": Skip.
      if (currentInputPart.equals(".")) continue;

      // "..": Scratch this and the previous name, if any.
      if (currentInputPart.equals("..")) {
        final int s = outputParts.size();
        if (s > previousDirsAtBeginning) {
          outputParts.remove(s-1);
        }
        else if (!this.isAbsolute()){
          outputParts.add(currentInputPart);
          previousDirsAtBeginning++;
        }
        continue;
      }

      // Otherwise add this name to the list.
      outputParts.add(currentInputPart);
    }

    final String prefix = seps[0] == -1 ? null : new String(path, 0, seps[0]+1);

    final FileSystem fs = getFileSystem();
    String norm;

    if (outputParts.size() > 0) {
      // Rebuild the normalized path.
      norm = (seps[0] == -1 ? "" : new String(path, 0, seps[0]+1)) +
             StringUtils.join(outputParts, fs.getSeparator());
    }
    else {
      // Whole path is just the prefix, if any.
      norm = prefix;
    }

    return norm == null ? null : fs.getPath(norm);
  }

  public Path relativize(Path other) {
    // FIXME: Implementing this will require some thought...

    if (other.equals(this)) return null;

    if (this.isAbsolute() != other.isAbsolute()) {
      throw new IllegalArgumentException();
    }
    
    int i = 0;
    final int ti = this.getNameCount();
    final int oi = other.getNameCount();

// FIXME: do this by seps to create fewer objects
    // find first name mismatch
    for ( ; i < ti && i < oi; i++) {
      if (!this.getName(i).equals(other.getName(i))) break;
    }

    final StringBuilder sb = new StringBuilder();

    // backup to the mismatch
    final int nc = ti - i;
    for (int j = 0; j < nc; ++j) sb.append("../");

    // append the rest of the other path, if any
    if (i < oi) sb.append(other.subpath(i, oi).toString());

    return getFileSystem().getPath(sb.toString());
  }

  public Path resolve(Path other) {
    if (other == null) return this;
    if (other.isAbsolute()) return other;

    final FileSystem fs = getFileSystem();

    final byte[] opath = other.toString().getBytes();
    final byte[] sep = fs.getSeparator().getBytes();
    final byte[] res = new byte[path.length + sep.length + opath.length];
    System.arraycopy(path, 0, res, 0, path.length);
    System.arraycopy(sep, 0, res, path.length, sep.length);
    System.arraycopy(opath, 0, res, path.length + sep.length, opath.length);
    return fs.getPath(new String(res));
  }

  public Path resolve(String other) {
    return resolve(getFileSystem().getPath(other));
  }

  public boolean startsWith(Path other) {
    if (this.isAbsolute() != other.isAbsolute()) return false;

    final int oc = other.getNameCount();
    final int tc = this.getNameCount();

    return oc == 0 ? true : 
           oc <= tc ? other.subpath(0,oc).equals(this.subpath(0, oc)) : false;
  }

  public Path subpath(int start, int end) {
    initSeps();  

    if (start < 0) throw new IllegalArgumentException();
    if (start >= seps.length-1) throw new IllegalArgumentException();
    if (end <= start) throw new IllegalArgumentException();
    if (end > seps.length-1) throw new IllegalArgumentException();

    return getFileSystem().getPath(
      new String(ArrayUtils.copyOfRange(path, seps[start]+1, seps[end])));
  }

  @Override
  public String toString() {
    return new String(path);
  }
}
