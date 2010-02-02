/*
 * Copyright 2007-2009 Sun Microsystems, Inc.  All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Sun Microsystems nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package VASSAL.tools.nio.file.zipfs;

import VASSAL.tools.nio.file.*;
//import java.nio.file.*;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.regex.*;


public class ZipPathParser {

  public static final String invalidChars = "\":<>*?";

  static class Result {

    String zipFile;
    byte[] pathInZip;

    Result(String path, String pathInZip) {
      this.zipFile = path;
      this.pathInZip = pathInZip.getBytes();
    }
  }

  static Result parse(String path) {

    if (path == null) {
      throw new NullPointerException();
    }
    if (path.equals("")) {
      throw new InvalidPathException(path, "path should not be empty");
    }
    int prefixLen = getPrefixLen(path);

    // We got the prefix length, and get all the components.
    int off = findZipComponent(prefixLen, path);
    if (off == -1) {
      try {
        Paths.get(path); //if any invalid char includes in the
      // path this statement throws IPE with specific position.
      } 
      catch (InvalidPathException e) {
        throw new InvalidPathException("", e.getMessage()); //This reduces long stack trace
      }
      throw new InvalidPathException(path, "Archive Jar/Zip Not Found in the path");
    }
    String pathZip = path.substring(0, off);
    Path pathUpToZip = Paths.get(pathZip);
    String nomalizedPathInZip = null;
    if (off != path.length()) {
      String pathInZip = path.substring(off, path.length());
      nomalizedPathInZip = normalize(pathInZip);
    }
    else {
      nomalizedPathInZip = "/"; //Default path
    }
    return new Result(pathUpToZip.toString(), nomalizedPathInZip);
  }

  static char fileSepa = java.io.File.separatorChar;

  static int getPrefixLen(String path) {

    int pathLen = path.length();
    if (pathLen == 0) {
      return -1;
    }
    char ch0 = path.charAt(0);
    if (pathLen > 0 && isSeparator(ch0)) {
      if (pathLen > 1) {
        char ch1 = path.charAt(1);
        if (isSeparator(ch1) && fileSepa == '\\') {
          int off = 2;
          off = nextNonSeparator(path, off); //server comp begin index
          if (off == pathLen) {
            throw new InvalidPathException(path, "does not contain Server component");
          }
          off = nextSeparator(path, off); //server comp end index
          off = nextNonSeparator(path, off); //share comp begin index
          if (off == pathLen) {
            throw new InvalidPathException(path, "does not contain share");
          }
          off = nextSeparator(path, off); //share comp end index
          off = nextNonSeparator(path, off); // if any separators follows immediately
          return off; // comment the above line also works perfect.
        }
      }
      int off = 1;
      off = nextNonSeparator(path, off); // unix - all the preceding separators
      return off; //you can comment the above line  incase you dont want
    //prefix length upto first component of the path
    }
    else if (pathLen > 1) {
      if (fileSepa == '\\' && path.charAt(1) == ':') { // fileSepa check is needed
        ch0 = Character.toUpperCase(ch0);      // as it is applicable only in Windows
        if (ch0 >= 'A' && ch0 <= 'Z') {
          if (pathLen > 2 && isSeparator(path.charAt(2))) {
            int off = 2;
            off = nextNonSeparator(path, off); //if any separators follows after C:////
            return off;  //incase you comment the above line return 3
          }
          else {
            return 2;
          }
        }
        throw new InvalidPathException(path, "Invalid Prefix in Windows"); //Illegal Prefix in Windows
      }
      return 0; // path length >1 and No Prefix
    }
    return 0; //Path length=1 and it does not have any prefix
  }

  static int nextNonSeparator(String path, int off) {
    int pathLen = path.length();
    while ((off < pathLen) && isSeparator(path.charAt(off))) {
      off++;
    }
    return off;
  }

  static int nextSeparator(String path, int off) {
    int pathLen = path.length();
    while ((off < pathLen) && !(isSeparator(path.charAt(off)))) {
      off++;
    }
    return off;
  }

  static boolean isSeparator(char ch) {
    if (fileSepa == '\\') {
      return ((ch == '\\') || (ch == '/')); //in Windows file separator is \\ or /
    }
    else {
      return (ch == '/'); //in unix file separator is /
    }
  }

  static LinkedList<String> getComponents(String path) {
    LinkedList<String> compList = new LinkedList<String>();
    if (path == null || path.equals("")) {
      return compList;
    }
    int firstChar = path.charAt(0);
    int prefix = (firstChar == '/') ? 1 : 0;
    int compStartIndex = prefix; // prefix starts with componenent begin index
    int compEndIndex = prefix;
    int pathLen = path.length();

    while (compEndIndex < pathLen) {
      compStartIndex = nextNonSeparator(path, compEndIndex);
      compEndIndex = nextSeparator(path, compStartIndex);
      if (compStartIndex != compEndIndex) {
        compList.add(path.substring(compStartIndex, compEndIndex));
      }
    }
    return compList;
  }

  private static int findZipComponent(int prefixLen, String path) {
    int compStartIndex = prefixLen; // prefix starts with componenent begin index
    int compEndIndex = prefixLen;
    int pathLen = path.length();

    Pattern pattern = Pattern.compile("\\.(?i)(zip|jar)");
    //".*\\.(?i)(\\Qzip\\E|\\Qjar\\E)[\\/]*"
    Matcher matcher = null;
    while (compEndIndex < pathLen) {
      compStartIndex = nextNonSeparator(path, compEndIndex);
      compEndIndex = nextSeparator(path, compStartIndex);
      String pathComp = path.substring(compStartIndex, compEndIndex);
      matcher = pattern.matcher(pathComp);
      boolean b = matcher.find();
      if (b) {
        return compEndIndex;
      }
    }
    return -1;
  }

  static String normalize(String pathInZip) {
    int len = pathInZip.length();
    char pathArr[] = new char[len];
    pathInZip.getChars(0, len, pathArr, 0);

    //Repleace all Separators \\ with Zip Separator /
    //throws InavalidPathException in Windows and Unix if char is not valid
    // in respective file systems.

    for (int i = 0; i < len; i++) {
      if (pathArr[i] == '\\') {
        pathArr[i] = '/';
      }
      if (fileSepa == '\\') { //If Path is in Windows
        if ((pathArr[i] < '\u0020') || (invalidChars.indexOf(pathArr[i]) != -1)) {
          throw new InvalidPathException(pathInZip, "Invalid char at " + i);
        }
        if ((i > 0) && (((pathArr[i] == '/') && (pathArr[i - 1] == ' ')) ||
            ((i == len - 1) && pathArr[i] == ' '))) {
          throw new InvalidPathException(pathInZip, "Trailing space at" + (i - 1));
        }
      }
      else if (fileSepa == '/') { //If path is in Unix
        if (pathArr[i] == '\u0000') {
          throw new InvalidPathException(pathInZip, "Null char at" + i);
        }
      }
    }

    // Remove if any extra slashes
    int size = len;
    for (int i = 1; i < size; i++) {
      if (pathArr[i] == '/' && pathArr[i - 1] == '/') {
        System.arraycopy(pathArr, i, pathArr, i - 1, size - i);
        size--;
        i--;
      }
    }
  
    // Remove trailing slash unless whole path is '/'
    if (size > 1 && pathArr[size-1] == '/') size--;

    return new String(pathArr, 0, size);
  }

  // Remove DotSlash(./) and resolve DotDot (..) components
  static String resolve(String path) {

    final boolean prefix = path.startsWith("/");
    final LinkedList<String> parts = getComponents(path);

    int previousDirsAtBeginning = 0;

    // Remove redundant parts.
    for (ListIterator<String> i = parts.listIterator(); i.hasNext(); ) {
      final String part = i.next();

      if (part.equals(".")) {
        // ".": Remove.
        i.remove();
      }
      else if (part.equals("..")) {  
        // "..": Absolute paths: Remove this and the previous name, if any.
        // Relative paths: Keep if we are part of a leading string of ".."
        // names; otherwise remove this and the previous name.
        if (prefix || i.previousIndex() > previousDirsAtBeginning) {
          i.remove();

          // hasPrevious() can be false in the case of the absolute path "/.."
          if (i.hasPrevious()) {
            i.previous();
            i.remove();
          }
        }
        else {
          // We are relative, must keep leading ".."
          previousDirsAtBeginning++;
        }
      }
      else {
        // Otherwise keep this name (for now).
      }
    }

    // Construct the resulting path string.
    final StringBuilder res = new StringBuilder(prefix ? "/" : "");

    final Iterator<String> i = parts.iterator();
    if (i.hasNext()) { 
      res.append(i.next());
      while (i.hasNext()) {
        res.append("/").append(i.next());
      }
    }

    return res.toString();
  }
}
