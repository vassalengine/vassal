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
import VASSAL.tools.nio.file.attribute.*;
import VASSAL.tools.nio.file.spi.FileSystemProvider;

//import java.nio.file.*;
//import java.nio.file.attribute.*;
//import java.nio.file.spi.FileSystemProvider;

import java.net.URI;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.channels.FileChannel;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ZipFileSystemProvider extends FileSystemProvider {

  private String scheme = "zip";

  private Map<URI,ZipFileSystem> fileSystems =
    Collections.synchronizedMap(new HashMap<URI,ZipFileSystem>());

  public ZipFileSystemProvider() {
  }

  @Override
  public String getScheme() {
    return scheme;
  }

  @Override
  public FileSystem newFileSystem(URI uri, Map<String, ?> env)
      throws IOException {
    String scheme1 = uri.getScheme();
    if ((scheme1 == null) || !scheme1.equalsIgnoreCase(scheme)) {
      throw new IllegalArgumentException("URI scheme is not '" + scheme + "'");
    }

    URI uriPath = null;
    try {
      uriPath = new URI("file", uri.getHost(), uri.getPath(), null);
    }
    catch (URISyntaxException e) {
      throw new AssertionError(e); //never thrown
    }

    Path nativePath = null;
    try {
      nativePath = Paths.get(uriPath); //making use of underlying URI path parsing
    }
    catch (InvalidPathException e) {
      throw e;
    }

    if (!checkZipFilePath(nativePath)) {
      throw new InvalidPathException(nativePath.toString(), "file name does not contain zip/jar File");
    }
    nativePath.checkAccess(); // check the existance of the path before proceed

    ZipFileSystem fileSystem = null;
    // construct uri to find in cached file systems
    try {
      uriPath = new URI("zip", uri.getHost(), uri.getPath(), null);
    }
    catch (URISyntaxException e) {
      throw new AssertionError(e); //never thrown
    }

    String pathStr = nativePath.toAbsolutePath().toString(); // think whether to pass .toRealPath(true) to follow links

    String defaultdir = null;
    Object obj = null;
    if (env != null) {
      obj = env.get("default.dir");
      if ((obj != null) && !(obj instanceof String)) {
        throw new IllegalArgumentException();
      }
      defaultdir = (String) obj;
    }

    if (defaultdir == null) {
      defaultdir = "/";
    }

    if (defaultdir.charAt(0) != '/') {
      throw new IllegalArgumentException("default dir should be absolute");
    }

    if (!defaultdir.equals("/")) {
      defaultdir = ZipPathParser.normalize(defaultdir);
    }

    fileSystem = new ZipFileSystem(this, pathStr, defaultdir);
    fileSystems.put(uriPath, fileSystem);
    return fileSystem;
  }

  @Override
  public FileSystem newFileSystem(FileRef file,
      Map<String, ?> env)
      throws IOException {
    ZipFileSystem fileSystem = null;
    if (!((Path) file).toUri().getScheme().equalsIgnoreCase("file")) {
      throw new UnsupportedOperationException();
    }

    if (!checkZipFilePath(file)) {
      throw new UnsupportedOperationException();
    }

    try {
      ((Path) file).checkAccess();
    }
    catch (IOException e) {
      throw e;
    }

    String pathStr = ((Path) file).toAbsolutePath().toString(); //follow links
    String defaultdir = null;
    Object obj = null;
    if (env != null) {
      obj = env.get("default.dir");
      if ((obj != null) && !(obj instanceof String)) {
        throw new IllegalArgumentException();
      }
      defaultdir = (String) obj;
    }

    if (defaultdir == null) {
      defaultdir = "/";
    }

    if (defaultdir.charAt(0) != '/') {
      throw new IllegalArgumentException("default dir should be absolute");
    }

    if (!defaultdir.equals("/")) {
      defaultdir = ZipPathParser.normalize(defaultdir);
    }

    fileSystem = new ZipFileSystem(this, pathStr, defaultdir);
    return fileSystem;
  }

  boolean checkZipFilePath(FileRef file) {
    // check file ends with zip file
    Path path = ((Path) file);
    if (path.getName() == null) {
      return false;
    }
    String fileName = path.getName().toString().toLowerCase();
    boolean b = (fileName.endsWith(".zip") || fileName.endsWith(".jar"));
    return (b);
  }

  @Override
  public ZipFilePath getPath(URI uri) {

    String scheme1 = uri.getScheme();

    if ((scheme1 == null) || !scheme1.equalsIgnoreCase(scheme)) {
      throw new IllegalArgumentException("URI scheme is not '" + scheme + "'");
    }

    String fragment = uri.getFragment();
    if (fragment == null) {
      throw new IllegalArgumentException("uri " + uri + " does not contain path fragment ex. zip:///c:/test.zip#/DirA");
    }

    URI uripath = null;
    try {
      uripath = new URI(uri.getScheme(), uri.getHost(), uri.getPath(), null);
    }
    catch (URISyntaxException e) {
      throw new AssertionError(e);
    }

    ZipFileSystem fileSystem = fileSystems.get(uripath);
    if (fileSystem == null) {
      throw new FileSystemNotFoundException();
    }
    // if fragment is empty, the following method throws InvalidPathException.
    ZipFilePath path = fileSystem.getPath(fragment);
    return path;
  }

// FIXME: It's weird that this uses the default fs!
  @Override
  public FileChannel newFileChannel(Path path,
      Set<? extends OpenOption> options,
      FileAttribute<?>... attrs)
      throws IOException {
    FileSystem defFileSystem = FileSystems.getDefault();
    Path nativePath = defFileSystem.getPath(path.toString());
    return defFileSystem.provider().newFileChannel(nativePath, options);
  }

  @Override
  public FileSystem getFileSystem(URI uri) {
    final String scheme1 = uri.getScheme();
    if (scheme1 == null || !scheme.equalsIgnoreCase(getScheme())) {
      throw new IllegalArgumentException(
        "URI scheme is not '" + getScheme() + "'");
    }

    //construct uri ignoring fragement in the given URI
    URI uriPath = null;
    try {
      uriPath = new URI("zip", uri.getHost(), uri.getPath(), null);
    }
    catch (URISyntaxException e) {
      throw new AssertionError(e); //never thrown
    }

    ZipFileSystem fileSystem = fileSystems.get(uriPath);
    if (fileSystem == null) {
      throw new FileSystemNotFoundException();
    }
    return fileSystem;
  }

  void removeFileSystem(URI uri) {
    //construct uri ignoring fragement in the given URI
    URI uriPath = null;
    try {
      uriPath = new URI("zip", uri.getHost(), uri.getPath(), null);
    }
    catch (URISyntaxException e) {
      throw new AssertionError(e); //never thrown
    }
    fileSystems.remove(uriPath);
  }
}
