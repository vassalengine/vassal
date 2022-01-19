/*
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import java.io.Closeable;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.NoSuchFileException;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.SecureClassLoader;
import java.security.cert.Certificate;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.JOptionPane;

import VASSAL.i18n.Resources;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.ZipArchive;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends SecureClassLoader implements Closeable {

  protected FileArchive archive;

  protected List<DataArchive> extensions = new ArrayList<>();

// FIXME: these should go into a cache, like images have
  private final Map<String, AudioClip> soundCache =
    new HashMap<>();

  protected SortedSet<String> localImages = null;
  protected SortedSet<String>[] cachedLocalImages = new SortedSet[4];

  public static final String IMAGE_DIR = "images/"; //NON-NLS
  protected String imageDir = IMAGE_DIR;

  public static final String SOUND_DIR = "sounds/"; //NON-NLS
  protected String soundDir = SOUND_DIR;

  public static final String ICON_DIR = "icons/"; //NON-NLS

  protected DataArchive() {
    super(DataArchive.class.getClassLoader());
    resetLocalImages();
  }

  public DataArchive(String zipName, String imageDir) throws IOException {
    this();
    archive = new ZipArchive(zipName);
    this.imageDir = imageDir;
  }

  public DataArchive(String zipName) throws IOException {
    this(zipName, IMAGE_DIR);
  }

  @Override
  public String getName() {
    return archive == null ? "data archive" : archive.getName(); //NON-NLS
  }

  public FileArchive getArchive() {
    return archive;
  }

  public String getImagePrefix() {
    return imageDir;
  }

  public AudioClip getCachedAudioClip(String name) throws IOException {
    final String path = soundDir + name;
    AudioClip clip = soundCache.get(path);
    if (clip == null) {
      if (name.toLowerCase().endsWith(".mp3")) { //NON-NLS
        clip = new Mp3AudioClip(path);
      }
      else {
        try (InputStream stream = getInputStream(path)) {
          clip = new AudioSystemClip(stream);
        }
      }

      soundCache.put(path, clip);
    }
    return clip;
  }

  /**
   * Get an {@link InputStream} for the given file in the archive.
   *
   * @param fileName the name of the file
   * @return an <code>InputStream</code> which contains the file
   * @throws IOException if there is a problem reading the file
   * @throws FileNotFoundException if the file doesn't exist
   */
  public InputStream getInputStream(String fileName)
                                    throws IOException, FileNotFoundException {
    // requested file is a resource, try our JARs
    if (fileName.startsWith("/")) {
      final InputStream in = getClass().getResourceAsStream(fileName);
      if (in != null) {
        return in;
      }
      throw new FileNotFoundException("Resource not found: " + fileName); //NON-NLS
    }

    // Look in this archive and its extensions
    InputStream in = getInputStreamImpl(fileName);
    if (in != null) {
      return in;
    }

    //
    // Ridiculous crap we have to check for backwards compatibility
    //

    // Maybe someone unzipped and rezipped this module on a Mac with an
    // HFS+ filesystem and the filename contains decomposable characters,
    // so it got munged into NFD. Aauugh! Seriously, DIAF Apple!
    final String nfd = Normalizer.normalize(fileName, Normalizer.Form.NFD);
    if (!fileName.equals(nfd)) {
      in = getInputStreamImpl(nfd);
      if (in != null) {
        return in;
      }
    }

    // Maybe it's a resource missing its initial slash. Aauugh!
    in = getClass().getResourceAsStream("/" + fileName);
    if (in != null) {
      return in;
    }

    // Maybe it's an extensionless GIF? Aauugh!
    in = getInputStreamImpl(fileName + ".gif"); //NON-NLS
    if (in != null) {
      return in;
    }

    // Maybe it's an extensionless GIF resource. Aauugh!
    in = getClass().getResourceAsStream("/" + fileName + ".gif"); //NON-NLS
    if (in != null) {
      return in;
    }

    //
    // End of ridiculous crap
    //

    throw new FileNotFoundException(
      "'" + fileName + "' not found in " + getName()
    );
  }

  private InputStream getInputStreamImpl(String fileName)
                                    throws IOException {
    // requested file is in this archive
    if (archive != null && archive.contains(fileName)) {
      return archive.getInputStream(fileName);
    }

    // we don't have it, try our extensions
    for (final DataArchive ext : extensions) {
      try {
        return ext.getInputStream(fileName);
      }
      catch (FileNotFoundException | NoSuchFileException ignored) {
        // not found in this extension, try the next
      }
    }

    return null;
  }

  /**
   * Returns a URL pointing to the archive.
   *
   * @return a URL corresponding to this archive
   * @throws IOException if the archive has not yet been saved
   */
  public URL getURL() throws IOException {
    if (archive == null) {
      throw new IOException("Must save before accessing contents");
    }

    return URLUtils.toJarURL(archive.getName());
  }

  /**
   * Returns a URL pointing to the named file.
   *
   * @param fileName the name of the file
   * @return a URL corresponding to the file
   * @throws FileNotFoundException if the file doesn't exist
   * @throws IOException if some other problem occurs
   */
  public URL getURL(String fileName) throws IOException, FileNotFoundException {
    // requested file is a resource
    if (fileName.startsWith("/")) {
      return getClass().getResource(fileName);
    }

    if (archive == null) {
      throw new IOException("Must save before accessing contents");
    }

    if (archive.contains(fileName)) {
      return new URL(getURL(), fileName);
    }

    for (final DataArchive ext : extensions) {
      try {
        return ext.getURL(fileName);
      }
      catch (FileNotFoundException | NoSuchFileException e) {
        // not found in this extension, try the next
      }
    }

    throw new FileNotFoundException(
      "'" + fileName + "' not found in " + getName());
  }

  public boolean contains(String fileName) throws IOException {
    if (archive == null) return false;
    return archive.contains(fileName);
  }

  @Override
  public void close() throws IOException {
    if (archive != null) {
      archive.revert(); // ensure that we don't modify the archive
      archive.close();
    }
  }

  public String[] getImageNames() {
    final SortedSet<String> s = getImageNameSet();
    return s.toArray(new String[0]);
  }

  public SortedSet<String> getImageNameSet() {
    return getImageNameSet(false, false);
  }

  public SortedSet<String> getImageNameSet(boolean localized, boolean fullPath) {
    final TreeSet<String> s = new TreeSet<>();
    getImageNamesRecursively(s, localized, fullPath);
    return s;
  }

  private int getLocalImagesCacheIndex(boolean localized, boolean fullPath) {
    int i = 0;
    if (localized) {
      i = 1;
    }
    if (fullPath) {
      i += 2;
    }
    return i;
  }

  protected void getImageNamesRecursively(SortedSet<String> s, boolean localized, boolean fullPath) {
    final int index = getLocalImagesCacheIndex(localized, fullPath);
    if (cachedLocalImages[index] == null) {
      cachedLocalImages[index] = getAllLocalImageNames(localized, fullPath);
      if (!localized && !fullPath) {
        localImages = cachedLocalImages[index];
      }
    }
    s.addAll(cachedLocalImages[index]);

    for (final DataArchive ext : extensions) {
      ext.getImageNamesRecursively(s, localized, fullPath);
    }
  }

  protected void getImageNamesRecursively(SortedSet<String> s) {
    getImageNamesRecursively(s, false, false);
  }

  protected SortedSet<String> getLocalImageNames() {
    return getAllLocalImageNames(false, true);
  }

  /* Localized directories always take the form images_XX with XX being a i18n code */
  protected void buildLocalizedDirectoryList(List<String> list) {
    List<String> files;
    try {
      files = archive.getFiles("");
    }
    catch (IOException e) {
      // FIXME: don't swallow this exception!
      e.printStackTrace();
      return;
    }

    final Set<String> dirs = new HashSet<>();
    final String base = imageDir.substring(0, imageDir.length() - 1) + "_";

    for (final String fname : files) {
      if (fname.startsWith(base)) {
        final int sep = fname.indexOf('/', base.length());
        if (sep != -1) {
          dirs.add(fname.substring(0, sep + 1));
        }
      }
    }

    list.addAll(dirs);
  }

  protected void getAllLocalImageNamesForDirectory(SortedSet<String> s, String directory, boolean fullPath) {
    // trim the trailing slash
    final int trimlen = directory.length();
    final String root = directory.substring(0, trimlen - 1);
    try {
      for (final String filename : archive.getFiles(root)) {
        final String trimmedFileName = filename.substring(trimlen);
        // Empty fn is the entry for the root directory; don't return that
        if (!trimmedFileName.isEmpty()) {
          final String fn = fullPath ? (root + '/' + trimmedFileName) : trimmedFileName;
          s.add(fn);
        }
      }
    }
    catch (IOException e) {
      // FIXME: don't swallow this exception!
      e.printStackTrace();
    }
  }

  protected SortedSet<String> getAllLocalImageNames(boolean localized, boolean fullPath) {
    final TreeSet<String> s = new TreeSet<>();

    if (archive != null) {
      final ArrayList<String> directories = new ArrayList<>();
      directories.add(imageDir);
      if (localized) {
        buildLocalizedDirectoryList(directories);
      }      
      for (final String directory : directories) {
        getAllLocalImageNamesForDirectory(s, directory, fullPath);
      }
    }
    return s;
  }


  /**
   * DataArchives can extend other archives. The extensions will be
   * searched for data if not found in the parent archive.
   *
   * @param ext the extension
   */
  public void addExtension(DataArchive ext) {
    extensions.add(ext);
  }

  /**
   * Return the writeable instance of DataArchive, either this or one
   * of its extensions. (At most one archive should be edited at a time.)
   *
   * @return writer
   */
  public ArchiveWriter getWriter() {
    if (this instanceof ArchiveWriter) return (ArchiveWriter) this;

    for (final DataArchive ext : extensions) {
      final ArchiveWriter writer = ext.getWriter();
      if (writer != null) return writer;
    }

    return null;
  }

/////////////////////////////////////////////////////////////////////
// Methods overridden from SecureClassLoader
/////////////////////////////////////////////////////////////////////

  @Override
  public synchronized Class<?> loadClass(String name, boolean resolve)
                                         throws ClassNotFoundException {
// FIXME: why is this method this way?
    Class<?> c;
    try {
//      c = findSystemClass(name);
      c = Class.forName(name);
    }
    catch (ClassNotFoundException e) {
      c = findLoadedClass(name);
    }

    if (c == null) {
      return findClass(name);
    }
    if (resolve) {
      resolveClass(c);
    }
    return c;
  }

  @Override
  protected PermissionCollection getPermissions(CodeSource codesource) {
    final PermissionCollection p = super.getPermissions(codesource);
    p.add(new AllPermission());
    return p;
  }

  private static final CodeSource cs =
    new CodeSource(null, (Certificate[]) null);

  @Override
  protected Class<?> findClass(String name) throws ClassNotFoundException {
    final String slashname = name.replace('.', '/');
    final byte[] data;

    try (InputStream stream = getInputStream(slashname + ".class")) { //NON-NLS
      data = stream.readAllBytes();
    }
    catch (IOException e) {
      throw new ClassNotFoundException("Unable to load class " + name, e);
    }

    final int minor = (data[4] << 8) | data[5];
    final int major = (data[6] << 8) | data[7];

    if (major > 55 || (major == 55 && minor != 0)) {
      ProblemDialog.showDisableable(
        JOptionPane.WARNING_MESSAGE,
        null,
        null,
        cs,
        Resources.getString("Dialogs.incompatible.title"),
        Resources.getString("Dialogs.incompatible.heading"),
        Resources.getString("Dialogs.incompatible.message", name) + "\n\n"
          + Resources.getString("Dialogs.check_for_updated_module")
      );
    }

    return defineClass(name, data, 0, data.length, cs);
  }

  private void resetLocalImages() {
    for (int i = 0; i < cachedLocalImages.length; ++i) {
      cachedLocalImages[i] = null;
    }
    localImages = null;
  }
}
