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

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.SecureClassLoader;
import java.security.cert.Certificate;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.zip.ZipFile;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import org.apache.xmlgraphics.image.loader.ImageSource;

import VASSAL.i18n.Resources;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.image.svg.SVGImageUtils;
import VASSAL.tools.image.svg.SVGRenderer;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.RotateScaleOp;
import VASSAL.tools.imageop.ScaleOp;
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
   * Get an {@link InputStream} for the given image file in the archive.
   *
   * @param fileName the name of the image file
   * @return an <code>InputStream</code> which contains the image file
   * @throws IOException if there is a problem reading the image file
   * @throws FileNotFoundException if the image file doesn't exist
   * @deprecated Use {@link #getInputStream(String)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public InputStream getImageInputStream(String fileName)
                                    throws IOException, FileNotFoundException {
    ProblemDialog.showDeprecated("2020-08-06");
    if (fileName.startsWith("/")) {
      final InputStream in = getClass().getResourceAsStream(fileName);
      if (in != null) return in;
      throw new FileNotFoundException("Resource not found: " + fileName); //NON-NLS
    }

    try {
      return getInputStream(imageDir + fileName);
    }
    catch (FileNotFoundException | NoSuchFileException ignored) {
    }
    try {
      return getInputStream(imageDir + fileName + ".gif"); //NON-NLS
    }
    catch (FileNotFoundException | NoSuchFileException ignored) {
    }

    final InputStream in =
      getClass().getResourceAsStream("/" + imageDir + fileName + ".gif"); //NON-NLS
    if (in != null) return in;

    throw new FileNotFoundException(
      "'" + imageDir + fileName + "' not found in " + getName());
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

  /**
   * Returns a URL pointing to the named image file.
   *
   * @param fileName the name of the image file
   * @return a URL corresponding to the image file
   * @throws FileNotFoundException if the file doesn't exist
   * @throws IOException if some other problem occurs
   * @deprecated Use {@link #getURL(String)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public URL getImageURL(String fileName) throws IOException,
                                                 FileNotFoundException {
    ProblemDialog.showDeprecated("2020-08-06");
    return getURL(fileName);
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
    final int rootlen = imageDir.length();
    final String root = imageDir.substring(0, rootlen - 1);
    try {
      for (final String fname : archive.getFiles("")) {
        final int fnamelen = fname.length();
        if (fname.charAt(fnamelen - 1) == '/') {
          final String fnamedir = fname.substring(0, fnamelen - 1);
          if (fnamedir.startsWith(root) && !fnamedir.equals(root) && fnamedir.charAt(rootlen - 1) == '_') {
            list.add(fname);
          }
        }
      }
    }
    catch (IOException e) {
      // FIXME: don't swallow this exception!
      e.printStackTrace();
    }
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

/////////////////////////////////////////////////////////////////////
// All methods deprecated below this point.
/////////////////////////////////////////////////////////////////////

  @Deprecated(since = "2020-08-06", forRemoval = true) public static final String SOUNDS_DIR = SOUND_DIR;
  @Deprecated(since = "2020-08-06", forRemoval = true) protected String soundsDir = SOUND_DIR;

  @Deprecated(since = "2020-08-06", forRemoval = true)
  private final Map<String, ImageSource> imageSources =
    new HashMap<>();

  private void resetLocalImages() {
    for (int i = 0; i < cachedLocalImages.length; ++i) {
      cachedLocalImages[i] = null;
    }
    localImages = null;
  }
  
  /**
   * Add an ImageSource under the given name, but only if no source is
   * yet registered under this name.
   *
   * @param name name
   * @param src source
   * @return true if the ImageSource was added, false if it existed already
   * @deprecated
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public boolean addImageSource(String name, ImageSource src) {
    ProblemDialog.showDeprecated("2020-08-06");
    if (!imageSources.containsKey(name)) {
      imageSources.put(name, src);
      resetLocalImages();
      return true;
    }
    return false;
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void removeImageSource(String name) {
    ProblemDialog.showDeprecated("2020-08-06");
    imageSources.remove(name);
    resetLocalImages();
  }

  /**
   * Get the size of an image without loading and decoding it.
   *
   * @param name filename of the image
   * @return the size of the image
   * @deprecated Use {@link ImageUtils#getImageSize(String, InputStream)} or
   *    {@link SVGImageUtils#getImageSize(String, InputStream)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Dimension getImageSize(String name) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    if (name.toLowerCase().endsWith(".svg")) { //NON-NLS
      return SVGImageUtils.getImageSize(name, getImageInputStream(name));
    }
    else {
      return ImageUtils.getImageSize(name, getImageInputStream(name));
    }
  }

  /**
   * Returns an {@link Image} from the archive.
   *
   * @param name the name of the image file
   * @return the <code>Image</code> contained in the image file
   * @throws IOException if there is a problem reading the image file
   * @deprecated Use {@link ImageUtils#getImage(String, InputStream)}  or
   *    {@link SVGImageUtils#getImageSize(String, InputStream)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public BufferedImage getImage(String name) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    if (name.toLowerCase().endsWith(".svg")) { //NON-NLS
      return new SVGRenderer(getURL(name),
                             getImageInputStream(name)).render();
    }
    else {
      return ImageUtils.getImage(name, getImageInputStream(name));
    }
  }

  @Deprecated(since = "2020-08-06", forRemoval = true) protected String[] imageNames;

  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected boolean isNameCacheStale() {
    ProblemDialog.showDeprecated("2020-08-06");
    return true;
  }

  /**
   * @return the names of the image files stored in this DataArchive
   * and its extensions
   * @deprecated Use {@link #getImageNameSet()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected SortedSet<String> setOfImageNames() {
    ProblemDialog.showDeprecated("2020-08-06");
    return getImageNameSet();
  }

// FIXME: hook these up to ImageOp methods
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void unCacheImage(@SuppressWarnings("unused") String file) {
    ProblemDialog.showDeprecated("2020-08-06");
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void unCacheImage(@SuppressWarnings("unused") Image im) {
    ProblemDialog.showDeprecated("2020-08-06");
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void clearTransformedImageCache() {
    ProblemDialog.showDeprecated("2020-08-06");
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void clearScaledImageCache() {
    ProblemDialog.showDeprecated("2020-08-06");
  }

  /**
   * Find an image from the archive
   * Once an image is found, cache it in our HashMap.
   * @deprecated Use {@link ImageOp}s instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image getCachedImage(String name) {
    ProblemDialog.showDeprecated("2020-08-06");
    // An ugly hack, but nothing should be using this method anyway.
    return Op.load(name).getImage();
  }

  /**
   * Return a transformed instance of the image.
   * The image will be retrieved from the cache if available, and cached
   * after retrieval if not.
   * @param base the untransformed Image
   * @param scale the scaling factor
   * @param theta the angle of rotation (in degrees) about the Image center
   * @deprecated Use {@link RotateScaleOp} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image getTransformedImage(Image base, double scale, double theta) {
    ProblemDialog.showDeprecated("2020-08-06");
    // An ugly hack, but nothing should be using this method anyway.
    return Op.rotateScale(Op.load(
      ImageUtils.toBufferedImage(base)), theta, scale).getImage();
  }

  /**
   * @deprecated Use {@link RotateScaleOp} instead.
   * @param base base
   * @param scale scale
   * @param theta theta
   * @param forceSmoothing forceSmoothing
   * @return transformed image
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image getTransformedImage(Image base, double scale, double theta, @SuppressWarnings("unused") boolean forceSmoothing) {
    ProblemDialog.showDeprecated("2020-08-06");
    return getTransformedImage(base, scale, theta);
  }

  /**
   * @deprecated Use {@link ScaleOp} instead.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base base
   * @param scale scale
   * @param reversed reversed
   * @param forceSmoothing If true, force smoothing.
   *  This usually yields better results, but can be slow for large images
   * @return Scaled image
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image getScaledImage(Image base, double scale, boolean reversed, @SuppressWarnings("unused") boolean forceSmoothing) {
    ProblemDialog.showDeprecated("2020-08-06");
    return getTransformedImage(base, scale, reversed ? 180.0 : 0.0);
  }

  /**
   * Return a scaled instance of the image.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base base
   * @param scale scale
   * @return Scaled image
   * @deprecated Use {@link ScaleOp} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image getScaledImage(Image base, double scale) {
    ProblemDialog.showDeprecated("2020-08-06");
    return getTransformedImage(base, scale, 0.0, true);
  }

  /**
   * @deprecated Use {@link #getImage} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static Image findImage(File zip, String file) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    return getImage(getFileStream(zip, file));
  }

  /**
   * @deprecated Use {@link #getImage} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static Image findImage(File dir, String zip, String file)
      throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    /*
     ** Looks for entry "file" in ZipFile "zip" in directory "dir"
     ** If no such zipfile, look for "file" in "dir"
     */
    if ((new File(dir, zip)).exists()) {
      return getImage(getFileStream(dir, zip, file));
    }
    else if ((new File(dir, file)).exists()) {
      return Toolkit.getDefaultToolkit().getImage(
        dir.getPath() + File.separatorChar + file
      );
    }
    else {
      throw new IOException("Image " + file + " not found in " + dir
                            + File.separator + zip);
    }
  }

  /**
   * @deprecated Use {@link #getFileStream(String)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static InputStream getFileStream(File zip, String file)
      throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    try {
      final ZipFile z = new ZipFile(zip);
      return z.getInputStream(z.getEntry(file));
    }
    catch (Exception e) {
      throw new IOException("Couldn't locate " + file + " in " + zip.getName()
                            + ": " + e.getMessage());
    }
  }

  /**
   * @deprecated Use {@link #getFileStream(String)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static InputStream getFileStream(File dir, String zipName, String file) {
    ProblemDialog.showDeprecated("2020-08-06");
    try {
      if ((new File(dir, zipName)).exists()) {
        final ZipFile zip = new ZipFile(new File(dir, zipName));
        return zip.getInputStream(zip.getEntry(file));
      }
      else {
        return Files.newInputStream(dir.toPath().resolve(file));
      }
    }
    catch (IOException e) {
      return null;
    }
  }

  /**
   * Get an {@link InputStream} for the given filename in the archive.
   *
   * @deprecated Use {@link #getInputStream(String)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public InputStream getFileStream(String fileName) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    return getInputStream(fileName);
  }

  /** Use {@link ImageUtils#getImage(String, InputStream)} instead. */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static Image getImage(InputStream in) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    return ImageUtils.getImage("", in);
  }

  /** @deprecated Use {@link #getURL()} instead. */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public String getArchiveURL() {
    ProblemDialog.showDeprecated("2020-08-06");
    return archive != null ? "jar:file://" + archive.getName() + "!/" : ""; //NON-NLS
  }

  /**
   * Read all available bytes from the given InputStream.
   * @deprecated Use {@link InputStream#readAllBytes()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static byte[] getBytes(InputStream in) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    return in.readAllBytes();
  }

  /**
   * Place the names of the image files stored in this DataArchive into
   * the argument Collection
   * @param l l
   * @deprecated Use {@link #getImageNameSet()} )} instead.
   */
  @SuppressWarnings("unchecked")
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected void listImageNames(@SuppressWarnings("rawtypes") Collection l) {
    ProblemDialog.showDeprecated("2020-08-06");
    l.addAll(setOfImageNames());
  }

  /**
   *
   * @param im image
   * @return the boundaries of this image,
   * where (0,0) is the center of the image
   * @deprecated Use {@link ImageUtils#getBounds(BufferedImage)}  instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static Rectangle getImageBounds(Image im) {
    ProblemDialog.showDeprecated("2020-08-06");
    final ImageIcon icon = new ImageIcon(im);
    return new Rectangle(-icon.getIconWidth() / 2, -icon.getIconHeight() / 2,
                          icon.getIconWidth(), icon.getIconHeight());
  }

  /**
   * @deprecated Don't use this. We've switched to Lanczos scaling.
   */
  @SuppressWarnings("removal")
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Image improvedScaling(Image img, int width, int height) {
    ProblemDialog.showDeprecated("2020-08-06");
    final ImageFilter filter;

    filter = new ImprovedAveragingScaleFilter(img.getWidth(null),
                                              img.getHeight(null),
                                              width, height);

    final ImageProducer prod;
    prod = new FilteredImageSource(img.getSource(), filter);
    return Toolkit.getDefaultToolkit().createImage(prod);
  }
}
