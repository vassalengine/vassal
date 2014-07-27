/*
 * $Id$
 *
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

////////////////////////////////////////////////////////
// These imports are used in deprecated methods only. //
////////////////////////////////////////////////////////
import java.applet.AudioClip;
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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.SecureClassLoader;
import java.security.cert.Certificate;
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

import sun.applet.AppletAudioClip;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.image.svg.SVGImageUtils;
import VASSAL.tools.image.svg.SVGRenderer;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.RotateScaleOp;
import VASSAL.tools.imageop.ScaleOp;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.ZipArchive;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends SecureClassLoader implements Closeable {

  protected FileArchive archive;

  protected List<DataArchive> extensions = new ArrayList<DataArchive>();

// FIXME: these should go into a cache, like images have
  private final Map<String,AudioClip> soundCache =
    new HashMap<String,AudioClip>();

  protected SortedSet<String> localImages = null;

  public static final String IMAGE_DIR = "images/";
  protected String imageDir = IMAGE_DIR;

  public static final String SOUND_DIR = "sounds/";
  protected String soundDir = SOUND_DIR;

  public static final String ICON_DIR = "icons/";

  protected DataArchive() {
    super(DataArchive.class.getClassLoader());
  }

  public DataArchive(String zipName, String imageDir) throws IOException {
    this();
    archive = new ZipArchive(zipName);
    this.imageDir = imageDir;
  }

  public DataArchive(String zipName) throws IOException {
    this(zipName, IMAGE_DIR);
  }

  public String getName() {
    return archive == null ? "data archive" : archive.getName();
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
      if (name.toLowerCase().endsWith(".mp3")) {
        clip = new Mp3AudioClip(path);
      }
      else {
        InputStream stream = null;
        try {
          stream = getInputStream(path);
          clip = new AppletAudioClip(IOUtils.toByteArray(stream));
          soundCache.put(path,clip);
        }
        finally {
          IOUtils.closeQuietly(stream);
        }
      }
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
  @Deprecated
  public InputStream getImageInputStream(String fileName)
                                    throws IOException, FileNotFoundException {
// FIXME: We should give notice that we're going to stop searching for
// GIFs by appending ".gif" to them. In general, a way of marking obsolete
// features would be good---something which pops up a dialog alerting the
// user when a module calls a deprecated method, maybe.

    if (fileName.startsWith("/")) {
      final InputStream in = getClass().getResourceAsStream(fileName);
      if (in != null) return in;
      throw new FileNotFoundException("Resource not found: " + fileName);
    }

    try {
      return getInputStream(imageDir + fileName);
    }
    catch (FileNotFoundException e) {
    }

////////////////////
// FIXME: Appending .gif should be considered deprecated behavior.
//
    try {
      return getInputStream(imageDir + fileName + ".gif");
    }
    catch (FileNotFoundException e) {
    }

    final InputStream in =
      getClass().getResourceAsStream("/" + imageDir + fileName + ".gif");
    if (in != null) return in;
//
///////////////////

    throw new FileNotFoundException(
      "\'" + imageDir + fileName + "\' not found in " + getName());
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
      throw new FileNotFoundException("Resource not found: " + fileName);
    }

    // Look in this archive and its extensions
    InputStream in = getInputStreamImpl(fileName);
    if (in != null) {
      return in;
    }

    //
    // Ridiculous crap we have to check for backwards compatibility
    //

    // Maybe it's a resource missing its initial slash. Aauugh!
    in = getClass().getResourceAsStream("/" + fileName);
    if (in != null) {
      return in;
    }

    // Maybe it's an extensionless GIF? Aauugh!
    in = getInputStreamImpl(fileName + ".gif");
    if (in != null) {
      return in;
    }

    // Maybe it's an extensionless GIF resource. Aauugh!
    in = getClass().getResourceAsStream("/" + fileName + ".gif");
    if (in != null) {
      return in;
    }

    //
    // End of ridiculous crap
    //

    throw new FileNotFoundException(
      "\'" + fileName + "\' not found in " + getName()
    );
  }

  private InputStream getInputStreamImpl(String fileName)
                                    throws IOException, FileNotFoundException {
    // requested file is in this archive
    if (archive != null && archive.contains(fileName)) {
      return archive.getInputStream(fileName);
    }

    // we don't have it, try our extensions
    for (DataArchive ext : extensions) {
      try {
        return ext.getInputStream(fileName);
      }
      catch (FileNotFoundException e) {
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

    for (DataArchive ext : extensions) {
      try {
        return ext.getURL(fileName);
      }
      catch (FileNotFoundException e) {
        // not found in this extension, try the next
      }
    }

    throw new FileNotFoundException(
      "\'" + fileName + "\' not found in " + getName());
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
  @Deprecated
  public URL getImageURL(String fileName) throws IOException,
                                                 FileNotFoundException {
    return getURL(fileName);
  }

  public boolean contains(String fileName) throws IOException {
    if (archive == null) return false;
    return archive.contains(fileName);
  }

  public void close() throws IOException {
    if (archive != null) {
      archive.revert(); // ensure that we don't modify the archive
      archive.close();
    }
  }

  public String[] getImageNames() {
    final SortedSet<String> s = getImageNameSet();
    return s.toArray(new String[s.size()]);
  }

  public SortedSet<String> getImageNameSet() {
    final TreeSet<String> s = new TreeSet<String>();
    getImageNamesRecursively(s);
    return s;
  }

  protected void getImageNamesRecursively(SortedSet<String> s) {
    if (localImages == null) localImages = getLocalImageNames();
    s.addAll(localImages);

    for (DataArchive ext : extensions) {
      ext.getImageNamesRecursively(s);
    }
  }

  protected SortedSet<String> getLocalImageNames() {
    final TreeSet<String> s = new TreeSet<String>();

    if (archive != null) {
      try {
//        for (String filename : archive.getFiles(imageDir)) {
        for (String filename : archive.getFiles("images")) {
          s.add(filename.substring(imageDir.length()));
        }
      }
      catch (IOException e) {
// FIXME: don't swallow this exception!
        e.printStackTrace();
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
   * @return
   */
  public ArchiveWriter getWriter() {
    if (this instanceof ArchiveWriter) return (ArchiveWriter) this;

    for (DataArchive ext : extensions) {
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
    new CodeSource((URL) null, (Certificate[]) null);

  @Override
  protected Class<?> findClass(String name) throws ClassNotFoundException {
    final String slashname = name.replace('.', '/');
    byte[] data = null;

    InputStream stream = null;
    try {
      stream = getInputStream(slashname + ".class");
      data = IOUtils.toByteArray(stream);
    }
    catch (IOException e) {
      throw new ClassNotFoundException("Unable to load class " + name, e);
    }
    finally {
      IOUtils.closeQuietly(stream);
    }

    final int minor = (data[4] << 8) | data[5];
    final int major = (data[6] << 8) | data[7];

    if (major > 49 || (major == 49 && minor != 0)) {
      ProblemDialog.showDisableable(
        JOptionPane.WARNING_MESSAGE,
        null,
        null,
        cs,
        "Incompatible Custom Code",
        "The Custom Code In This Module Should Be Recompiled",
        "This module contains custom Java code (" + name + ") which was not compiled to be Java 5 compatible. As a result, this module will not run on all versions of Java which VASSAL itself supports.\n\nPlease check whether there is an updated version of this module. If not, please contact the maintainer of this module and request that it be fixed."
      );
    }

    return defineClass(name, data, 0, data.length, cs);
  }

/////////////////////////////////////////////////////////////////////
// All methods deprecated below this point.
/////////////////////////////////////////////////////////////////////

  @Deprecated public static final String SOUNDS_DIR = SOUND_DIR;
  @Deprecated protected String soundsDir = SOUND_DIR;

  @Deprecated
  private final Map<String,ImageSource> imageSources =
    new HashMap<String,ImageSource>();

  /**
   * Add an ImageSource under the given name, but only if no source is
   * yet registered under this name.
   *
   * @param name
   * @param src
   * @return true if the ImageSource was added, false if it existed already
   * @deprecated
   */
  @Deprecated
  public boolean addImageSource(String name, ImageSource src) {
    if (!imageSources.containsKey(name)) {
      imageSources.put(name,src);
      localImages = null;
      return true;
    }
    return false;
  }

  @Deprecated
  public void removeImageSource(String name) {
    imageSources.remove(name);
    localImages = null;
  }

  /**
   * Get the size of an image without loading and decoding it.
   *
   * @param name filename of the image
   * @return the size of the image
   * @deprecated Use {@link ImageUtils.getImageSize} or
   *    {@link SVGImageUtils.getImageSize} instead.
   */
  @Deprecated
  public Dimension getImageSize(String name) throws IOException {
    final ImageSource src;

    if (name.startsWith("/")) {
      if (name.toLowerCase().endsWith(".svg"))
        return SVGImageUtils.getImageSize(name, getImageInputStream(name));
      else
        return ImageUtils.getImageSize(name, getImageInputStream(name));
    }
    else if ((src = imageSources.get(name)) != null) {
      final Image image = src.getImage();
      return image != null ?
        new Dimension(image.getWidth(null), image.getHeight(null)) :
        new Dimension();
    }
    else if (name.toLowerCase().endsWith(".svg")) {
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
   * @deprecated Use {@link ImageUtils.getImage} or
   *    {@link SVGImageUtils.getImage} instead.
   */
  @Deprecated
  public BufferedImage getImage(String name) throws IOException {
    final ImageSource src;

    if (name.startsWith("/")) {
      if (name.toLowerCase().endsWith(".svg")) {
        return new SVGRenderer(getURL(name),
                               getImageInputStream(name)).render();
      }
      else {
        return ImageUtils.getImage(name, getImageInputStream(name));
      }
    }
    else if ((src = imageSources.get(name)) != null) {
      return ImageUtils.toBufferedImage(src.getImage());
    }
    else if (name.toLowerCase().endsWith(".svg")) {
      return new SVGRenderer(getURL(name),
                             getImageInputStream(name)).render();
    }
    else {
      return ImageUtils.getImage(name, getImageInputStream(name));
    }
  }

  /**
   * Does the actual work of transforming an image.
   */
/*
  @Deprecated
  protected Image createTransformedInstance(Image im, double zoom,
    double theta) {
    // get smoothing preferences
    if (smoothPrefs == null) {
      smoothPrefs = (BooleanConfigurer) GameModule.getGameModule()
        .getPrefs().getOption(GlobalOptions.SCALER_ALGORITHM);
      if (smoothPrefs == null) {
        smoothPrefs = new BooleanConfigurer(null, null, Boolean.FALSE);
      }
      smoothPrefs.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          clearTransformedImageCache();
        }
      });
    }

    final boolean smooth = Boolean.TRUE.equals(smoothPrefs.getValue());
    return new RotateScaleOp(new ImageSourceOp(im), theta, zoom).getImage(null);
  }
*/

  @Deprecated protected String[] imageNames;

  @Deprecated
  protected boolean isNameCacheStale() {
    return true;
  }

  /**
   * @return the names of the image files stored in this DataArchive
   * and its extensions
   * @deprecated Use {@link #getImageNameSet()} instead.
   */
  @Deprecated
  protected SortedSet<String> setOfImageNames() {
    return getImageNameSet();
  }

// FIXME: hook these up to ImageOp methods
  @Deprecated
  public void unCacheImage(String file) { }

  @Deprecated
  public void unCacheImage(Image im) { }

  @Deprecated
  public void clearTransformedImageCache() { }

  @Deprecated
  public void clearScaledImageCache() { }

  /**
   * Find an image from the archive
   * Once an image is found, cache it in our HashMap.
   * @deprecated Use {@link ImageOp}s instead.
   */
  @Deprecated
  public Image getCachedImage(String name) throws IOException {
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
  @Deprecated
  public Image getTransformedImage(Image base, double scale, double theta) {
    // An ugly hack, but nothing should be using this method anyway.
    return Op.rotateScale(Op.load(
      ImageUtils.toBufferedImage(base)), theta, scale).getImage();
  }

  /**
   * @deprecated Use {@link RotateScaleOp} instead.
   * @param base
   * @param scale
   * @param theta
   * @param forceSmoothing
   * @return
   */
  @Deprecated
  public Image getTransformedImage(Image base, double scale, double theta,
                                   boolean forceSmoothing) {
    return getTransformedImage(base, scale, theta);
  }

  /**
   * @deprecated Use {@link ScaleOp} instead.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base
   * @param scale
   * @param reversed
   * @param forceSmoothing If true, force smoothing.
   *  This usually yields better results, but can be slow for large images
   * @return
   */
  @Deprecated
  public Image getScaledImage(Image base, double scale, boolean reversed,
                              boolean forceSmoothing) {
    return getTransformedImage(base, scale,
                               reversed ? 180.0 : 0.0);
  }

  /**
   * Return a scaled instance of the image.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base
   * @param scale
   * @return
   * @deprecated Use {@link ScaleOp} instead.
   */
  @Deprecated
  public Image getScaledImage(Image base, double scale) {
    return getTransformedImage(base, scale, 0.0, true);
  }

  /**
   * @deprecated Use {@link #getImage} instead.
   */
  @Deprecated
  public static Image findImage(File zip, String file) throws IOException {
    return getImage(getFileStream(zip, file));
  }

  /**
   * @deprecated Use {@link #getImage} instead.
   */
  @Deprecated
  public static Image findImage(File dir, String zip, String file)
      throws IOException {
    /*
     ** Looks for entry "file" in ZipFile "zip" in directory "dir"
     ** If no such zipfile, look for "file" in "dir"
     */
    if ((new File(dir, zip)).exists()) {
      return getImage(getFileStream(dir, zip, file));
    }
    else if ((new File(dir, file)).exists()) {
      return Toolkit.getDefaultToolkit().getImage
          (dir.getPath() + File.separatorChar + file);
    }
    else {
      throw new IOException("Image " + file + " not found in " + dir
                            + File.separator + zip);
    }
  }

  /**
   * @deprecated Use {@link #getFileStream(String)} instead.
   */
  @Deprecated
  public static InputStream getFileStream(File zip, String file)
      throws IOException {
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
  @Deprecated
  public static InputStream getFileStream(File dir, String zipName, String file) {
    try {
      if ((new File(dir, zipName)).exists()) {
        final ZipFile zip = new ZipFile(new File(dir, zipName));
        return zip.getInputStream(zip.getEntry(file));
      }
      else {
        return new FileInputStream(new File(dir, file));
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
  @Deprecated
  public InputStream getFileStream(String fileName) throws IOException {
    return getInputStream(fileName);
  }

  /** Use {@link ImageUtils.getImage(InputStream)} instead. */
  @Deprecated
  public static Image getImage(InputStream in) throws IOException {
    return ImageUtils.getImage(in);
  }

  /** @deprecated Use {@link getURL()} instead. */
  @Deprecated
  public String getArchiveURL() {
    return archive != null ? "jar:file://" + archive.getName() + "!/" : "";
  }

  /**
   * Read all available bytes from the given InputStream.
   * @deprecated Use {@link IOUtils.toBytesArray(InputStream)} instead.
   */
  @Deprecated
  public static byte[] getBytes(InputStream in) throws IOException {
    return IOUtils.toByteArray(in);
  }

  /**
   * Place the names of the image files stored in this DataArchive into
   * the argument Collection
   * @param l
   * @deprecated Use {@link #listImageNames()} instead.
   */
  @Deprecated
  @SuppressWarnings("unchecked")
  protected void listImageNames(Collection l) {
    l.addAll(setOfImageNames());
  }

  /**
   *
   * @param im
   * @return the boundaries of this image,
   * where (0,0) is the center of the image
   * @deprecated Use {@link ImageUtils.getBounds(BufferedImage)} instead.
   */
  @Deprecated
  public static Rectangle getImageBounds(Image im) {
    ImageIcon icon = new ImageIcon(im);
    return new Rectangle(-icon.getIconWidth() / 2, -icon.getIconHeight() / 2,
                          icon.getIconWidth(), icon.getIconHeight());
  }

  /**
   * @deprecated Don't use this. We've switched to Lanczos scaling.
   */
  @Deprecated
  public Image improvedScaling(Image img, int width, int height) {
    ImageFilter filter;

    filter = new ImprovedAveragingScaleFilter(img.getWidth(null),
                                              img.getHeight(null),
                                              width, height);

    ImageProducer prod;
    prod = new FilteredImageSource(img.getSource(), filter);
    return Toolkit.getDefaultToolkit().createImage(prod);
  }
}
