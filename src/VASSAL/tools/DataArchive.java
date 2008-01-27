/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Joel Uckelman
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

import java.applet.AudioClip;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.AllPermission;
import java.security.CodeSource;
import java.security.PermissionCollection;
import java.security.SecureClassLoader;
import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.event.IIOReadProgressListener;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;
import javax.swing.ImageIcon;

import sun.applet.AppletAudioClip;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.tools.imageop.ImageSourceOp;
import VASSAL.tools.imageop.RotateScaleOp;
import VASSAL.tools.imageop.SourceOp;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends SecureClassLoader {
  protected ZipFile archive = null;
  protected List<DataArchive> extensions = new ArrayList<DataArchive>();

  private final Map<String,AudioClip> soundCache =
    new HashMap<String,AudioClip>();

// FIXME: this could be done with ImageSourceOp instead?
  private final Map<String,ImageSource> imageSources =
    new HashMap<String,ImageSource>();

  protected SortedSet<String> localImages = new TreeSet<String>();

  public static final String IMAGE_DIR = "images/";
  public static final String SOUNDS_DIR = "sounds/";
  private CodeSource cs;
  protected SVGManager svgManager;

  protected DataArchive() {
    super(DataArchive.class.getClassLoader());
  }

  public DataArchive(String zipName) throws IOException {
    this();
    archive = new ZipFile(zipName);
  }

  public String getName() {
    return archive == null ? "data archive" : archive.getName();
  }

  public ZipFile getArchive() {
    return archive;
  }

  public AudioClip getCachedAudioClip(String name) throws IOException {
    String path = SOUNDS_DIR + name;
    AudioClip clip = soundCache.get(path);
    if (clip == null) {
      clip = new AppletAudioClip(getBytes(getFileStream(path)));
      soundCache.put(path,clip);
    }
    return clip;
  }

  /**
   * Get the size of an image without loading and decoding it.
   *
   * @param name filename of the image
   * @return the size of the image
   */
  public Dimension getImageSize(String name) throws IOException {
    final String path = IMAGE_DIR + name;
    final ImageSource src;

    if ((src = imageSources.get(name)) != null) {
// FIXME: is this case necessary? Does this even work?
      Image image = src.getImage();
      if (image != null) {
        final int width = image.getWidth(null);
        final int height = image.getHeight(null);
        return new Dimension(width, height);
      }
      else return new Dimension();
    }
    else if (name.toLowerCase().endsWith(".svg")) {
      if (svgManager == null) svgManager = new SVGManager(this);

      return svgManager.getImageSize("jar:file://" +
         (archive != null ? archive.getName() : "null") + "!/" + path,
        getFileStream(path));
    }
    else {
      InputStream is = null;
      try {

// FIXME: This is a big, ugly mess. We should have a clearly documented
// standard for image locations.
//
// FIXME: We should give notice that we're going to stop searching for
// GIFs by appending ".gif" to them. In general, a way of marking obsolete
// features would be good---something which pops up a dialog alerting the
// user when a module calls a deprecated method, maybe.
 
        if (name.charAt(0) == '/') {
          is = getClass().getResourceAsStream(name); 
        }
        else {
          try {
            is = getFileStream(path);
          }
          catch (IOException e) {
            is = getFileStream(path + ".gif");
          }
        }

        final ImageInputStream in = new MemoryCacheImageInputStream(is);
        final ImageReader reader = ImageIO.getImageReaders(in).next();
        reader.setInput(in);
        return new Dimension(reader.getWidth(0), reader.getHeight(0));
      }
      finally {
        if (is != null) is.close();
      }
    }
  }

/*
  private Shape getImageShape(String imageName) {
    Shape s = (Shape) imageShapes.get(imageName);
    if (s == null) {
      Area a = new Area();
      try {
        Image im = getCachedImage(imageName);
        ImageIcon icon = new ImageIcon(im);
        int width = icon.getIconWidth();
        int height = icon.getIconHeight();
        int[] pixels = new int[width * height];
        PixelGrabber pg = new PixelGrabber(im, 0, 0, width, height, pixels, 0, width);
        long time = System.currentTimeMillis();
        pg.grabPixels();
        System.err.println("Grab "+imageName+" took "+(System.currentTimeMillis()-time));
        time = System.currentTimeMillis();
        for (int j = 0; j < height; ++j) {
          for (int i = 0; i < width; ++i) {
            if (((pixels[i + j * width] >> 24) & 0xff) > 0) {
              a.add(new Area(new Rectangle(i, j, 1, 1)));
            }
          }
        }
        System.err.println("Build shape "+imageName+" took "+(System.currentTimeMillis()-time));
      }
      catch (IOException e) {
      }
      catch (InterruptedException e) {

      }
      s = a;
      imageShapes.put(imageName,s);
    }
    return s;
  }
*/

  public Image getImage(String name) throws IOException {
    final String path = IMAGE_DIR + name;
    final String gifPath = path + ".gif";
    final ImageSource src;
    Image image = null;

    if (name.charAt(0) == '/') {
      image = getImage(getClass().getResourceAsStream(name)); 
    }
    else if ((src = imageSources.get(name)) != null) {
      image = src.getImage();
    }
    else if (name.toLowerCase().endsWith(".svg")) {
      if (svgManager == null) svgManager = new SVGManager(this);

      image = svgManager.loadSVGImage("jar:file://" +
         (archive != null ? archive.getName() : "null") + "!/" + path,
        getFileStream(path));
    }
    else {
      try {
        image = getImage(getFileStream(path));
      }
      catch (IOException e) {
        image = getImage(getFileStream(gifPath));
      }
    }
    return image;
  }
  
  public static Image getImage(InputStream in) throws IOException {
    ImageInputStream stream = null;
    try {
      stream = new MemoryCacheImageInputStream(in);
      final ImageReader reader = ImageIO.getImageReaders(stream).next();
      reader.setInput(stream);
      reader.addIIOReadProgressListener(new IIOReadProgressListener() {
        public void imageComplete(ImageReader source) {
          System.out.print("\n");
        }

        public void imageProgress(ImageReader source, float percentageDone) {
          System.out.print(".");
        }

        public void imageStarted(ImageReader soruce, int imageIndex) {
          System.out.println("");
        }
 
        public void readAborted(ImageReader source) { }

        public void sequenceComplete(ImageReader source) { }

        public void sequenceStarted(ImageReader source, int minIndex) { }

        public void thumbnailComplete(ImageReader source) { }

        public void thumbnailProgress(ImageReader source,
                                      float percentageDone) { }

        public void thumbnailStarted(ImageReader source, int imageIndex,
                                     int thumbnailIndex) { }
      });

      return reader.read(0);
    }
    finally {
      if (stream != null) stream.close();
    }
//    return ImageIO.read(in);

// FIXME: maybe convert all images to TYPE_INT_ARGB here?
//    return Toolkit.getDefaultToolkit().createImage(getBytes(in));
  }

  /**
   * Add an ImageSource under the given name, but only if no source is
   * yet registered under this name.
   * @param name
   * @param src
   * @return true if the ImageSource was added, false if it existed already
   */
  public boolean addImageSource(String name, ImageSource src) {
    if (!imageSources.containsKey(name)) {
      imageSources.put(name,src);
      localImages = null;
      return true;
    }
    return false;
  }

  public void removeImageSource(String name) {
    imageSources.remove(name);
    localImages = null;
  }

  public String[] getImageNames() {
    final SortedSet<String> s = getImageNameSet();
    return s.toArray(new String[s.size()]);
  }

  public SortedSet<String> getImageNameSet() {
    final TreeSet<String> s = 
      new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
    getImageNamesRecursively(s);
    return s;    
  }

  protected void getImageNamesRecursively(SortedSet<String> s) {
    if (localImages == null) {
      localImages = getLocalImageNames();
    }
    s.addAll(localImages);
    for (DataArchive ext : extensions) {
      ext.getImageNamesRecursively(s);
    }
  }

  protected SortedSet<String> getLocalImageNames() {
    final TreeSet<String> s =
      new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);

    s.addAll(imageSources.keySet());

    if (archive != null) {
      ZipInputStream zis = null;
      try {
        zis = new ZipInputStream(new FileInputStream(archive.getName()));

        ZipEntry entry = null;
        while ((entry = zis.getNextEntry()) != null) {
          if (entry.getName().startsWith(IMAGE_DIR)) {
            s.add(entry.getName().substring(IMAGE_DIR.length()));
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      finally {
        if (zis != null) {
          try {
            zis.close();
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      }
    }
    return s;
  }

  /**
   * Read all available bytes from the given InputStream
   */
  public static byte[] getBytes(InputStream in) throws IOException {
    final BufferedInputStream bufIn = new BufferedInputStream(in);
    final int nLen = bufIn.available();
    int nCurBytes = 0;
    byte buffer[] = null;
    final byte abyte0[] = new byte[nLen];

    while ((nCurBytes = bufIn.read(abyte0, 0, abyte0.length)) > 0) {
      if (buffer == null) {
        buffer = new byte[nCurBytes];
        System.arraycopy(abyte0, 0, buffer, 0, nCurBytes);
      }
      else {
        byte oldbuf[] = buffer;

        buffer = new byte[oldbuf.length + nCurBytes];

        System.arraycopy(oldbuf, 0, buffer, 0, oldbuf.length);
        System.arraycopy(abyte0, 0, buffer, oldbuf.length, nCurBytes);
      }
    }
    return buffer != null ? buffer : new byte[0];
  }

  public URL getURL(String fileName) throws IOException {
    if (archive == null) {
      throw new IOException("Must save before accessing contents");
    }
    URL url = null;
    final ZipEntry entry = archive.getEntry(fileName);
    if (entry != null) {
      final String archiveURL =
        HelpFile.toURL(new File(archive.getName())).toString();
      url = new URL("jar:" + archiveURL + "!/" + fileName);
    }
    else {
      url = getURLFromExtension(fileName);
    }
    if (url == null) {
      throw new IOException("\'" + fileName + "\' not found in " + archive.getName());
    }
    return url;
  }

  protected URL getURLFromExtension(String fileName) {
    URL url = null;
    for (int i = 0; i < extensions.size() && url == null; ++i) {
      final DataArchive ext = extensions.get(i);
      try {
        url = ext.getURL(fileName);
      }
      catch (IOException e) {
        // Not found in this extension.  Try the next.
      }
    }
    return url;
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
   * of its extensions. (At most one archive should be being edited at a time)
   * @return
   */
  public ArchiveWriter getWriter() {
    ArchiveWriter writer = null;
    if (this instanceof ArchiveWriter) {
      writer = (ArchiveWriter) this;
    }
    else {
      for (DataArchive ext : extensions) {
        if (ext instanceof ArchiveWriter) {
          writer = (ArchiveWriter) ext;
          break;
        }
      }
    }
    return writer;
  }

  /**
   * Get an inputstream from the given filename in the archive
   */
  public InputStream getFileStream(String file) throws IOException {
    InputStream stream = null;
    final ZipEntry entry = archive.getEntry(file);
    if (entry != null) {
      stream = archive.getInputStream(entry);
    }
    else {
      stream = getFileStreamFromExtension(file);
    }

    if (stream == null) {
      throw new IOException("\'" + file + "\' not found in " + archive.getName());
    }
    return stream;
  }

  protected InputStream getFileStreamFromExtension(String file) {
    InputStream stream = null;
    for (int i = 0; i < extensions.size() && stream == null; ++i) {
      final DataArchive ext = extensions.get(i);
      try {
        stream = ext.getFileStream(file);
      }
      catch (IOException e) {
        // Not found in this extension.  Try the next.
      }
    }
    return stream;
  }

// FIXME: could these two getFileStream methods be combined?
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
    catch (Exception e) {
      return null;
    }
  }

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

  public synchronized Class loadClass(String name, boolean resolve)
                                      throws ClassNotFoundException {
    Class c;
    try {
//      c = findSystemClass(name);
      c = Class.forName(name);
    }
    catch (Exception noClass) {
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

  protected PermissionCollection getPermissions(CodeSource codesource) {
    final PermissionCollection p = super.getPermissions(codesource);
    p.add(new AllPermission());
    return p;
  }

  protected Class findClass(String name) throws ClassNotFoundException {
    if (cs == null) {
      cs = new CodeSource((URL) null, (Certificate[]) null);
    }
    try {
      final String slashname = name.replace('.', '/');
      final InputStream in = getFileStream(slashname + ".class");
      final byte[] data = getBytes(in);
      return defineClass(name, data, 0, data.length, cs);
    }
    catch (IOException e) {
      throw new ClassNotFoundException(
        "Unable to load " + name + "\n" + e.getMessage());
    }
  }

  ////////////////////////////// Deprecated ////////////////////////////////
  @Deprecated private BooleanConfigurer smoothPrefs;

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
    final TreeSet<String> s = new TreeSet<String>();
    if (archive != null) {
      try {
        final ZipInputStream zis
            = new ZipInputStream(new FileInputStream(archive.getName()));

        ZipEntry entry = null;
        while ((entry = zis.getNextEntry()) != null) {
          if (entry.getName().startsWith(IMAGE_DIR)) {
            s.add(entry.getName().substring(IMAGE_DIR.length()));
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    for (DataArchive ext : extensions) {
      s.addAll(ext.setOfImageNames());
    }
    return s;
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
    try {
      return new SourceOp(name).getImage(null);
    }
    catch (CancellationException e) {
      throw new IOException(e); 
    }
    catch (InterruptedException e) {
      throw new IOException(e); 
    }
    catch (ExecutionException e) {
      throw new IOException(e); 
    }
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
    try {
      return new RotateScaleOp(
        new ImageSourceOp(base), theta, scale).getImage(null);
    }
    catch (CancellationException e) {
      return null;
    }
    catch (InterruptedException e) {
      return null;
    }
    catch (ExecutionException e) {
      return null;
    }
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
