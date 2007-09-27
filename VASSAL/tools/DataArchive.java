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
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.MemoryCacheImageInputStream;
import javax.swing.ImageIcon;

import sun.applet.AppletAudioClip;
import VASSAL.build.module.documentation.HelpFile;

/**
 * Wrapper around a Zip archive with methods to cache images
 */
public class DataArchive extends SecureClassLoader {
  protected ZipFile archive = null;
  protected List<DataArchive> extensions = new ArrayList<DataArchive>();
  private Map<String,Image> imageCache = new HashMap<String,Image>();
  private Map<String,AudioClip> soundCache = new HashMap<String,AudioClip>();
  private Map<TransformedCacheKey,Image> transImageCache =
    new HashMap<TransformedCacheKey,Image>();
  private Map<String,ImageSource> imageSources =
    new HashMap<String,ImageSource>();
  protected String[] imageNames;
  public static final String IMAGE_DIR = "images/";
  public static final String SOUNDS_DIR = "sounds/";
  private CodeSource cs;
  protected SVGManager svgManager;

  // empty image for images scaled to zero size
  private static final Image NULL_IMAGE =
   new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR);

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

  /**
   * @deprecated Use {@link getImage} instead.
   */
  @Deprecated public static Image findImage(File zip, String file) throws IOException {
    return getImage(getFileStream(zip, file));
  }

  public static InputStream getFileStream(File zip, String file) throws IOException {
    try {
      ZipFile z = new ZipFile(zip);
      return z.getInputStream(z.getEntry(file));
    }
    catch (Exception e) {
      throw new IOException("Couldn't locate " + file + " in " + zip.getName()
                            + ": " + e.getMessage());
    }
  }
 
  /**
   * @deprecated Use {@link getImage} instead.
   */
  @Deprecated public static Image findImage(File dir, String zip, String file)
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

  /*
   ** Find an image from the archive
   * Once an image is found, cache it in our HashMap.
   */
  public Image getCachedImage(String name) throws IOException {
    if (name == null) {
      return null;
    }
    String path = IMAGE_DIR + name;
    String gifPath = path + ".gif";
    Image image = imageCache.get(path);
    ImageSource src;
    if (image != null) {
      return image;
    }
    else if ((image = imageCache.get(gifPath)) != null) {
      return image;
    }
    else {
      if ((src = imageSources.get(name)) != null) {
         image = src.getImage();
      }
      else {
         image = getImage(name);
      }   
   
      imageCache.put(path,image);
      return image;
    }
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
   * @deprecated forceSmoothing is ignored
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
   * Return a transformed instance of the image.
   * The image will be retrieved from the cache if available, and cached
   * after retrieval if not.
   * @param base the untransformed Image
   * @param scale the scaling factor
   * @param theta the angle of rotation (in degrees) about the Image center
   */
  public Image getTransformedImage(Image base, double scale, double theta) {
    if (base == null) {
      return null;
    }

    Dimension d = getImageBounds(base).getSize();
    d.width *= scale;
    d.height *= scale;
    if (d.width == 0 || d.height == 0) {
      return NULL_IMAGE;
    }

    TransformedCacheKey key = new TransformedCacheKey(base, scale, theta);
    Image trans = transImageCache.get(key);
    if (trans == null) {
      trans = createTransformedInstance(base, scale, theta);
      new ImageIcon(trans); // Wait for the image to load
      transImageCache.put(key, trans);
    }
    return trans;
  }

  /**
   * @deprecated forceSmoothing is ignored
   * @param im
   * @param zoom
   * @param theta
   * @param forceSmoothing
   * @return
   */
  @Deprecated
  protected Image createTransformedInstance(Image im, double zoom,
      double theta, boolean forceSmoothing) {
    return createTransformedInstance(im, zoom, theta);
  }
  /**
   * Does the actual work of transforming an image.
   */
  protected Image createTransformedInstance(Image im, double zoom,
    double theta) {
    if (zoom == 1.0 && theta == 0.0) return im;

    if (im instanceof SVGManager.SVGBufferedImage) {
      // render SVG
      return ((SVGManager.SVGBufferedImage) im)
        .getTransformedInstance(zoom, theta);
    }
    else {
      // do high-quality scaling
      if (theta != 0.0) {
        final Rectangle ubox = getImageBounds(im);

        final AffineTransform t = new AffineTransform();
        t.rotate(-Math.PI/180*theta, ubox.getCenterX(), ubox.getCenterY());
        
        final Rectangle rbox = t.createTransformedShape(ubox).getBounds();

        final BufferedImage rot =
          new BufferedImage(rbox.width, rbox.height,
                            BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g = rot.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                           RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON);

        final AffineTransform tx = new AffineTransform();
        tx.translate(-rbox.x, -rbox.y);
        tx.rotate(-Math.PI/180*theta, ubox.getCenterX(), ubox.getCenterY());
        tx.translate(ubox.x, ubox.y);

        g.drawImage(im, tx, null);
        g.dispose();
        im = rot;
      }

      if (!(im instanceof BufferedImage) ||
          ((BufferedImage) im).getType() != BufferedImage.TYPE_INT_ARGB) {
        final BufferedImage tmp =
          new BufferedImage(im.getWidth(null), im.getHeight(null),
                            BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g = tmp.createGraphics();
        g.drawImage(im, 0, 0, null);
        g.dispose();
        im = tmp;
      }

      final AffineTransform t = AffineTransform.getScaleInstance(zoom, zoom);
      final Rectangle sbox =
        t.createTransformedShape(getImageBounds(im)).getBounds();
      return GeneralFilter.zoom(sbox, (BufferedImage) im,
                                new GeneralFilter.Lanczos3Filter());
    }
  }

  /**
   * Return a scaled instance of the image.
   * The image will be retrieved from cache if available, cached otherwise
   * @param base
   * @param scale
   * @return
   */
  public Image getScaledImage(Image base, double scale) {
    return getTransformedImage(base, scale, 0.0, true);
  }

  /**
   * @deprecated use getTransformedImage
   * The image will be retrieved from cache if available, cached otherwise
   * @param base
   * @param scale
   * @param reversed
   * @param forceSmoothing If true, force smoothing.  This usually yields better results, but can be slow for large images
   * @return
   */
  @Deprecated
  public Image getScaledImage(Image base, double scale, boolean reversed,
                              boolean forceSmoothing) {
    return getTransformedImage(base, scale,
                               reversed ? 180.0 : 0.0);
  }

  /**
   *
   * @param im
   * @return the boundaries of this image, where (0,0) is the center of the image
   */
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

  /**
   * Get the size of an image without loading and decoding it.
   *
   * @param name filename of the image
   * @return the size of the image
   */
  public Dimension getImageSize(String name) throws IOException {
    String path = IMAGE_DIR + name;
    String gifPath = path + ".gif";

    if (name.toLowerCase().endsWith(".svg")) {
      if (svgManager == null) svgManager = new SVGManager(this);

      return svgManager.getImageSize("jar:file://" +
         (archive != null ? archive.getName() : "null") + "!/" + path,
        getFileStream(path));
    }
    else {
      String ext = name.substring(name.lastIndexOf('.') + 1);
      ImageReader reader = ImageIO.getImageReadersBySuffix(ext).next();

      try {
         reader.setInput(new MemoryCacheImageInputStream(getFileStream(path)));
      }
      catch (IOException e) {
         reader.setInput(
            new MemoryCacheImageInputStream(getFileStream(gifPath)));
      }

      return new Dimension(reader.getWidth(0), reader.getHeight(0));
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

  public void unCacheImage(String file) {
    imageCache.remove(IMAGE_DIR + file);
  }

  public void unCacheImage(Image im) {
    ArrayList<TransformedCacheKey> toClear =
      new ArrayList<TransformedCacheKey>();
    for (TransformedCacheKey key : transImageCache.keySet()) {
      if (im.equals(key.base)) {
        toClear.add(key);
      }
    }

    for (TransformedCacheKey key : toClear) {
      transImageCache.remove(key);
    }
  }
  
  public void clearTransformedImageCache() {
    transImageCache.clear();
    for (DataArchive ext : extensions) {
      ext.clearTransformedImageCache();
    }
  }
 
  /**
   * @deprecated Use {@link #clearTransformedImageCache()} instead.
   */ 
  @Deprecated public void clearScaledImageCache() {
    clearTransformedImageCache();
  }

  public Image getImage(String name) throws IOException {
    String path = IMAGE_DIR + name;
    String gifPath = path + ".gif";
    Image image = null;

    if (name.toLowerCase().endsWith(".svg")) {
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
    return Toolkit.getDefaultToolkit().createImage(getBytes(in));
  }

  /**
   * Add an ImageSource under the given name, but only if no source is yet registered under this name.
   * @param name
   * @param src
   * @return true if the ImageSource was added, false if it existed already
   */
  public boolean addImageSource(String name, ImageSource src) {
    if (!imageSources.containsKey(name)) {
      imageSources.put(name,src);
      imageNames = null;
      return true;
    }
    return false;
  }

  public void removeImageSource(String name) {
    imageSources.remove(name);
    imageNames = null;
    unCacheImage(name);
  }

  /**
   * Read all available bytes from the given InputStream
   */
  public static byte[] getBytes(InputStream in) throws IOException {
    BufferedInputStream bufIn = new BufferedInputStream(in);
    int nLen = bufIn.available();
    int nCurBytes = 0;
    byte buffer[] = null;
    byte abyte0[] = new byte[nLen];

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

  /**
   * Get an inputstream from the given filename in the archive
   */
  public InputStream getFileStream(String file) throws IOException {
    InputStream stream = null;
    ZipEntry entry = archive.getEntry(file);
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
    InputStream stream = null;;
    for (int i = 0; i < extensions.size() && stream == null; ++i) {
      DataArchive ext = extensions.get(i);
      try {
        stream = ext.getFileStream(file);
      }
      catch (IOException e) {
        // Not found in this extension.  Try the next.
      }
    }
    return stream;
  }

  public URL getURL(String fileName) throws IOException {
    if (archive == null) {
      throw new IOException("Must save before accessing contents");
    }
    URL url = null;
    ZipEntry entry = archive.getEntry(fileName);
    if (entry != null) {
      String archiveURL = HelpFile.toURL(new File(archive.getName())).toString();
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
      DataArchive ext = extensions.get(i);
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

  public static InputStream getFileStream(File dir, String zipName, String file) {
    try {
      if ((new File(dir, zipName)).exists()) {
        ZipFile zip = new ZipFile(new File(dir, zipName));
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

  public synchronized Class loadClass(String name,
                                      boolean resolve) throws ClassNotFoundException {
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
    PermissionCollection p = super.getPermissions(codesource);
    p.add(new AllPermission());
    return p;
  }

  protected Class findClass(String name) throws ClassNotFoundException {
    if (cs == null) {
      cs = new CodeSource((URL) null, (Certificate[]) null);
    }
    try {
      String slashname = name.replace('.', '/');
      InputStream in = getFileStream(slashname + ".class");
      byte[] data = getBytes(in);
      return defineClass(name, data, 0, data.length, cs);
    }
    catch (IOException e) {
      throw new ClassNotFoundException("Unable to load " + name + "\n" + e.getMessage());
    }
  }

  public String[] getImageNames() {
    if (isNameCacheStale()) {
      Set<String> s = setOfImageNames();
      imageNames = s.toArray(new String[s.size()]);
      Arrays.sort(imageNames, String.CASE_INSENSITIVE_ORDER);
    }
    return imageNames;
  }

  protected boolean isNameCacheStale() {
    if (imageNames == null) return true;
    for (DataArchive ext : extensions) {
      if (ext.imageNames == null) return true;
    }
    return false;
  }

  /**
   * @return the names of the image files stored in this DataArchive
   * and its extensions
   */
  protected Set<String> setOfImageNames() {
    HashSet<String> s = new HashSet<String>();
    if (archive != null) {
      try {
        ZipInputStream zis
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
   * The hash key class for use with {@link #transImageCache}.
   */
  private static class TransformedCacheKey {
    private Image base;
    private double zoom;
    private double theta;

    public TransformedCacheKey(Image base, double zoom, double theta) {
      this.base = base;
      this.zoom = zoom;
      this.theta = theta;
    }

    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof TransformedCacheKey)) return false;

      final TransformedCacheKey t = (TransformedCacheKey) o;

      if (base == null) return t.base == null;
      if (!base.equals(t.base) || zoom != t.zoom || theta != t.theta) {
        return false;
      }

      return true;
    }

    public int hashCode() {
      int result = 17;
      result = 37 * result + base.hashCode();
      long l = Double.doubleToLongBits(zoom);
      result = 37 * result + (int)(l^(l >>> 32)); 
      l = Double.doubleToLongBits(theta);
      result = 37 * result + (int)(l^(l >>> 32)); 
      return result;
    }
  }
}
