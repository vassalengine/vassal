/*
 *
 * Copyright (c) 2008 Brent Easton
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
package VASSAL.tools.icon;

import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.swing.Icon;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.IllegalBuildException;
import VASSAL.i18n.Resources;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.JarArchive;
import VASSAL.tools.ReadErrorDialog;

/**
 * Manage and Provide Icons in standard sizes.
 *
 */
public final class IconFactory {
  private static final Logger logger = LoggerFactory.getLogger(IconFactory.class);

  static final String FILE = "file:"; //$NON-NLS-1$
  static final String JAR = "jar:"; //$NON-NLS-1$

  private static final JarArchive jar = new JarArchive();

  private static IconFactory instance = new IconFactory();
  private final Map<String, IconFamily> iconFamilies = new ConcurrentHashMap<>();
  private static final Object preloadLock = new Object();
  private final Thread preloadThread;

  /**
   * Set the Singleton instance
   * @param i instance
   */
  static void setInstance(IconFactory i) {
    instance = i;
  }

  /**
   * Return the Singleton instance.
   */
  static IconFactory getInstance() {
    if (instance == null) {
      throw new IllegalStateException("No IconFactory instance"); //$NON-NLS-1$
    }
    return instance;
  }

  /**
   * Create a new IconFactory.
   */
  public IconFactory() {

// FIXME: Maybe send this off to an executor?
// FIXME: preloadThread is never set to null, cannot be gc'd
    // Find all available Icon Families within Vassal.
    // May take a little while, so run it on a background thread
    preloadThread = new Thread(() -> {
      synchronized (preloadLock) {
        try {
          initVassalIconFamilys();
        }
        catch (IllegalBuildException e) {
          ErrorDialog.bug(e);
        }
      }
    }, "IconFactory-preload"); //$NON-NLS-1$
    preloadThread.start();
  }

  /**
   * Return an Icon of the specified size. Standard sizes are defined in IconFamily
   *
   * @param iconFamilyName Name of Icon family
   * @param size Size (See IconFamily)
   * @return Sized Icon
   */
  public static Icon getIcon(String iconFamilyName, int size) {
    final IconFamily family = getInstance().getFamily(iconFamilyName);
    if (family == null) {
      throw new IllegalStateException(Resources.getString("Error.not_found", IconFamily.getConfigureTypeName() + " " + iconFamilyName)); //$NON-NLS-1$
    }
    return family.getIcon(size);
  }

  /**
    Return an Icon of the specified size as an Image. Standard sizes are defined in IconFamily
   *
   * @param iconFamilyName Name of Icon family
   * @param size Size (See IconFamily)
   * @return Sized Image
   */
  public static BufferedImage getImage(String iconFamilyName, int size) {
    final IconFamily family = getInstance().getFamily(iconFamilyName);
    if (family == null) {
      throw new IllegalStateException(Resources.getString("Error.not_found", IconFamily.getConfigureTypeName() + " " + iconFamilyName)); //$NON-NLS-1$
    }
    return family.getImage(size);
  }

  /**
   * @return Return a sorted list of all Icon Family names.
   */
  public static List<String> getIconFamilyNames() {
    return getInstance().getIconFamilyList();
  }

  /**
   * Add a new IconFamily
   * @param family Icon Family Name
   */
  public static void addIconFamily(IconFamily family) {
    getInstance().add(family);
  }

  /**
   * Remove an IconFamily
   * @param family Icon Family Name
   */
  public static void removeIconFamily(IconFamily family) {
    getInstance().remove(family);
  }

  /**
   * Rename an IconFamily
   *
   * @param oldName Old Icon Family Name
   * @param family New Icon Family Name
   */
  public static void renameIconFamily(String oldName, IconFamily family) {
    getInstance().rename(oldName, family);
  }

  /**
   * @param name name of family
   * @return Return an Icon Family
   */
  public static IconFamily getIconFamily(String name) {
    return getInstance().getFamily(name);
  }

  /**
   * Add an Icon Family. Don't overwrite an existing Icon Family
   * of the same name.
   *
   * @param family family to add
   */
  private void add(IconFamily family) {
    iconFamilies.putIfAbsent(family.getName(), family);
  }

  /**
   * Remove an Icon Family. Ensure that the family to be removed is
   * the same as the one on the list.
   * @param family family to remove
   */
  private void remove(IconFamily family) {
    final IconFamily old = iconFamilies.get(family.getName());
    if (old != null && old == family) {
      iconFamilies.remove(family.getName());
    }
  }

  /**
   * Rename an IconFamily. Does not affect existing families with the
   * same name.
   *
   * @param oldFamilyName old name
   * @param iconFamily new name
   */
  private void rename(String oldFamilyName, IconFamily iconFamily) {
    final IconFamily oldFamily = iconFamilies.get(oldFamilyName);
    if (oldFamily != null && oldFamily == iconFamily) {
      iconFamilies.remove(oldFamilyName);
    }
    add(iconFamily);
  }


  /**
   * Return an individual named IconFamily.
   * Ensure the Vassal icon prescan has completed first.
   *
   * @param iconFamilyName name of family
   * @return Icon Family
   */
  IconFamily getFamily(String iconFamilyName) {
    try {

// FIXME: This is bad---we should wait on a Future instead.
      // Ensure preload is complete
      if (preloadThread.isAlive()) {
        try {
          preloadThread.join();
        }
        catch (InterruptedException e) {
        }
      }

      return iconFamilies.get(iconFamilyName);
    }
    catch (IllegalStateException e) {
      ErrorDialog.bug(e);
    }
    return null;
  }

  /**
   * Return a sorted list of all available IconFamily names.
   *
   * @return Icon Family name list
   */
  private List<String> getIconFamilyList() {
    final List<String> names;
    synchronized (preloadLock) {
      names = new ArrayList<>(iconFamilies.keySet());
    }
    Collections.sort(names);
    return names;
  }

  /** -------------------------------------------------------------------
   * Inspect the Jar file (for a standard installation) or the local file
   * system (Vassal running under a debugger) and determine all available
   * Icons of all sizes and collect them into named Icon Families.
   *
   * This is essentially a cross-reference of all available Icons to ensure
   * fast processing of requests for Icons. No Icons are created at this
   * stage.
   */
  private void initVassalIconFamilys() {
    URL imageUrl = null;
    try {
      //Build a URL to the Vassal images folder. It is guaranteed to exist
      // in any version of Vassal
      // Bug 9670 BUT there may also be an images folder in another Jar file in the Classloader path before the Vengine.jar
      // so look for an Icon we know must exist there.
      // imageUrl = jar.getURL(DataArchive.IMAGE_DIR
      imageUrl = jar.getURL(DataArchive.IMAGE_DIR + ApplicationIcons.VASSAL_ICON_LARGE);
      imageUrl = new URL(imageUrl.toString().substring(0, imageUrl.toString().length() - ApplicationIcons.VASSAL_ICON_LARGE.length()));

      logger.debug("VASSAL images folder found at " + imageUrl); //NON-NLS

      // Determine if we are running locally under a debugger, or
      // from an installation package. If running an installed version
      // of Vassal, the images URL will start with "jar:".
      if (imageUrl.toString().startsWith(FILE)) {
        findLocalScalableIcons();
        for (int size = 0; size < IconFamily.SIZE_DIRS.length; size++) {
          findLocalSizedIcons(size);
        }
      }
      else if (imageUrl.toString().startsWith(JAR)) {
        findJarIcons();
      }
      else {
        throw new IllegalBuildException("Unknown Vassal Image source type: " + imageUrl); //$NON-NLS-1$
      }
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, imageUrl.toString());
    }
  }

  /**
   * Record all icons of the specified size found in the local file system
   * NB. Vassal is not running from a bundled Jar file
   *
   * @param size size to look for
   * @throws IOException oops
   */
  private void findLocalSizedIcons(int size) throws IOException {
    final String path = DataArchive.ICON_DIR + IconFamily.SIZE_DIRS[size];
    final URL sizeURL = jar.getURL(path);

    try (InputStream in = sizeURL.openStream();
         InputStreamReader isr = new InputStreamReader(in, Charset.defaultCharset());
         BufferedReader br = new BufferedReader(isr)) {
      String imageName = ""; //$NON-NLS-1$
      while (imageName != null) {
        imageName = br.readLine();
        if (imageName != null) {
          final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
          IconFamily family = iconFamilies.get(familyName);
          if (family == null) {
            family = new IconFamily(familyName);
            logger.debug("Icon family " + familyName + " created for " + imageName); //NON-NLS
          }
          family.setSizeIconPath(size, "/" + path + imageName); //$NON-NLS-1$ //$NON-NLS-2$
          iconFamilies.put(familyName, family);
        }
      }
    }
  }

  /**
   * Record all scalable icons found on the local file system.
   * NB. Vassal is not running from a bundled Jar file
   *
   * @throws IOException oops
   */
  private void findLocalScalableIcons() throws IOException {
    final String scalablePath = DataArchive.ICON_DIR + IconFamily.SCALABLE_DIR;
    final URL url = jar.getURL(scalablePath);

    try (InputStream in = url.openStream();
         InputStreamReader isr = new InputStreamReader(in, Charset.defaultCharset());
         BufferedReader br = new BufferedReader(isr)) {
      String imageName = ""; //$NON-NLS-1$
      while (imageName != null) {
        imageName = br.readLine();
        if (imageName != null) {
          final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
          IconFamily family = iconFamilies.get(familyName);
          if (family == null) {
            family = new IconFamily(familyName);
            logger.debug("Icon family " + familyName + " created for " + imageName); //NON-NLS
          }
          family.setScalableIconPath("/" + scalablePath + imageName); //$NON-NLS-1$ //$NON-NLS-2$
          iconFamilies.put(familyName, family);
        }
      }
    }
  }

  /**
   * Process the installed Vassal JarFile to find contained Icons
   * @throws IOException oops
   */
  private void findJarIcons() throws IOException {

    // Path to scalable icons
    final String scalablePath = DataArchive.ICON_DIR + IconFamily.SCALABLE_DIR;
    // Path to sized icons
    final String[] sizePaths = new String[IconFamily.SIZE_COUNT];
    for (int size = 0; size < IconFamily.SIZE_COUNT; size++) {
      sizePaths[size] = DataArchive.ICON_DIR + IconFamily.SIZE_DIRS[size];
    }

    final JarURLConnection j = (JarURLConnection) jar.getURL(DataArchive.IMAGE_DIR).openConnection();
    final JarFile vengine = j.getJarFile();

    for (final Enumeration<JarEntry> e = vengine.entries(); e.hasMoreElements();) {
      final JarEntry entry = e.nextElement();
      final String entryName = entry.getName();

      if (entryName.startsWith(DataArchive.ICON_DIR)) {
        if (entryName.startsWith(scalablePath) && ! entryName.equals(scalablePath)) {
          final String imageName = entryName.substring(scalablePath.length());
          final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
          IconFamily family = iconFamilies.get(familyName);
          if (family == null) {
            family = new IconFamily(familyName);
            logger.debug("Icon family " + familyName + " created for " + imageName); //NON-NLS
          }
          family.setScalableIconPath("/" + entryName); //$NON-NLS-1$
          iconFamilies.put(familyName, family);
          continue;
        }

        for (int size = 0; size < IconFamily.SIZE_COUNT; size++) {
          if (entryName.startsWith(sizePaths[size]) && ! entryName.equals(sizePaths[size])) {
            final String imageName = entryName.substring(sizePaths[size].length());
            final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
            IconFamily family = iconFamilies.get(familyName);
            if (family == null) {
              family = new IconFamily(familyName);
              logger.debug("Icon family " + familyName + " created for " + imageName); //NON-NLS
            }
            family.setSizeIconPath(size, "/" + entryName); //$NON-NLS-1$
            iconFamilies.put(familyName, family);
            break;
          }
        }
      }
    }
  }
}
