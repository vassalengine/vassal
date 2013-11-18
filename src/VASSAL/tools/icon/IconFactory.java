/*
 * $Id$
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
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.JarURLConnection;
import java.net.URL;
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
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.JarArchive;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.io.IOUtils;

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
  private Map<String, IconFamily> iconFamilies = new ConcurrentHashMap<String, IconFamily>();
  private static final Object preloadLock = new Object();
  private Thread preloadThread;

  /**
   * Set the Singleton instance
   * @param i
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
  public IconFactory () {

// FIXME: Maybe send this off to an executor?
// FIXME: preloadThread is never set to null, cannot be gc'd
    // Find all available Icon Familys within Vassal.
    // May take a little while, so run it on a background thread
    preloadThread = new Thread(new Runnable(){
      public void run() {
        synchronized (preloadLock) {
          try {
            initVassalIconFamilys();
          }
          catch (IllegalBuildException e) {
            ErrorDialog.bug(e);
          }
        }
      }
    }, "IconFactory-preload"); //$NON-NLS-1$
    preloadThread.start();
  }

  /**
   * Return an Icon of the specified size. Standard sizes are defined in IconFamily
   *
   * @param IconFamilyName Name of Icon family
   * @param size Size (See IconFamily)
   * @return Sized Icon
   */
  public static Icon getIcon(String iconFamilyName, int size) {
    IconFamily family = getInstance().getFamily(iconFamilyName);
    if (family == null) {
      throw new IllegalStateException(Resources.getString("Error.not_found", IconFamily.getConfigureTypeName() + " " + iconFamilyName)); //$NON-NLS-1$
    }
    return family.getIcon(size);
  }

  /**
    Return an Icon of the specified size as an Image. Standard sizes are defined in IconFamily
   *
   * @param IconFamilyName Name of Icon family
   * @param size Size (See IconFamily)
   * @return Sized Image
   */
  public static BufferedImage getImage(String iconFamilyName, int size) {
    final IconFamily family = getInstance().getFamily(iconFamilyName);
    if (family == null) {
      throw new IllegalStateException(Resources.getString("Error.not_found", IconFamily.getConfigureTypeName()+ " " + iconFamilyName)); //$NON-NLS-1$
    }
    return family.getImage(size);
  }

  /**
   * Return a sorted list of all Icon Family names.
   * @return
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
   * @param name Icon Family Name
   */
  public static void removeIconFamily(IconFamily family) {
    getInstance().remove(family);
  }

  /**
   * Rename an IconFamily
   *
   * @param oldName Old Icon Family Name
   * @param newName New Icon Family Name
   */
  public static void renameIconFamily(String oldName, IconFamily family) {
    getInstance().rename(oldName, family);
  }

  /**
   * Return an Icon Family
   * @param name
   * @return
   */
  public static IconFamily getIconFamily(String name) {
    return getInstance().getFamily(name);
  }

  /**
   * Add an Icon Family. Don't overwrite an existing Icon Family
   * of the same name.
   *
   * @param family
   */
  private void add(IconFamily family) {
    if (iconFamilies.get(family.getName()) == null) {
      iconFamilies.put(family.getName(), family);
    }
  }

  /**
   * Remove an Icon Family. Ensure that the family to be removed is
   * the same as the one on the list.
   * @param family
   */
  private void remove(IconFamily family) {
    final IconFamily old = iconFamilies.get(family.getName());
    if (old != null && old == family) {
      iconFamilies.remove(family.getName());
    }
  }

  /**
   * Rename an IconFamily. Do not affect existing families with the
   * same name.
   *
   * @param oldFamilyName
   * @param iconFamily
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
   * @param iconFamilyName
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
    final ArrayList<String> names = new ArrayList<String>();
    synchronized (preloadLock) {
      for (String s : iconFamilies.keySet()) {
        names.add(s);
      }
    }
    Collections.sort(names);
    return names;
  }

  /** -------------------------------------------------------------------
   * Inspect the Jar file (for a standard installation) or the local file
   * system (Vassal running under a debugger) and determine all available
   * Icons of all sizes and collect them into named Icon Familys.
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
      imageUrl = jar.getURL(DataArchive.IMAGE_DIR);
      logger.info("VASSAL images folder found at "+imageUrl);
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
        throw new IllegalBuildException ("Unknown Vassal Image source type: "+imageUrl.toString()); //$NON-NLS-1$
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
   * @param size
   * @throws IOException
   */
  protected void findLocalSizedIcons(int size) throws IOException {
    final String path = DataArchive.ICON_DIR+IconFamily.SIZE_DIRS[size];
    final URL sizeURL = jar.getURL(path);
    BufferedReader br = null;
    try {
      br = new BufferedReader(new InputStreamReader(sizeURL.openStream()));
      String imageName = ""; //$NON-NLS-1$
      while (imageName != null) {
        imageName = br.readLine();
        if (imageName != null) {
          final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
          IconFamily family = iconFamilies.get(familyName);
          if (family == null) {
            family = new IconFamily(familyName);
            logger.info("Icon family "+familyName+" created for "+imageName);
          }
          family.setSizeIconPath(size, "/" + path + imageName); //$NON-NLS-1$ //$NON-NLS-2$
          iconFamilies.put(familyName, family);
        }
      }
    }
    finally {
      IOUtils.closeQuietly(br);
    }
  }

  /**
   * Record all scalable icons found on the local file system.
   * NB. Vassal is not running from a bundled Jar file
   *
   * @throws IOException
   */
  private void findLocalScalableIcons() throws IOException {
    final String scalablePath = DataArchive.ICON_DIR+IconFamily.SCALABLE_DIR;
    final URL url = jar.getURL(scalablePath);
    BufferedReader br = null;
    try {
      br= new BufferedReader(new InputStreamReader(url.openStream()));
      String imageName = ""; //$NON-NLS-1$
      while (imageName != null) {
        imageName = br.readLine();
        if (imageName != null) {
          final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
          IconFamily family = iconFamilies.get(familyName);
          if (family == null) {
            family = new IconFamily(familyName);
            logger.info("Icon family "+familyName+" created for "+imageName);
          }
          family.setScalableIconPath("/" + scalablePath + imageName); //$NON-NLS-1$ //$NON-NLS-2$
          iconFamilies.put(familyName, family);
        }
      }
    }
    finally {
      IOUtils.closeQuietly(br);
    }
  }

  /**
   * Process the installed Vassal JarFile to find contained Icons
   * @throws IOException
   */
  private void findJarIcons() throws IOException {

    // Path to scalable icons
    final String scalablePath = DataArchive.ICON_DIR+IconFamily.SCALABLE_DIR;
    // Path to sized icons
    final String[] sizePaths = new String[IconFamily.SIZE_COUNT];
    for (int size = 0; size < IconFamily.SIZE_COUNT; size++) {
      sizePaths[size] = DataArchive.ICON_DIR+IconFamily.SIZE_DIRS[size];
    }

    final JarURLConnection j = (JarURLConnection) jar.getURL(DataArchive.IMAGE_DIR).openConnection();
    final JarFile vengine = j.getJarFile();

    for (Enumeration<JarEntry> e = vengine.entries(); e.hasMoreElements();) {
      final JarEntry entry = e.nextElement();
      final String entryName = entry.getName();

      if (entryName.startsWith(DataArchive.ICON_DIR)) {
        if (entryName.startsWith(scalablePath) && ! entryName.equals(scalablePath)) {
          final String imageName = entryName.substring(scalablePath.length());
          final String familyName = imageName.split("\\.")[0]; //$NON-NLS-1$
          IconFamily family = iconFamilies.get(familyName);
          if (family == null) {
            family = new IconFamily(familyName);
            logger.info("Icon family "+familyName+" created for "+imageName);
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
              logger.info("Icon family "+familyName+" created for "+imageName);
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
