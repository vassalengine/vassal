/*
 * $Id$
 *
 * Copyright (c) 2010, 2011 by Joel Uckelman
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

package VASSAL.tools.image.tilecache;

import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.STARTING_IMAGE;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILE_WRITTEN;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILING_FINISHED;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.ArrayUtils;
import VASSAL.tools.concurrent.DaemonThreadFactory;
import VASSAL.tools.image.FallbackImageTypeConverter;
import VASSAL.tools.image.ImageIOImageLoader;
import VASSAL.tools.image.ImageLoader;
import VASSAL.tools.image.ImageTypeConverter;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.io.ZipArchive;
import VASSAL.tools.lang.Callback;

/**
 * Tiles images contained in a ZIP archive.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class ZipFileImageTiler {
  private static final Logger logger =
    LoggerFactory.getLogger(ZipFileImageTiler.class);

  public static void main(String[] args) {
    try {
      logger.info("Starting");

      // Oh we have no heads, we have no HEADS!
      System.setProperty("java.awt.headless", "true");

      // Ensure that exceptions are logged.
      Thread.setDefaultUncaughtExceptionHandler(
                                        new Thread.UncaughtExceptionHandler() {
        public void uncaughtException(Thread thread, Throwable thrown) {
          logger.error(thread.getName(), thrown);
        }
      });

      // Parse the arguments
      final String zpath = args[0];
      final String tpath = args[1];
      final int tw = Integer.parseInt(args[2]);
      final int th = Integer.parseInt(args[3]);

      // Get the image paths from stdin, one per line
      final List<String> pl = new ArrayList<String>();
      BufferedReader stdin = null;
      try {
        stdin = new BufferedReader(new InputStreamReader(System.in));
        String s;
        while ((s = stdin.readLine()) != null) {
          pl.add(s);
        }
      }
      catch (IOException e) {
        logger.error("", e);
      }
      finally {
        IOUtils.closeQuietly(stdin);
      }

      final String[] ipaths = pl.toArray(new String[pl.size()]);

      // TODO: Determine what the optimal number of threads is.
      final Runtime runtime = Runtime.getRuntime();
      final ExecutorService exec = new ThreadPoolExecutor(
        runtime.availableProcessors(),
        runtime.availableProcessors()+1,
        60, TimeUnit.SECONDS,
        new LinkedBlockingQueue<Runnable>(),
        new DaemonThreadFactory(ZipFileImageTiler.class.getSimpleName())
      );

      final TemporaryFileFactory tfac = new TemporaryFileFactory() {
        public File create() throws IOException {
          return File.createTempFile("img", null, new File(tpath));
        }
      };

      final ImageTypeConverter itc = new FallbackImageTypeConverter(tfac);
      final ImageLoader loader = new ImageIOImageLoader(itc);

      final TileSlicer slicer = new TileSlicerImpl();
      final FileArchiveImageTiler tiler = new FileArchiveImageTiler();

      final String portProp = System.getProperty("VASSAL.port");

      Socket sock = null;
      DataOutputStream dout = null;

      try {
        if (portProp != null) {
          final int port = Integer.parseInt(portProp);
          final InetAddress lo = InetAddress.getByName(null);
          sock = new Socket(lo, port);
          sock.shutdownInput();
          dout = new DataOutputStream(sock.getOutputStream());
        }
        else {
          dout = new DataOutputStream(System.err);
        }

        final DataOutputStream out = dout;

        final Callback<String> imageL = new Callback<String>() {
          public void receive(String ipath) throws IOException {
            out.writeByte(STARTING_IMAGE);
            out.writeUTF(ipath);
            out.flush();
          }
        };

        final Callback<Void> tileL = new Callback<Void>() {
          public void receive(Void obj) throws IOException {
            out.writeByte(TILE_WRITTEN);
            out.flush();
          }
        };

        final Callback<Void> doneL = new Callback<Void>() {
          public void receive(Void obj) throws IOException {
            out.writeByte(TILING_FINISHED);
            out.flush();
          }
        };

        FileArchive fa = null;
        try {
          fa = new ZipArchive(zpath);

          // Tile the images
          tiler.run(
            fa, tpath, tw, th, ipaths, exec,
            loader, slicer, imageL, tileL, doneL
          );

          fa.close();
        }
        catch (IOException e) {
          logger.error("", e);
        }
        finally {
          IOUtils.closeQuietly(fa);
        }

        dout.close();
        if (sock != null) {
          sock.close();
        }
      }
      catch (IOException e) {
        logger.error("", e);
      }
      finally {
        IOUtils.closeQuietly(dout);
        IOUtils.closeQuietly(sock);
      }
    }
    finally {
      logger.info("Exiting");
    }
  }
}
