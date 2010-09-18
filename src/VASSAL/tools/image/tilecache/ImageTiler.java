package VASSAL.tools.image.tilecache;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.DataArchive;
import VASSAL.tools.concurrent.Exec;
import VASSAL.tools.concurrent.Progressor;
import VASSAL.tools.image.GeneralFilter;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.io.IOUtils;

//public class ImageTiler implements Callable<Void> {
public class ImageTiler implements Callable<Void> {
  private static final Logger logger =
    LoggerFactory.getLogger(ImageTiler.class);

  protected final DataArchive archive;
  protected final File tdir;

  protected static final int tw = 256;
  protected static final int th = 256;

  public static final int FIND_IMAGES = 0;
  public static final int FIND_LARGE_IMAGES = 1;
  public static final int WRITE_TILES = 2;
  public static final int DONE = 3;

  public ImageTiler(DataArchive archive, File tdir) {
    this.archive = archive;
    this.tdir = tdir;
  }

  protected final PropertyChangeSupport pcs = new PropertyChangeSupport(this);

  public void addPropertyChangeListener(PropertyChangeListener listener) {
    pcs.addPropertyChangeListener(listener);
  }

  public void removePropertyChangeListener(PropertyChangeListener listener) {
    pcs.removePropertyChangeListener(listener);
  }

  public Void call() throws IOException,
                            ExecutionException,
                            InterruptedException {
    pcs.firePropertyChange("step", null, FIND_IMAGES);

    // build a list of all multi-tile images and count tiles
    final Set<String> images = archive.getImageNameSet();
    final int icount = images.size();
    int idone = 0;

    pcs.firePropertyChange("step", FIND_IMAGES, FIND_LARGE_IMAGES);

    final List<String> mtis = new ArrayList<String>();
    int tcount = 0;

    String prev_iname = null;
    for (String iname : images) {
      if (Thread.interrupted()) throw new InterruptedException();

      pcs.firePropertyChange("image", prev_iname, iname);
      prev_iname = iname;

      Dimension d = null;

      // check whether the image is more than one tile
      InputStream in = null;
      try {
        in = archive.getImageInputStream(iname);
        d = ImageUtils.getImageSize(iname, in);
        in.close();
      }
      catch (IOException e) {
        // can't read file, skip it
        logger.error("", e);
        continue;
      }
      finally {
        IOUtils.closeQuietly(in);
      }

      final int tcols = (int) Math.ceil((double) d.width / tw);
      final int trows = (int) Math.ceil((double) d.height / th);

      if (tcols > 1 || trows > 1) {
        mtis.add(iname);
      
        for (int div = 1; d.width/div > 0 && d.height/div > 0; div <<= 1) {
          final int tc = (int) Math.ceil((double) (d.width/div) / tw);
          final int tr = (int) Math.ceil((double) (d.height/div) / th);
          tcount += tc*tr;
        }
      }
    }

    pcs.firePropertyChange("step", FIND_LARGE_IMAGES, WRITE_TILES);
    
    final Progressor prog = new Progressor(pcs, 0, tcount);

    // write tiles for each multi-tile image
    for (String iname : mtis) {
      if (Thread.interrupted()) throw new InterruptedException();

      pcs.firePropertyChange("image", prev_iname, iname);
      prev_iname = iname;

      // read the image
      BufferedImage img = null;
      InputStream in = null;
      try {
        in = archive.getImageInputStream(iname);
      }
      catch (FileNotFoundException e) {
        throw new ImageNotFoundException(iname, e);
      }
      catch (IOException e) {
        throw new ImageIOException(iname, e);
      }

      img = ImageUtils.getImage(iname, in);

/*
      for (Map m : Map.getMapList()) {
        final Zoomer z = m.getZoomer();
        for (String l : z.getAttributeValueString(Zoomer.ZOOM_LEVELS)) {
          final double scale = Double.parseDouble(l);





        }
      }
*/
      
      final int iw = img.getWidth();
      final int ih = img.getHeight();

      final GeneralFilter.Filter filter = new GeneralFilter.Lanczos3Filter();

      for (int div = 1; iw/div > 0 && ih/div > 0; div <<= 1) {
        final BufferedImage src;

        if (div > 1) {
          final Rectangle dstR = new Rectangle(0, 0, iw/div, ih/div);
// FIXME: Scale per-tile to save memory
          src = GeneralFilter.zoom(dstR, img, filter); 
        }
        else {
          src = img;
        }

        final int sw = src.getWidth();
        final int sh = src.getHeight();

        final int tcols = (int) Math.ceil((double) sw / tw);
        final int trows = (int) Math.ceil((double) sh / th);

        final List<Future<Void>> futures =
          new ArrayList<Future<Void>>(tcols*trows);

        for (int tx = 0; tx < tcols; ++tx) {
          for (int ty = 0; ty < trows; ++ty) {
            final String tn = iname + "(" + tx + "," + ty + ")@1:" + div;
            final File f = new File(tdir, tn);

            final TileWriter c = new TileWriter(src, f, tx, ty, tw, th) {
              @Override
              public Void call() throws IOException {
                super.call();
                prog.increment();
                return null;
              }
            };

            futures.add(Exec.ex.submit(c));
          }
        }
      
        try { 
          for (Future<Void> f : futures) f.get();
        }
        catch (CancellationException e) {
          throw (InterruptedException) new InterruptedException().initCause(e);
        }
        finally {
          // cancel everything if anything fails
          for (Future<Void> f : futures) {
            if (!f.isDone()) f.cancel(true);
          }
        }
      }
    }
/*
      final int w = img.getWidth();
      final int h = img.getHeight();

      final int tcols = (int) Math.ceil((double) w / tw);
      final int trows = (int) Math.ceil((double) h / th);
  
      // build and queue up the tile-writing tasks
      final BlockingQueue<TileWriter> queue =
        new ArrayBlockingQueue<TileWriter>(tcols*trows);

      final List<Future<Void>> futures =
        new ArrayList<Future<Void>>(tcols*trows);

// FIXME: get list of zoom levels and create scaled tiles, too.

      try {
        for (int tx = 0; tx < tcols; ++tx) {
          for (int ty = 0; ty < trows; ++ty) {
            final File f = new File(tdir, iname + "(" + tx + "," + ty + ")");

            final TileWriter c = new TileWriter(img, f, tx, ty, tw, th) {
              @Override
              public Void call() throws IOException {
                super.call();
                prog.increment();
                return null;
              }
            };

            queue.put(c);
            futures.add(Exec.ex.submit(new QueueJoiner(queue)));
          }
        }

// FIXME: overly complex? why not just have this thread wait on completion
// of the TileWriters?

        TileWriter c;
        while ((c = queue.poll()) != null) {
          c.call();
          if (Thread.interrupted()) throw new InterruptedException();
        }

        for (Future<Void> f : futures) {
          try {
            f.get();
          }
          catch (CancellationException e) {
            throw (InterruptedException)
              new InterruptedException().initCause(e);
          }
        }
      }
      finally {
        for (Future<Void> f : futures) f.cancel(true);
      }  
    }
*/

    pcs.firePropertyChange("step", WRITE_TILES, DONE);
    return null;
  }
}
