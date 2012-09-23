/*
 * $Id$
 *
 * Copyright (c) 2000-2010 by Rodney Kinney, Joel Uckelman
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
package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.event.IIOWriteProgressListener;
import javax.imageio.stream.ImageOutputStream;
import javax.swing.SwingUtilities;

import org.jdesktop.swingworker.SwingWorker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.PNGFileFilter;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.swing.ProgressDialog;

// FIXME: Replace this in 3.2 with tiling code.

/**
 * This allows the user to capture a snapshot of the entire map into
 * a PNG file.
 */
public class ImageSaver extends AbstractConfigurable {

  private static final Logger logger =
    LoggerFactory.getLogger(ImageSaver.class);

  protected LaunchButton launch;
  protected Map map;
  protected boolean promptToSplit = false;
  protected static final String DEFAULT_ICON = "/images/camera.gif";

  protected static ProgressDialog dialog;

  public ImageSaver() {
    final ActionListener al = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        writeMapAsImage();
      }
    };

    launch =
      new LaunchButton(null, TOOLTIP, BUTTON_TEXT, HOTKEY, ICON_NAME, al);

    // Set defaults for backward compatibility
    launch.setAttribute(TOOLTIP, "Save Map as PNG image");
    launch.setAttribute(BUTTON_TEXT, "");
    launch.setAttribute(ICON_NAME, DEFAULT_ICON);
  }

  public ImageSaver(Map m) {
    super();
    map = m;
  }

  /**
   * Expects to be added to a {@link Map}. Adds a button to the map window
   * toolbar that initiates the capture
   */
  public void addTo(Buildable b) {
    map = (Map) b;
    map.getToolBar().add(launch);
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.getToolBar().remove(launch);
    map.getToolBar().revalidate();
  }

  protected static final String HOTKEY = "hotkey";
  protected static final String BUTTON_TEXT = "buttonText";
  protected static final String TOOLTIP = "tooltip";
  protected static final String ICON_NAME = "icon";

  public String[] getAttributeNames() {
    return new String[] {
      BUTTON_TEXT,
      TOOLTIP,
      ICON_NAME,
      HOTKEY
    };
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
        Resources.getString(Resources.BUTTON_TEXT),
        Resources.getString(Resources.TOOLTIP_TEXT),
        Resources.getString(Resources.BUTTON_ICON),
        Resources.getString(Resources.HOTKEY_LABEL),
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, DEFAULT_ICON);
    }
  }

  public void setAttribute(String key, Object value) {
    launch.setAttribute(key, value);
  }

  public String getAttributeValueString(String key) {
    return launch.getAttributeValueString(key);
  }

  /**
   * Write a PNG-encoded snapshot of the map.
   */
  public void writeMapAsImage() {
    // prompt user for image filename
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.setSelectedFile(
      new File(fc.getCurrentDirectory(),
      GameModule.getGameModule().getLocalizedGameName() + "Map.png")
    );
    fc.addChoosableFileFilter(new PNGFileFilter());

    final Frame frame =
      (Frame) SwingUtilities.getAncestorOfClass(Frame.class, map.getView());

    if (fc.showSaveDialog(frame) != FileChooser.APPROVE_OPTION) return;

    final File file = fc.getSelectedFile();

    dialog = new ProgressDialog(frame, "Saving Map Image",
                                       "Saving map image...");

    // force the dialog to be a reasonable width
    // FIXME: this is not really a good way to do this---should do
    // something with the minimum size or font metrics
    final int l = "Saving map image as ".length() + file.getName().length() + 6;
    final StringBuilder b = new StringBuilder();
    for (int i = 0; i < l; i++) b.append("N");
    dialog.setLabel(b.toString());

    dialog.pack();
    dialog.setLabel("Saving map image as ");

    dialog.setIndeterminate(true);
    dialog.setLocationRelativeTo(frame);

    // get the dimensions of the image to write
    final Dimension s = map.mapSize();

    if (s.width == 0) s.width = 1;
    if (s.height == 0) s.height = 1;

    int w = (int) Math.round(s.width * map.getZoom());
    int h = (int) Math.round(s.height * map.getZoom());

    // ensure that the resulting image is at least 1x1
    if (w < 1 || h < 1) {
      if (s.width < s.height) {
        w = 1;
        h = s.height/s.width;
      }
      else {
        h = 1;
        w = s.width/s.height;
      }
    }

    writeMapRectAsImage(file, 0, 0, w, h);

    dialog.setVisible(true);
  }

  /**
   * Helper method for writing images.
   *
   * @param file the file to write
   * @param x the left edge of the map area to write
   * @param y the top edge of the map area to write
   * @param w the width of the map area to write
   * @param h the height of the map area to write
   */
  protected void writeMapRectAsImage(File file, int x, int y, int w, int h) {
    final SnapshotTask task = new SnapshotTask(file, x, y, w, h);

    task.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent e) {
        if ("progress".equals(e.getPropertyName())) {
          dialog.setProgress((Integer) e.getNewValue());
        }
        else if ("state".equals(e.getPropertyName())) {
          if ((SwingWorker.StateValue) e.getNewValue() ==
              SwingWorker.StateValue.DONE) {
            // close the dialog on cancellation or completion
            dialog.setVisible(false);
            dialog.dispose();
          }
        }
      }
    });

    dialog.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        task.cancel(true);
      }
    });

    task.execute();
  }

  private class SnapshotTask extends SwingWorker<Void,Void> {
    private int tiles;
    private int tilesDone = 0;

    private final File file;
    @SuppressWarnings("unused")
    private final int x;
    @SuppressWarnings("unused")
    private final int y;
    private final int w;
    private final int h;

    private final Color bg = ColorConfigurer.stringToColor(
      map.getAttributeValueString(Map.BACKGROUND_COLOR));

    private final List<File> files = new ArrayList<File>();

// FIXME: SnapshotTask ignores x,y!
    public SnapshotTask(File file, int x, int y, int w, int h) {
      this.file = file;
      this.x = x;
      this.y = y;
      this.w = w;
      this.h = h;
    }

    private void writeImage(final File f, BufferedImage img, Rectangle r)
                                                          throws IOException {

      files.add(f);

      // make sure that we can write the file before proceeding
      if (f.exists()) {
        if (!f.canWrite()) {
          throw new IOException(
            "Cannot write to the file \"" + f.getAbsolutePath() + "\""
          );
        }
      }
      else {
        final File p = f.getParentFile();
        if (p.isDirectory() && !p.canWrite()) {
          throw new IOException(
            "Cannot write to the directory \"" + p.getAbsolutePath() + "\""
          );
        }
      }

      // update the dialog on the EDT
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          dialog.setLabel("Saving map image as " + f.getName() + ":");
          dialog.setIndeterminate(true);
        }
      });

      // FIXME: do something to estimate how long painting will take
      final Graphics2D g = img.createGraphics();

      final Color oc = g.getColor();
      g.setColor(bg);
      g.fillRect(0, 0, img.getWidth(), img.getHeight());
      g.setColor(oc);

      g.translate(-r.x, -r.y);
      map.paintRegion(g, r, null);
      g.dispose();

      // update the dialog on the EDT
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          dialog.setIndeterminate(false);
        }
      });

      final ImageWriter iw = ImageIO.getImageWritersByFormatName("png").next();
      iw.addIIOWriteProgressListener(new IIOWriteProgressListener() {
        public void imageComplete(ImageWriter source) { }

        public void imageProgress(ImageWriter source, float percentageDone) {
          setProgress(Math.round((100*tilesDone + percentageDone)/tiles));
        }

        public void imageStarted(ImageWriter source, int imageIndex) { }

        public void thumbnailComplete(ImageWriter source) { }

        public void thumbnailProgress(ImageWriter source,
                                      float percentageDone) { }

        public void thumbnailStarted(ImageWriter source,
                                     int imageIndex, int thumbnailIndex) { }

        public void writeAborted(ImageWriter source) { }
      });

      ImageOutputStream os = null;
      try {
        os = ImageIO.createImageOutputStream(f);

        if (os == null) {
          throw new IOException("Failed to write file " + f.getAbsolutePath());
        }

        iw.setOutput(os);
        iw.write(img);
        os.close();
      }
      finally {
        iw.dispose();
        IOUtils.closeQuietly(os);
      }
    }

    @Override
    public Void doInBackground() throws IOException {
      setProgress(0);

      int tw = w;
      int th = h;
      BufferedImage img = null;

      // ensure that the size of the image data array (4 bytes per pixel)
      // does not exceed the maximum array size, 2^31-1 elements;
      // otherwise we'll overflow an int and have a negavive array size
      while ((long)tw * (long)th > Integer.MAX_VALUE/4) {
        if (tw > th) {
          tw = (int) Math.ceil(tw/2.0);
        }
        else {
          th = (int) Math.ceil(th/2.0);
        }
      }

      // find a size of BufferedImage we can allocate successfully
      while (img == null) {
        try {
          img = new BufferedImage(tw, th, BufferedImage.TYPE_INT_ARGB);
        }
        catch (OutOfMemoryError e) {
          if (tw > th) {
            tw = (int) Math.ceil(tw/2.0);
          }
          else {
            th = (int) Math.ceil(th/2.0);
          }
        }
      }

      if (tw == w && th == h) {
        // write the whole map as one image
        tiles = 1;
        writeImage(file, img, new Rectangle(0, 0, w, h));
      }
      else {
        // get the base name of the files to write
        final String base;
        final String suffix;
        final String s = file.getName();
        if (s.endsWith(".png")) {
          base = s.substring(0, s.lastIndexOf('.'));
          suffix = ".png";
        }
        else {
          base = s;
          suffix = "";
        }

        // calculate total tiles
        final int tcols = (int) Math.ceil((double) w / tw);
        final int trows = (int) Math.ceil((double) h / th);

        tiles = tcols * trows;

        // tile across the map with images of size tw by th.
        for (int tx = 0; tx < tcols; ++tx) {
          for (int ty = 0; ty < trows; ++ty) {
            final File f = new File(file.getParent(),
              base + "." + tx + "." + ty + suffix);

            final Rectangle r = new Rectangle(
              tw*tx,
              th*ty,
              Math.min(tw, w - tw*tx),
              Math.min(th, h - th*ty)
            );

            writeImage(f, img, r);
            ++tilesDone;
          }
        }
      }

      return null;
    }

    @Override
    protected void done() {
      try {
        get();
      }
      catch (CancellationException e) {
        // on cancellation, remove all files we created
        for (File f : files) f.delete();
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
      }
      catch (ExecutionException e) {
        final Throwable c = e.getCause();
        if (c instanceof IOException) {
          WriteErrorDialog.error(e, (IOException) c, files.get(files.size()-1));
        }
        else {
          ErrorDialog.bug(e);
        }
      }
    }
  }

  /**
   * Write a PNG-encoded snapshot of the map to the given OutputStreams,
   * dividing the map into vertical sections, one per stream
   *
   * @deprecated
   */
  @Deprecated
  public void writeImage(OutputStream[] out) throws IOException {
    Dimension buffer = map.getEdgeBuffer();
    int totalWidth =
      (int) ((map.mapSize().width - 2 * buffer.width) * map.getZoom());
    int totalHeight =
      (int) ((map.mapSize().height - 2 * buffer.height) * map.getZoom());
    for (int i = 0; i < out.length; ++i) {
      int height = totalHeight / out.length;
      if (i == out.length - 1) {
        height = totalHeight - height * (out.length - 1);
      }

      Image output = map.getView().createImage(totalWidth, height);
      Graphics2D gg = (Graphics2D) output.getGraphics();

      map.paintRegion(gg, new Rectangle(
        -(int) (map.getZoom() * buffer.width),
        -(int) (map.getZoom() * buffer.height) + height * i,
        totalWidth, totalHeight), null);
      gg.dispose();
      try {
        MediaTracker t = new MediaTracker(map.getView());
        t.addImage(output, 0);
        t.waitForID(0);
      }
      catch (Exception e) {
        logger.error("", e);
      }

      try {
        if (output instanceof RenderedImage) {
          ImageIO.write((RenderedImage) output, "png", out[i]);
        }
        else {
          throw new IOException("Bad image type");
        }
      }
      finally {
        try {
          out[i].close();
        }
        catch (IOException e) {
          logger.error("", e);
        }
      }
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "ImageCapture");
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ImageSaver.component_type"); //$NON-NLS-1$
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }
}
