package VASSAL.tools.image.tilecache;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.concurrent.Callable;
import java.util.zip.GZIPOutputStream;

import VASSAL.tools.io.IOUtils;

public class TileWriter implements Callable<Void> {
  protected final BufferedImage src;
  protected final File dst;
  protected final int tx;
  protected final int ty;
  protected final int tw;
  protected final int th;

  public TileWriter(BufferedImage src, File dst,
                    int tx, int ty, int tw, int th) {
    this.src = src;
    this.dst = dst;
    this.tx = tx;
    this.ty = ty;
    this.tw = tw;
    this.th = th;
  }

  public Void call() throws IOException {
    OutputStream out = null;
    try {
      out = new GZIPOutputStream(
              new BufferedOutputStream(
                new FileOutputStream(dst)));
      writeTile(src, out, tx, ty, tw, th);
      out.close();
      return null;
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }

  protected static void writeTile(
    BufferedImage src,
    OutputStream out,
    int tx,
    int ty,
    int tw,
    int th)
    throws IOException
  {
    // get actual tile width, height (edge tiles might be less than full size)
    final int atw = Math.min(tw, src.getWidth() - tx*tw);  
    final int ath = Math.min(th, src.getHeight() - ty*th);

    // cut the tile from the source image
    final BufferedImage tile = new BufferedImage(
      atw, ath, BufferedImage.TYPE_INT_ARGB_PRE
    );

    final Graphics2D g = tile.createGraphics();
    g.drawImage(src, 0, 0, atw, ath, tx*tw, ty*th, tx*tw+atw, ty*th+ath, null);
    g.dispose();

    final int[] data =
      ((DataBufferInt) tile.getRaster().getDataBuffer()).getData();

    // write the tile
    final ByteBuffer buf = ByteBuffer.allocate(4*data.length + 9);
    buf.putInt(atw).putInt(ath).put((byte) 1).asIntBuffer().put(data);

    out.write(buf.array());
  }
}
