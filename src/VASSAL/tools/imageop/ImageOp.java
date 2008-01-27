package VASSAL.tools.imageop;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public interface ImageOp {
  public ImageOp getSource();

  public Image apply() throws Exception;

  public Image getImage(ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  public Future<Image> getFutureImage(ImageOpObserver obs)
    throws ExecutionException;

  public Dimension getSize();

  public int getWidth();

  public int getHeight();

  public Dimension getTileSize();

  public int getTileHeight();

  public int getTileWidth();

  public int getNumXTiles();

  public int getNumYTiles();

  public Image getTile(Point p, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  public Image getTile(int tileX, int tileY, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  public Future<Image> getFutureTile(Point p, ImageOpObserver obs)
    throws ExecutionException;

  public Future<Image> getFutureTile(int tileX, int tileY, ImageOpObserver obs)
    throws ExecutionException;

  public Point[] getTileIndices(Rectangle rect);
}
