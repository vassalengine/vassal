package VASSAL.tools.image.tilecache;

public class ZipFileImageTilerState {
  private ZipFileImageTilerState() {}

  public static final byte TILER_READY = 1;
  public static final byte STARTING_IMAGE  = 2;
  public static final byte TILE_WRITTEN    = 3;
  public static final byte TILING_FINISHED = 4;
}
