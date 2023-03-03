package VASSAL.tools.image.tilecache;

public class ZipFileImageTilerState {
  private ZipFileImageTilerState() {}

  @Deprecated(since = "2023-03-03", forRemoval = true)
  public static final byte TILER_READY = 1;
  @Deprecated(since = "2023-03-03", forRemoval = true)
  public static final byte STARTING_IMAGE  = 2;
  @Deprecated(since = "2023-03-03", forRemoval = true)
  public static final byte TILE_WRITTEN    = 3;
  @Deprecated(since = "2023-03-03", forRemoval = true)
  public static final byte TILING_FINISHED = 4;
  @Deprecated(since = "2023-03-03", forRemoval = true)
  public static final byte IMAGE_FINISHED  = 5;

  public static final String READY = "TILER READY";
  public static final char IMAGE_BEGIN = 'i';
  public static final char IMAGE_END = 'I';
  public static final char TILE_END = '*';
  public static final char DONE = 'D';
}
