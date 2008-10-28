package VASSAL.tools.memmap;

import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.SampleModel;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.stream.FileCacheImageInputStream;
import javax.imageio.stream.ImageInputStream;

import VASSAL.build.GameModule;
import VASSAL.tools.IOUtils;
import VASSAL.tools.TempFileManager;


public class MappedImageUtils {

  private MappedImageUtils() { }

  public static MappedBufferedImage getImage(InputStream in)
                                                           throws IOException {
    final MappedBufferedImage img = loadImage(in);

    switch (img.getType()) {
    case BufferedImage.TYPE_INT_RGB:
    case BufferedImage.TYPE_INT_ARGB:
      return img;
    default:
      return toType(img, img.getTransparency() == BufferedImage.OPAQUE ?
        BufferedImage.TYPE_INT_RGB : BufferedImage.TYPE_INT_ARGB);
    }
  }

  private static MappedBufferedImage loadImage(InputStream in)
                                                           throws IOException {
    MappedBufferedImage img = null;
    ImageInputStream iis = null;
    ImageReader reader = null;
    try {
      iis = new FileCacheImageInputStream(
        in, TempFileManager.getInstance().getSessionRoot()
      );

      final Iterator<ImageReader> i = ImageIO.getImageReaders(iis);
      if (!i.hasNext()) throw new IOException("Unrecognized image format");

      reader = i.next();
      reader.setInput(iis);
  
      final int w = reader.getWidth(0);
      final int h = reader.getHeight(0);

      final ImageTypeSpecifier type = reader.getImageTypes(0).next();

      // get our ColorModel and SampleModel
      final ColorModel cm = type.getColorModel();
      final SampleModel sm =
        type.getSampleModel().createCompatibleSampleModel(w,h);
      
      img = new MappedBufferedImage(cm, sm);
      
      final ImageReadParam param = reader.getDefaultReadParam();
      param.setDestination(img);
      reader.read(0, param);

      iis.close();
      in.close();

      return img;
    }
    finally {
      reader.dispose();
      IOUtils.closeQuietly(iis);
      IOUtils.closeQuietly(in);
    }
  }

  private static MappedBufferedImage toType(MappedBufferedImage src, int type)
                                                           throws IOException {
    return rowByRowCopy(
      src, 
      new MappedBufferedImage(src.getWidth(), src.getHeight(), type)
    );
  }

  private static MappedBufferedImage rowByRowCopy(MappedBufferedImage src,
                                                  MappedBufferedImage dst) {
    final int h = src.getHeight();
    final int[] row = new int[src.getWidth()];
    for (int y = 0; y < h; ++y) {
      src.getRGB(0, y, row.length, 1, row, 0, row.length);
      dst.setRGB(0, y, row.length, 1, row, 0, row.length);
    }
    return dst;
  }
}
