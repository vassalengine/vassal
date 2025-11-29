package VASSAL.build.module.videoexport;

import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Thin wrapper around an ffmpeg process that accepts raw BGR frames.
 */
class FfmpegWriter implements AutoCloseable {
  private final Process process;
  private final OutputStream output;

  FfmpegWriter(int width, int height, int fps, File outputFile) throws IOException {
    process = new ProcessBuilder(
      "ffmpeg",
      "-y",
      "-f", "rawvideo",
      "-v", "debug",
      "-pix_fmt", "bgr24",
      "-s", width + "x" + height,
      "-r", Integer.toString(fps),
      "-i", "-",
      "-an",
      "-c:v", "libx264",
      "-pix_fmt", "yuv420p",
      outputFile.getAbsolutePath()
    ).redirectError(ProcessBuilder.Redirect.INHERIT).start();
    output = process.getOutputStream();
  }

  void writeFrame(BufferedImage frame) throws IOException {
    final byte[] data = ((DataBufferByte) frame.getRaster().getDataBuffer()).getData();
    output.write(data);
  }

  @Override
  public void close() throws IOException {
    try {
      output.close();
      process.waitFor();
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
  }
}
