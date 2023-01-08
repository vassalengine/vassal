package VASSAL.tools.swing;

import java.net.URL;
import java.io.InputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.ImageView;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.tools.DataArchive;

/**
 * Extended HTML Editor kit to let the <src> tag display images from the
 * module DataArchive where no pathname is included in the image name.
 */
public class DataArchiveHTMLEditorKit extends HTMLEditorKit {
  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(DataArchiveHTMLEditorKit.class);

  private final DataArchive arch;

  public DataArchiveHTMLEditorKit(DataArchive arch) {
    this.arch = arch;
  }

  @Override
  public ViewFactory getViewFactory() {
    return new DataArchiveHTMLFactory();
  }

  private class DataArchiveImageView extends ImageView {
    public DataArchiveImageView(Element e) {
      super(e);
    }

    @Override
    public URL getImageURL() {
      final String src = (String) getElement().getAttributes().getAttribute(HTML.Attribute.SRC);

      URL url = null;
      final Path out = Info.getTempDir().toPath().resolve(src);

      try {
        if (!Files.exists(out)) {
          try (InputStream in = arch.getInputStream("images/" + src)) {
            Files.copy(in, out);
          }
          out.toFile().deleteOnExit();
        }

        url = out.toUri().toURL();
      }
      catch (IOException e) {
        logger.error("Failed to load {}", src, e);
      }

      return url;
    }
  }

  private class DataArchiveHTMLFactory extends HTMLFactory {
    @Override
    public View create(Element e) {
      final AttributeSet attrs = e.getAttributes();
      final HTML.Tag kind = (HTML.Tag) (attrs.getAttribute(StyleConstants.NameAttribute));

      if (kind == HTML.Tag.IMG) {
        final String file = (String) attrs.getAttribute(HTML.Attribute.SRC);
        // file may be null if invalid src file specified
        if (file != null && !file.isBlank() && !file.contains("/")) {
          return new DataArchiveImageView(e);
        }
      }

      return super.create(e);
    }
  }
}
