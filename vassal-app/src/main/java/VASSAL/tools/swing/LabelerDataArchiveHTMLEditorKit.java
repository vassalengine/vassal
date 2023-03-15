package VASSAL.tools.swing;

import VASSAL.tools.DataArchive;

import javax.swing.text.AttributeSet;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.text.html.InlineView;

/**
 * Extended HTML Editor kit to let the <src> tag display images from the
 * module DataArchive where no pathname is included in the image name.
 *
 * This version also allows the inline images to be vertically aligned w/r/t the text
 */
public class LabelerDataArchiveHTMLEditorKit extends DataArchiveHTMLEditorKit {

  public LabelerDataArchiveHTMLEditorKit(DataArchive arch) {
    super(arch);
  }

  float valignImages = 0.5f;
  float valignText   = 0.5f;

  public void setValignImages(float align) {
    valignImages = align;
  }

  public void setValignText(float align) {
    valignText = align;
  }

  @Override
  public ViewFactory getViewFactory() {
    return new LabelerDataArchiveHTMLFactory();
  }

  protected class LabelerInlineView extends InlineView {
    public LabelerInlineView(Element e) {
      super(e);
    }

    public float getAlignment(int axis) {
      switch (axis) {
      case View.Y_AXIS:
        return valignText;
      default:
        return super.getAlignment(axis);
      }
    }
  }

  private class LabelerDataArchiveImageView extends DataArchiveImageView {
    public LabelerDataArchiveImageView(Element e) {
      super(e);
      setLoadsSynchronously(true); //BR// make sure these actually load
    }

    public float getAlignment(int axis) {
      switch (axis) {
      case View.Y_AXIS:
        return valignImages;
      default:
        return super.getAlignment(axis);
      }
    }
  }

  private class LabelerDataArchiveHTMLFactory extends DataArchiveHTMLFactory {
    @Override
    public View create(Element e) {
      final AttributeSet attrs = e.getAttributes();
      final HTML.Tag kind = (HTML.Tag) (attrs.getAttribute(StyleConstants.NameAttribute));

      if (kind == HTML.Tag.CONTENT) {
        return new LabelerInlineView(e);
      }

      if (kind == HTML.Tag.IMG) {
        final String file = (String) attrs.getAttribute(HTML.Attribute.SRC);
        // file may be null if invalid src file specified
        if (file != null && !file.isBlank() && !file.contains("/")) {
          return new LabelerDataArchiveImageView(e);
        }
      }

      return super.create(e);
    }
  }
}
