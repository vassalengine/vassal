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
  private static final long serialVersionUID = 1L;

  public LabelerDataArchiveHTMLEditorKit(DataArchive arch) {
    super(arch);
  }

  float valignImagesDefault = 0.5f;
  float valignTextDefault   = 0.5f;

  public void setValignImagesDefault(float align) {
    valignImagesDefault = align;
  }

  public void setValignTextDefault(float align) {
    valignTextDefault = align;
  }

  private float getValign(Element e, float def) {
    final AttributeSet attrs = e.getAttributes();
    final String v = (String) attrs.getAttribute(HTML.Attribute.VALIGN);

    if ("top".equals(v)) { //NON-NLS
      return 0f;
    }
    else if ("middle".equals(v)) { //NON-NLS
      return .5f;
    }
    else if ("bottom".equals(v)) { //NON-NLS
      return 1.0f;
    }
    return def;
  }

  @Override
  public ViewFactory getViewFactory() {
    return new LabelerDataArchiveHTMLFactory();
  }

  protected class LabelerInlineView extends InlineView {
    protected float valign;

    public LabelerInlineView(Element e) {
      super(e);
      valign = getValign(e, valignTextDefault);
    }

    @Override
    public float getAlignment(int axis) {
      switch (axis) {
      case View.Y_AXIS:
        return valign;
      default:
        return super.getAlignment(axis);
      }
    }
  }

  private class LabelerDataArchiveImageView extends DataArchiveImageView {
    protected float valign;

    public LabelerDataArchiveImageView(Element e) {
      super(e);
      setLoadsSynchronously(true); //BR// make sure these actually load
      valign = getValign(e, valignImagesDefault);
    }

    @Override
    public float getAlignment(int axis) {
      switch (axis) {
      case View.Y_AXIS:
        return valign;
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
